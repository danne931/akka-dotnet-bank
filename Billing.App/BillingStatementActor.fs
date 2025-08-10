[<RequireQualifiedAccess>]
module BillingStatementActor

open System
open System.Threading.Tasks
open Akka.Actor
open Akkling
open Akka.Streams
open Akkling.Streams

open BillingStatement
open BillingSqlMapper
open BillingSaga
open ActorUtil
open Lib.Postgres
open Lib.SharedTypes
open Lib.Types
open Lib.BulkWriteStreamFlow
open BankActorRegistry

type BillingPersistence = {
   saveBillingStatements: BillingPersistable list -> Task<Result<int list, Err>>
}

let initQueueSource
   (system: ActorSystem)
   (persistence: BillingPersistence)
   (chunking: StreamChunkingEnvConfig)
   (restartSettings: Akka.Streams.RestartSettings)
   (retryAfter: TimeSpan)
   (registry: #ISagaGuaranteedDeliveryActor)
   (billingStatementActorRef: IActorRef<obj>)
   =
   let flow, bulkWriteRef =
      initBulkWriteFlow<BillingPersistable> system restartSettings {
         RetryAfter = retryAfter
         persist =
            fun (statements: BillingPersistable seq) ->
               statements |> List.ofSeq |> persistence.saveBillingStatements
         // Feed failed inserts back into the stream
         onRetry =
            fun (statements: BillingPersistable seq) ->
               for bill in statements do
                  billingStatementActorRef
                  <! BillingStatementMessage.BulkPersist bill
         onPersistOk =
            fun statements response ->
               SystemLog.info system $"Saved billing statements {response}"

               for s in statements do
                  let msg =
                     BillingSagaEvent.BillingStatementPersisted
                     |> AppSaga.Message.billing s.OrgId s.CorrelationId
                     |> AppSaga.Message.guaranteedDelivery s.CorrelationId

                  registry.SagaGuaranteedDeliveryActor() <! msg
      }

   let source =
      Source.queue OverflowStrategy.Backpressure 1000
      |> Source.groupedWithin chunking.Size chunking.Duration
      |> Source.via flow

   bulkWriteRef, source

let actorProps
   (system: ActorSystem)
   (persistence: BillingPersistence)
   (chunking: StreamChunkingEnvConfig)
   (restartSettings: Akka.Streams.RestartSettings)
   (retryFailedPersistenceAfter: TimeSpan)
   registry
   =
   let rec init (ctx: Actor<obj>) = actor {
      let! msg = ctx.Receive()

      match msg with
      | LifecycleEvent e ->
         match e with
         | PreStart ->
            logInfo ctx "Prestart - Init BillingStatementActor Queue Source"

            let bulkWriteRef, queueSource =
               initQueueSource
                  system
                  persistence
                  chunking
                  restartSettings
                  retryFailedPersistenceAfter
                  registry
                  ctx.Self

            let queue =
               queueSource
               |> Source.toMat Sink.ignore Keep.left
               |> Graph.run (system.Materializer())

            return! processing ctx queue bulkWriteRef
         | _ -> return ignored ()
      | msg ->
         logError ctx $"Unknown msg {msg}"
         return unhandled ()
   }

   and processing
      (ctx: Actor<obj>)
      (queue: ISourceQueueWithComplete<BillingPersistable>)
      (bulkWriteRef: IActorRef<BulkWriteMessage<BillingPersistable>>)
      =
      actor {
         let! msg = ctx.Receive()

         match msg with
         | :? BillingStatementMessage as msg ->
            match msg with
            | BillingStatementMessage.BulkPersist persistable ->
               let! result = queueOffer<BillingPersistable> queue persistable

               return
                  match result with
                  | Ok effect -> effect
                  | Error errMsg -> failwith errMsg
            | BillingStatementMessage.GetFailedWrites ->
               let! failedWrites =
                  bulkWriteRef <? BulkWriteMessage.GetFailedWrites

               ctx.Sender() <! failedWrites
         | LifecycleEvent e ->
            match e with
            | PostStop ->
               queue.Complete()
               return! init ctx
            | _ -> return ignored ()
         | msg ->
            logError ctx $"Unknown msg {msg}"
            return unhandled ()
      }

   props init

let private billingStatementSqlParams (bill: BillingStatement) = [
   "@transactions", BillingSqlWriter.transactions bill.Transactions
   "@month", BillingSqlWriter.month bill.Month
   "@year", BillingSqlWriter.year bill.Year
   "@balance", BillingSqlWriter.balance bill.Balance
   "@accountName", BillingSqlWriter.accountName bill.AccountName
   "@accountId", BillingSqlWriter.accountId bill.AccountId
   "@parentAccountId", BillingSqlWriter.parentAccountId bill.ParentAccountId
   "@orgId", BillingSqlWriter.orgId bill.OrgId
]

let private snapshotStoreSqlParams (o: BillingPersistable) = [
   "@persistenceId", Sql.text <| string o.ParentAccountId
   "@sequenceNumber", Sql.int64 o.LastPersistedEventSequenceNumber
   "@created", Sql.int64 <| DateTime.UtcNow.ToFileTimeUtc()
   "@snapshot", Sql.bytea o.ParentAccountSnapshot
   "@serializerId", Sql.int 931
   "@manifest", Sql.string "ParentAccountSnapshot"
]

// NOTE:
// Taking a snapshot used to be implemented in the AccountActor application logic.
// These DB ops would occur for each account during a billing cycle,
// leading to Postgres connection pool errors.
// To fix this I have removed this application logic from the AccountActor,
// instead opting to carry out these DB ops in a transaction along with
// insertion of the billing statements.
let private saveBillingStatements (items: BillingPersistable list) =
   let sqlParams =
      items
      |> List.fold
         (fun acc item -> {|
            Billing =
               acc.Billing
               @ (item.Statements |> List.map billingStatementSqlParams)
            SnapshotStore = snapshotStoreSqlParams item :: acc.SnapshotStore
         |})
         {| Billing = []; SnapshotStore = [] |}

   pgTransaction [
      $"""
      INSERT into {BillingSqlMapper.table}
         ({BillingFields.transactions},
          {BillingFields.month},
          {BillingFields.year},
          {BillingFields.balance},
          {BillingFields.accountName},
          {BillingFields.accountId},
          {BillingFields.parentAccountId},
          {BillingFields.orgId})
      VALUES
         (@transactions,
          @month,
          @year,
          @balance,
          @accountName,
          @accountId,
          @parentAccountId,
          @orgId)
      """,
      sqlParams.Billing

      """
      INSERT into akka_snapshots
         (persistence_id,
          sequence_number,
          created,
          snapshot,
          serializer_id,
          manifest)
      VALUES
         (@persistenceId,
          @sequenceNumber,
          @created,
          @snapshot,
          @serializerId,
          @manifest)
      """,
      sqlParams.SnapshotStore
   ]

/// Collects monthly billing statements & parent account snapshots to insert in batches.
/// Persisted in a single transaction once the batch size limit is reached or
/// after some duration in seconds from the initial BillingStatement message.
let start
   (system: ActorSystem)
   (chunking: StreamChunkingEnvConfig)
   (restartSettings: Akka.Streams.RestartSettings)
   (retryFailedPersistenceAfter: TimeSpan)
   registry
   : IActorRef<BillingStatementMessage>
   =
   spawn system ActorMetadata.billingStatement.Name
   <| actorProps
         system
         {
            saveBillingStatements = saveBillingStatements
         }
         chunking
         restartSettings
         retryFailedPersistenceAfter
         registry
   |> retype
