[<RequireQualifiedAccess>]
module BillingCycleBulkWriteActor

open System
open Akka.Actor
open Akka.Hosting
open Akka.Dispatch
open Akka.Configuration
open Akka.Streams
open Akkling

open BillingStatement
open Bank.BillingCycle.Api
open Bank.Account.Domain
open ActorUtil

let private forwardToAccount
   (getAccountRef: EntityRefGetter<AccountMessage>)
   (id: string)
   =
   let idOpt =
      try
         Guid id |> Some
      with _ ->
         None

   if idOpt.IsSome then
      getAccountRef idOpt.Value <! AccountMessage.BillingCycle

let private fanOutBillingCycleMessage
   system
   (getAccountRef: EntityRefGetter<AccountMessage>)
   =
   task {
      do!
         ActorUtil
            .readJournal(system)
            .CurrentPersistenceIds()
            .RunForeach(forwardToAccount getAccountRef, system.Materializer())

      return BillingCycleFinished
   }

// Configure mailbox so that PersistBillingStatements
// messages are sent to the front.
type PriorityMailbox(settings: Settings, config: Config) =
   inherit UnboundedPriorityMailbox(settings, config)

   override x.PriorityGenerator(message: obj) =
      match message with
      | :? BillingMessage as msg ->
         match msg with
         | PersistBillingStatements -> 0
         | RegisterBillingStatement _ -> 1
         | GetWriteReadyStatements -> 2
         | BillingCycleFanout -> 3
         | BillingCycleFinished -> 4
      | _ -> 33

type BulkWriteState = {
   Billing: BillingStatement list
   IsScheduled: bool
}

let private schedulePersist (ctx: Actor<BillingMessage>) (seconds: float) =
   ctx.Schedule (TimeSpan.FromSeconds seconds) ctx.Self PersistBillingStatements
   |> ignore

let batchSizeLimit = 1000
let initState = { Billing = []; IsScheduled = false }

// Collects billing statement records to insert in batches.
// Billing statements are persisted in a single transaction
// once the batch size limit is reached or after some duration
// in seconds from the initial RegisterBillingStatement message.
let actorProps
   (getAccountRef: EntityRefGetter<AccountMessage>)
   (persistence: BillingPersistence)
   =
   let handler (ctx: Actor<BillingMessage>) =
      let schedulePersist = schedulePersist ctx
      let logInfo, logError = logInfo ctx, logError ctx

      let rec loop (state: BulkWriteState) =
         function
         | GetWriteReadyStatements ->
            ctx.Sender() <! state
            ignored ()
         | RegisterBillingStatement statement ->
            let newState = {
               state with
                  Billing = statement :: state.Billing
            }

            if newState.Billing.Length >= batchSizeLimit then
               ctx.Self <! PersistBillingStatements
               become <| loop newState
            elif state.IsScheduled then
               become <| loop newState
            else
               schedulePersist 5.

               become <| loop { newState with IsScheduled = true }
         | PersistBillingStatements ->
            if state.Billing.Length > 0 then
               let res = persistence.saveBillingStatements(state.Billing).Result

               match res with
               | Ok o ->
                  logInfo $"Saved billing statements {o}"
                  become <| loop initState
               | Error e ->
                  logError $"Error saving billing statements {e}"
                  schedulePersist 10.
                  unhandled ()
            else
               become <| loop initState
         | BillingCycleFanout ->
            logInfo "Start billing cycle"

            fanOutBillingCycleMessage ctx.System getAccountRef
            |> Async.AwaitTask
            |!> retype ctx.Self
            |> ignored
         | BillingCycleFinished ->
            logInfo "Billing cycle finished"
            ignored ()

      loop initState

   {
      (props <| actorOf2 handler) with
         Mailbox = Some "billing-cycle-bulk-write-mailbox"
   }

let get (system: ActorSystem) : IActorRef<BillingMessage> =
   typed
   <| ActorRegistry.For(system).Get<ActorMetadata.BillingCycleBulkWriteMarker>()

let initProps (getAccountRef: EntityRefGetter<AccountMessage>) =
   actorProps getAccountRef {
      saveBillingStatements = saveBillingStatements
   }
