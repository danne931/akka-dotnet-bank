[<RequireQualifiedAccess>]
module SagaAlarmClockActor

open Akka.Actor
open Akkling.Streams
open Akkling
open Akkling.Cluster.Sharding
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.Postgres
open Lib.Saga
open AppSagaSqlMapper

let actorProps
   (system: ActorSystem)
   (getSagaActor: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   (getTimedOutSagas: unit -> Async<Result<Option<CorrelationId list>, Err>>)
   =
   let mat = system.Materializer()

   let handler (ctx: Actor<AppSaga.SagaAlarmClockMessage>) = actor {
      let! _ = ctx.Receive()

      logInfo
         ctx
         "Check for txn sagas which have remaining activities that timed out."

      do!
         getTimedOutSagas ()
         |> Source.ofAsync
         |> Source.choose (fun res ->
            match res with
            | Error e ->
               logError ctx $"Error fetching timed out txn sagas {e}"
               None
            | Ok opt ->
               match opt with
               | None ->
                  logWarning ctx "No timed out txn sagas with unfinished work."
               | Some ids ->
                  logInfo
                     ctx
                     $"Check for remaining work for {ids.Length} sagas."

               opt)
         |> Source.collect id
         |> Source.runForEach mat (fun sagaId ->
            getSagaActor sagaId <! SagaMessage.CheckForRemainingWork)

      return ignored ()
   }

   props handler

(*
Indicates that a saga actor will not attempt to retry any unfinished work
and is asleep (passivated & no longer in memory).
Get all sagas which need to be woken up to process unfinished business.
*)
let getSleepingSagas (limit: int) = asyncResultOption {
   let query =
      $"""
      SELECT {Fields.id} FROM {table}
      WHERE
         {Fields.status} IN ('Scheduled', 'InProgress', 'Compensating')
         AND {Fields.inactivityTimeout} IS NOT NULL
         -- Indicates some time, greater than the inactivity timeout, has
         -- elapsed since a saga action was invoked.  Time to wake the saga
         -- and evaluate remaining work.
         AND ({Fields.updatedAt} + {Fields.inactivityTimeout}) < current_timestamp
      LIMIT @limit
      """

   let qParams = [ "limit", Sql.int limit ]

   let! sagaIds = pgQuery<CorrelationId> query (Some qParams) Reader.id
   return sagaIds
}

let initProps
   (system: ActorSystem)
   (getSagaActor: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   =
   actorProps system getSagaActor (fun () ->
      getSleepingSagas Env.config.SagaWakeFromSleepBurstLimit)
