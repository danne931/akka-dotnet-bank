[<RequireQualifiedAccess>]
module SagaAlarmClockActor

open Akka.Actor
open Akka.Hosting
open Akka.Streams
open Akkling.Streams
open Akkling
open Akkling.Cluster.Sharding
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open ActorUtil
open Lib.Types
open Lib.Postgres
open Lib.Saga
open AppSagaSqlMapper

let actorProps
   (system: ActorSystem)
   (getSagaActor: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   (getTimedOutSagas: unit -> Async<Result<Option<CorrelationId list>, Err>>)
   (throttle: StreamThrottle)
   =
   let mat = system.Materializer()

   let handler (ctx: Actor<SagaAlarmClockMessage>) = actor {
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
         |> Source.throttle
               ThrottleMode.Shaping
               throttle.Burst
               throttle.Count
               throttle.Duration
         |> Source.runForEach mat (fun sagaId ->
            let actorRef = getSagaActor sagaId
            actorRef <! SagaMessage.CheckForRemainingWork)

      return ignored ()
   }

   props handler

let get (system: ActorSystem) : IActorRef<SagaAlarmClockMessage> =
   typed <| ActorRegistry.For(system).Get<ActorMetadata.SagaAlarmClockMarker>()

(*
Indicates that a saga actor will not attempt to retry any unfinished work
and may be asleep (passivated & no longer in memory).
Get all sagas which need to be woken up to process unfinished business.
*)
let getSleepingSagas () = asyncResultOption {
   let query =
      $"""
      SELECT {Fields.id} FROM {table}
      WHERE
         {Fields.activityInProgressCount} > 0
         AND {Fields.inactivityTimeout} IS NOT NULL
         AND ({Fields.updatedAt} + {Fields.inactivityTimeout}) < current_timestamp
   """

   let! sagaIds = pgQuery<CorrelationId> query None Reader.id
   return sagaIds
}

let initProps
   (system: ActorSystem)
   (getSagaActor: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   =
   actorProps
      system
      getSagaActor
      getSleepingSagas
      Env.config.SleepingSagaThrottle
