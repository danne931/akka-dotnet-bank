[<RequireQualifiedAccess>]
module SagaAlarmClockActor

open Akka.Actor
open Akka.Hosting
open Akka.Streams
open Akkling.Streams
open Akkling
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open ActorUtil
open Lib.Types
open Lib.Postgres
open Lib.Saga
open AppSagaSqlMapper

[<RequireQualifiedAccess>]
type Message = | WakeUpIfUnfinishedBusiness

let actorProps
   (system: ActorSystem)
   (getSagaActor: unit -> IActorRef<AppSaga.AppSagaMessage>)
   (getTimedOutSagas: unit -> Async<Result<Option<CorrelationId list>, Err>>)
   (throttle: StreamThrottleEnvConfig)
   =
   let mat = system.Materializer()

   let handler (ctx: Actor<Message>) = actor {
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
            getSagaActor ()
            <! GuaranteedDelivery.message
                  (CorrelationId.get sagaId)
                  SagaMessage.CheckForRemainingWork)

      return ignored ()
   }

   props handler

let get (system: ActorSystem) : IActorRef<Message> =
   typed <| ActorRegistry.For(system).Get<ActorMetadata.SagaAlarmClockMarker>()

(*
Indicates that a saga actor will not attempt to retry any unfinished work
and is asleep (passivated & no longer in memory).
Get all sagas which need to be woken up to process unfinished business.
NOTE:
*)
let getSleepingSagas () = asyncResultOption {
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
      """

   let! sagaIds = pgQuery<CorrelationId> query None Reader.id
   return sagaIds
}

let initProps
   (system: ActorSystem)
   (getSagaActor: unit -> IActorRef<AppSaga.AppSagaMessage>)
   =
   actorProps
      system
      getSagaActor
      getSleepingSagas
      Env.config.SleepingSagaThrottle
