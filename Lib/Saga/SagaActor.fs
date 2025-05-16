[<RequireQualifiedAccess>]
module SagaActor

open Akka.Actor
open Akka.Persistence
open Akka.Persistence.Extras
open Akkling
open Akkling.Persistence
open Akkling.Cluster.Sharding
open System

open Lib.SharedTypes
open Lib.Types
open ActorUtil
open Lib.Saga

type SagaHandler<'Saga, 'SagaEvent when 'Saga :> ISaga> = {
   getEvaluateRemainingWorkEvent: 'Saga -> 'SagaEvent
   getResetInProgressActivitiesEvent: 'Saga -> 'SagaEvent
   stateTransition:
      'Saga option
         -> 'SagaEvent
         -> DateTime
         -> Result<'Saga option, SagaStateTransitionError>
   applyEvent: 'Saga option -> 'SagaEvent -> DateTime -> 'Saga option
   onEventPersisted:
      Eventsourced<obj> -> 'SagaEvent -> 'Saga option -> 'Saga -> unit
}

let actorProps<'Saga, 'SagaEvent when 'Saga :> ISaga>
   (sagaPassivateIdleEntityAfter: TimeSpan)
   (sagaHandler: SagaHandler<'Saga, 'SagaEvent>)
   =
   let handler (mailbox: Eventsourced<obj>) =
      let mutable workCheckTimer: ICancelable option = None

      let setWorkCheckTimer (delay: TimeSpan) =
         workCheckTimer <-
            mailbox.Schedule
               delay
               mailbox.Self
               SagaMessage<'SagaEvent>.CheckForRemainingWork
            |> Some

      let rec loop (state: 'Saga option) = actor {
         let! msg = mailbox.Receive()

         match msg with
         | Persisted mailbox (:? SagaEvent<'SagaEvent> as e) ->
            let priorState = state
            let updatedState = sagaHandler.applyEvent state e.Data e.Timestamp

            match updatedState with
            | Some state ->
               sagaHandler.onEventPersisted mailbox e.Data priorState state
               return! loop updatedState
            | None ->
               logError
                  mailbox
                  $"Persisted event which transitioned saga state to None.  Check your saga handler stateTransition implementation. {mailbox.Pid}"

               return unhandled ()
         | :? SnapshotOffer as o -> return! loop (unbox o.Snapshot)
         | :? ConfirmableMessageEnvelope as envelope ->
            let unknownMsg msg =
               logError
                  mailbox
                  $"Unknown message in ConfirmableMessageEnvelope - {msg}"

               unhandled ()

            match envelope.Message with
            | :? SagaMessage<'SagaEvent> as msg ->
               match msg with
               | SagaMessage.Event evt ->
                  let validation =
                     sagaHandler.stateTransition state evt.Data evt.Timestamp

                  match validation with
                  | Error err ->
                     logWarning
                        mailbox
                        $"Will not persist saga event {evt.GetType().FullName} due to {err} {mailbox.Pid}"

                     return ignored ()
                  | Ok _ ->
                     return! confirmPersist mailbox evt envelope.ConfirmationId
               | _ -> return unknownMsg msg
            | msg -> return unknownMsg msg
         | :? SagaMessage<'SagaEvent> as msg ->
            match msg with
            | SagaMessage.GetSaga ->
               mailbox.Sender() <! state
               return ignored ()
            // Useful for manually retrying activities which will not
            // attempt to retry automatically due to attempting the action
            // MaxAttempts times.
            | SagaMessage.ResetInProgressActivities ->
               return
                  match state with
                  | Some saga ->
                     if saga.ActivityAttemptsExhaustedCount = 0 then
                        ignored ()
                     else
                        Persist(
                           SagaEvent.create
                              saga.OrgId
                              saga.SagaId
                              (sagaHandler.getResetInProgressActivitiesEvent
                                 saga)
                        )
                  | None -> ignored ()
            | SagaMessage.CheckForRemainingWork ->
               return
                  match state with
                  | Some saga ->
                     if saga.ActivityInProgressCount = 0 then
                        logDebug
                           mailbox
                           $"Saga complete - passivate {mailbox.Pid}"

                        passivate ()
                     elif saga.ExhaustedAllAttempts then
                        logWarning
                           mailbox
                           $"Exhausted saga attempts {mailbox.Pid}"

                        passivate ()
                     else
                        match saga.InactivityTimeout with
                        | None ->
                           logDebug
                              mailbox
                              $"Wait for external stimulus {mailbox.Pid}"

                           passivate ()
                        | Some timeout ->
                           if
                              saga.ActivityRetryableAfterInactivityCount > 0
                           then
                              logDebug
                                 mailbox
                                 $"Evaluate remaining saga work {mailbox.Pid}"

                              if timeout < sagaPassivateIdleEntityAfter then
                                 setWorkCheckTimer timeout

                              Persist(
                                 SagaEvent.create
                                    saga.OrgId
                                    saga.SagaId
                                    (sagaHandler.getEvaluateRemainingWorkEvent
                                       saga)
                              )
                           else
                              logDebug
                                 mailbox
                                 $"No remaining work at this time {mailbox.Pid}"

                              if timeout < sagaPassivateIdleEntityAfter then
                                 setWorkCheckTimer timeout
                                 ignored ()
                              else
                                 // If timeout is greater than the automatic saga actor
                                 // passivation then just passivate immediately and
                                 // wait for the SagaAlarmClockActor to wake the
                                 // actor back up to check for remaining work.
                                 passivate ()
                  | None ->
                     logWarning
                        mailbox
                        $"Checking for remaining work when state is None {mailbox.Pid}"

                     ignored ()
         // Event replay on actor start
         | :? SagaEvent<'SagaEvent> as e when mailbox.IsRecovering() ->
            return! loop <| sagaHandler.applyEvent state e.Data e.Timestamp
         | msg ->
            PersistentActorEventHandler.handleEvent
               {
                  PersistentActorEventHandler.init with
                     LifecyclePreStart =
                        fun _ ->
                           setWorkCheckTimer (TimeSpan.FromSeconds 10)
                           ignored ()
                     LifecyclePostStop =
                        fun _ ->
                           logDebug
                              mailbox
                              $"TRANSACTION POSTSTOP {mailbox.Pid}"

                           workCheckTimer |> Option.iter _.Cancel()

                           ignored ()
               }
               mailbox
               msg
      }

      loop None

   propsPersist handler

let get<'SagaEvent>
   (sys: ActorSystem)
   (correlationId: CorrelationId)
   : IEntityRef<SagaMessage<'SagaEvent>>
   =
   getEntityRef
      sys
      ClusterMetadata.sagaShardRegion
      (CorrelationId.get correlationId)

let isPersistableMessage<'SagaEvent> (msg: obj) =
   match msg with
   | :? SagaMessage<'SagaEvent> as msg ->
      match msg with
      | SagaMessage.Event _ -> true
      | _ -> false
   | _ -> false

let initProps<'Saga, 'SagaEvent when 'Saga :> ISaga>
   (supervisorOpts: PersistenceSupervisorOptions)
   (sagaPassivateIdleEntityAfter: TimeSpan)
   (persistenceId: string)
   (sagaHandler: SagaHandler<'Saga, 'SagaEvent>)
   =
   persistenceSupervisor
      supervisorOpts
      isPersistableMessage<'SagaEvent>
      (actorProps sagaPassivateIdleEntityAfter sagaHandler)
      persistenceId
