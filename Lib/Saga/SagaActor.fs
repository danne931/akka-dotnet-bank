[<RequireQualifiedAccess>]
module SagaActor

open Akka.Actor
open Akka.Persistence
open Akka.Persistence.Extras
open Akka.Delivery
open Akkling
open Akkling.Persistence
open Akkling.Cluster.Sharding
open System

open Lib.Types
open Lib.Saga

type SagaHandler<'Saga, 'SagaStartEvent, 'SagaEvent when 'Saga :> ISaga> = {
   getEvaluateRemainingWorkEvent: 'Saga -> 'SagaEvent
   getResetInProgressActivitiesEvent: 'Saga -> 'SagaEvent
   applyStartEvent: 'SagaStartEvent -> DateTime -> 'Saga
   applyEvent: 'Saga -> 'SagaEvent -> DateTime -> 'Saga
   stateTransitionStart:
      'SagaStartEvent -> DateTime -> Result<'Saga, SagaStateTransitionError>
   stateTransition:
      'Saga -> 'SagaEvent -> DateTime -> Result<'Saga, SagaStateTransitionError>
   onStartEventPersisted: Eventsourced<obj> -> 'SagaStartEvent -> 'Saga -> unit
   onEventPersisted: Eventsourced<obj> -> 'SagaEvent -> 'Saga -> 'Saga -> unit
}

let actorProps<'Saga, 'SagaStartEvent, 'SagaEvent when 'Saga :> ISaga>
   (sagaPassivateIdleEntityAfter: TimeSpan)
   (guaranteedDeliveryConsumerControllerRef:
      Option<
         IActorRef<
            ConsumerController.IConsumerCommand<
               SagaMessage<'SagaStartEvent, 'SagaEvent>
             >
          >
       >)
   (sagaHandler: SagaHandler<'Saga, 'SagaStartEvent, 'SagaEvent>)
   =
   let persistableStartEvent =
      SagaPersistableEvent<'SagaStartEvent, 'SagaEvent>.StartEvent >> box

   let persistableEvent =
      SagaPersistableEvent<'SagaStartEvent, 'SagaEvent>.Event >> box

   let handler (ctx: Eventsourced<obj>) =
      let logWarning msg = logWarning ctx $"{msg}"
      let logError msg = logError ctx $"{msg}"
      let logDebug msg = logDebug ctx $"{msg}"

      let mutable workCheckTimer: ICancelable option = None

      let setWorkCheckTimer (delay: TimeSpan) =
         workCheckTimer <-
            ctx.Schedule
               delay
               ctx.Self
               SagaMessage<'SagaStartEvent, 'SagaEvent>.CheckForRemainingWork
            |> Some

      let rec loop (state: 'Saga option) = actor {
         let! msg = ctx.Receive()

         match msg with
         | Persisted ctx (:? SagaPersistableEvent<'SagaStartEvent, 'SagaEvent> as e) ->
            match state, e with
            | Some _, SagaPersistableEvent.StartEvent _ ->
               logError SagaStateTransitionError.HasAlreadyStarted
               return unhandled ()
            | None, SagaPersistableEvent.Event _ ->
               logError SagaStateTransitionError.HasNotStarted
               return unhandled ()
            | None, SagaPersistableEvent.StartEvent e ->
               let newState = sagaHandler.applyStartEvent e.Data e.Timestamp
               sagaHandler.onStartEventPersisted ctx e.Data newState
               return! loop (Some newState)
            | Some existingState, SagaPersistableEvent.Event e ->
               let updatedState =
                  sagaHandler.applyEvent existingState e.Data e.Timestamp

               sagaHandler.onEventPersisted
                  ctx
                  e.Data
                  existingState
                  updatedState

               return! loop (Some updatedState)
         | :? SnapshotOffer as o -> return! loop (unbox o.Snapshot)
         | :? ConsumerController.Delivery<
            SagaMessage<'SagaStartEvent, 'SagaEvent>
            > as msg ->
            GuaranteedDelivery.ack msg

            // Send message to parent actor (Persistence Supervisor)
            // for message command to confirmed event persistence.
            ctx.Parent() <! msg.Message
         | :? ConfirmableMessageEnvelope as envelope ->
            let unhandledMsg msg =
               logError
                  $"Unhandled message in ConfirmableMessageEnvelope - {msg}"

               unhandled ()

            match envelope.Message with
            | :? SagaMessage<'SagaStartEvent, 'SagaEvent> as msg ->
               match msg with
               | SagaMessage.Start startEvent ->
                  match state with
                  | Some _ ->
                     logWarning SagaStateTransitionError.HasAlreadyStarted
                     return ignored ()
                  | None ->
                     match
                        sagaHandler.stateTransitionStart
                           startEvent.Data
                           startEvent.Timestamp
                     with
                     | Error err ->
                        logWarning
                           $"Will not persist saga start event due to {err}"

                        return ignored ()
                     | Ok _ ->
                        return!
                           ActorUtil.confirmPersist
                              ctx
                              envelope.ConfirmationId
                              (persistableStartEvent startEvent)
               | SagaMessage.Event evt ->
                  match state with
                  | None ->
                     logWarning SagaStateTransitionError.HasNotStarted
                     return ignored ()
                  | Some currentState ->
                     match
                        sagaHandler.stateTransition
                           currentState
                           evt.Data
                           evt.Timestamp
                     with
                     | Error err ->
                        logWarning $"Will not persist saga event due to {err}"
                        return ignored ()
                     | Ok _ ->
                        return!
                           ActorUtil.confirmPersist
                              ctx
                              envelope.ConfirmationId
                              (persistableEvent evt)
               | other -> unhandledMsg other
            | other -> unhandledMsg other
         | :? SagaMessage<'SagaStartEvent, 'SagaEvent> as msg ->
            match msg with
            | SagaMessage.GetSaga ->
               ctx.Sender() <! state
               return ignored ()
            // Useful for manually retrying activities which will not
            // attempt to retry automatically due to attempting the action
            // MaxAttempts times.
            | SagaMessage.ResetInProgressActivities ->
               return
                  match state with
                  | Some saga when saga.ActivityAttemptsExhaustedCount = 0 ->
                     ignored ()
                  | Some saga ->
                     Persist(
                        SagaEvent.create
                           saga.OrgId
                           saga.SagaId
                           (sagaHandler.getResetInProgressActivitiesEvent saga)
                        |> persistableEvent
                     )
                  | None -> ignored ()
            | SagaMessage.CheckForRemainingWork ->
               return
                  match state with
                  | None ->
                     logWarning "Checking for remaining work when state is None"
                     ignored ()
                  | Some saga when saga.ActivityInProgressCount = 0 ->
                     logDebug "Saga complete - passivate"
                     passivate ()
                  | Some saga when saga.ExhaustedAllAttempts ->
                     logWarning "Exhausted saga attempts"
                     passivate ()
                  | Some saga ->
                     match saga.InactivityTimeout with
                     | None ->
                        logDebug "Wait for external saga stimulus"
                        passivate ()
                     | Some timeout when
                        saga.ActivityRetryableAfterInactivityCount > 0
                        ->
                        logDebug "Evaluate remaining saga work"

                        if timeout < sagaPassivateIdleEntityAfter then
                           setWorkCheckTimer timeout

                        Persist(
                           SagaEvent.create
                              saga.OrgId
                              saga.SagaId
                              (sagaHandler.getEvaluateRemainingWorkEvent saga)
                           |> persistableEvent
                        )
                     | Some timeout when timeout < sagaPassivateIdleEntityAfter ->
                        logDebug "No remaining saga work at this time"
                        setWorkCheckTimer timeout
                        ignored ()
                     | Some _ ->
                        logDebug
                           "No remaining saga work at this time. Time for a nap."
                        // If timeout is greater than the automatic saga actor
                        // passivation then just passivate immediately and
                        // wait for the SagaAlarmClockActor to wake the
                        // actor back up to check for remaining work.
                        passivate ()
            // Some messages are sent through traditional AtMostOnceDelivery via
            // a reference to the cluster sharded entity ref rather than Akka.Delivery
            // AtLeastOnceDelivery producer ref so will not hit the
            // ConsumerController.Delivery match case above so need to send message
            // to parent actor (Persistence Supervisor) so the command gets wrapped in a
            // ConfirmableMessageEnvelope for Akka.Persistence.Extras.Confirmation/
            | SagaMessage.Start _
            | SagaMessage.Event _ ->
               if guaranteedDeliveryConsumerControllerRef.IsSome then
                  logError
                     "Expects saga message to be delivered in GuaranteedDelivery.Message envelope."

                  unhandled ()
               else
                  ctx.Parent() <! msg
                  return ignored ()

         // Event replay on actor start
         | :? SagaPersistableEvent<'SagaStartEvent, 'SagaEvent> as e when
            ctx.IsRecovering()
            ->
            match state, e with
            | Some _, SagaPersistableEvent.StartEvent _ ->
               logError SagaStateTransitionError.HasAlreadyStarted
               return ignored ()
            | None, SagaPersistableEvent.Event _ ->
               logError SagaStateTransitionError.HasNotStarted
               return! ignored ()
            | None, SagaPersistableEvent.StartEvent e ->
               let newState = sagaHandler.applyStartEvent e.Data e.Timestamp
               return! loop (Some newState)
            | Some currentState, SagaPersistableEvent.Event e ->
               let updatedState =
                  sagaHandler.applyEvent currentState e.Data e.Timestamp

               return! loop (Some updatedState)
         | msg ->
            ActorUtil.PersistentActorEventHandler.handleEvent
               {
                  ActorUtil.PersistentActorEventHandler.init with
                     LifecyclePreStart =
                        fun _ ->
                           setWorkCheckTimer (TimeSpan.FromSeconds 10.)

                           let startConsumerCtrlMsg =
                              new ConsumerController.Start<
                                 SagaMessage<'SagaStartEvent, 'SagaEvent>
                               >(
                                 untyped ctx.Self
                              )

                           guaranteedDeliveryConsumerControllerRef
                           |> Option.iter (fun aref ->
                              aref <! startConsumerCtrlMsg)

                           ignored ()
                     LifecyclePostStop =
                        fun _ ->
                           logDebug "SAGA POSTSTOP"

                           workCheckTimer |> Option.iter _.Cancel()

                           ignored ()
               }
               ctx
               msg
      }

      loop None

   propsPersist handler

let isPersistableMessage<'SagaStartEvent, 'SagaEvent> (msg: obj) =
   match msg with
   | :? SagaMessage<'SagaStartEvent, 'SagaEvent> as msg ->
      match msg with
      | SagaMessage.Start _
      | SagaMessage.Event _ -> true
      | _ -> false
   | _ -> false

let initProps<'Saga, 'SagaStartEvent, 'SagaEvent when 'Saga :> ISaga>
   (supervisorOpts: PersistenceSupervisorOptions)
   (sagaPassivateIdleEntityAfter: TimeSpan)
   (persistenceId: string)
   (guaranteedDeliveryConsumerControllerRef:
      Option<
         IActorRef<
            ConsumerController.IConsumerCommand<
               SagaMessage<'SagaStartEvent, 'SagaEvent>
             >
          >
       >)
   (sagaHandler: SagaHandler<'Saga, 'SagaStartEvent, 'SagaEvent>)
   =
   ActorUtil.persistenceSupervisor
      supervisorOpts
      isPersistableMessage<'SagaStartEvent, 'SagaEvent>
      (actorProps
         sagaPassivateIdleEntityAfter
         guaranteedDeliveryConsumerControllerRef
         sagaHandler)
      persistenceId
      guaranteedDeliveryConsumerControllerRef.IsSome
