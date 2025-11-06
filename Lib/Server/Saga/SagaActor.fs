[<RequireQualifiedAccess>]
module SagaActor

open Akka.Actor
open Akka.Persistence
open Akka.Delivery
open Akkling
open Akkling.Persistence
open Akkling.Cluster.Sharding
open System

open Lib.Saga
open ActorUtil

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
   (persistenceId: string)
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

      // Passivate after a delay.
      // The ReadModelSync actor relies on GetSaga messages returning
      // the latest saga state for persisting as a read model.
      // This syncing occurs a few seconds after immediate passivation
      // so is unnecessarily putting the actor to sleep and waking it back up.
      // In bursty workloads, with thousands of saga actors waking up
      // simultaneously & replaying events to attain the current state,
      // sql connection overloaded errors may be observed.
      //
      // Introducing this passivation delay solves that by allowing the GetSaga
      // message from the ReadModelSync actor to be received before passivating.
      //
      // It may be nice if the ReadModelSync actor did not have to rely on
      // GetSaga Ask messages for the latest state.
      // Other persistent actor's ReadModelSync actors accomplish this by using
      // fine-grained updates to the read model.  However, fine-grained read model
      // updates will be tedious to code for these sagas so I send these
      // GetSaga message Asks to the actor to get the latest state.
      //
      // It may make sense to include the state with each event persisted
      // allowing the ReadModelSync actor to write the latest saga state
      // to the read model without having to perform GetSaga ask messages.
      // I will want to consider this if too many simultaneous Ask messages
      // becomes a performance concern but no perceived issue for now.
      let sleep (reason: string) (preparingForSleep: bool) =
         if not preparingForSleep then
            logDebug $"Passivate after delay: {reason}"

            ctx.Schedule
               (TimeSpan.FromSeconds 10.)
               ctx.Self
               (SagaMessage<'SagaStartEvent, 'SagaEvent>.Sleep reason)
            |> ignore

      let handleSagaProgression (state: 'Saga option) msg envelope =
         let withAck persistentEffect =
            persistentEffect
            |> Effects.andThen (fun _ ->
               envelope |> Option.iter GuaranteedDelivery.ack)
            :> Effect<_>

         match msg with
         | SagaMessage.Start startEvent ->
            match state with
            | Some _ ->
               logWarning SagaStateTransitionError.HasAlreadyStarted
               ignored ()
            | None ->
               match
                  sagaHandler.stateTransitionStart
                     startEvent.Data
                     startEvent.Timestamp
               with
               | Error err ->
                  logWarning
                     $"Will not persist saga start event due to {err} {startEvent}"

                  ignored ()
               | Ok _ -> Persist(persistableStartEvent startEvent) |> withAck
         | SagaMessage.Event evt ->
            match state with
            | None ->
               logWarning $"{SagaStateTransitionError.HasNotStarted} {evt}"
               ignored ()
            | Some currentState ->
               match
                  sagaHandler.stateTransition
                     currentState
                     evt.Data
                     evt.Timestamp
               with
               | Error err ->
                  logWarning $"Will not persist saga event due to {err} {evt}"
                  ignored ()
               | Ok _ -> Persist(persistableEvent evt) |> withAck
         | msg ->
            logError $"Unhandled message during saga progression - {msg}"
            unhandled ()

      let rec loop (prepForSleep: bool) (state: 'Saga option) = actor {
         let! msg = ctx.Receive()

         let sleep reason =
            sleep reason prepForSleep
            loop true state

         let loop = loop false

         match msg with
         | Persisted ctx (:? SagaPersistableEvent<'SagaStartEvent, 'SagaEvent> as e) ->
            match state, e with
            | Some _, SagaPersistableEvent.StartEvent _ ->
               logError SagaStateTransitionError.HasAlreadyStarted
               return unhandled ()
            | None, SagaPersistableEvent.Event e ->
               logError $"{SagaStateTransitionError.HasNotStarted} {e}"
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
            > as envelope ->
            return handleSagaProgression state envelope.Message (Some envelope)
         // Some messages are sent through traditional AtMostOnceDelivery via
         // a reference to the cluster sharded entity ref rather than Akka.Delivery
         // AtLeastOnceDelivery producer ref so will not hit the
         // ConsumerController.Delivery match case above
         | :? SagaMessage<'SagaStartEvent, 'SagaEvent> as msg ->
            match msg with
            | SagaMessage.Start _
            | SagaMessage.Event _ -> return handleSagaProgression state msg None
            | SagaMessage.Sleep reason ->
               logDebug $"Passivating: {reason}"
               return passivate ()
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
            | SagaMessage.CheckForRemainingWork when prepForSleep ->
               return ignored ()
            | SagaMessage.CheckForRemainingWork ->
               return
                  match state with
                  | None ->
                     logWarning "Checking for remaining work when state is None"
                     ignored ()
                  | Some saga when saga.ActivityInProgressCount = 0 ->
                     sleep "Saga complete"
                  | Some saga when saga.ExhaustedAllAttempts ->
                     sleep "Exhausted saga attempts"
                  | Some saga ->
                     match saga.InactivityTimeout with
                     | None -> sleep "Wait for external saga stimulus"
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
                        // If timeout is greater than the automatic saga actor
                        // passivation then just passivate immediately and
                        // wait for the SagaAlarmClockActor to wake the
                        // actor back up to check for remaining work.
                        sleep "No remaining saga work at this time."
         // Event replay on actor start
         | :? SagaPersistableEvent<'SagaStartEvent, 'SagaEvent> as e when
            ctx.IsRecovering()
            ->
            match state, e with
            | Some _, SagaPersistableEvent.StartEvent _ ->
               logError SagaStateTransitionError.HasAlreadyStarted
               return ignored ()
            | None, SagaPersistableEvent.Event e ->
               logError $"{SagaStateTransitionError.HasNotStarted} {e}"
               return ignored ()
            | None, SagaPersistableEvent.StartEvent e ->
               let newState = sagaHandler.applyStartEvent e.Data e.Timestamp
               return! loop (Some newState)
            | Some currentState, SagaPersistableEvent.Event e ->
               let updatedState =
                  sagaHandler.applyEvent currentState e.Data e.Timestamp

               return! loop (Some updatedState)
         | msg ->
            PersistentActorEventHandler.handleEvent
               {
                  PersistentActorEventHandler.init with
                     LifecyclePreStart =
                        fun _ ->
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

      loop false None

   propsPersistShardingHosted handler persistenceId

let initProps<'Saga, 'SagaStartEvent, 'SagaEvent when 'Saga :> ISaga>
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
   actorProps
      persistenceId
      sagaPassivateIdleEntityAfter
      guaranteedDeliveryConsumerControllerRef
      sagaHandler
