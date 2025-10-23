[<RequireQualifiedAccess>]
module AppSaga

open System
open Akka.Actor
open Akka.Delivery
open Akkling
open Akkling.Cluster.Sharding
open FSharp.Control

open Lib.SharedTypes
open Lib.Types
open Lib.Saga
open CachedOrgSettings
open PurchaseSaga
open DomesticTransferSaga
open PlatformTransferSaga
open PaymentRequestSaga
open OrgOnboardingSaga
open EmployeeOnboardingSaga
open CardSetupSaga
open BillingSaga
open AppSaga
open BankActorRegistry

let sagaHandler
   (registry:
      #IOrgActor & #IEmployeeActor & #IAccountActor & #IEmailActor & #ISchedulerActor & #IKYCServiceActor & #IPartnerBankServiceActor & #ICardIssuerServiceActor & #ISagaActor)
   (orgSettingsCache: OrgSettingsCache)
   (broadcaster: SignalRBroadcast.SignalRBroadcast)
   : SagaActor.SagaHandler<Saga, StartEvent, Event>
   =
   let sendMessageToPaymentSaga orgId (paymentId: PaymentRequestId) evt =
      let corrId = paymentId.AsCorrelationId
      let msg = Message.paymentRequest orgId corrId evt
      registry.SagaActor corrId <! msg

   {
      getEvaluateRemainingWorkEvent =
         function
         | Saga.OrgOnboarding _ ->
            OrgOnboardingSagaEvent.EvaluateRemainingWork |> Event.OrgOnboarding
         | Saga.EmployeeOnboarding _ ->
            EmployeeOnboardingSagaEvent.EvaluateRemainingWork
            |> Event.EmployeeOnboarding
         | Saga.CardSetup _ ->
            CardSetupSagaEvent.EvaluateRemainingWork |> Event.CardSetup
         | Saga.Purchase _ ->
            PurchaseSagaEvent.EvaluateRemainingWork |> Event.Purchase
         | Saga.DomesticTransfer _ ->
            DomesticTransferSagaEvent.EvaluateRemainingWork
            |> Event.DomesticTransfer
         | Saga.PlatformTransfer _ ->
            PlatformTransferSagaEvent.EvaluateRemainingWork
            |> Event.PlatformTransfer
         | Saga.PaymentRequest _ ->
            PaymentRequestSagaEvent.EvaluateRemainingWork
            |> Event.PaymentRequest
         | Saga.Billing _ ->
            BillingSagaEvent.EvaluateRemainingWork |> Event.Billing
      getResetInProgressActivitiesEvent =
         function
         | Saga.OrgOnboarding _ ->
            OrgOnboardingSagaEvent.ResetInProgressActivityAttempts
            |> Event.OrgOnboarding
         | Saga.EmployeeOnboarding _ ->
            EmployeeOnboardingSagaEvent.ResetInProgressActivityAttempts
            |> Event.EmployeeOnboarding
         | Saga.CardSetup _ ->
            CardSetupSagaEvent.ResetInProgressActivityAttempts
            |> Event.CardSetup
         | Saga.Purchase _ ->
            PurchaseSagaEvent.ResetInProgressActivityAttempts |> Event.Purchase
         | Saga.DomesticTransfer _ ->
            DomesticTransferSagaEvent.ResetInProgressActivityAttempts
            |> Event.DomesticTransfer
         | Saga.PlatformTransfer _ ->
            PlatformTransferSagaEvent.ResetInProgressActivityAttempts
            |> Event.PlatformTransfer
         | Saga.PaymentRequest _ ->
            PaymentRequestSagaEvent.ResetInProgressActivityAttempts
            |> Event.PaymentRequest
         | Saga.Billing _ ->
            BillingSagaEvent.ResetInProgressActivityAttempts |> Event.Billing
      stateTransitionStart =
         fun (evt: StartEvent) (timestamp: DateTime) ->
            match evt with
            | StartEvent.OrgOnboarding e ->
               OrgOnboardingSaga.stateTransitionStart e timestamp
               |> Result.map Saga.OrgOnboarding
            | StartEvent.EmployeeOnboarding e ->
               EmployeeOnboardingSaga.stateTransitionStart e timestamp
               |> Result.map Saga.EmployeeOnboarding
            | StartEvent.CardSetup e ->
               CardSetupSaga.stateTransitionStart e timestamp
               |> Result.map Saga.CardSetup
            | StartEvent.Purchase e ->
               PurchaseSaga.stateTransitionStart e timestamp
               |> Result.map Saga.Purchase
            | StartEvent.DomesticTransfer e ->
               DomesticTransferSaga.stateTransitionStart e timestamp
               |> Result.map Saga.DomesticTransfer
            | StartEvent.PlatformTransfer e ->
               PlatformTransferSaga.stateTransitionStart e timestamp
               |> Result.map Saga.PlatformTransfer
            | StartEvent.PaymentRequest e ->
               PaymentRequestSaga.stateTransitionStart e timestamp
               |> Result.map Saga.PaymentRequest
            | StartEvent.Billing e ->
               BillingSaga.stateTransitionStart e timestamp
               |> Result.map Saga.Billing
      applyStartEvent =
         fun (evt: StartEvent) (timestamp: DateTime) ->
            match evt with
            | StartEvent.OrgOnboarding e ->
               OrgOnboardingSaga.applyStartEvent e timestamp
               |> Saga.OrgOnboarding
            | StartEvent.EmployeeOnboarding e ->
               EmployeeOnboardingSaga.applyStartEvent e timestamp
               |> Saga.EmployeeOnboarding
            | StartEvent.CardSetup e ->
               CardSetupSaga.applyStartEvent e timestamp |> Saga.CardSetup
            | StartEvent.Purchase e ->
               PurchaseSaga.applyStartEvent e timestamp |> Saga.Purchase
            | StartEvent.DomesticTransfer e ->
               DomesticTransferSaga.applyStartEvent e timestamp
               |> Saga.DomesticTransfer
            | StartEvent.PlatformTransfer e ->
               PlatformTransferSaga.applyStartEvent e timestamp
               |> Saga.PlatformTransfer
            | StartEvent.PaymentRequest e ->
               PaymentRequestSaga.applyStartEvent e timestamp
               |> Saga.PaymentRequest
            | StartEvent.Billing e ->
               BillingSaga.applyStartEvent e timestamp |> Saga.Billing
      stateTransition =
         fun (state: Saga) (evt: Event) (timestamp: DateTime) ->
            match state, evt with
            | Saga.OrgOnboarding state, Event.OrgOnboarding e ->
               OrgOnboardingSaga.stateTransition state e timestamp
               |> Result.map Saga.OrgOnboarding
            | Saga.EmployeeOnboarding state, Event.EmployeeOnboarding e ->
               EmployeeOnboardingSaga.stateTransition state e timestamp
               |> Result.map Saga.EmployeeOnboarding
            | Saga.CardSetup state, Event.CardSetup e ->
               CardSetupSaga.stateTransition state e timestamp
               |> Result.map Saga.CardSetup
            | Saga.Purchase state, Event.Purchase e ->
               PurchaseSaga.stateTransition state e timestamp
               |> Result.map Saga.Purchase
            | Saga.DomesticTransfer state, Event.DomesticTransfer e ->
               DomesticTransferSaga.stateTransition state e timestamp
               |> Result.map Saga.DomesticTransfer
            | Saga.PlatformTransfer state, Event.PlatformTransfer e ->
               PlatformTransferSaga.stateTransition state e timestamp
               |> Result.map Saga.PlatformTransfer
            | Saga.PaymentRequest state, Event.PaymentRequest e ->
               PaymentRequestSaga.stateTransition state e timestamp
               |> Result.map Saga.PaymentRequest
            | Saga.Billing state, Event.Billing e ->
               BillingSaga.stateTransition state e timestamp
               |> Result.map Saga.Billing
            | _ ->
               Error SagaStateTransitionError.ReceivedEventOfDifferentSagaType
      applyEvent =
         fun (state: Saga) (evt: Event) (timestamp: DateTime) ->
            match state, evt with
            | Saga.OrgOnboarding state, Event.OrgOnboarding e ->
               OrgOnboardingSaga.applyEvent state e timestamp
               |> Saga.OrgOnboarding
            | Saga.EmployeeOnboarding state, Event.EmployeeOnboarding e ->
               EmployeeOnboardingSaga.applyEvent state e timestamp
               |> Saga.EmployeeOnboarding
            | Saga.CardSetup state, Event.CardSetup e ->
               CardSetupSaga.applyEvent state e timestamp |> Saga.CardSetup
            | Saga.Purchase state, Event.Purchase e ->
               PurchaseSaga.applyEvent state e timestamp |> Saga.Purchase
            | Saga.DomesticTransfer state, Event.DomesticTransfer e ->
               DomesticTransferSaga.applyEvent state e timestamp
               |> Saga.DomesticTransfer
            | Saga.PlatformTransfer state, Event.PlatformTransfer e ->
               PlatformTransferSaga.applyEvent state e timestamp
               |> Saga.PlatformTransfer
            | Saga.PaymentRequest state, Event.PaymentRequest e ->
               PaymentRequestSaga.applyEvent state e timestamp
               |> Saga.PaymentRequest
            | Saga.Billing state, Event.Billing e ->
               BillingSaga.applyEvent state e timestamp |> Saga.Billing
            | _ -> state
      onStartEventPersisted =
         fun mailbox evt state ->
            let notHandled () =
               logError
                  mailbox
                  $"Can not handle saga start event persisted - {evt}"

            match evt with
            | StartEvent.OrgOnboarding e ->
               if state.IsOrgOnboarding then
                  OrgOnboardingSaga.onStartEventPersisted registry e
               else
                  notHandled ()
            | StartEvent.EmployeeOnboarding e ->
               if state.IsEmployeeOnboarding then
                  EmployeeOnboardingSaga.onStartEventPersisted registry e
               else
                  notHandled ()
            | StartEvent.CardSetup e ->
               if state.IsCardSetup then
                  CardSetupSaga.onStartEventPersisted registry e
               else
                  notHandled ()
            | StartEvent.DomesticTransfer evt ->
               if state.IsDomesticTransfer then
                  DomesticTransferSaga.onStartEventPersisted registry evt
               else
                  notHandled ()
            | StartEvent.PlatformTransfer evt ->
               if state.IsPlatformTransfer then
                  PlatformTransferSaga.onStartEventPersisted registry evt
               else
                  notHandled ()
            | StartEvent.PaymentRequest evt ->
               match state with
               | Saga.PaymentRequest saga ->
                  PaymentRequestSaga.onStartEventPersisted
                     saga
                     registry
                     sendMessageToPaymentSaga
                     evt
               | _ -> notHandled ()
            | StartEvent.Billing e ->
               if state.IsBilling then
                  BillingSaga.onStartEventPersisted registry e
               else
                  notHandled ()
            | StartEvent.Purchase e ->
               if state.IsPurchase then
                  PurchaseSaga.onStartEventPersisted e
               else
                  notHandled ()
      onEventPersisted =
         fun mailbox evt priorState state ->
            let notHandled () =
               logError
                  mailbox
                  $"Can not handle saga event persisted -{priorState}- {evt}"

            match evt with
            | Event.OrgOnboarding e ->
               match priorState, state with
               | Saga.OrgOnboarding priorState, Saga.OrgOnboarding state ->
                  OrgOnboardingSaga.onEventPersisted
                     registry
                     {
                        OrgSettingsCache = orgSettingsCache
                        logError = logError mailbox
                        sendEventToSelf =
                           fun orgId corrId asyncEvt ->
                              let asyncMsg =
                                 asyncEvt
                                 |> Async.map (Message.orgOnboard orgId corrId)

                              mailbox.Parent() <!| asyncMsg
                     }
                     priorState
                     state
                     e
               | _ -> notHandled ()
            | Event.EmployeeOnboarding e ->
               match priorState, state with
               | Saga.EmployeeOnboarding priorState,
                 Saga.EmployeeOnboarding state ->
                  EmployeeOnboardingSaga.onEventPersisted
                     registry
                     priorState
                     state
                     e
               | _ -> notHandled ()
            | Event.CardSetup e ->
               match priorState, state with
               | Saga.CardSetup priorState, Saga.CardSetup state ->
                  CardSetupSaga.onEventPersisted registry priorState state e
               | _ -> notHandled ()
            | Event.Purchase e ->
               match priorState, state with
               | Saga.Purchase priorState, Saga.Purchase state ->
                  PurchaseSaga.onEventPersisted
                     broadcaster
                     registry
                     priorState
                     state
                     e
               | _ -> notHandled ()
            | Event.DomesticTransfer evt ->
               let OperationEnv: DomesticTransferSaga.OperationEnv = {
                  logError = logError mailbox
                  sendEventToSelf =
                     fun transfer evt ->
                        let msg =
                           Message.domesticTransfer
                              transfer.Originator.OrgId
                              transfer.TransferId.AsCorrelationId
                              evt

                        mailbox.Parent() <! msg
               }

               match priorState, state with
               | Saga.DomesticTransfer priorState, Saga.DomesticTransfer state ->
                  DomesticTransferSaga.onEventPersisted
                     broadcaster
                     registry
                     OperationEnv
                     priorState
                     state
                     evt
               | _ -> notHandled ()
            | Event.PlatformTransfer evt ->
               let OperationEnv: PlatformTransferSaga.OperationEnv = {
                  sendEventToSelf =
                     fun transfer evt ->
                        let msg =
                           Message.platformTransfer
                              transfer.Sender.OrgId
                              transfer.TransferId.AsCorrelationId
                              evt

                        mailbox.Parent() <! msg
               }

               match priorState, state with
               | Saga.PlatformTransfer priorState, Saga.PlatformTransfer state ->
                  PlatformTransferSaga.onEventPersisted
                     broadcaster
                     registry
                     OperationEnv
                     priorState
                     state
                     evt
               | _ -> notHandled ()
            | Event.PaymentRequest evt ->
               let OperationEnv: PaymentRequestSaga.OperationEnv = {
                  sendEventToPaymentSaga = sendMessageToPaymentSaga
               }

               match priorState, state with
               | Saga.PaymentRequest priorState, Saga.PaymentRequest state ->
                  PaymentRequestSaga.onEventPersisted
                     broadcaster
                     registry
                     OperationEnv
                     priorState
                     state
                     evt
               | _ -> notHandled ()
            | Event.Billing evt ->
               match priorState, state with
               | Saga.Billing priorState, Saga.Billing state ->
                  BillingSaga.onEventPersisted registry priorState state evt
               | _ -> notHandled ()
   }

let getEntityRef
   (sys: ActorSystem)
   (correlationId: CorrelationId)
   : IEntityRef<AppSagaMessage>
   =
   ActorUtil.getEntityRef
      sys
      ActorUtil.ClusterMetadata.sagaShardRegion
      correlationId.Value

let initProps
   registry
   (orgSettingsCache: OrgSettingsCache)
   (broadcaster: SignalRBroadcast.SignalRBroadcast)
   (persistenceSupervisorEnvConfig: PersistenceSupervisorEnvConfig)
   (sagaPassivateIdleEntityAfter: TimeSpan)
   (persistenceId: string)
   (guaranteedDeliveryConsumerControllerRef:
      Option<IActorRef<ConsumerController.IConsumerCommand<AppSagaMessage>>>)
   =
   SagaActor.initProps<Saga, StartEvent, Event>
      persistenceSupervisorEnvConfig
      sagaPassivateIdleEntityAfter
      persistenceId
      guaranteedDeliveryConsumerControllerRef
      (sagaHandler registry orgSettingsCache broadcaster)
