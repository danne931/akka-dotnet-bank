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
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Employee.Domain
open Email
open PurchaseSaga
open DomesticTransferSaga
open PlatformTransferSaga
open PlatformPaymentSaga
open OrgOnboardingSaga
open EmployeeOnboardingSaga
open CardSetupSaga
open Bank.Scheduler
open BillingSaga
open PartnerBank.Service.Domain
open CardIssuer.Service.Domain
open TransferMessages

[<RequireQualifiedAccess>]
type StartEvent =
   | OrgOnboarding of OrgOnboardingSagaStartEvent
   | EmployeeOnboarding of EmployeeOnboardingSagaStartEvent
   | CardSetup of CardSetupSagaStartEvent
   | Purchase of PurchaseSagaStartEvent
   | DomesticTransfer of DomesticTransferSagaStartEvent
   | PlatformTransfer of PlatformTransferSagaStartEvent
   | PlatformPayment of PlatformPaymentSagaStartEvent
   | Billing of BillingSagaStartEvent

   member x.Name =
      match x with
      | OrgOnboarding e ->
         match e with
         | OrgOnboardingSagaStartEvent.ApplicationSubmitted _ ->
            "OrgOnboardingApplicationSubmitted"
      | EmployeeOnboarding e ->
         match e with
         | EmployeeOnboardingSagaStartEvent.AccountOwnerCreated _ ->
            "EmployeeOnboardingAccountOwnerCreated"
         | EmployeeOnboardingSagaStartEvent.EmployeeCreated _ ->
            "EmployeeOnboardingEmployeeCreated"
         | EmployeeOnboardingSagaStartEvent.EmployeeAccessRestored _ ->
            "EmployeeOnboardingEmployeeAccessRestored"
      | CardSetup _ -> "CardSetupStart"
      | Purchase e ->
         match e with
         | PurchaseSagaStartEvent.PurchaseIntent _ -> "PurchaseIntent"
         | PurchaseSagaStartEvent.PurchaseRejectedByCard _ ->
            "PurchaseRejectedByCard"
      | DomesticTransfer e ->
         match e with
         | DomesticTransferSagaStartEvent.SenderReservedFunds _ ->
            "DomesticTransferSenderReservedFunds"
         | DomesticTransferSagaStartEvent.ScheduleTransferRequest _ ->
            "DomesticTransferScheduleTransferRequest"
      | PlatformTransfer e ->
         match e with
         | PlatformTransferSagaStartEvent.SenderReservedFunds _ ->
            "PlatformTransferSenderReservedFunds"
         | PlatformTransferSagaStartEvent.ScheduleTransferRequest _ ->
            "PlatformTransferScheduleTransferRequest"
      | PlatformPayment e ->
         match e with
         | PlatformPaymentSagaStartEvent.PaymentRequested _ ->
            "PlatformPaymentPaymentRequested"
      | Billing _ -> "BillingSagaStart"

[<RequireQualifiedAccess>]
type Event =
   | OrgOnboarding of OrgOnboardingSagaEvent
   | EmployeeOnboarding of EmployeeOnboardingSagaEvent
   | CardSetup of CardSetupSagaEvent
   | Purchase of PurchaseSagaEvent
   | DomesticTransfer of DomesticTransferSagaEvent
   | PlatformTransfer of PlatformTransferSagaEvent
   | PlatformPayment of PlatformPaymentSagaEvent
   | Billing of BillingSagaEvent

   member x.Name =
      match x with
      | Event.OrgOnboarding e ->
         match e with
         | OrgOnboardingSagaEvent.ApplicationProcessingNotificationSent ->
            "OrgOnboardingApplicationProcessingNotificationSent"
         | OrgOnboardingSagaEvent.KYCResponse _ ->
            "OrgOnboardingKnowYourCustomerServiceResponse"
         | OrgOnboardingSagaEvent.ReceivedInfoFixDemandedByKYCService _ ->
            "OrgOnboardingReceivedInfoFixDemandedByKYCService"
         | OrgOnboardingSagaEvent.LinkAccountToPartnerBankResponse _ ->
            "OrgOnboardingLinkAccountToPartnerBankResponse"
         | OrgOnboardingSagaEvent.InitializedPrimaryVirtualAccount ->
            "OrgOnboardingInitializedPrimaryVirtualAccount"
         | OrgOnboardingSagaEvent.ApplicationAcceptedNotificationSent ->
            "OrgOnboardingApplicationAcceptedNotificationSent"
         | OrgOnboardingSagaEvent.ApplicationRejectedNotificationSent ->
            "OrgOnboardingApplicationRejectedNotificationSent"
         | OrgOnboardingSagaEvent.ApplicationRequiresRevisionForKYCServiceNotificationSent ->
            "OrgOnboardingApplicationRequiresRevisionForKYCServiceNotificationSent"
         | OrgOnboardingSagaEvent.SupportTeamResolvedPartnerBankLink ->
            "OrgOnboardingSupportTeamResolvedPartnerBankLink"
         | OrgOnboardingSagaEvent.EvaluateRemainingWork ->
            "OrgOnboardingEvaluateRemainingWork"
         | OrgOnboardingSagaEvent.ResetInProgressActivityAttempts ->
            "OrgOnboardingResetInProgressActivityAttempts"
         | OrgOnboardingSagaEvent.OrgActivated -> "OrgOnboardingOrgActivated"
      | Event.EmployeeOnboarding e ->
         match e with
         | EmployeeOnboardingSagaEvent.AccessRequestPending ->
            "EmployeeOnboardingAccessRequestPending"
         | EmployeeOnboardingSagaEvent.InviteNotificationSent ->
            "EmployeeOnboardingInviteNotificationSent"
         | EmployeeOnboardingSagaEvent.AccessApproved ->
            "EmployeeOnboardingAccessApproved"
         | EmployeeOnboardingSagaEvent.CardAssociatedWithEmployee ->
            "EmployeeOnboardingCardAssociatedWithEmployee"
         | EmployeeOnboardingSagaEvent.OnboardingFailNotificationSent ->
            "EmployeeOnboardingFailNotificationSent"
         | EmployeeOnboardingSagaEvent.CardCreateResponse _ ->
            "EmployeeOnboardingCardCreateResponse"
         | EmployeeOnboardingSagaEvent.InviteCancelled _ ->
            "EmployeeOnboardingInviteCancelled"
         | EmployeeOnboardingSagaEvent.InviteConfirmed ->
            "EmployeeOnboardingInviteConfirmed"
         | EmployeeOnboardingSagaEvent.InviteTokenRefreshed _ ->
            "EmployeeOnboardingInviteTokenRefreshed"
         | EmployeeOnboardingSagaEvent.ResetInProgressActivityAttempts ->
            "EmployeeOnboardingResetInProgressActivityAttempts"
         | EmployeeOnboardingSagaEvent.EvaluateRemainingWork ->
            "EmployeeOnboardingEvaluateRemainingWork"
      | Event.CardSetup e ->
         match e with
         | CardSetupSagaEvent.CardCreateResponse _ -> "CardSetupCreateResponse"
         | CardSetupSagaEvent.CardSetupSuccessNotificationSent ->
            "CardSetupSuccessNotificationSent"
         | CardSetupSagaEvent.CardSetupFailNotificationSent ->
            "CardSetupFailNotificationSent"
         | CardSetupSagaEvent.ProviderCardIdLinked ->
            "CardSetupProviderCardIdLinked"
         | CardSetupSagaEvent.ResetInProgressActivityAttempts ->
            "CardSetupResetInProgressActivityAttempts"
         | CardSetupSagaEvent.EvaluateRemainingWork ->
            "CardSetupEvaluateRemainingWork"
      | Event.Purchase e ->
         match e with
         | PurchaseSagaEvent.PurchaseRejectedCardNetworkResponse _ ->
            "PurchaseRejectedCardNetworkResponse"
         | PurchaseSagaEvent.CardNetworkResponse _ -> "CardNetworkResponse"
         | PurchaseSagaEvent.PurchaseSettledWithAccount ->
            "PurchaseSettledWithAccount"
         | PurchaseSagaEvent.PurchaseSettledWithCard ->
            "PurchaseSettledWithCard"
         | PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard ->
            "PurchaseFailureAcknowledgedByCard"
         | PurchaseSagaEvent.PurchaseFailureAcknowledgedByAccount ->
            "PurchaseFailureAcknowledgedByAccount"
         | PurchaseSagaEvent.AccountReservedFunds _ ->
            "PurchaseAccountReservedFunds"
         | PurchaseSagaEvent.PurchaseRejectedByAccount _ ->
            "PurchaseRejectedByAccount"
         | PurchaseSagaEvent.PurchaseNotificationSent ->
            "PurchaseNotificationSent"
         | PurchaseSagaEvent.PartnerBankSyncResponse _ ->
            "PurchasePartnerBankSyncResponse"
         | PurchaseSagaEvent.SupportTeamResolvedPartnerBankSync ->
            "PurchaseSupportTeamResolvedPartnerBankSync"
         | PurchaseSagaEvent.EvaluateRemainingWork ->
            "PurchaseEvaluateRemainingWork"
         | PurchaseSagaEvent.ResetInProgressActivityAttempts ->
            "PurchaseResetInProgressActivityAttempts"
      | Event.DomesticTransfer e ->
         match e with
         | DomesticTransferSagaEvent.ScheduledTransferActivated ->
            "DomesticTransferScheduledTransferActivated"
         | DomesticTransferSagaEvent.SenderReservedFunds ->
            "DomesticTransferSenderReservedFunds"
         | DomesticTransferSagaEvent.SenderReleasedReservedFunds ->
            "DomesticTransferSenderReleasedReservedFunds"
         | DomesticTransferSagaEvent.SenderUnableToReserveFunds _ ->
            "DomesticTransferSenderUnableToReserveFunds"
         | DomesticTransferSagaEvent.TransferProcessorProgressUpdate _ ->
            "DomesticTransferTransferProcessorProgressUpdate"
         | DomesticTransferSagaEvent.RetryTransferServiceRequest _ ->
            "DomesticTransferRetryTransferServiceRequest"
         | DomesticTransferSagaEvent.SenderDeductedFunds ->
            "DomesticTransferSenderDeductedFunds"
         | DomesticTransferSagaEvent.TransferInitiatedNotificationSent ->
            "DomesticTransferInitiatedNotificationSent"
         | DomesticTransferSagaEvent.EvaluateRemainingWork ->
            "DomesticTransferEvaluateRemainingWork"
         | DomesticTransferSagaEvent.ResetInProgressActivityAttempts ->
            "DomesticTransferResetInProgressActivityAttempts"
      | Event.PlatformTransfer e ->
         match e with
         | PlatformTransferSagaEvent.ScheduledTransferActivated ->
            "PlatformTransferScheduledTransferActivated"
         | PlatformTransferSagaEvent.SenderReservedFunds _ ->
            "PlatformTransferSenderReservedFunds"
         | PlatformTransferSagaEvent.SenderUnableToReserveFunds _ ->
            "PlatformTransferSenderUnableToReserveFunds"
         | PlatformTransferSagaEvent.RecipientDepositedFunds _ ->
            "PlatformTransferRecipientDepositedFunds"
         | PlatformTransferSagaEvent.RecipientUnableToDepositFunds _ ->
            "PlatformTransferRecipientUnableToDepositFunds"
         | PlatformTransferSagaEvent.PartnerBankSyncResponse _ ->
            "PlatformTransferPartnerBankSyncResponse"
         | PlatformTransferSagaEvent.TransferSettled ->
            "PlatformTransferSettled"
         | PlatformTransferSagaEvent.SupportTeamResolvedPartnerBankSync ->
            "PlatformTransferSupportTeamResolvedPartnerBankSync"
         | PlatformTransferSagaEvent.SenderReleasedReservedFunds ->
            "PlatformTransferSenderReleasedReservedFunds"
         | PlatformTransferSagaEvent.RecipientDepositUndo ->
            "PlatformTransferRecipientDepositUndo"
         | PlatformTransferSagaEvent.TransferNotificationSent ->
            "PlatformTransferNotificationSent"
         | PlatformTransferSagaEvent.TransferDepositNotificationSent ->
            "PlatformTransferDepositNotificationSent"
         | PlatformTransferSagaEvent.EvaluateRemainingWork ->
            "PlatformTransferEvaluateRemainingWork"
         | PlatformTransferSagaEvent.ResetInProgressActivityAttempts ->
            "PlatformTransferResetInProgressActivityAttempts"
      | Event.PlatformPayment e ->
         match e with
         | PlatformPaymentSagaEvent.PaymentRequestCancelled ->
            "PlatformPaymentRequestCancelled"
         | PlatformPaymentSagaEvent.PaymentRequestDeclined ->
            "PlatformPaymentRequestDeclined"
         | PlatformPaymentSagaEvent.PaymentRequestNotificationSentToPayer ->
            "PlatformPaymentRequestNotificationSentToPayer"
         | PlatformPaymentSagaEvent.PaymentFailed _ -> "PlatformPaymentFailed"
         | PlatformPaymentSagaEvent.PaymentFulfilled _ ->
            "PlatformPaymentFulfilled"
         | PlatformPaymentSagaEvent.PaymentDeclinedNotificationSentToPayee ->
            "PlatformPaymentDeclinedNotificationSentToPayee"
         | PlatformPaymentSagaEvent.EvaluateRemainingWork ->
            "PlatformPaymentEvaluateRemainingWork"
         | PlatformPaymentSagaEvent.ResetInProgressActivityAttempts ->
            "PlatformPaymentResetInProgressActivityAttempts"
      | Event.Billing e ->
         match e with
         | BillingSagaEvent.BillingStatementProcessing _ ->
            "BillingStatementProcessing"
         | BillingSagaEvent.BillingStatementPersisted ->
            "BillingStatementPersisted"
         | BillingSagaEvent.MaintenanceFeeProcessed ->
            "BillingStatementMaintenanceFeeProcessed"
         | BillingSagaEvent.BillingEmailSent -> "BillingStatementEmailSent"
         | BillingSagaEvent.EvaluateRemainingWork ->
            "BillingStatementEvaluateRemainingWork"
         | BillingSagaEvent.ResetInProgressActivityAttempts ->
            "BillingStatementResetInProgressActivityAttempts"

[<RequireQualifiedAccess>]
type Saga =
   | OrgOnboarding of OrgOnboardingSaga
   | EmployeeOnboarding of EmployeeOnboardingSaga
   | CardSetup of CardSetupSaga
   | Purchase of PurchaseSaga
   | DomesticTransfer of DomesticTransferSaga
   | PlatformTransfer of PlatformTransferSaga
   | PlatformPayment of PlatformPaymentSaga
   | Billing of BillingSaga

   interface ISaga with
      member x.SagaId =
         match x with
         | Saga.OrgOnboarding s -> s.CorrelationId
         | Saga.EmployeeOnboarding s -> s.CorrelationId
         | Saga.CardSetup s -> s.CorrelationId
         | Saga.Purchase s -> s.PurchaseInfo.CorrelationId
         | Saga.DomesticTransfer s ->
            TransferId.toCorrelationId s.TransferInfo.TransferId
         | Saga.PlatformTransfer s ->
            TransferId.toCorrelationId s.TransferInfo.TransferId
         | Saga.PlatformPayment s -> PaymentId.toCorrelationId s.PaymentInfo.Id
         | Saga.Billing s -> s.CorrelationId

      member x.OrgId =
         match x with
         | Saga.OrgOnboarding s -> s.OrgId
         | Saga.EmployeeOnboarding s -> s.OrgId
         | Saga.CardSetup s -> s.OrgId
         | Saga.Purchase s -> s.PurchaseInfo.OrgId
         | Saga.DomesticTransfer s -> s.TransferInfo.Sender.OrgId
         | Saga.PlatformTransfer s -> s.TransferInfo.Sender.OrgId
         | Saga.PlatformPayment s -> s.PaymentInfo.Payee.OrgId
         | Saga.Billing s -> s.OrgId

      member x.ActivityInProgressCount =
         match x with
         | Saga.OrgOnboarding s -> s.LifeCycle.InProgress.Length
         | Saga.EmployeeOnboarding s -> s.LifeCycle.InProgress.Length
         | Saga.CardSetup s -> s.LifeCycle.InProgress.Length
         | Saga.Purchase s -> s.LifeCycle.InProgress.Length
         | Saga.DomesticTransfer s -> s.LifeCycle.InProgress.Length
         | Saga.PlatformTransfer s -> s.LifeCycle.InProgress.Length
         | Saga.PlatformPayment s -> s.LifeCycle.InProgress.Length
         | Saga.Billing s -> s.LifeCycle.InProgress.Length

      member x.ActivityAttemptsExhaustedCount =
         match x with
         | Saga.OrgOnboarding s ->
            s.LifeCycle.ActivitiesWithAttemptsExhausted.Length
         | Saga.EmployeeOnboarding s ->
            s.LifeCycle.ActivitiesWithAttemptsExhausted.Length
         | Saga.CardSetup s ->
            s.LifeCycle.ActivitiesWithAttemptsExhausted.Length
         | Saga.Purchase s -> s.LifeCycle.ActivitiesWithAttemptsExhausted.Length
         | Saga.DomesticTransfer s ->
            s.LifeCycle.ActivitiesWithAttemptsExhausted.Length
         | Saga.PlatformTransfer s ->
            s.LifeCycle.ActivitiesWithAttemptsExhausted.Length
         | Saga.PlatformPayment s ->
            s.LifeCycle.ActivitiesWithAttemptsExhausted.Length
         | Saga.Billing s -> s.LifeCycle.ActivitiesWithAttemptsExhausted.Length

      member x.ActivityRetryableAfterInactivityCount =
         match x with
         | Saga.OrgOnboarding s ->
            s.LifeCycle.ActivitiesRetryableAfterInactivity.Length
         | Saga.EmployeeOnboarding s ->
            s.LifeCycle.ActivitiesRetryableAfterInactivity.Length
         | Saga.CardSetup s ->
            s.LifeCycle.ActivitiesRetryableAfterInactivity.Length
         | Saga.Purchase s ->
            s.LifeCycle.ActivitiesRetryableAfterInactivity.Length
         | Saga.DomesticTransfer s ->
            s.LifeCycle.ActivitiesRetryableAfterInactivity.Length
         | Saga.PlatformTransfer s ->
            s.LifeCycle.ActivitiesRetryableAfterInactivity.Length
         | Saga.PlatformPayment s ->
            s.LifeCycle.ActivitiesRetryableAfterInactivity.Length
         | Saga.Billing s ->
            s.LifeCycle.ActivitiesRetryableAfterInactivity.Length

      member x.ExhaustedAllAttempts =
         match x with
         | Saga.OrgOnboarding s -> s.LifeCycle.SagaExhaustedAttempts
         | Saga.EmployeeOnboarding s -> s.LifeCycle.SagaExhaustedAttempts
         | Saga.CardSetup s -> s.LifeCycle.SagaExhaustedAttempts
         | Saga.Purchase s -> s.LifeCycle.SagaExhaustedAttempts
         | Saga.DomesticTransfer s -> s.LifeCycle.SagaExhaustedAttempts
         | Saga.PlatformTransfer s -> s.LifeCycle.SagaExhaustedAttempts
         | Saga.PlatformPayment s -> s.LifeCycle.SagaExhaustedAttempts
         | Saga.Billing s -> s.LifeCycle.SagaExhaustedAttempts

      member x.InactivityTimeout =
         match x with
         | Saga.OrgOnboarding s -> s.LifeCycle.InactivityTimeout
         | Saga.EmployeeOnboarding s -> s.LifeCycle.InactivityTimeout
         | Saga.CardSetup s -> s.LifeCycle.InactivityTimeout
         | Saga.Purchase s -> s.LifeCycle.InactivityTimeout
         | Saga.DomesticTransfer s -> s.LifeCycle.InactivityTimeout
         | Saga.PlatformTransfer s -> s.LifeCycle.InactivityTimeout
         | Saga.PlatformPayment s -> s.LifeCycle.InactivityTimeout
         | Saga.Billing s -> s.LifeCycle.InactivityTimeout

type AppSagaPersistableEvent = SagaPersistableEvent<StartEvent, Event>

type AppSagaMessage = SagaMessage<StartEvent, Event>

module Message =
   // NOTE:
   // Saga start messages are sent with AtLeastOnceDelivery using Akka's GuaranteedDelivery module.
   // It is up to the consumer to decide whether they want to include the extra overhead cost for
   // non-start messages.
   //
   // For non-start messages to saga actors it is likely not necessary to use GuaranteedDelivery
   // as Lib/Saga/SagaActor.fs will reattempt activities for which it does not receive a response before
   // the Saga activity's inactivity timeout.
   let private startMessage
      orgId
      corrId
      (startEvent: StartEvent)
      : GuaranteedDelivery.Message<AppSagaMessage>
      =
      SagaEvent.create orgId corrId startEvent
      |> SagaMessage.Start
      |> GuaranteedDelivery.message (CorrelationId.get corrId)

   let private message orgId corrId (evt: Event) : AppSagaMessage =
      SagaEvent.create orgId corrId evt |> SagaMessage.Event

   let guaranteedDelivery
      corrId
      (msg: AppSagaMessage)
      : GuaranteedDelivery.Message<AppSagaMessage>
      =
      GuaranteedDelivery.message (CorrelationId.get corrId) msg

   let orgOnboardStart orgId corrId (evt: OrgOnboardingSagaStartEvent) =
      startMessage orgId corrId (StartEvent.OrgOnboarding evt)

   let orgOnboard orgId corrId (evt: OrgOnboardingSagaEvent) =
      message orgId corrId (Event.OrgOnboarding evt)

   let employeeOnboardStart
      orgId
      corrId
      (evt: EmployeeOnboardingSagaStartEvent)
      =
      startMessage orgId corrId (StartEvent.EmployeeOnboarding evt)

   let employeeOnboard orgId corrId (evt: EmployeeOnboardingSagaEvent) =
      message orgId corrId (Event.EmployeeOnboarding evt)

   let purchaseStart orgId corrId (evt: PurchaseSagaStartEvent) =
      startMessage orgId corrId (StartEvent.Purchase evt)

   let purchase orgId corrId (evt: PurchaseSagaEvent) =
      message orgId corrId (Event.Purchase evt)

   let cardSetupStart orgId corrId (evt: CardSetupSagaStartEvent) =
      startMessage orgId corrId (StartEvent.CardSetup evt)

   let cardSetup orgId corrId (evt: CardSetupSagaEvent) =
      message orgId corrId (Event.CardSetup evt)

   let domesticTransferStart
      orgId
      corrId
      (evt: DomesticTransferSagaStartEvent)
      =
      startMessage orgId corrId (StartEvent.DomesticTransfer evt)

   let domesticTransfer orgId corrId (evt: DomesticTransferSagaEvent) =
      message orgId corrId (Event.DomesticTransfer evt)

   let platformTransferStart
      orgId
      corrId
      (evt: PlatformTransferSagaStartEvent)
      =
      startMessage orgId corrId (StartEvent.PlatformTransfer evt)

   let platformTransfer orgId corrId (evt: PlatformTransferSagaEvent) =
      message orgId corrId (Event.PlatformTransfer evt)

   let platformPaymentStart orgId corrId (evt: PlatformPaymentSagaStartEvent) =
      startMessage orgId corrId (StartEvent.PlatformPayment evt)

   let platformPayment orgId corrId (evt: PlatformPaymentSagaEvent) =
      message orgId corrId (Event.PlatformPayment evt)

   let billingStart orgId corrId (evt: BillingSagaStartEvent) =
      startMessage orgId corrId (StartEvent.Billing evt)

   let billing orgId corrId (evt: BillingSagaEvent) =
      message orgId corrId (Event.Billing evt)

let sagaHandler
   (getOrgRef: OrgId -> IEntityRef<OrgMessage>)
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (getDomesticTransferRef:
      ActorSystem -> IActorRef<DomesticTransferServiceMessage>)
   (getSchedulingRef: ActorSystem -> IActorRef<SchedulerMessage>)
   (getKYCServiceRef: ActorSystem -> IActorRef<KYCMessage>)
   (getPartnerBankServiceRef:
      ActorSystem -> IActorRef<PartnerBankServiceMessage>)
   (getCardIssuerServiceRef: ActorSystem -> IActorRef<CardIssuerMessage>)
   : SagaActor.SagaHandler<Saga, StartEvent, Event>
   =
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
         | Saga.PlatformPayment _ ->
            PlatformPaymentSagaEvent.EvaluateRemainingWork
            |> Event.PlatformPayment
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
         | Saga.PlatformPayment _ ->
            PlatformPaymentSagaEvent.ResetInProgressActivityAttempts
            |> Event.PlatformPayment
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
            | StartEvent.PlatformPayment e ->
               PlatformPaymentSaga.stateTransitionStart e timestamp
               |> Result.map Saga.PlatformPayment
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
            | StartEvent.PlatformPayment e ->
               PlatformPaymentSaga.applyStartEvent e timestamp
               |> Saga.PlatformPayment
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
            | Saga.PlatformPayment state, Event.PlatformPayment e ->
               PlatformPaymentSaga.stateTransition state e timestamp
               |> Result.map Saga.PlatformPayment
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
            | Saga.PlatformPayment state, Event.PlatformPayment e ->
               PlatformPaymentSaga.applyEvent state e timestamp
               |> Saga.PlatformPayment
            | Saga.Billing state, Event.Billing e ->
               BillingSaga.applyEvent state e timestamp |> Saga.Billing
            | _ -> state
      onStartEventPersisted =
         fun mailbox evt state ->
            let getEmailRef () = getEmailRef mailbox.System

            let notHandled () =
               logError
                  mailbox
                  $"Can not handle saga start event persisted - {evt}"

            match evt with
            | StartEvent.OrgOnboarding e ->
               if state.IsOrgOnboarding then
                  OrgOnboardingSaga.onStartEventPersisted
                     {
                        getEmailRef = getEmailRef
                        getKYCServiceRef =
                           fun () -> getKYCServiceRef mailbox.System
                     }
                     e
               else
                  notHandled ()
            | StartEvent.EmployeeOnboarding e ->
               if state.IsEmployeeOnboarding then
                  EmployeeOnboardingSaga.onStartEventPersisted
                     {
                        getOrgRef = getOrgRef
                        getEmailRef = getEmailRef
                     }
                     e
               else
                  notHandled ()
            | StartEvent.CardSetup e ->
               if state.IsCardSetup then
                  CardSetupSaga.onStartEventPersisted
                     (fun () -> getCardIssuerServiceRef mailbox.System)
                     e
               else
                  notHandled ()
            | StartEvent.DomesticTransfer evt ->
               if state.IsDomesticTransfer then
                  DomesticTransferSaga.onStartEventPersisted
                     (fun () -> getDomesticTransferRef mailbox.System)
                     evt
               else
                  notHandled ()
            | StartEvent.PlatformTransfer evt ->
               if state.IsPlatformTransfer then
                  PlatformTransferSaga.onStartEventPersisted getAccountRef evt
               else
                  notHandled ()
            | StartEvent.PlatformPayment evt ->
               if state.IsPlatformPayment then
                  PlatformPaymentSaga.onStartEventPersisted getEmailRef evt
               else
                  notHandled ()
            | StartEvent.Billing e ->
               if state.IsBilling then
                  BillingSaga.onStartEventPersisted
                     {
                        getAccountRef = getAccountRef
                        getEmailRef = getEmailRef
                     }
                     e
               else
                  notHandled ()
            | StartEvent.Purchase e ->
               if state.IsPurchase then
                  PurchaseSaga.onStartEventPersisted
                     {
                        getAccountRef = getAccountRef
                        getEmailRef = getEmailRef
                        cardNetworkRejectPurchase =
                           PurchaseSaga.cardNetworkRejectPurchase
                        sendMessageToSelf =
                           fun purchase asyncEvt ->
                              let asyncMsg =
                                 asyncEvt
                                 |> Async.map (
                                    Message.purchase
                                       purchase.OrgId
                                       purchase.CorrelationId
                                 )

                              mailbox.Parent() <!| asyncMsg
                     }
                     e
               else
                  notHandled ()
      onEventPersisted =
         fun mailbox evt priorState state ->
            let getEmailRef () = getEmailRef mailbox.System
            let getSchedulingRef () = getSchedulingRef mailbox.System

            let getCardIssuerServiceRef =
               fun () -> getCardIssuerServiceRef mailbox.System

            let getPartnerBankServiceRef () =
               getPartnerBankServiceRef mailbox.System

            let notHandled () =
               logError
                  mailbox
                  $"Can not handle saga event persisted -{priorState}- {evt}"

            match evt with
            | Event.OrgOnboarding e ->
               match priorState, state with
               | Saga.OrgOnboarding priorState, Saga.OrgOnboarding state ->
                  OrgOnboardingSaga.onEventPersisted
                     {
                        getOrgRef = getOrgRef
                        getAccountRef = getAccountRef
                        getEmailRef = getEmailRef
                        getKYCServiceRef =
                           fun () -> getKYCServiceRef mailbox.System
                        getPartnerBankServiceRef = getPartnerBankServiceRef
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
                     {
                        getOrgRef = getOrgRef
                        getEmployeeRef = getEmployeeRef
                        getEmailRef = getEmailRef
                        getCardIssuerServiceRef = getCardIssuerServiceRef
                     }
                     priorState
                     state
                     e
               | _ -> notHandled ()
            | Event.CardSetup e ->
               let deps: CardSetupSaga.PersistenceHandlerDependencies = {
                  getEmployeeRef = getEmployeeRef
                  getEmailRef = getEmailRef
                  getCardIssuerServiceRef = getCardIssuerServiceRef
               }

               match priorState, state with
               | Saga.CardSetup priorState, Saga.CardSetup state ->
                  CardSetupSaga.onEventPersisted deps priorState state e
               | _ -> notHandled ()
            | Event.Purchase e ->
               let purchaseDeps: PurchaseSaga.PersistenceHandlerDependencies = {
                  getEmployeeRef = getEmployeeRef
                  getAccountRef = getAccountRef
                  getEmailRef = getEmailRef
                  getPartnerBankServiceRef = getPartnerBankServiceRef
                  cardNetworkConfirmPurchase =
                     PurchaseSaga.cardNetworkConfirmPurchase
                  cardNetworkRejectPurchase =
                     PurchaseSaga.cardNetworkRejectPurchase
                  sendMessageToSelf =
                     fun purchase asyncEvt ->
                        let asyncMsg =
                           asyncEvt
                           |> Async.map (
                              Message.purchase
                                 purchase.OrgId
                                 purchase.CorrelationId
                           )

                        mailbox.Parent() <!| asyncMsg
               }

               match priorState, state with
               | Saga.Purchase priorState, Saga.Purchase state ->
                  PurchaseSaga.onEventPersisted purchaseDeps priorState state e
               | _ -> notHandled ()
            | Event.DomesticTransfer evt ->
               let deps: DomesticTransferSaga.PersistenceHandlerDependencies = {
                  getSchedulingRef = getSchedulingRef
                  getDomesticTransferRef =
                     fun () -> getDomesticTransferRef mailbox.System
                  getAccountRef = getAccountRef
                  getEmailRef = getEmailRef
                  logError = logError mailbox
                  sendEventToSelf =
                     fun transfer evt ->
                        let msg =
                           Message.domesticTransfer
                              transfer.Sender.OrgId
                              (TransferId.toCorrelationId transfer.TransferId)
                              evt

                        mailbox.Parent() <! msg
               }

               match priorState, state with
               | Saga.DomesticTransfer priorState, Saga.DomesticTransfer state ->
                  DomesticTransferSaga.onEventPersisted
                     deps
                     priorState
                     state
                     evt
               | _ -> notHandled ()
            | Event.PlatformTransfer evt ->
               let deps: PlatformTransferSaga.PersistenceHandlerDependencies = {
                  getAccountRef = getAccountRef
                  getEmailRef = getEmailRef
                  getPartnerBankServiceRef = getPartnerBankServiceRef
                  sendEventToSelf =
                     fun transfer evt ->
                        let msg =
                           Message.platformTransfer
                              transfer.Sender.OrgId
                              (TransferId.toCorrelationId transfer.TransferId)
                              evt

                        mailbox.Parent() <! msg
               }

               match priorState, state with
               | Saga.PlatformTransfer priorState, Saga.PlatformTransfer state ->
                  PlatformTransferSaga.onEventPersisted
                     deps
                     priorState
                     state
                     evt
               | _ -> notHandled ()
            | Event.PlatformPayment evt ->
               let deps: PlatformPaymentSaga.PersistenceHandlerDependencies = {
                  getAccountRef = getAccountRef
                  getEmailRef = getEmailRef
                  getPartnerBankServiceRef = getPartnerBankServiceRef
                  sendMessageToSelf =
                     fun payment asyncEvt ->
                        let orgId = payment.Payee.OrgId
                        let corrId = PaymentId.toCorrelationId payment.Id

                        let asyncMsg =
                           asyncEvt
                           |> Async.map (Message.platformPayment orgId corrId)

                        mailbox.Parent() <!| asyncMsg
               }

               match priorState, state with
               | Saga.PlatformPayment priorState, Saga.PlatformPayment state ->
                  PlatformPaymentSaga.onEventPersisted deps priorState state evt
               | _ -> notHandled ()
            | Event.Billing evt ->
               let deps: BillingSaga.PersistenceHandlerDependencies = {
                  getAccountRef = getAccountRef
                  getEmailRef = getEmailRef
               }

               match priorState, state with
               | Saga.Billing priorState, Saga.Billing state ->
                  BillingSaga.onEventPersisted deps priorState state evt
               | _ -> notHandled ()
   }

// Send a message to a cluster sharded saga actor with AtMostOnceDelivery
let getEntityRef
   (sys: ActorSystem)
   (correlationId: CorrelationId)
   : IEntityRef<AppSagaMessage>
   =
   ActorUtil.getEntityRef
      sys
      ActorUtil.ClusterMetadata.sagaShardRegion
      (CorrelationId.get correlationId)

/// Send a message to a cluster sharded saga actor with AtLeastOnceDelivery
let getGuaranteedDeliveryProducerRef
   (system: ActorSystem)
   : IActorRef<GuaranteedDelivery.Message<AppSagaMessage>>
   =
   typed
   <| Akka.Hosting.ActorRegistry
      .For(system)
      .Get<ActorUtil.ActorMetadata.SagaGuaranteedDeliveryProducerMarker>()

let initProps
   (getOrgRef: OrgId -> IEntityRef<OrgMessage>)
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (getEmailActor: ActorSystem -> IActorRef<EmailMessage>)
   (getDomesticTransferRef:
      ActorSystem -> IActorRef<DomesticTransferServiceMessage>)
   (getSchedulingRef: ActorSystem -> IActorRef<SchedulerMessage>)
   (getKYCServiceRef: ActorSystem -> IActorRef<KYCMessage>)
   (getPartnerBankServiceRef:
      ActorSystem -> IActorRef<PartnerBankServiceMessage>)
   (getCardIssuerServiceRef: ActorSystem -> IActorRef<CardIssuerMessage>)
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
      (sagaHandler
         getOrgRef
         getEmployeeRef
         getAccountRef
         getEmailActor
         getDomesticTransferRef
         getSchedulingRef
         getKYCServiceRef
         getPartnerBankServiceRef
         getCardIssuerServiceRef)
