module AppSaga

open Lib.SharedTypes
open Lib.Saga
open Bank.Transfer.Domain
open PurchaseSaga
open DomesticTransferSaga
open PlatformTransferSaga
open PaymentRequestSaga
open OrgOnboardingSaga
open EmployeeOnboardingSaga
open CardSetupSaga
open BillingSaga

[<RequireQualifiedAccess>]
type StartEvent =
   | OrgOnboarding of OrgOnboardingSagaStartEvent
   | EmployeeOnboarding of EmployeeOnboardingSagaStartEvent
   | CardSetup of CardSetupSagaStartEvent
   | Purchase of PurchaseSagaStartEvent
   | DomesticTransfer of DomesticTransferSagaStartEvent
   | PlatformTransfer of PlatformTransferSagaStartEvent
   | PaymentRequest of PaymentRequestSagaStartEvent
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
      | PaymentRequest e ->
         match e with
         | PaymentRequestSagaStartEvent.PaymentRequested _ -> "PaymentRequested"
      | Billing _ -> "BillingSagaStart"

[<RequireQualifiedAccess>]
type Event =
   | OrgOnboarding of OrgOnboardingSagaEvent
   | EmployeeOnboarding of EmployeeOnboardingSagaEvent
   | CardSetup of CardSetupSagaEvent
   | Purchase of PurchaseSagaEvent
   | DomesticTransfer of DomesticTransferSagaEvent
   | PlatformTransfer of PlatformTransferSagaEvent
   | PaymentRequest of PaymentRequestSagaEvent
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
         | OrgOnboardingSagaEvent.CreateLegalEntityWithPartnerBankResponse _ ->
            "OrgOnboardingCreateLegalEntityWithPartnerBankResponse"
         | OrgOnboardingSagaEvent.CreateInternalAccountWithPartnerBankResponse _ ->
            "OrgOnboardingCreateInternalAccountWithPartnerBankResponse"
         | OrgOnboardingSagaEvent.InitializedPrimaryVirtualAccount ->
            "OrgOnboardingInitializedPrimaryVirtualAccount"
         | OrgOnboardingSagaEvent.InitializeOrgSettingsCacheResponse _ ->
            "OrgOnboardingInitializeOrgSettingsCacheResponse"
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
         | EmployeeOnboardingSagaEvent.CardSetupSagaCompleted _ ->
            "EmployeeOnboardingCardSetupSagaCompleted"
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
         | PurchaseSagaEvent.CardIssuerUpdatedPurchaseProgress _ ->
            "PurchaseCardIssuerUpdatedPurchaseProgress"
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
         | PurchaseSagaEvent.CardReservedFunds -> "PurchaseCardReservedFunds"
         | PurchaseSagaEvent.PurchaseRejected _ -> "PurchaseRejected"
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
         | DomesticTransferSagaEvent.PartnerBankProgressUpdate _ ->
            "DomesticTransferPartnerBankProgressUpdate"
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
      | Event.PaymentRequest e ->
         match e with
         | PaymentRequestSagaEvent.ScheduledPaymentReminderActivated ->
            "PaymentRequestScheduledPaymentReminderActivated"
         | PaymentRequestSagaEvent.PaymentRequestCancelled ->
            "PaymentRequestCancelled"
         | PaymentRequestSagaEvent.PaymentRequestDeclined ->
            "PaymentRequestDeclined"
         | PaymentRequestSagaEvent.PaymentRequestNotificationSentToPayer ->
            "PaymentRequestNotificationSentToPayer"
         | PaymentRequestSagaEvent.PaymentFailed _ -> "PaymentFailed"
         | PaymentRequestSagaEvent.PaymentFulfilled _ -> "PaymentFulfilled"
         | PaymentRequestSagaEvent.PaymentDeclinedNotificationSentToPayee ->
            "PaymentDeclinedNotificationSentToPayee"
         | PaymentRequestSagaEvent.PaymentSagaStartedForNextRecurringPayment ->
            "PaymentSagaStartedForNextRecurringPayment"
         | PaymentRequestSagaEvent.EvaluateRemainingWork ->
            "PaymentEvaluateRemainingWork"
         | PaymentRequestSagaEvent.ResetInProgressActivityAttempts ->
            "PaymentResetInProgressActivityAttempts"
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
   | PaymentRequest of PaymentRequestSaga
   | Billing of BillingSaga

   interface ISaga with
      member x.SagaId =
         match x with
         | Saga.OrgOnboarding s -> s.CorrelationId
         | Saga.EmployeeOnboarding s -> s.CorrelationId
         | Saga.CardSetup s -> s.CorrelationId
         | Saga.Purchase s -> s.PurchaseInfo.CorrelationId
         | Saga.DomesticTransfer s -> s.TransferInfo.TransferId.AsCorrelationId
         | Saga.PlatformTransfer s -> s.TransferInfo.TransferId.AsCorrelationId
         | Saga.PaymentRequest s ->
            s.PaymentInfo.SharedDetails.Id.AsCorrelationId
         | Saga.Billing s -> s.CorrelationId

      member x.OrgId =
         match x with
         | Saga.OrgOnboarding s -> s.OrgId
         | Saga.EmployeeOnboarding s -> s.OrgId
         | Saga.CardSetup s -> s.OrgId
         | Saga.Purchase s -> s.PurchaseInfo.OrgId
         | Saga.DomesticTransfer s -> s.TransferInfo.Originator.OrgId
         | Saga.PlatformTransfer s -> s.TransferInfo.Sender.OrgId
         | Saga.PaymentRequest s -> s.PaymentInfo.SharedDetails.Payee.OrgId
         | Saga.Billing s -> s.OrgId

      member x.ActivityInProgressCount =
         match x with
         | Saga.OrgOnboarding s -> s.LifeCycle.InProgress.Length
         | Saga.EmployeeOnboarding s -> s.LifeCycle.InProgress.Length
         | Saga.CardSetup s -> s.LifeCycle.InProgress.Length
         | Saga.Purchase s -> s.LifeCycle.InProgress.Length
         | Saga.DomesticTransfer s -> s.LifeCycle.InProgress.Length
         | Saga.PlatformTransfer s -> s.LifeCycle.InProgress.Length
         | Saga.PaymentRequest s -> s.LifeCycle.InProgress.Length
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
         | Saga.PaymentRequest s ->
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
         | Saga.PaymentRequest s ->
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
         | Saga.PaymentRequest s -> s.LifeCycle.SagaExhaustedAttempts
         | Saga.Billing s -> s.LifeCycle.SagaExhaustedAttempts

      member x.InactivityTimeout =
         match x with
         | Saga.OrgOnboarding s -> s.LifeCycle.InactivityTimeout
         | Saga.EmployeeOnboarding s -> s.LifeCycle.InactivityTimeout
         | Saga.CardSetup s -> s.LifeCycle.InactivityTimeout
         | Saga.Purchase s -> s.LifeCycle.InactivityTimeout
         | Saga.DomesticTransfer s -> s.LifeCycle.InactivityTimeout
         | Saga.PlatformTransfer s -> s.LifeCycle.InactivityTimeout
         | Saga.PaymentRequest s -> s.LifeCycle.InactivityTimeout
         | Saga.Billing s -> s.LifeCycle.InactivityTimeout

type AppSagaPersistableEvent = SagaPersistableEvent<StartEvent, Event>
type AppSagaMessage = SagaMessage<StartEvent, Event>

[<RequireQualifiedAccess>]
type SagaAlarmClockMessage = | WakeUpIfUnfinishedBusiness

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
      |> GuaranteedDelivery.message corrId.Value

   let private message orgId corrId (evt: Event) : AppSagaMessage =
      SagaEvent.create orgId corrId evt |> SagaMessage.Event

   let guaranteedDelivery
      (corrId: CorrelationId)
      (msg: AppSagaMessage)
      : GuaranteedDelivery.Message<AppSagaMessage>
      =
      GuaranteedDelivery.message corrId.Value msg

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

   let paymentRequestStart orgId corrId (evt: PaymentRequestSagaStartEvent) =
      startMessage orgId corrId (StartEvent.PaymentRequest evt)

   let paymentRequest orgId corrId (evt: PaymentRequestSagaEvent) =
      message orgId corrId (Event.PaymentRequest evt)

   let billingStart orgId corrId (evt: BillingSagaStartEvent) =
      startMessage orgId corrId (StartEvent.Billing evt)

   let billing orgId corrId (evt: BillingSagaEvent) =
      message orgId corrId (Event.Billing evt)
