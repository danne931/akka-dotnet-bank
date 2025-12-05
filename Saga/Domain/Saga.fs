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
open SagaDTO

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
         | PurchaseSagaStartEvent.PurchaseProgress _ ->
            "PurchaseIntentFromProgressUpdate"
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
         | PurchaseSagaEvent.PurchaseSettledWithAccount _ ->
            "PurchaseSettledWithAccount"
         | PurchaseSagaEvent.PurchaseSettledWithCard _ ->
            "PurchaseSettledWithCard"
         | PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard ->
            "PurchaseFailureAcknowledgedByCard"
         | PurchaseSagaEvent.PurchaseFailureAcknowledgedByAccount ->
            "PurchaseFailureAcknowledgedByAccount"
         | PurchaseSagaEvent.AccountReservedFunds ->
            "PurchaseAccountReservedFunds"
         | PurchaseSagaEvent.CardReservedFunds -> "PurchaseCardReservedFunds"
         | PurchaseSagaEvent.PurchaseRejected _ -> "PurchaseRejected"
         | PurchaseSagaEvent.PurchaseNotificationSent ->
            "PurchaseNotificationSent"
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
         | PlatformTransferSagaEvent.TransferSettled _ ->
            "PlatformTransferSettled"
         | PlatformTransferSagaEvent.AckPaymentFulfillment ->
            "PlatformTransferAckPaymentFulfillment"
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

   member x.StartedAt =
      match x with
      | Saga.OrgOnboarding s -> s.StartedAt
      | Saga.EmployeeOnboarding s -> s.StartedAt
      | Saga.CardSetup s -> s.StartedAt
      | Saga.Purchase s -> s.StartedAt
      | Saga.DomesticTransfer s -> s.StartedAt
      | Saga.PlatformTransfer s -> s.StartedAt
      | Saga.PaymentRequest s -> s.StartedAt
      | Saga.Billing s -> s.StartedAt

   member x.Kind =
      match x with
      | Saga.OrgOnboarding _ -> SagaKind.OrgOnboarding
      | Saga.EmployeeOnboarding _ -> SagaKind.EmployeeOnboarding
      | Saga.CardSetup _ -> SagaKind.CardSetup
      | Saga.Purchase _ -> SagaKind.Purchase
      | Saga.DomesticTransfer _ -> SagaKind.DomesticTransfer
      | Saga.PlatformTransfer _ -> SagaKind.PlatformTransfer
      | Saga.PaymentRequest _ -> SagaKind.PaymentRequest
      | Saga.Billing _ -> SagaKind.BillingStatement

   member x.Status =
      let saga = x

      let inProgressOrExhausted =
         if (saga :> ISaga).ExhaustedAllAttempts then
            SagaDTOStatus.Exhausted
         else
            SagaDTOStatus.InProgress

      let compensatingOrFailed =
         let s = saga :> ISaga

         if s.ActivityInProgressCount > 0 then
            if s.ExhaustedAllAttempts then
               SagaDTOStatus.CompensationExhausted
            else
               SagaDTOStatus.Compensating
         else
            SagaDTOStatus.Failed

      match saga with
      | Saga.OrgOnboarding s ->
         match s.Status with
         | OrgOnboardingSagaStatus.InProgress -> inProgressOrExhausted
         | OrgOnboardingSagaStatus.Completed -> SagaDTOStatus.Completed
         | OrgOnboardingSagaStatus.Failed _ -> compensatingOrFailed
      | Saga.EmployeeOnboarding s ->
         match s.Status with
         | EmployeeOnboardingSagaStatus.InProgress -> inProgressOrExhausted
         | EmployeeOnboardingSagaStatus.Failed -> compensatingOrFailed
         | EmployeeOnboardingSagaStatus.Completed -> SagaDTOStatus.Completed
         | EmployeeOnboardingSagaStatus.Aborted _ -> SagaDTOStatus.Aborted
      | Saga.CardSetup s ->
         match s.Status with
         | CardSetupSagaStatus.InProgress -> inProgressOrExhausted
         | CardSetupSagaStatus.Completed -> SagaDTOStatus.Completed
         | CardSetupSagaStatus.Failed _ -> compensatingOrFailed
      | Saga.Purchase s ->
         match s.Status with
         | PurchaseSagaStatus.InProgress -> inProgressOrExhausted
         | PurchaseSagaStatus.Completed -> SagaDTOStatus.Completed
         | PurchaseSagaStatus.Failed _ -> compensatingOrFailed
      | Saga.DomesticTransfer s ->
         match s.Status with
         | DomesticTransferProgress.Scheduled -> SagaDTOStatus.Scheduled
         | DomesticTransferProgress.ProcessingAccountDeduction
         | DomesticTransferProgress.WaitingForTransferServiceAck
         | DomesticTransferProgress.PartnerBank _ -> inProgressOrExhausted
         | DomesticTransferProgress.Settled -> SagaDTOStatus.Completed
         | DomesticTransferProgress.Failed _ -> compensatingOrFailed
      | Saga.PlatformTransfer s ->
         match s.Status with
         | PlatformTransferSagaStatus.Scheduled -> SagaDTOStatus.Scheduled
         | PlatformTransferSagaStatus.InProgress -> inProgressOrExhausted
         | PlatformTransferSagaStatus.Completed -> SagaDTOStatus.Completed
         | PlatformTransferSagaStatus.Failed _ -> compensatingOrFailed
      | Saga.PaymentRequest s ->
         match s.Status with
         | PaymentRequestSagaStatus.InProgress _ -> inProgressOrExhausted
         | PaymentRequestSagaStatus.Completed -> SagaDTOStatus.Completed
         | PaymentRequestSagaStatus.Failed _ -> compensatingOrFailed
      | Saga.Billing s ->
         match s.Status with
         | BillingSagaStatus.InProgress -> inProgressOrExhausted
         | BillingSagaStatus.Completed -> SagaDTOStatus.Completed
         | BillingSagaStatus.Failed -> compensatingOrFailed

   member x.AsDTO: SagaDTO =
      let dto = {
         Id = (x :> ISaga).SagaId
         StartedAt = x.StartedAt
         Name = ""
         LifeCycle = []
         Status = x.Status
         SagaKind = x.Kind
         StatusDetail = ""
         Events = Serialization.serialize []
         RecoverableActivity = None
         Amount = None
      }

      let activitiesToDTO
         (life: SagaLifeCycle<'Activity>)
         : SagaActivityDTO list
         =
         let activityToDTO status (a: ActivityLifeCycle<'Activity>) = {
            Start = a.Start
            End = a.End
            Name = string a.Activity |> _.Split(" ") |> _.Head()
            Attempts = a.Attempts
            MaxAttempts = a.MaxAttempts
            Status = status
         }

         let inProgress =
            life.InProgress
            |> List.map (activityToDTO SagaActivityDTOStatus.InProgress)

         let completed =
            life.Completed
            |> List.map (activityToDTO SagaActivityDTOStatus.Completed)

         let failed =
            life.Failed |> List.map (activityToDTO SagaActivityDTOStatus.Failed)

         let aborted =
            life.Aborted
            |> List.map (activityToDTO SagaActivityDTOStatus.Aborted)

         inProgress @ failed @ aborted @ completed
         |> List.sortBy (fun a ->
            match a.Status with
            | SagaActivityDTOStatus.Completed -> a.End
            | _ -> Some a.Start)

      match x with
      | Saga.Purchase saga -> {
         dto with
            Name = "Purchase"
            LifeCycle = activitiesToDTO saga.LifeCycle
            Events = Serialization.serialize saga.Events
            StatusDetail = Serialization.serialize saga.Status
            Amount = Some saga.PurchaseInfo.Amount
        }
      | Saga.DomesticTransfer saga -> {
         dto with
            Name = "Domestic Transfer"
            LifeCycle = activitiesToDTO saga.LifeCycle
            Events = Serialization.serialize saga.Events
            StatusDetail = Serialization.serialize saga.Status
            RecoverableActivity =
               if saga.RequiresTransferServiceDevelopmentFix then
                  Some
                     ActivityRecoverableByHumanInTheLoop.DomesticTransferServiceDevelopmentFix
               else
                  None
            Amount = Some saga.TransferInfo.Amount
        }
      | Saga.PlatformTransfer saga -> {
         dto with
            Name = "Platform Transfer"
            LifeCycle = activitiesToDTO saga.LifeCycle
            Events = Serialization.serialize saga.Events
            StatusDetail = Serialization.serialize saga.Status
            Amount = Some saga.TransferInfo.Amount
        }
      | Saga.PaymentRequest saga -> {
         dto with
            Name = "Payment Request"
            LifeCycle = activitiesToDTO saga.LifeCycle
            Events = Serialization.serialize saga.Events
            StatusDetail = Serialization.serialize saga.Status
            Amount = Some saga.PaymentInfo.SharedDetails.Amount
        }
      | Saga.Billing saga -> {
         dto with
            Name = "Billing"
            LifeCycle = activitiesToDTO saga.LifeCycle
            Events = Serialization.serialize saga.Events
            StatusDetail = Serialization.serialize saga.Status
        }
      | Saga.CardSetup saga -> {
         dto with
            Name = "Card Setup"
            LifeCycle = activitiesToDTO saga.LifeCycle
            Events = Serialization.serialize saga.Events
            StatusDetail = Serialization.serialize saga.Status
        }
      | Saga.EmployeeOnboarding saga -> {
         dto with
            Name = "Employee Onboarding"
            LifeCycle = activitiesToDTO saga.LifeCycle
            Events = Serialization.serialize saga.Events
            StatusDetail = Serialization.serialize saga.Status
        }
      | Saga.OrgOnboarding saga -> {
         dto with
            Name = "Organization Onboarding"
            LifeCycle = activitiesToDTO saga.LifeCycle
            Events = Serialization.serialize saga.Events
            StatusDetail = Serialization.serialize saga.Status
        }

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
   // For non-start messages to saga actors it may not necessary to use GuaranteedDelivery since
   // Lib/Saga/SagaActor.fs will reattempt activities for which it does not receive a response before
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

   let redelivered (msg: AppSagaMessage) =
      match msg with
      | AppSagaMessage.Start e -> AppSagaMessage.Start e.AsDeliveryRetry
      | AppSagaMessage.Event e -> AppSagaMessage.Event e.AsDeliveryRetry
      | _ -> msg

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
      SagaEvent.create orgId corrId (StartEvent.Purchase evt)
      |> SagaMessage.Start

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
