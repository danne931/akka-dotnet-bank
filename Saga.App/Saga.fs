[<RequireQualifiedAccess>]
module AppSaga

open System
open Akka.Actor
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
         | OrgOnboardingSagaEvent.Start e ->
            match e with
            | OrgOnboardingSagaStartEvent.ApplicationSubmitted _ ->
               "OrgOnboardingApplicationSubmitted"
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
         | EmployeeOnboardingSagaEvent.Start e ->
            match e with
            | EmployeeOnboardingSagaStartEvent.AccountOwnerCreated _ ->
               "EmployeeOnboardingAccountOwnerCreated"
            | EmployeeOnboardingSagaStartEvent.EmployeeCreated _ ->
               "EmployeeOnboardingEmployeeCreated"
            | EmployeeOnboardingSagaStartEvent.EmployeeAccessRestored _ ->
               "EmployeeOnboardingEmployeeAccessRestored"
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
         | CardSetupSagaEvent.Start _ -> "CardSetupStart"
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
         | PurchaseSagaEvent.Start e ->
            match e with
            | PurchaseSagaStartEvent.DeductedCardFunds _ -> "DeductedCardFunds"
            | PurchaseSagaStartEvent.PurchaseRejectedByCard _ ->
               "PurchaseRejectedByCard"
         | PurchaseSagaEvent.PurchaseRejectedCardNetworkResponse _ ->
            "PurchaseRejectedCardNetworkResponse"
         | PurchaseSagaEvent.CardNetworkResponse _ -> "CardNetworkResponse"
         | PurchaseSagaEvent.PurchaseRefundedToCard -> "PurchaseRefundedToCard"
         | PurchaseSagaEvent.PurchaseRefundedToAccount ->
            "PurchaseRefundedToAccount"
         | PurchaseSagaEvent.PurchaseConfirmedByAccount ->
            "PurchaseConfirmedByAccount"
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
         | DomesticTransferSagaEvent.Start e ->
            match e with
            | DomesticTransferSagaStartEvent.SenderAccountDeductedFunds _ ->
               "DomesticTransferSenderAccountDeductedFunds"
            | DomesticTransferSagaStartEvent.ScheduleTransferRequest _ ->
               "DomesticTransferScheduleTransferRequest"
         | DomesticTransferSagaEvent.ScheduledJobCreated ->
            "DomesticTransferScheduledJobCreated"
         | DomesticTransferSagaEvent.ScheduledJobExecuted ->
            "DomesticTransferScheduledJobExecuted"
         | DomesticTransferSagaEvent.SenderAccountDeductedFunds ->
            "DomesticTransferSenderAccountDeductedFunds"
         | DomesticTransferSagaEvent.SenderAccountUnableToDeductFunds _ ->
            "DomesticTransferSenderAccountUnableToDeductFunds"
         | DomesticTransferSagaEvent.TransferProcessorProgressUpdate _ ->
            "DomesticTransferTransferProcessorProgressUpdate"
         | DomesticTransferSagaEvent.RetryTransferServiceRequest ->
            "DomesticTransferRetryTransferServiceRequest"
         | DomesticTransferSagaEvent.TransferMarkedAsSettled ->
            "DomesticTransferMarkedAsSettled"
         | DomesticTransferSagaEvent.SenderAccountRefunded ->
            "DomesticTransferSenderAccountRefunded"
         | DomesticTransferSagaEvent.TransferInitiatedNotificationSent ->
            "DomesticTransferInitiatedNotificationSent"
         | DomesticTransferSagaEvent.EvaluateRemainingWork ->
            "DomesticTransferEvaluateRemainingWork"
         | DomesticTransferSagaEvent.ResetInProgressActivityAttempts ->
            "DomesticTransferResetInProgressActivityAttempts"
      | Event.PlatformTransfer e ->
         match e with
         | PlatformTransferSagaEvent.Start e ->
            match e with
            | PlatformTransferSagaStartEvent.SenderAccountDeductedFunds _ ->
               "PlatformTransferSenderAccountDeductedFunds"
            | PlatformTransferSagaStartEvent.ScheduleTransferRequest _ ->
               "PlatformTransferScheduleTransferRequest"
         | PlatformTransferSagaEvent.ScheduledJobCreated ->
            "PlatformTransferScheduledJobCreated"
         | PlatformTransferSagaEvent.ScheduledJobExecuted ->
            "PlatformTransferScheduledJobExecuted"
         | PlatformTransferSagaEvent.SenderAccountDeductedFunds ->
            "PlatformTransferSenderAccountDeductedFunds"
         | PlatformTransferSagaEvent.SenderAccountUnableToDeductFunds _ ->
            "PlatformTransferSenderAccountUnableToDeductFunds"
         | PlatformTransferSagaEvent.RecipientAccountDepositedFunds ->
            "PlatformTransferRecipientAccountDepositedFunds"
         | PlatformTransferSagaEvent.RecipientAccountUnableToDepositFunds _ ->
            "PlatformTransferRecipientAccountUnableToDepositFunds"
         | PlatformTransferSagaEvent.PartnerBankSyncResponse _ ->
            "PlatformTransferPartnerBankSyncResponse"
         | PlatformTransferSagaEvent.SupportTeamResolvedPartnerBankSync ->
            "PlatformTransferSupportTeamResolvedPartnerBankSync"
         | PlatformTransferSagaEvent.SenderAccountRefunded ->
            "PlatformTransferSenderAccountRefunded"
         | PlatformTransferSagaEvent.RecipientAccountDepositUndo ->
            "PlatformTransferRecipientAccountDepositUndo"
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
         | PlatformPaymentSagaEvent.Start e ->
            match e with
            | PlatformPaymentSagaStartEvent.PaymentRequested _ ->
               "PlatformPaymentPaymentRequested"
         | PlatformPaymentSagaEvent.PaymentRequestCancelled ->
            "PlatformPaymentRequestCancelled"
         | PlatformPaymentSagaEvent.PaymentRequestDeclined ->
            "PlatformPaymentRequestDeclined"
         | PlatformPaymentSagaEvent.PayerAccountDeductedFunds _ ->
            "PlatformPaymentPayerAccountDeductedFunds"
         | PlatformPaymentSagaEvent.PayerAccountUnableToDeductFunds _ ->
            "PlatformPaymentPayerAccountUnableToDeductFunds"
         | PlatformPaymentSagaEvent.PayeeAccountDepositedFunds ->
            "PlatformPaymentPayeeAccountDepositedFunds"
         | PlatformPaymentSagaEvent.PayeeAccountUnableToDepositFunds _ ->
            "PlatformPaymentPayeeAccountUnableToDepositFunds"
         | PlatformPaymentSagaEvent.PartnerBankSyncResponse _ ->
            "PlatformPaymentPartnerBankSyncResponse"
         | PlatformPaymentSagaEvent.SupportTeamResolvedPartnerBankSync ->
            "PlatformPaymentSupportTeamResolvedPartnerBankSync"
         | PlatformPaymentSagaEvent.PayerAccountRefunded ->
            "PlatformPaymentPayerAccountRefunded"
         | PlatformPaymentSagaEvent.ThirdPartyPaymentMethodRefunded ->
            "PlatformPaymentThirdPartyPaymentMethodRefunded"
         | PlatformPaymentSagaEvent.PaymentRequestNotificationSentToPayer ->
            "PlatformPaymentRequestNotificationSentToPayer"
         | PlatformPaymentSagaEvent.PaymentPaidNotificationSentToPayer ->
            "PlatformPaymentPaidNotificationSentToPayer"
         | PlatformPaymentSagaEvent.PaymentDepositedNotificationSentToPayee ->
            "PlatformPaymentDepositedNotificationSentToPayee"
         | PlatformPaymentSagaEvent.PaymentDeclinedNotificationSentToPayee ->
            "PlatformPaymentDeclinedNotificationSentToPayee"
         | PlatformPaymentSagaEvent.EvaluateRemainingWork ->
            "PlatformPaymentEvaluateRemainingWork"
         | PlatformPaymentSagaEvent.ResetInProgressActivityAttempts ->
            "PlatformPaymentResetInProgressActivityAttempts"
      | Event.Billing e ->
         match e with
         | BillingSagaEvent.Start _ -> "BillingStatementStart"
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


/// Wrap an AppSaga event in a saga message.
let sagaMessage orgId correlationId (evt: Event) =
   SagaEvent.create orgId correlationId evt |> SagaMessage.Event

let sagaHandler
   (getOrgRef: OrgId -> IEntityRef<OrgMessage>)
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (getDomesticTransferRef: ActorSystem -> IActorRef<DomesticTransferMessage>)
   (getSchedulingRef: ActorSystem -> IActorRef<SchedulerMessage>)
   : SagaActor.SagaHandler<Saga, Event>
   =
   {
      getEvaluateRemainingWorkEvent =
         fun saga ->
            match saga with
            | Saga.OrgOnboarding _ ->
               OrgOnboardingSagaEvent.EvaluateRemainingWork
               |> Event.OrgOnboarding
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
         fun saga ->
            match saga with
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
               PurchaseSagaEvent.ResetInProgressActivityAttempts
               |> Event.Purchase
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
      applyEvent =
         fun (state: Saga option) (evt: Event) (timestamp: DateTime) ->
            match evt, state with
            | Event.OrgOnboarding e, None ->
               OrgOnboardingSaga.applyEvent None e timestamp
               |> Option.map Saga.OrgOnboarding
            | Event.OrgOnboarding e, Some(Saga.OrgOnboarding state) ->
               OrgOnboardingSaga.applyEvent (Some state) e timestamp
               |> Option.map Saga.OrgOnboarding
            | Event.EmployeeOnboarding e, None ->
               EmployeeOnboardingSaga.applyEvent None e timestamp
               |> Option.map Saga.EmployeeOnboarding
            | Event.EmployeeOnboarding e, Some(Saga.EmployeeOnboarding state) ->
               EmployeeOnboardingSaga.applyEvent (Some state) e timestamp
               |> Option.map Saga.EmployeeOnboarding
            | Event.CardSetup e, None ->
               CardSetupSaga.applyEvent None e timestamp
               |> Option.map Saga.CardSetup
            | Event.CardSetup e, Some(Saga.CardSetup state) ->
               CardSetupSaga.applyEvent (Some state) e timestamp
               |> Option.map Saga.CardSetup
            | Event.Purchase e, None ->
               PurchaseSaga.applyEvent None e timestamp
               |> Option.map Saga.Purchase
            | Event.Purchase e, Some(Saga.Purchase state) ->
               PurchaseSaga.applyEvent (Some state) e timestamp
               |> Option.map Saga.Purchase
            | Event.DomesticTransfer e, None ->
               DomesticTransferSaga.applyEvent None e timestamp
               |> Option.map Saga.DomesticTransfer
            | Event.DomesticTransfer e, Some(Saga.DomesticTransfer state) ->
               DomesticTransferSaga.applyEvent (Some state) e timestamp
               |> Option.map Saga.DomesticTransfer
            | Event.PlatformTransfer e, None ->
               PlatformTransferSaga.applyEvent None e timestamp
               |> Option.map Saga.PlatformTransfer
            | Event.PlatformTransfer e, Some(Saga.PlatformTransfer state) ->
               PlatformTransferSaga.applyEvent (Some state) e timestamp
               |> Option.map Saga.PlatformTransfer
            | Event.PlatformPayment e, None ->
               PlatformPaymentSaga.applyEvent None e timestamp
               |> Option.map Saga.PlatformPayment
            | Event.PlatformPayment e, Some(Saga.PlatformPayment state) ->
               PlatformPaymentSaga.applyEvent (Some state) e timestamp
               |> Option.map Saga.PlatformPayment
            | Event.Billing e, None ->
               BillingSaga.applyEvent None e timestamp
               |> Option.map Saga.Billing
            | Event.Billing e, Some(Saga.Billing state) ->
               BillingSaga.applyEvent (Some state) e timestamp
               |> Option.map Saga.Billing
            | _ -> state
      stateTransition =
         fun (state: Saga option) (evt: Event) (timestamp: DateTime) ->
            match evt, state with
            | Event.OrgOnboarding e, Some(Saga.OrgOnboarding state) ->
               OrgOnboardingSaga.stateTransition (Some state) e timestamp
               |> Result.map (Option.map Saga.OrgOnboarding)
            | Event.OrgOnboarding e, None ->
               OrgOnboardingSaga.stateTransition None e timestamp
               |> Result.map (Option.map Saga.OrgOnboarding)
            | Event.EmployeeOnboarding e, Some(Saga.EmployeeOnboarding state) ->
               EmployeeOnboardingSaga.stateTransition (Some state) e timestamp
               |> Result.map (Option.map Saga.EmployeeOnboarding)
            | Event.EmployeeOnboarding e, None ->
               EmployeeOnboardingSaga.stateTransition None e timestamp
               |> Result.map (Option.map Saga.EmployeeOnboarding)
            | Event.CardSetup e, Some(Saga.CardSetup state) ->
               CardSetupSaga.stateTransition (Some state) e timestamp
               |> Result.map (Option.map Saga.CardSetup)
            | Event.CardSetup e, None ->
               CardSetupSaga.stateTransition None e timestamp
               |> Result.map (Option.map Saga.CardSetup)
            | Event.Purchase e, None ->
               PurchaseSaga.stateTransition None e timestamp
               |> Result.map (Option.map Saga.Purchase)
            | Event.Purchase e, Some(Saga.Purchase state) ->
               PurchaseSaga.stateTransition (Some state) e timestamp
               |> Result.map (Option.map Saga.Purchase)
            | Event.DomesticTransfer e, None ->
               DomesticTransferSaga.stateTransition None e timestamp
               |> Result.map (Option.map Saga.DomesticTransfer)
            | Event.DomesticTransfer e, Some(Saga.DomesticTransfer state) ->
               DomesticTransferSaga.stateTransition (Some state) e timestamp
               |> Result.map (Option.map Saga.DomesticTransfer)
            | Event.PlatformTransfer e, None ->
               PlatformTransferSaga.stateTransition None e timestamp
               |> Result.map (Option.map Saga.PlatformTransfer)
            | Event.PlatformTransfer e, Some(Saga.PlatformTransfer state) ->
               PlatformTransferSaga.stateTransition (Some state) e timestamp
               |> Result.map (Option.map Saga.PlatformTransfer)
            | Event.PlatformPayment e, None ->
               PlatformPaymentSaga.stateTransition None e timestamp
               |> Result.map (Option.map Saga.PlatformPayment)
            | Event.PlatformPayment e, Some(Saga.PlatformPayment state) ->
               PlatformPaymentSaga.stateTransition (Some state) e timestamp
               |> Result.map (Option.map Saga.PlatformPayment)
            | Event.Billing e, None ->
               BillingSaga.stateTransition None e timestamp
               |> Result.map (Option.map Saga.Billing)
            | Event.Billing e, Some(Saga.Billing state) ->
               BillingSaga.stateTransition (Some state) e timestamp
               |> Result.map (Option.map Saga.Billing)
            | _ ->
               Error
                  Lib.Saga.SagaStateTransitionError.ReceivedEventOfDifferentSagaType
      onEventPersisted =
         fun mailbox evt priorState state ->
            let getEmailRef () = getEmailRef mailbox.System
            let getSchedulingRef () = getSchedulingRef mailbox.System

            let notHandled () =
               logError
                  mailbox
                  $"Can not handle saga event persisted -{priorState}- {evt}"

            match evt with
            | Event.OrgOnboarding e ->
               let deps: OrgOnboardingSaga.PersistenceHandlerDependencies = {
                  getOrgRef = getOrgRef
                  getEmployeeRef = getEmployeeRef
                  getAccountRef = getAccountRef
                  getEmailRef = getEmailRef
                  linkAccountToPartnerBank =
                     OrgOnboardingSaga.linkAccountToPartnerBank
                  kycVerification =
                     OrgOnboardingSaga.KnowYourCustomerService.verifyOrg
                  sendMessageToSelf =
                     fun orgId corrId asyncEvt ->
                        let asyncMsg =
                           asyncEvt
                           |> Async.map (
                              Event.OrgOnboarding >> sagaMessage orgId corrId
                           )

                        mailbox.Parent() <!| asyncMsg
               }

               match priorState, state, e with
               | None, Saga.OrgOnboarding _, OrgOnboardingSagaEvent.Start evt ->
                  OrgOnboardingSaga.onStartEventPersisted deps evt
               | Some(Saga.OrgOnboarding priorState),
                 Saga.OrgOnboarding state,
                 _ -> OrgOnboardingSaga.onEventPersisted deps priorState state e
               | _ -> notHandled ()
            | Event.EmployeeOnboarding e ->
               let deps: EmployeeOnboardingSaga.PersistenceHandlerDependencies = {
                  getOrgRef = getOrgRef
                  getEmployeeRef = getEmployeeRef
                  getEmailRef = getEmailRef
                  createCardViaThirdPartyProvider =
                     EmployeeOnboardingSaga.createCardViaThirdPartyProvider
                  sendMessageToSelf =
                     fun orgId corrId asyncEvt ->
                        let asyncMsg =
                           asyncEvt
                           |> Async.map (
                              Event.EmployeeOnboarding
                              >> sagaMessage orgId corrId
                           )

                        mailbox.Parent() <!| asyncMsg
               }

               match priorState, state, e with
               | None,
                 Saga.EmployeeOnboarding _,
                 EmployeeOnboardingSagaEvent.Start evt ->
                  EmployeeOnboardingSaga.onStartEventPersisted deps evt
               | Some(Saga.EmployeeOnboarding priorState),
                 Saga.EmployeeOnboarding state,
                 _ ->
                  EmployeeOnboardingSaga.onEventPersisted
                     deps
                     priorState
                     state
                     e
               | _ -> notHandled ()
            | Event.CardSetup e ->
               let deps: CardSetupSaga.PersistenceHandlerDependencies = {
                  getEmployeeRef = getEmployeeRef
                  getEmailRef = getEmailRef
                  createCardViaThirdPartyProvider =
                     CardSetupSaga.createCardViaThirdPartyProvider
                  sendMessageToSelf =
                     fun orgId corrId asyncEvt ->
                        let asyncMsg =
                           asyncEvt
                           |> Async.map (
                              Event.CardSetup >> sagaMessage orgId corrId
                           )

                        mailbox.Parent() <!| asyncMsg
               }

               match priorState, state, e with
               | None, Saga.CardSetup _, CardSetupSagaEvent.Start evt ->
                  CardSetupSaga.onStartEventPersisted deps evt
               | Some(Saga.CardSetup priorState), Saga.CardSetup state, _ ->
                  CardSetupSaga.onEventPersisted deps priorState state e
               | _ -> notHandled ()
            | Event.Purchase e ->
               let purchaseDeps: PurchaseSaga.PersistenceHandlerDependencies = {
                  getEmployeeRef = getEmployeeRef
                  getAccountRef = getAccountRef
                  getEmailRef = getEmailRef
                  cardNetworkConfirmPurchase =
                     PurchaseSaga.cardNetworkConfirmPurchase
                  cardNetworkRejectPurchase =
                     PurchaseSaga.cardNetworkRejectPurchase
                  syncPurchaseToPartnerBank =
                     PurchaseSaga.syncPurchaseToPartnerBank
                  sendMessageToSelf =
                     fun purchase asyncEvt ->
                        let asyncMsg =
                           asyncEvt
                           |> Async.map (
                              Event.Purchase
                              >> sagaMessage
                                    purchase.OrgId
                                    purchase.CorrelationId
                           )

                        mailbox.Parent() <!| asyncMsg
               }

               match priorState, state, e with
               | None, Saga.Purchase _, PurchaseSagaEvent.Start evt ->
                  PurchaseSaga.onStartEventPersisted purchaseDeps evt
               | Some(Saga.Purchase priorState), Saga.Purchase state, _ ->
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
               }

               match priorState, state, evt with
               | None,
                 Saga.DomesticTransfer _,
                 DomesticTransferSagaEvent.Start evt ->
                  DomesticTransferSaga.onStartEventPersisted deps evt
               | Some(Saga.DomesticTransfer priorState),
                 Saga.DomesticTransfer state,
                 _ ->
                  DomesticTransferSaga.onEventPersisted
                     deps
                     priorState
                     state
                     evt
               | _ -> notHandled ()
            | Event.PlatformTransfer evt ->
               let deps: PlatformTransferSaga.PersistenceHandlerDependencies = {
                  getSchedulingRef = getSchedulingRef
                  getAccountRef = getAccountRef
                  getEmailRef = getEmailRef
                  syncToPartnerBank =
                     PlatformTransferSaga.syncTransferToPartnerBank
                  sendMessageToSelf =
                     fun transfer asyncEvt ->
                        let asyncMsg =
                           asyncEvt
                           |> Async.map (
                              Event.PlatformTransfer
                              >> sagaMessage
                                    transfer.Sender.OrgId
                                    (TransferId.toCorrelationId
                                       transfer.TransferId)
                           )

                        mailbox.Parent() <!| asyncMsg
               }

               match priorState, state, evt with
               | None,
                 Saga.PlatformTransfer _,
                 PlatformTransferSagaEvent.Start evt ->
                  PlatformTransferSaga.onStartEventPersisted deps evt
               | Some(Saga.PlatformTransfer priorState),
                 Saga.PlatformTransfer state,
                 _ ->
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
                  syncToPartnerBank =
                     PlatformPaymentSaga.syncPaymentToPartnerBank
                  refundPaymentToThirdParty =
                     PlatformPaymentSaga.refundPaymentToThirdParty
                  sendMessageToSelf =
                     fun payment asyncEvt ->
                        let asyncMsg =
                           asyncEvt
                           |> Async.map (
                              Event.PlatformPayment
                              >> sagaMessage
                                    payment.Payee.OrgId
                                    (PaymentId.toCorrelationId payment.Id)
                           )

                        mailbox.Parent() <!| asyncMsg
               }

               match priorState, state, evt with
               | None,
                 Saga.PlatformPayment _,
                 PlatformPaymentSagaEvent.Start evt ->
                  PlatformPaymentSaga.onStartEventPersisted deps evt
               | Some(Saga.PlatformPayment priorState),
                 Saga.PlatformPayment state,
                 _ ->
                  PlatformPaymentSaga.onEventPersisted deps priorState state evt
               | _ -> notHandled ()
            | Event.Billing evt ->
               let deps: BillingSaga.PersistenceHandlerDependencies = {
                  getAccountRef = getAccountRef
                  getEmailRef = getEmailRef
               }

               match priorState, state, evt with
               | None, Saga.Billing _, BillingSagaEvent.Start evt ->
                  BillingSaga.onStartEventPersisted deps evt
               | Some(Saga.Billing priorState), Saga.Billing state, _ ->
                  BillingSaga.onEventPersisted deps priorState state evt
               | _ -> notHandled ()
   }

let getEntityRef = SagaActor.get<Event>

let initProps
   (getOrgRef: OrgId -> IEntityRef<OrgMessage>)
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (getEmailActor: ActorSystem -> IActorRef<EmailMessage>)
   (getDomesticTransferRef: ActorSystem -> IActorRef<DomesticTransferMessage>)
   (getSchedulingRef: ActorSystem -> IActorRef<SchedulerMessage>)
   (supervisorOpts: PersistenceSupervisorOptions)
   (sagaPassivateIdleEntityAfter: TimeSpan)
   (persistenceId: string)
   =
   SagaActor.initProps<Saga, Event>
      supervisorOpts
      sagaPassivateIdleEntityAfter
      persistenceId
      (sagaHandler
         getOrgRef
         getEmployeeRef
         getAccountRef
         getEmailActor
         getDomesticTransferRef
         getSchedulingRef)
