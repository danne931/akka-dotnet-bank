module PlatformPaymentSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Saga
open Bank.Account.Domain
open Bank.Transfer.Domain
open Email
open PartnerBank.Service.Domain

[<RequireQualifiedAccess>]
type PlatformPaymentSagaStartEvent =
   | PaymentRequested of BankEvent<PlatformPaymentRequested>

[<RequireQualifiedAccess>]
type PlatformPaymentSagaEvent =
   | PaymentRequestCancelled
   | PaymentRequestDeclined
   | PayerAccountDeductedFunds of
      BankEvent<PlatformPaymentPending> *
      PartnerBankAccountLink
   | PayerAccountUnableToDeductFunds of PlatformPaymentFailReason
   | PayeeAccountDepositedFunds of PartnerBankAccountLink
   | PayeeAccountUnableToDepositFunds of
      PlatformPaymentFailReason *
      PaymentMethod
   | PaymentRequestNotificationSentToPayer
   | PaymentPaidNotificationSentToPayer
   | PaymentDepositedNotificationSentToPayee
   | PaymentDeclinedNotificationSentToPayee
   | PartnerBankSyncResponse of Result<SettlementId, string>
   | PaymentSettled
   | SupportTeamResolvedPartnerBankSync
   | ThirdPartyPaymentMethodRefunded
   | PayerAccountRefunded
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

type private StartEvent = PlatformPaymentSagaStartEvent
type private Event = PlatformPaymentSagaEvent

[<RequireQualifiedAccess>]
type PlatformPaymentSagaStatus =
   | InProgress of PlatformPaymentStatus
   | Completed
   | Failed of PlatformPaymentFailReason

[<RequireQualifiedAccess>]
type Activity =
   | NotifyPayerOfRequest
   | NotifyPayeeOfDecline
   | DepositToPayeeAccount
   | SyncToPartnerBank
   | SettlePayment
   | NotifyPayerOfPaymentSent
   | NotifyPayeeOfPaymentDeposit
   | RefundThirdPartyPaymentMethod
   | RefundPayerAccount
   | WaitForPayment
   | WaitForSupportTeamToResolvePartnerBankSync

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForPayment
         | WaitForSupportTeamToResolvePartnerBankSync -> 0
         | SyncToPartnerBank -> 4
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | WaitForPayment
         | WaitForSupportTeamToResolvePartnerBankSync -> None
         | SyncToPartnerBank
         | RefundThirdPartyPaymentMethod
         | NotifyPayerOfPaymentSent
         | NotifyPayerOfRequest
         | NotifyPayeeOfDecline
         | NotifyPayeeOfPaymentDeposit -> Some(TimeSpan.FromMinutes 4)
         | DepositToPayeeAccount
         | RefundPayerAccount
         | SettlePayment -> Some(TimeSpan.FromSeconds 5)

type PlatformPaymentSaga = {
   StartEvent: PlatformPaymentSagaStartEvent
   Events: PlatformPaymentSagaEvent list
   Status: PlatformPaymentSagaStatus
   PaymentInfo: PlatformPaymentBaseInfo
   LifeCycle: SagaLifeCycle<Activity>
   PartnerBankSenderAccountLink: PartnerBankAccountLink option
   PartnerBankRecipientAccountLink: PartnerBankAccountLink option
   InitiatedBy: Initiator
} with

   member x.PaymentSyncedToPartnerBank =
      x.LifeCycle.Completed
      |> List.exists (fun w -> w.Activity = Activity.SyncToPartnerBank)

   member x.IsSettled =
      x.LifeCycle.Completed
      |> List.exists (fun w -> w.Activity = Activity.SettlePayment)

   member x.SettlementId =
      x.Events
      |> List.tryPick (function
         | PlatformPaymentSagaEvent.PartnerBankSyncResponse(Ok settlementId) ->
            Some settlementId
         | _ -> None)

   member x.PaymentMethod =
      x.Events
      |> List.tryPick (function
         | PlatformPaymentSagaEvent.PayerAccountDeductedFunds(e, _) ->
            Some e.Data.PaymentMethod
         | _ -> None)

   member x.PaymentPaidNotificationSentToPayer =
      x.LifeCycle.Completed
      |> List.exists (fun w -> w.Activity = Activity.NotifyPayerOfPaymentSent)

   member x.PaymentDepositedNotificationSentToPayee =
      x.LifeCycle.Completed
      |> List.exists (fun w ->
         w.Activity = Activity.NotifyPayeeOfPaymentDeposit)

let applyStartEvent (e: PlatformPaymentSagaStartEvent) (timestamp: DateTime) =
   match e with
   | StartEvent.PaymentRequested evt -> {
      StartEvent = e
      Events = []
      Status = PlatformPaymentSagaStatus.InProgress PlatformPaymentStatus.Unpaid
      PaymentInfo = evt.Data.BaseInfo
      PartnerBankSenderAccountLink = None
      PartnerBankRecipientAccountLink = None
      InitiatedBy = evt.InitiatedBy
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init timestamp Activity.NotifyPayerOfRequest
               ActivityLifeCycle.init timestamp Activity.WaitForPayment
            ]
      }
     }

let applyEvent
   (saga: PlatformPaymentSaga)
   (evt: PlatformPaymentSagaEvent)
   (timestamp: DateTime)
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp
   let failActivity = SagaLifeCycle.failActivity timestamp
   let retryActivity = SagaLifeCycle.retryActivity timestamp

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | Event.PaymentRequestCancelled -> {
      saga with
         Status = PlatformPaymentSagaStatus.Completed
         LifeCycle = saga.LifeCycle |> finishActivity Activity.WaitForPayment
     }
   | Event.PaymentRequestDeclined -> {
      saga with
         Status =
            PlatformPaymentSagaStatus.InProgress PlatformPaymentStatus.Declined
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.WaitForPayment
            |> addActivity Activity.NotifyPayeeOfDecline
     }
   | Event.PayerAccountDeductedFunds(_, senderLink) -> {
      saga with
         PartnerBankSenderAccountLink = Some senderLink
         Status =
            PlatformPaymentSagaStatus.InProgress
               PlatformPaymentStatus.PaymentPending
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.WaitForPayment
            |> addActivity Activity.DepositToPayeeAccount
     }
   | Event.PayerAccountUnableToDeductFunds reason -> {
      saga with
         LifeCycle = saga.LifeCycle |> failActivity Activity.WaitForPayment
         Status = PlatformPaymentSagaStatus.Failed reason
     }
   | Event.PayeeAccountDepositedFunds recipientLink -> {
      saga with
         PartnerBankRecipientAccountLink = Some recipientLink
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.DepositToPayeeAccount
            |> addActivity Activity.SyncToPartnerBank
            |> addActivity Activity.NotifyPayerOfPaymentSent
            |> addActivity Activity.NotifyPayeeOfPaymentDeposit
         Status =
            PlatformPaymentSagaStatus.InProgress PlatformPaymentStatus.Deposited
     }
   | Event.PayeeAccountUnableToDepositFunds(reason, payMethod) -> {
      saga with
         Status = PlatformPaymentSagaStatus.Failed reason
         LifeCycle =
            saga.LifeCycle
            |> failActivity Activity.DepositToPayeeAccount
            |> addActivity (
               match payMethod with
               | PaymentMethod.Platform _ -> Activity.RefundPayerAccount
               | PaymentMethod.ThirdParty _ ->
                  Activity.RefundThirdPartyPaymentMethod
            )
     }
   | Event.PartnerBankSyncResponse res ->
      match res with
      | Error err ->
         let activity = Activity.SyncToPartnerBank

         if saga.LifeCycle.ActivityHasRemainingAttempts activity then
            {
               saga with
                  LifeCycle = retryActivity activity saga.LifeCycle
            }
         else
            {
               saga with
                  Status =
                     PlatformPaymentFailReason.PartnerBankSync err
                     |> PlatformPaymentSagaStatus.Failed
                  LifeCycle =
                     saga.LifeCycle
                     |> failActivity Activity.SyncToPartnerBank
                     |> addActivity
                           Activity.WaitForSupportTeamToResolvePartnerBankSync
            }
      | Ok _ -> {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.SyncToPartnerBank
               |> addActivity Activity.SettlePayment
        }
   | Event.PaymentSettled -> {
      saga with
         Status =
            if
               saga.PaymentPaidNotificationSentToPayer
               && saga.PaymentDepositedNotificationSentToPayee
            then
               PlatformPaymentSagaStatus.Completed
            else
               saga.Status
         LifeCycle = saga.LifeCycle |> finishActivity Activity.SettlePayment
     }
   | Event.SupportTeamResolvedPartnerBankSync -> {
      saga with
         Status =
            PlatformPaymentSagaStatus.InProgress PlatformPaymentStatus.Deposited
         LifeCycle =
            saga.LifeCycle
            |> finishActivity
                  Activity.WaitForSupportTeamToResolvePartnerBankSync
            |> addActivity Activity.SyncToPartnerBank
     }
   | Event.PaymentRequestNotificationSentToPayer -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.NotifyPayerOfRequest
     }
   | Event.PaymentPaidNotificationSentToPayer -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.NotifyPayerOfPaymentSent
         Status =
            if
               saga.PaymentDepositedNotificationSentToPayee && saga.IsSettled
            then
               PlatformPaymentSagaStatus.Completed
            else
               saga.Status
     }
   | Event.PaymentDepositedNotificationSentToPayee -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.NotifyPayeeOfPaymentDeposit
         Status =
            if saga.PaymentPaidNotificationSentToPayer && saga.IsSettled then
               PlatformPaymentSagaStatus.Completed
            else
               saga.Status
     }
   | Event.PaymentDeclinedNotificationSentToPayee -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.NotifyPayeeOfDecline
         Status = PlatformPaymentSagaStatus.Completed
     }
   | Event.ThirdPartyPaymentMethodRefunded -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.RefundThirdPartyPaymentMethod
     }
   | Event.PayerAccountRefunded -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.RefundPayerAccount
     }
   | Event.EvaluateRemainingWork -> {
      saga with
         LifeCycle =
            SagaLifeCycle.retryActivitiesAfterInactivity
               timestamp
               saga.LifeCycle
     }
   | Event.ResetInProgressActivityAttempts -> {
      saga with
         LifeCycle = SagaLifeCycle.resetInProgressActivities saga.LifeCycle
     }

let stateTransitionStart
   (evt: PlatformPaymentSagaStartEvent)
   (timestamp: DateTime)
   : Result<PlatformPaymentSaga, SagaStateTransitionError>
   =
   Ok(applyStartEvent evt timestamp)

let stateTransition
   (saga: PlatformPaymentSaga)
   (evt: PlatformPaymentSagaEvent)
   (timestamp: DateTime)
   : Result<PlatformPaymentSaga, SagaStateTransitionError>
   =
   let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

   let invalidStepProgression =
      match evt with
      | PlatformPaymentSagaEvent.EvaluateRemainingWork
      | PlatformPaymentSagaEvent.ResetInProgressActivityAttempts -> false
      | PlatformPaymentSagaEvent.PayerAccountDeductedFunds _
      | PlatformPaymentSagaEvent.PayerAccountUnableToDeductFunds _
      | PlatformPaymentSagaEvent.PaymentRequestDeclined
      | PlatformPaymentSagaEvent.PaymentRequestCancelled ->
         activityIsDone Activity.WaitForPayment
      | PlatformPaymentSagaEvent.PayeeAccountUnableToDepositFunds _
      | PlatformPaymentSagaEvent.PayeeAccountDepositedFunds _ ->
         activityIsDone Activity.DepositToPayeeAccount
      | PlatformPaymentSagaEvent.PartnerBankSyncResponse _ ->
         activityIsDone Activity.SyncToPartnerBank
      | PlatformPaymentSagaEvent.PaymentSettled ->
         activityIsDone Activity.SettlePayment
      | PlatformPaymentSagaEvent.SupportTeamResolvedPartnerBankSync ->
         activityIsDone Activity.WaitForSupportTeamToResolvePartnerBankSync
      | PlatformPaymentSagaEvent.PaymentRequestNotificationSentToPayer ->
         activityIsDone Activity.NotifyPayerOfRequest
      | PlatformPaymentSagaEvent.PaymentPaidNotificationSentToPayer ->
         activityIsDone Activity.NotifyPayerOfPaymentSent
      | PlatformPaymentSagaEvent.PaymentDepositedNotificationSentToPayee ->
         activityIsDone Activity.NotifyPayeeOfPaymentDeposit
      | PlatformPaymentSagaEvent.PaymentDeclinedNotificationSentToPayee ->
         activityIsDone Activity.NotifyPayeeOfDecline
      | PlatformPaymentSagaEvent.PayerAccountRefunded ->
         activityIsDone Activity.RefundPayerAccount
      | PlatformPaymentSagaEvent.ThirdPartyPaymentMethodRefunded ->
         activityIsDone Activity.RefundThirdPartyPaymentMethod

   if saga.Status = PlatformPaymentSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

type PersistenceHandlerDependencies = {
   getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   getPartnerBankServiceRef: unit -> IActorRef<PartnerBankServiceMessage>
   refundPaymentToThirdParty:
      PlatformPaymentBaseInfo -> ThirdPartyPaymentMethod -> Async<Event>
   sendMessageToSelf: PlatformPaymentBaseInfo -> Async<Event> -> unit
}

let notifyPayerOfPaymentRequest emailRef (payment: PlatformPaymentBaseInfo) =
   let msg =
      EmailMessage.create
         payment.Payer.OrgId
         (PaymentId.toCorrelationId payment.Id)
         (EmailInfo.PlatformPaymentRequested {
            Amount = payment.Amount
            PayeeBusinessName = payment.Payee.OrgName
            PayerBusinessName = payment.Payer.OrgName
         })

   emailRef <! msg

let onStartEventPersisted
   (dep: PersistenceHandlerDependencies)
   (evt: StartEvent)
   =
   match evt with
   | StartEvent.PaymentRequested e ->
      notifyPayerOfPaymentRequest (dep.getEmailRef ()) e.Data.BaseInfo

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
   (previousState: PlatformPaymentSaga)
   (state: PlatformPaymentSaga)
   (evt: Event)
   =
   let payment = state.PaymentInfo
   let correlationId = PaymentId.toCorrelationId payment.Id
   let emailRef = dep.getEmailRef ()

   let notifyPayerOfPaymentSent () =
      let msg =
         EmailMessage.create
            payment.Payer.OrgId
            correlationId
            (EmailInfo.PlatformPaymentPaid {
               PayerBusinessName = payment.Payer.OrgName
               PayeeBusinessName = payment.Payee.OrgName
               Amount = payment.Amount
            })

      emailRef <! msg

   let notifyPayeeOfPaymentDeposit () =
      let msg =
         EmailMessage.create
            payment.Payee.OrgId
            correlationId
            (EmailInfo.PlatformPaymentDeposited {
               PayerBusinessName = payment.Payer.OrgName
               PayeeBusinessName = payment.Payee.OrgName
               Amount = payment.Amount
            })

      emailRef <! msg

   let notifyPayeeOfPaymentDecline () =
      let msg =
         EmailMessage.create
            payment.Payee.OrgId
            correlationId
            (EmailInfo.PlatformPaymentDeclined {
               PayerBusinessName = payment.Payer.OrgName
               PayeeBusinessName = payment.Payee.OrgName
               Amount = payment.Amount
            })

      emailRef <! msg

   let depositToPayeeAccount (e: BankEvent<PlatformPaymentPending>) =
      let msg =
         DepositPlatformPaymentCommand.create e.InitiatedBy {
            BaseInfo = payment
            PaymentMethod = e.Data.PaymentMethod
         }
         |> AccountCommand.DepositPlatformPayment
         |> AccountMessage.StateChange

      dep.getAccountRef payment.Payee.ParentAccountId <! msg

   let refundPayment (reason, payMethod) =
      match payMethod with
      | PaymentMethod.Platform _ ->
         let msg =
            RefundPlatformPaymentCommand.create state.InitiatedBy {
               BaseInfo = payment
               Reason = PlatformPaymentRefundReason.PaymentFailed reason
               PaymentMethod = payMethod
            }
            |> AccountCommand.RefundPlatformPayment
            |> AccountMessage.StateChange

         dep.getAccountRef payment.Payee.ParentAccountId <! msg
      | PaymentMethod.ThirdParty payMethod ->
         dep.sendMessageToSelf
            payment
            (dep.refundPaymentToThirdParty payment payMethod)

   let syncToPartnerBank () =
      match
         state.PartnerBankSenderAccountLink,
         state.PartnerBankRecipientAccountLink
      with
      | Some sender, Some recipient ->
         dep.getPartnerBankServiceRef ()
         <! PartnerBankServiceMessage.TransferBetweenOrganizations {
            Amount = payment.Amount
            From = sender
            To = recipient
            Metadata = {
               OrgId = payment.Payee.OrgId
               CorrelationId = correlationId
            }
            ReplyTo = TransferSagaReplyTo.PlatformPayment
         }
      | _ -> ()

   let settlePayment settlementId payMethod =
      let msg =
         SettlePlatformPaymentCommand.create state.InitiatedBy {
            BaseInfo = payment
            SettlementId = settlementId
            PaymentMethod = payMethod
         }
         |> AccountCommand.SettlePlatformPayment
         |> AccountMessage.StateChange

      dep.getAccountRef payment.Payee.ParentAccountId <! msg

   match evt with
   | Event.PaymentRequestCancelled -> ()
   | Event.PaymentRequestDeclined -> notifyPayeeOfPaymentDecline ()
   | Event.PayerAccountDeductedFunds(e, _) -> depositToPayeeAccount e
   | Event.PayeeAccountDepositedFunds _ ->
      notifyPayerOfPaymentSent ()
      notifyPayeeOfPaymentDeposit ()
      syncToPartnerBank ()
   | Event.PartnerBankSyncResponse res ->
      match res with
      | Ok settlementId ->
         state.PaymentMethod |> Option.iter (settlePayment settlementId)
      | Error reason ->
         if
            previousState.LifeCycle.ActivityHasRemainingAttempts
               Activity.SyncToPartnerBank
         then
            syncToPartnerBank ()
         else
            let msg =
               EmailMessage.create
                  payment.Payee.OrgId
                  correlationId
                  (EmailInfo.ApplicationErrorRequiresSupport reason)

            emailRef <! msg
   | Event.PaymentSettled -> ()
   | Event.PayerAccountUnableToDeductFunds _ -> ()
   | Event.PayeeAccountUnableToDepositFunds(reason, payMethod) ->
      refundPayment (reason, payMethod)
   | Event.SupportTeamResolvedPartnerBankSync ->
      // Support team resolved dispute with partner bank so
      // reattempt syncing transaction to partner bank.
      syncToPartnerBank ()
   | Event.ResetInProgressActivityAttempts
   | Event.ThirdPartyPaymentMethodRefunded
   | Event.PayerAccountRefunded
   | Event.PaymentDeclinedNotificationSentToPayee
   | Event.PaymentRequestNotificationSentToPayer
   | Event.PaymentPaidNotificationSentToPayer
   | Event.PaymentDepositedNotificationSentToPayee -> ()
   | Event.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.NotifyPayerOfRequest ->
            notifyPayerOfPaymentRequest emailRef payment
         | Activity.NotifyPayeeOfDecline -> notifyPayeeOfPaymentDecline ()
         | Activity.NotifyPayerOfPaymentSent -> notifyPayerOfPaymentSent ()
         | Activity.NotifyPayeeOfPaymentDeposit ->
            notifyPayeeOfPaymentDeposit ()
         | Activity.SyncToPartnerBank -> syncToPartnerBank ()
         | Activity.SettlePayment ->
            match state.SettlementId, state.PaymentMethod with
            | Some id, Some payMethod -> settlePayment id payMethod
            | _ -> ()
         | Activity.DepositToPayeeAccount ->
            state.Events
            |> List.tryPick (function
               | PlatformPaymentSagaEvent.PayerAccountDeductedFunds(e, _) ->
                  Some e
               | _ -> None)
            |> Option.iter depositToPayeeAccount
         | Activity.RefundThirdPartyPaymentMethod
         | Activity.RefundPayerAccount ->
            state.Events
            |> List.tryPick (function
               | Event.PayeeAccountUnableToDepositFunds(reason, payMethod) ->
                  Some(reason, payMethod)
               | _ -> None)
            |> Option.iter refundPayment
         | Activity.WaitForPayment
         | Activity.WaitForSupportTeamToResolvePartnerBankSync -> ()

let refundPaymentToThirdParty
   (info: PlatformPaymentBaseInfo)
   (payMethod: ThirdPartyPaymentMethod)
   =
   async {
      do! Async.Sleep(1500)

      return Event.ThirdPartyPaymentMethodRefunded
   }
