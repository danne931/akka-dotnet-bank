module PlatformPaymentSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Saga
open Bank.Account.Domain
open Bank.Transfer.Domain
open Email

[<RequireQualifiedAccess>]
type PlatformPaymentSagaStartEvent =
   | PaymentRequested of BankEvent<PlatformPaymentRequested>

[<RequireQualifiedAccess>]
type PlatformPaymentSagaEvent =
   | Start of PlatformPaymentSagaStartEvent
   | PaymentRequestCancelled
   | PaymentRequestDeclined
   | PayerAccountDeductedFunds of BankEvent<PlatformPaymentPaid>
   | PayerAccountUnableToDeductFunds of PlatformPaymentFailReason
   | PayeeAccountDepositedFunds
   | PayeeAccountUnableToDepositFunds of
      PlatformPaymentFailReason *
      PaymentMethod
   | PaymentRequestNotificationSentToPayer
   | PaymentPaidNotificationSentToPayer
   | PaymentDepositedNotificationSentToPayee
   | PaymentDeclinedNotificationSentToPayee
   | PartnerBankSyncResponse of Result<string, string>
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
         | WaitForSupportTeamToResolvePartnerBankSync -> 1
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
         | RefundPayerAccount -> Some(TimeSpan.FromSeconds 5)

type PlatformPaymentSaga = {
   Events: PlatformPaymentSagaEvent list
   Status: PlatformPaymentSagaStatus
   PaymentInfo: PlatformPaymentBaseInfo
   LifeCycle: SagaLifeCycle<Activity>
} with

   member x.PaymentSyncedToPartnerBank =
      x.LifeCycle.Completed
      |> List.exists (fun w -> w.Activity = Activity.SyncToPartnerBank)

   member x.PaymentPaidNotificationSentToPayer =
      x.LifeCycle.Completed
      |> List.exists (fun w -> w.Activity = Activity.NotifyPayerOfPaymentSent)

   member x.PaymentDepositedNotificationSentToPayee =
      x.LifeCycle.Completed
      |> List.exists (fun w ->
         w.Activity = Activity.NotifyPayeeOfPaymentDeposit)

let applyEvent
   (state: PlatformPaymentSaga option)
   (e: PlatformPaymentSagaEvent)
   (timestamp: DateTime)
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp
   let failActivity = SagaLifeCycle.failActivity timestamp
   let retryActivity = SagaLifeCycle.retryActivity timestamp

   match state with
   | None ->
      match e with
      | Event.Start evt ->
         match evt with
         | StartEvent.PaymentRequested evt ->
            Some {
               Events = [ e ]
               Status =
                  PlatformPaymentSagaStatus.InProgress
                     PlatformPaymentStatus.Unpaid
               PaymentInfo = evt.Data.BaseInfo
               LifeCycle = {
                  SagaLifeCycle.empty with
                     InProgress = [
                        ActivityLifeCycle.init
                           timestamp
                           Activity.NotifyPayerOfRequest
                        ActivityLifeCycle.init timestamp Activity.WaitForPayment
                     ]
               }
            }
      | _ -> state
   | Some state ->
      let state =
         match e with
         | Event.Start _ -> state
         | Event.PaymentRequestCancelled -> {
            state with
               Status = PlatformPaymentSagaStatus.Completed
               LifeCycle =
                  state.LifeCycle |> finishActivity Activity.WaitForPayment
           }
         | Event.PaymentRequestDeclined -> {
            state with
               Status =
                  PlatformPaymentSagaStatus.InProgress
                     PlatformPaymentStatus.Declined
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.WaitForPayment
                  |> addActivity Activity.NotifyPayeeOfDecline
           }
         | Event.PayerAccountDeductedFunds _ -> {
            state with
               Status =
                  PlatformPaymentSagaStatus.InProgress
                     PlatformPaymentStatus.Paid
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.WaitForPayment
                  |> addActivity Activity.DepositToPayeeAccount
           }
         | Event.PayerAccountUnableToDeductFunds reason -> {
            state with
               LifeCycle =
                  state.LifeCycle |> failActivity Activity.WaitForPayment
               Status = PlatformPaymentSagaStatus.Failed reason
           }
         | Event.PayeeAccountDepositedFunds -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.DepositToPayeeAccount
                  |> addActivity Activity.SyncToPartnerBank
                  |> addActivity Activity.NotifyPayerOfPaymentSent
                  |> addActivity Activity.NotifyPayeeOfPaymentDeposit
               Status =
                  PlatformPaymentSagaStatus.InProgress
                     PlatformPaymentStatus.Deposited
           }
         | Event.PayeeAccountUnableToDepositFunds(reason, payMethod) -> {
            state with
               Status = PlatformPaymentSagaStatus.Failed reason
               LifeCycle =
                  state.LifeCycle
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

               if state.LifeCycle.ActivityHasRemainingAttempts activity then
                  {
                     state with
                        LifeCycle = retryActivity activity state.LifeCycle
                  }
               else
                  {
                     state with
                        Status =
                           PlatformPaymentFailReason.PartnerBankSync err
                           |> PlatformPaymentSagaStatus.Failed
                        LifeCycle =
                           state.LifeCycle
                           |> failActivity Activity.SyncToPartnerBank
                           |> addActivity
                                 Activity.WaitForSupportTeamToResolvePartnerBankSync
                  }
            | Ok _ -> {
               state with
                  Status =
                     if
                        state.PaymentPaidNotificationSentToPayer
                        && state.PaymentDepositedNotificationSentToPayee
                     then
                        PlatformPaymentSagaStatus.Completed
                     else
                        state.Status
                  LifeCycle =
                     state.LifeCycle
                     |> finishActivity Activity.SyncToPartnerBank
              }
         | Event.SupportTeamResolvedPartnerBankSync -> {
            state with
               Status =
                  PlatformPaymentSagaStatus.InProgress
                     PlatformPaymentStatus.Deposited
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity
                        Activity.WaitForSupportTeamToResolvePartnerBankSync
                  |> addActivity Activity.SyncToPartnerBank
           }
         | Event.PaymentRequestNotificationSentToPayer -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.NotifyPayerOfRequest
           }
         | Event.PaymentPaidNotificationSentToPayer -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.NotifyPayerOfPaymentSent
               Status =
                  if
                     state.PaymentDepositedNotificationSentToPayee
                     && state.PaymentSyncedToPartnerBank
                  then
                     PlatformPaymentSagaStatus.Completed
                  else
                     state.Status
           }
         | Event.PaymentDepositedNotificationSentToPayee -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.NotifyPayeeOfPaymentDeposit
               Status =
                  if
                     state.PaymentPaidNotificationSentToPayer
                     && state.PaymentSyncedToPartnerBank
                  then
                     PlatformPaymentSagaStatus.Completed
                  else
                     state.Status
           }
         | Event.PaymentDeclinedNotificationSentToPayee -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.NotifyPayeeOfDecline
               Status = PlatformPaymentSagaStatus.Completed
           }
         | Event.ThirdPartyPaymentMethodRefunded -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.RefundThirdPartyPaymentMethod
           }
         | Event.PayerAccountRefunded -> {
            state with
               LifeCycle =
                  state.LifeCycle |> finishActivity Activity.RefundPayerAccount
           }
         | Event.EvaluateRemainingWork -> {
            state with
               LifeCycle =
                  SagaLifeCycle.retryActivitiesAfterInactivity
                     timestamp
                     state.LifeCycle
           }
         | Event.ResetInProgressActivityAttempts -> {
            state with
               LifeCycle =
                  SagaLifeCycle.resetInProgressActivities state.LifeCycle
           }

      Some {
         state with
            Events = e :: state.Events
      }

let stateTransition
   (state: PlatformPaymentSaga option)
   (evt: PlatformPaymentSagaEvent)
   (timestamp: DateTime)
   : Result<PlatformPaymentSaga option, SagaStateTransitionError>
   =
   match state with
   | None ->
      match evt with
      | PlatformPaymentSagaEvent.Start _ -> Ok(applyEvent state evt timestamp)
      | _ -> Error SagaStateTransitionError.HasNotStarted
   | Some saga ->
      let eventIsStartEvent =
         match evt with
         | PlatformPaymentSagaEvent.Start _ -> true
         | _ -> false

      let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

      let invalidStepProgression =
         match evt with
         | PlatformPaymentSagaEvent.Start _
         | PlatformPaymentSagaEvent.EvaluateRemainingWork
         | PlatformPaymentSagaEvent.ResetInProgressActivityAttempts -> false
         | PlatformPaymentSagaEvent.PayerAccountDeductedFunds _
         | PlatformPaymentSagaEvent.PayerAccountUnableToDeductFunds _
         | PlatformPaymentSagaEvent.PaymentRequestDeclined
         | PlatformPaymentSagaEvent.PaymentRequestCancelled ->
            activityIsDone Activity.WaitForPayment
         | PlatformPaymentSagaEvent.PayeeAccountUnableToDepositFunds _
         | PlatformPaymentSagaEvent.PayeeAccountDepositedFunds ->
            activityIsDone Activity.DepositToPayeeAccount
         | PlatformPaymentSagaEvent.PartnerBankSyncResponse _ ->
            activityIsDone Activity.SyncToPartnerBank
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
      elif eventIsStartEvent then
         Error SagaStateTransitionError.HasAlreadyStarted
      elif invalidStepProgression then
         Error SagaStateTransitionError.InvalidStepProgression
      else
         Ok(applyEvent state evt timestamp)

type PersistenceHandlerDependencies = {
   getAccountRef: AccountId -> IEntityRef<AccountMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   refundPaymentToThirdParty:
      PlatformPaymentBaseInfo -> ThirdPartyPaymentMethod -> Async<Event>
   syncToPartnerBank: PlatformPaymentBaseInfo -> Async<Event>
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

   let depositToPayeeAccount (e: BankEvent<PlatformPaymentPaid>) =
      let msg =
         DepositPlatformPaymentCommand.create
            (payment.Payee.AccountId, payment.Payee.OrgId)
            e.InitiatedBy
            {
               BaseInfo = payment
               PaymentMethod = e.Data.PaymentMethod
            }
         |> AccountCommand.DepositPlatformPayment
         |> AccountMessage.StateChange

      dep.getAccountRef payment.Payee.AccountId <! msg

   let refundPayment (reason, payMethod) =
      match payMethod with
      | PaymentMethod.Platform _ ->
         let msg =
            RefundPlatformPaymentCommand.create
               (payment.Payee.AccountId, payment.Payee.OrgId)
               Initiator.System
               {
                  BaseInfo = payment
                  Reason = PlatformPaymentRefundReason.PaymentFailed reason
               }
            |> AccountCommand.RefundPlatformPayment
            |> AccountMessage.StateChange

         dep.getAccountRef payment.Payee.AccountId <! msg
      | PaymentMethod.ThirdParty payMethod ->
         dep.sendMessageToSelf
            payment
            (dep.refundPaymentToThirdParty payment payMethod)

   let syncToPartnerBank () =
      dep.sendMessageToSelf payment (dep.syncToPartnerBank payment)

   match evt with
   | Event.Start _ -> ()
   | Event.PaymentRequestCancelled -> ()
   | Event.PaymentRequestDeclined -> notifyPayeeOfPaymentDecline ()
   | Event.PayerAccountDeductedFunds e -> depositToPayeeAccount e
   | Event.PayeeAccountDepositedFunds ->
      notifyPayerOfPaymentSent ()
      notifyPayeeOfPaymentDeposit ()
      syncToPartnerBank ()
   | Event.PartnerBankSyncResponse res ->
      match res with
      | Ok _ -> ()
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
         | Activity.DepositToPayeeAccount ->
            state.Events
            |> List.tryPick (function
               | PlatformPaymentSagaEvent.PayerAccountDeductedFunds e -> Some e
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

let syncPaymentToPartnerBank (info: PlatformPaymentBaseInfo) = async {
   // HTTP to partner bank
   do! Async.Sleep(1000)

   return Ok "some res" |> Event.PartnerBankSyncResponse
}

let refundPaymentToThirdParty
   (info: PlatformPaymentBaseInfo)
   (payMethod: ThirdPartyPaymentMethod)
   =
   async {
      do! Async.Sleep(1500)

      return Event.ThirdPartyPaymentMethodRefunded
   }
