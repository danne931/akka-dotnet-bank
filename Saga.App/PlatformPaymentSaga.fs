module PlatformPaymentSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Saga
open Bank.Account.Domain
open Bank.Payment.Domain
open Email
open PartnerBank.Service.Domain

// If payment due date is 24+ hours in the future then schedule a
// reminder notification to be sent 24 hours before the due date.
let shouldSchedulePaymentReminder (dueAt: DateTime) =
   if dueAt > DateTime.UtcNow.AddHours 24 then
      let reminderTime = dueAt.AddHours -24
      let timeUntil = reminderTime - DateTime.UtcNow
      Some timeUntil
   else
      None

[<RequireQualifiedAccess>]
type PlatformPaymentSagaStartEvent =
   | PaymentRequested of BankEvent<PlatformPaymentRequested>

[<RequireQualifiedAccess>]
type PlatformPaymentSagaEvent =
   | PaymentRequestCancelled
   | PaymentRequestDeclined
   | PaymentFulfilled of PaymentFulfilled
   | PaymentFailed of TransferId * PlatformPaymentFailReason
   | PaymentRequestNotificationSentToPayer
   | PaymentDeclinedNotificationSentToPayee
   | ScheduledPaymentReminderActivated
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

type private StartEvent = PlatformPaymentSagaStartEvent
type private Event = PlatformPaymentSagaEvent

[<RequireQualifiedAccess>]
type PlatformPaymentSagaStatus =
   | InProgress of PaymentRequestStatus
   | Completed
   | Failed of PlatformPaymentFailReason

[<RequireQualifiedAccess; CustomEquality; NoComparison>]
type Activity =
   | NotifyPayerOfRequest
   | NotifyPayeeOfDecline
   | WaitForScheduledPaymentReminder of TimeSpan
   | WaitForPayment

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForScheduledPaymentReminder _
         | WaitForPayment -> 0
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | WaitForPayment -> None
         | WaitForScheduledPaymentReminder time -> Some time
         | NotifyPayerOfRequest
         | NotifyPayeeOfDecline -> Some(TimeSpan.FromMinutes 4.)

   // Custom equality check so we can check for completeness
   // of WaitForScheduledPaymentReminder without comparing the inner value.
   // Ex: activityIsDone (Activity.WaitForScheduledPaymentReminder TimeSpan.Zero)
   override x.Equals compareTo =
      match compareTo with
      | :? Activity as compareTo -> x.GetHashCode() = compareTo.GetHashCode()
      | _ -> false

   override x.GetHashCode() =
      match x with
      | WaitForScheduledPaymentReminder _ ->
         hash "WaitForScheduledPaymentReminder"
      | _ -> hash (string x)

type PlatformPaymentSaga = {
   StartEvent: PlatformPaymentSagaStartEvent
   Events: PlatformPaymentSagaEvent list
   Status: PlatformPaymentSagaStatus
   PaymentInfo: PlatformPaymentBaseInfo
   LifeCycle: SagaLifeCycle<Activity>
   InitiatedBy: Initiator
}

let applyStartEvent (e: PlatformPaymentSagaStartEvent) (timestamp: DateTime) =
   match e with
   | StartEvent.PaymentRequested evt -> {
      StartEvent = e
      Events = []
      Status =
         PlatformPaymentSagaStatus.InProgress PaymentRequestStatus.Requested
      PaymentInfo = evt.Data.BaseInfo
      InitiatedBy = evt.InitiatedBy
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init timestamp Activity.NotifyPayerOfRequest
               ActivityLifeCycle.init timestamp Activity.WaitForPayment

               match shouldSchedulePaymentReminder evt.Data.Expiration with
               | Some timeUntil ->
                  ActivityLifeCycle.init
                     timestamp
                     (Activity.WaitForScheduledPaymentReminder timeUntil)
               | None -> ()
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
   let abortActivity = SagaLifeCycle.abortActivity timestamp

   // Finishes waiting for payment and aborts the payment reminder, if any.
   let finishWaitingForPayment =
      finishActivity Activity.WaitForPayment
      >> abortActivity (Activity.WaitForScheduledPaymentReminder TimeSpan.Zero)

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | Event.ScheduledPaymentReminderActivated -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity (
               Activity.WaitForScheduledPaymentReminder TimeSpan.Zero
            )
     }
   | Event.PaymentRequestCancelled -> {
      saga with
         Status = PlatformPaymentSagaStatus.Completed
         LifeCycle = finishWaitingForPayment saga.LifeCycle
     }
   | Event.PaymentRequestDeclined -> {
      saga with
         Status =
            PlatformPaymentSagaStatus.InProgress PaymentRequestStatus.Declined
         LifeCycle =
            saga.LifeCycle
            |> finishWaitingForPayment
            |> addActivity Activity.NotifyPayeeOfDecline
     }
   | Event.PaymentFulfilled _ -> {
      saga with
         LifeCycle = finishWaitingForPayment saga.LifeCycle
         Status = PlatformPaymentSagaStatus.Completed
     }
   | Event.PaymentFailed(_, reason) -> {
      saga with
         Status = PlatformPaymentSagaStatus.Failed reason
         LifeCycle =
            saga.LifeCycle
            |> failActivity Activity.WaitForPayment
            |> abortActivity (
               Activity.WaitForScheduledPaymentReminder TimeSpan.Zero
            )
     }
   | Event.PaymentRequestNotificationSentToPayer -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.NotifyPayerOfRequest
     }
   | Event.PaymentDeclinedNotificationSentToPayee -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.NotifyPayeeOfDecline
         Status = PlatformPaymentSagaStatus.Completed
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
      | PlatformPaymentSagaEvent.PaymentRequestDeclined
      | PlatformPaymentSagaEvent.PaymentRequestCancelled
      | PlatformPaymentSagaEvent.PaymentFailed _
      | PlatformPaymentSagaEvent.PaymentFulfilled _ ->
         activityIsDone Activity.WaitForPayment
      | PlatformPaymentSagaEvent.PaymentRequestNotificationSentToPayer ->
         activityIsDone Activity.NotifyPayerOfRequest
      | PlatformPaymentSagaEvent.PaymentDeclinedNotificationSentToPayee ->
         activityIsDone Activity.NotifyPayeeOfDecline
      | PlatformPaymentSagaEvent.ScheduledPaymentReminderActivated ->
         activityIsDone (Activity.WaitForScheduledPaymentReminder TimeSpan.Zero)

   if saga.Status = PlatformPaymentSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

let notifyPayerOfPaymentRequest emailRef (payment: PlatformPaymentBaseInfo) =
   let msg =
      EmailMessage.create
         payment.Payer.OrgId
         (PaymentRequestId.toCorrelationId payment.Id)
         (EmailInfo.PlatformPaymentRequested {
            Amount = payment.Amount
            PayeeBusinessName = payment.Payee.OrgName
            PayerBusinessName = payment.Payer.OrgName
         })

   emailRef <! msg

let onStartEventPersisted
   (getEmailRef: unit -> IActorRef<EmailMessage>)
   (evt: StartEvent)
   =
   match evt with
   | StartEvent.PaymentRequested e ->
      notifyPayerOfPaymentRequest (getEmailRef ()) e.Data.BaseInfo

type PersistenceHandlerDependencies = {
   getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   getPartnerBankServiceRef: unit -> IActorRef<PartnerBankServiceMessage>
   sendEventToSelf: PlatformPaymentBaseInfo -> PlatformPaymentSagaEvent -> unit
}

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
   (previousState: PlatformPaymentSaga)
   (state: PlatformPaymentSaga)
   (evt: Event)
   =
   let payment = state.PaymentInfo
   let correlationId = PaymentRequestId.toCorrelationId payment.Id
   let emailRef = dep.getEmailRef ()

   let notifyPayeeOfPaymentDecline () =
      let msg =
         EmailMessage.create
            payment.Payee.OrgId
            correlationId
            (EmailInfo.PlatformPaymentRequested {
               Amount = payment.Amount
               PayeeBusinessName = payment.Payee.OrgName
               PayerBusinessName = payment.Payer.OrgName
            })

      emailRef <! msg

   let remindPayerOfPaymentRequest () =
      let msg =
         EmailMessage.create
            payment.Payer.OrgId
            correlationId
            (EmailInfo.PlatformPaymentReminder {
               Amount = payment.Amount
               PayeeBusinessName = payment.Payee.OrgName
               PayerBusinessName = payment.Payer.OrgName
            })

      emailRef <! msg

   match evt with
   | Event.ScheduledPaymentReminderActivated -> remindPayerOfPaymentRequest ()
   | Event.PaymentRequestCancelled -> ()
   | Event.PaymentRequestDeclined -> notifyPayeeOfPaymentDecline ()
   | Event.PaymentFulfilled _ -> ()
   | Event.PaymentFailed _ -> ()
   | Event.ResetInProgressActivityAttempts
   | Event.PaymentDeclinedNotificationSentToPayee
   | Event.PaymentRequestNotificationSentToPayer
   | Event.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.NotifyPayerOfRequest ->
            notifyPayerOfPaymentRequest emailRef payment
         | Activity.WaitForScheduledPaymentReminder _ ->
            dep.sendEventToSelf
               payment
               PlatformPaymentSagaEvent.ScheduledPaymentReminderActivated
         | Activity.NotifyPayeeOfDecline -> notifyPayeeOfPaymentDecline ()
         | Activity.WaitForPayment -> ()
