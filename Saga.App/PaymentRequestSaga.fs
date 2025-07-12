module PaymentRequestSaga

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
type PaymentRequestSagaStartEvent =
   | PaymentRequested of BankEvent<PaymentRequested>

[<RequireQualifiedAccess>]
type PaymentRequestSagaEvent =
   | PaymentRequestCancelled
   | PaymentRequestDeclined
   | PaymentFulfilled of PaymentFulfillment
   | PaymentFailed of TransferId * PaymentFailReason
   | PaymentRequestNotificationSentToPayer
   | PaymentDeclinedNotificationSentToPayee
   | ScheduledPaymentReminderActivated
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

type private StartEvent = PaymentRequestSagaStartEvent
type private Event = PaymentRequestSagaEvent

[<RequireQualifiedAccess>]
type PaymentRequestSagaStatus =
   | InProgress of PaymentRequestStatus
   | Completed
   | Failed of PaymentFailReason

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

type PaymentRequestSaga = {
   StartEvent: PaymentRequestSagaStartEvent
   Events: PaymentRequestSagaEvent list
   Status: PaymentRequestSagaStatus
   PaymentInfo: PaymentRequested
   LifeCycle: SagaLifeCycle<Activity>
   InitiatedBy: Initiator
}

let applyStartEvent (e: PaymentRequestSagaStartEvent) (timestamp: DateTime) =
   match e with
   | StartEvent.PaymentRequested evt -> {
      StartEvent = e
      Events = []
      Status =
         PaymentRequestSagaStatus.InProgress PaymentRequestStatus.Requested
      PaymentInfo = evt.Data
      InitiatedBy = evt.InitiatedBy
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init timestamp Activity.NotifyPayerOfRequest
               ActivityLifeCycle.init timestamp Activity.WaitForPayment

               match
                  shouldSchedulePaymentReminder
                     evt.Data.SharedDetails.Expiration
               with
               | Some timeUntil ->
                  ActivityLifeCycle.init
                     timestamp
                     (Activity.WaitForScheduledPaymentReminder timeUntil)
               | None -> ()
            ]
      }
     }

let applyEvent
   (saga: PaymentRequestSaga)
   (evt: PaymentRequestSagaEvent)
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
         Status = PaymentRequestSagaStatus.Completed
         LifeCycle = finishWaitingForPayment saga.LifeCycle
     }
   | Event.PaymentRequestDeclined -> {
      saga with
         Status =
            PaymentRequestSagaStatus.InProgress PaymentRequestStatus.Declined
         LifeCycle =
            saga.LifeCycle
            |> finishWaitingForPayment
            |> addActivity Activity.NotifyPayeeOfDecline
     }
   | Event.PaymentFulfilled _ -> {
      saga with
         LifeCycle = finishWaitingForPayment saga.LifeCycle
         Status = PaymentRequestSagaStatus.Completed
     }
   | Event.PaymentFailed(_, reason) -> {
      saga with
         Status = PaymentRequestSagaStatus.Failed reason
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
         Status = PaymentRequestSagaStatus.Completed
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
   (evt: PaymentRequestSagaStartEvent)
   (timestamp: DateTime)
   : Result<PaymentRequestSaga, SagaStateTransitionError>
   =
   Ok(applyStartEvent evt timestamp)

let stateTransition
   (saga: PaymentRequestSaga)
   (evt: PaymentRequestSagaEvent)
   (timestamp: DateTime)
   : Result<PaymentRequestSaga, SagaStateTransitionError>
   =
   let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

   let invalidStepProgression =
      match evt with
      | PaymentRequestSagaEvent.EvaluateRemainingWork
      | PaymentRequestSagaEvent.ResetInProgressActivityAttempts -> false
      | PaymentRequestSagaEvent.PaymentRequestDeclined
      | PaymentRequestSagaEvent.PaymentRequestCancelled
      | PaymentRequestSagaEvent.PaymentFailed _
      | PaymentRequestSagaEvent.PaymentFulfilled _ ->
         activityIsDone Activity.WaitForPayment
      | PaymentRequestSagaEvent.PaymentRequestNotificationSentToPayer ->
         activityIsDone Activity.NotifyPayerOfRequest
      | PaymentRequestSagaEvent.PaymentDeclinedNotificationSentToPayee ->
         activityIsDone Activity.NotifyPayeeOfDecline
      | PaymentRequestSagaEvent.ScheduledPaymentReminderActivated ->
         activityIsDone (Activity.WaitForScheduledPaymentReminder TimeSpan.Zero)

   if saga.Status = PaymentRequestSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

let private notifyPayerOfPaymentRequest emailRef (payment: PaymentRequested) =
   let corrId = PaymentRequestId.toCorrelationId payment.SharedDetails.Id
   let payeeOrgName = payment.SharedDetails.Payee.OrgName
   let amount = payment.SharedDetails.Amount

   let msg =
      match payment with
      | PaymentRequested.Platform info ->
         EmailMessage.create
            info.Payer.OrgId
            corrId
            (EmailInfo.PlatformPaymentRequested {
               Amount = amount
               PayeeBusinessName = payeeOrgName
               PayerBusinessName = info.Payer.OrgName
            })
      | PaymentRequested.ThirdParty info ->
         EmailMessage.create
            payment.SharedDetails.Payee.OrgId
            corrId
            (EmailInfo.ThirdPartyPaymentRequested {
               Amount = amount
               PayeeBusinessName = payeeOrgName
               PayerEmail = info.Payer.Email
               SecurePaymentFormUrl = info.ShortId.AsUrl
            })

   emailRef <! msg

let private notifyPayeeOfPaymentDecline
   (payment: PaymentRequested)
   (emailRef: IActorRef<EmailMessage>)
   =
   let shared = payment.SharedDetails

   let msg =
      EmailMessage.create
         shared.Payee.OrgId
         (PaymentRequestId.toCorrelationId shared.Id)
         (EmailInfo.PlatformPaymentDeclined {
            Amount = shared.Amount
            PayeeBusinessName = shared.Payee.OrgName
            PayerBusinessName = payment.PayerName
         })

   emailRef <! msg

let private remindPayerOfPaymentRequest
   (payment: PaymentRequested)
   (emailRef: IActorRef<EmailMessage>)
   =
   let shared = payment.SharedDetails
   let corrId = PaymentRequestId.toCorrelationId shared.Id

   let msg =
      match payment with
      | PaymentRequested.Platform info ->
         EmailMessage.create
            info.Payer.OrgId
            corrId
            (EmailInfo.PlatformPaymentReminder {
               Amount = shared.Amount
               PayeeBusinessName = shared.Payee.OrgName
               PayerBusinessName = info.Payer.OrgName
            })
      | PaymentRequested.ThirdParty info ->
         EmailMessage.create
            shared.Payee.OrgId
            corrId
            (EmailInfo.ThirdPartyPaymentReminder {
               Amount = shared.Amount
               PayeeBusinessName = shared.Payee.OrgName
               PayerEmail = info.Payer.Email
               SecurePaymentFormUrl = info.ShortId.AsUrl
            })

   emailRef <! msg

let onStartEventPersisted
   (getEmailRef: unit -> IActorRef<EmailMessage>)
   (evt: StartEvent)
   =
   match evt with
   | StartEvent.PaymentRequested e ->
      notifyPayerOfPaymentRequest (getEmailRef ()) e.Data

type PersistenceHandlerDependencies = {
   getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   getPartnerBankServiceRef: unit -> IActorRef<PartnerBankServiceMessage>
   sendEventToSelf: PaymentRequested -> PaymentRequestSagaEvent -> unit
}

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
   (previousState: PaymentRequestSaga)
   (state: PaymentRequestSaga)
   (evt: Event)
   =
   let payment = state.PaymentInfo
   let emailRef = dep.getEmailRef ()

   match evt with
   | Event.ScheduledPaymentReminderActivated ->
      remindPayerOfPaymentRequest payment emailRef
   | Event.PaymentRequestCancelled -> ()
   | Event.PaymentRequestDeclined ->
      notifyPayeeOfPaymentDecline payment emailRef
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
               PaymentRequestSagaEvent.ScheduledPaymentReminderActivated
         | Activity.NotifyPayeeOfDecline ->
            notifyPayeeOfPaymentDecline payment emailRef
         | Activity.WaitForPayment -> ()
