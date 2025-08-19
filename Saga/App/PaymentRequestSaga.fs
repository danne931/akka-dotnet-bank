module PaymentRequestSaga

open System
open Akkling

open Lib.SharedTypes
open Lib.Saga
open Bank.Account.Domain
open Bank.Payment.Domain
open EmailMessage
open RecurringPaymentSchedule
open PaymentRequestSaga
open BankActorRegistry

type private StartEvent = PaymentRequestSagaStartEvent
type private Event = PaymentRequestSagaEvent

let computedNotificationTimesMappedToActivity
   props
   : ActivityLifeCycle<Activity> list
   =
   RecurringPaymentSchedule.computePaymentRequestNotificationSchedule props
   |> List.map (fun time ->
      let activity =
         if time = TimeSpan.Zero then
            Activity.NotifyPayerOfRequest
         else
            Activity.WaitForScheduledPaymentReminder time

      ActivityLifeCycle.init props.Now activity)

/// Cancel sending of all scheduled payment request reminder notifications.
let abortScheduledPaymentReminders
   (timestamp: DateTime)
   (state: SagaLifeCycle<Activity>)
   : SagaLifeCycle<Activity>
   =
   let activitiesToAbort =
      state.InProgress
      |> List.filter _.Activity.IsWaitForScheduledPaymentReminder
      |> List.map (fun w -> w.finish timestamp)

   let aborted = activitiesToAbort @ state.Aborted

   let wip =
      state.InProgress
      |> List.filter (_.Activity.IsWaitForScheduledPaymentReminder >> not)

   {
      state with
         InProgress = wip
         Aborted = aborted
   }

// If multiple payment reminders scheduled then will mark
// the most recent one as finished.
// (Most recent = smallest InactivityTimeout TimeSpan)
let finishPaymentReminderActivity
   (timestamp: DateTime)
   (state: SagaLifeCycle<Activity>)
   : SagaLifeCycle<Activity>
   =
   let reminders =
      state.InProgress
      |> List.filter _.Activity.IsWaitForScheduledPaymentReminder

   let reminderOpt =
      match reminders with
      | [] -> None
      | _ ->
         reminders
         |> List.mapi (fun index ts -> index, ts)
         |> List.minBy (fun (_, ts) ->
            let (Activity.WaitForScheduledPaymentReminder time) = ts.Activity
            time)
         |> Some

   let complete =
      match reminderOpt with
      | Some(_, activity) -> activity.finish timestamp :: state.Completed
      | None -> state.Completed

   let wip =
      match reminderOpt with
      | Some(index, _) -> state.InProgress |> List.removeAt index
      | None -> state.InProgress

   {
      state with
         InProgress = wip
         Completed = complete
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
               ActivityLifeCycle.init timestamp Activity.WaitForPayment

               yield!
                  computedNotificationTimesMappedToActivity {
                     DueAt = evt.Data.SharedDetails.DueAt
                     Now = timestamp
                     IsSubsequentRecurringPayment =
                        match evt.Data.RecurringPaymentReference with
                        | Some info -> info.Settings.PaymentsRequestedCount > 1
                        | None -> false
                  }
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

   // Finishes waiting for payment and aborts the payment reminder, if any.
   let finishWaitingForPayment =
      finishActivity Activity.WaitForPayment
      >> abortScheduledPaymentReminders timestamp

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | Event.ScheduledPaymentReminderActivated -> {
      saga with
         LifeCycle = finishPaymentReminderActivity timestamp saga.LifeCycle
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
   | Event.PaymentFulfilled fulfillment ->
      let saga = {
         saga with
            LifeCycle = finishWaitingForPayment saga.LifeCycle
      }

      let nextPaymentDueDateOpt =
         saga.PaymentInfo.RecurringPaymentReference
         |> Option.bind (fun info ->
            RecurringPaymentSchedule.hasNextPaymentDueDate
               info.Settings
               saga.PaymentInfo.SharedDetails.DueAt)

      match nextPaymentDueDateOpt with
      | Some dueDate -> {
         saga with
            Status =
               PaymentRequestStatus.Fulfilled fulfillment
               |> PaymentRequestSagaStatus.InProgress
            LifeCycle =
               saga.LifeCycle
               |> addActivity (Activity.ScheduleNextRecurringPayment dueDate)
        }
      | _ -> {
         saga with
            Status = PaymentRequestSagaStatus.Completed
        }
   | Event.PaymentFailed(_, reason) -> {
      saga with
         Status = PaymentRequestSagaStatus.Failed reason
         LifeCycle =
            saga.LifeCycle
            |> failActivity Activity.WaitForPayment
            |> abortScheduledPaymentReminders timestamp
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
   | Event.PaymentSagaStartedForNextRecurringPayment -> {
      saga with
         LifeCycle =
            finishActivity
               (Activity.ScheduleNextRecurringPayment DateTime.UtcNow)
               saga.LifeCycle
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
      | PaymentRequestSagaEvent.PaymentSagaStartedForNextRecurringPayment ->
         activityIsDone (Activity.ScheduleNextRecurringPayment DateTime.UtcNow)

   if saga.Status = PaymentRequestSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

let private notifyPayerOfPaymentRequest emailRef (payment: PaymentRequested) =
   let corrId = payment.SharedDetails.Id.AsCorrelationId
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

let private notifyPayeeOfPaymentDecline (payment: PaymentRequested) emailRef =
   let shared = payment.SharedDetails

   let msg =
      EmailMessage.create
         shared.Payee.OrgId
         shared.Id.AsCorrelationId
         (EmailInfo.PlatformPaymentDeclined {
            Amount = shared.Amount
            PayeeBusinessName = shared.Payee.OrgName
            PayerBusinessName = payment.PayerName
         })

   emailRef <! msg

let private remindPayerOfPaymentRequest (payment: PaymentRequested) emailRef =
   let shared = payment.SharedDetails
   let corrId = shared.Id.AsCorrelationId

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
   (saga: PaymentRequestSaga)
   (registry: #IEmailActor)
   (sendEventToPaymentSaga:
      OrgId -> PaymentRequestId -> PaymentRequestSagaEvent -> unit)
   (evt: StartEvent)
   =
   match evt with
   | StartEvent.PaymentRequested e ->
      if saga.NotifyPayerOfRequest then
         notifyPayerOfPaymentRequest (registry.EmailActor()) e.Data

      e.Data.RecurringPaymentReference
      |> Option.iter (fun info ->
         sendEventToPaymentSaga
            e.OrgId
            info.OriginPaymentId
            PaymentRequestSagaEvent.PaymentSagaStartedForNextRecurringPayment)

type OperationEnv = {
   sendEventToPaymentSaga:
      OrgId -> PaymentRequestId -> PaymentRequestSagaEvent -> unit
}

let onEventPersisted
   (registry: #IEmailActor & #IAccountActor)
   (operationEnv: OperationEnv)
   (previousState: PaymentRequestSaga)
   (state: PaymentRequestSaga)
   (evt: Event)
   =
   let payment = state.PaymentInfo
   let emailRef = registry.EmailActor()

   let scheduleNextRecurringPayment (dueDate: DateTime) =
      let nextPaymentRequestMsg =
         RequestPaymentCommand.fromRecurring state.InitiatedBy payment dueDate
         |> AccountCommand.RequestPayment
         |> AccountMessage.StateChange

      registry.AccountActor payment.SharedDetails.Payee.ParentAccountId
      <! nextPaymentRequestMsg

   match evt with
   | Event.ScheduledPaymentReminderActivated ->
      remindPayerOfPaymentRequest payment emailRef
   | Event.PaymentRequestCancelled -> ()
   | Event.PaymentRequestDeclined ->
      notifyPayeeOfPaymentDecline payment emailRef
   | Event.PaymentFulfilled _ ->
      state.NextRecurringPaymentDueDate
      |> Option.iter scheduleNextRecurringPayment
   | Event.PaymentSagaStartedForNextRecurringPayment
   | Event.PaymentFailed _
   | Event.ResetInProgressActivityAttempts
   | Event.PaymentDeclinedNotificationSentToPayee
   | Event.PaymentRequestNotificationSentToPayer -> ()
   | Event.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.NotifyPayerOfRequest ->
            notifyPayerOfPaymentRequest emailRef payment
         | Activity.WaitForScheduledPaymentReminder _ ->
            operationEnv.sendEventToPaymentSaga
               payment.SharedDetails.Payee.OrgId
               payment.SharedDetails.Id
               PaymentRequestSagaEvent.ScheduledPaymentReminderActivated
         | Activity.NotifyPayeeOfDecline ->
            notifyPayeeOfPaymentDecline payment emailRef
         | Activity.WaitForPayment -> ()
         | Activity.ScheduleNextRecurringPayment nextDate ->
            scheduleNextRecurringPayment nextDate
