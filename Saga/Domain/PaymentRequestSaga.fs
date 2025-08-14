module PaymentRequestSaga

open System

open Lib.SharedTypes
open Lib.Saga
open Bank.Payment.Domain
open RecurringPaymentSchedule

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
   | PaymentSagaStartedForNextRecurringPayment
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

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
   | ScheduleNextRecurringPayment of nextDueDate: DateTime

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
         | ScheduleNextRecurringPayment _ -> Some(TimeSpan.FromSeconds 10.)

   override x.Equals compareTo =
      match compareTo with
      | :? Activity as compareTo -> x.GetHashCode() = compareTo.GetHashCode()
      | _ -> false

   override x.GetHashCode() =
      match x with
      | ScheduleNextRecurringPayment _ -> hash "ScheduleNextRecurringPayment"
      | _ -> hash (string x)

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

type PaymentRequestSaga = {
   StartEvent: PaymentRequestSagaStartEvent
   Events: PaymentRequestSagaEvent list
   Status: PaymentRequestSagaStatus
   PaymentInfo: PaymentRequested
   LifeCycle: SagaLifeCycle<Activity>
   InitiatedBy: Initiator
} with

   member x.NextRecurringPaymentDueDate =
      x.LifeCycle.InProgress
      |> List.tryPick (fun activity ->
         match activity.Activity with
         | Activity.ScheduleNextRecurringPayment dueDate -> Some dueDate
         | _ -> None)

   member x.NotifyPayerOfRequest =
      x.LifeCycle.InProgress
      |> List.exists (fun activity ->
         match activity.Activity with
         | Activity.NotifyPayerOfRequest -> true
         | _ -> false)
