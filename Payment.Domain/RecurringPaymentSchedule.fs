module RecurringPaymentSchedule

open System

type RecurrenceScheduleId =
   | RecurrenceScheduleId of Guid

   member x.Value = let (RecurrenceScheduleId id) = x in id

[<RequireQualifiedAccess>]
type WeekOfMonth =
   | First
   | Second
   | Third
   | Fourth
   | Last

[<RequireQualifiedAccess>]
type RecurrenceIntervalMonthly =
   | DayOfMonth of int
   /// Ex: (Second, Wednesday) of month
   | WeekAndDay of WeekOfMonth * DayOfWeek

[<RequireQualifiedAccess>]
type RecurrenceInterval =
   | Monthly of RecurrenceIntervalMonthly
   | Weekly of DayOfWeek

type RecurrencePattern = {
   RepeatEvery: int
   Interval: RecurrenceInterval
}

[<RequireQualifiedAccess>]
type RecurrenceTerminationCondition =
   | EndDate of DateTime
   | MaxPayments of int
   | Never

type RecurrenceSettings = {
   Id: RecurrenceScheduleId
   Pattern: RecurrencePattern
   Termination: RecurrenceTerminationCondition
   PaymentsRequestedCount: int
}

let rec private shiftToTargetDayOfWeek (date: DateTime) (dayOfWeek: DayOfWeek) =
   if date.DayOfWeek = dayOfWeek then
      date
   else
      shiftToTargetDayOfWeek (date.AddDays 1) dayOfWeek

let nextPaymentDueDate (settings: RecurrenceSettings) (dueAt: DateTime) =
   match settings.Pattern.Interval with
   | RecurrenceInterval.Weekly dayOfWeek ->
      let date = shiftToTargetDayOfWeek dueAt dayOfWeek
      date.AddDays(float (settings.Pattern.RepeatEvery * 7))
   | RecurrenceInterval.Monthly interval ->
      match interval with
      | RecurrenceIntervalMonthly.DayOfMonth day ->
         let daysInMonth = DateTime.DaysInMonth(dueAt.Year, dueAt.Month)

         // Protect from day out of bounds (< 1 or > daysInMonth)
         let day = max day 1 |> min daysInMonth

         let date = DateTime(dueAt.Year, dueAt.Month, day)
         date.AddMonths settings.Pattern.RepeatEvery
      | RecurrenceIntervalMonthly.WeekAndDay(weekOfMonth, dayOfWeek) ->
         let nextMonth = dueAt.AddMonths settings.Pattern.RepeatEvery
         let year = nextMonth.Year
         let month = nextMonth.Month

         let firstOfMonth = DateTime(year, month, 1)

         let targetWeekStart =
            match weekOfMonth with
            | WeekOfMonth.First -> firstOfMonth
            | WeekOfMonth.Second -> firstOfMonth.AddDays 7
            | WeekOfMonth.Third -> firstOfMonth.AddDays 14
            | WeekOfMonth.Fourth -> firstOfMonth.AddDays 21
            | WeekOfMonth.Last ->
               let daysInMonth =
                  DateTime.DaysInMonth(nextMonth.Year, nextMonth.Month)

               let lastOfMonth = DateTime(year, month, daysInMonth)

               if lastOfMonth.DayOfWeek = dayOfWeek then
                  lastOfMonth
               else
                  lastOfMonth.AddDays -7

         shiftToTargetDayOfWeek targetWeekStart dayOfWeek

let canSendAnotherPaymentRequest
   (settings: RecurrenceSettings)
   (previousDueDate: DateTime)
   =
   match settings.Termination with
   | RecurrenceTerminationCondition.Never -> true
   | RecurrenceTerminationCondition.EndDate endDate ->
      let nextDue = nextPaymentDueDate settings previousDueDate
      nextDue.Date <= endDate.Date
   | RecurrenceTerminationCondition.MaxPayments maxPayments ->
      settings.PaymentsRequestedCount < maxPayments

let hasNextPaymentDueDate
   (settings: RecurrenceSettings)
   (dueAt: DateTime)
   : DateTime option
   =
   if canSendAnotherPaymentRequest settings dueAt then
      Some(nextPaymentDueDate settings dueAt)
   else
      None

let hasNextPaymentDueIn
   (settings: RecurrenceSettings)
   (dueAt: DateTime)
   : TimeSpan option
   =
   hasNextPaymentDueDate settings dueAt |> Option.map ((-) dueAt)

type PaymentScheduleComputeProps = {
   Settings: RecurrenceSettings
   DueAt: DateTime
   MaxPayments: int
}

let computePaymentDueDateSchedule
   (opts: PaymentScheduleComputeProps)
   : DateTime list
   =
   let maxPaymentsDueDates = min opts.MaxPayments 100

   (0, opts.DueAt)
   |> List.unfold (fun (counter, currDueDate) ->
      let agg due = due, (counter + 1, due)

      if counter = 0 then
         Some(agg currDueDate)
      elif counter = maxPaymentsDueDates then
         None
      else
         hasNextPaymentDueDate opts.Settings currDueDate |> Option.map agg)

type PaymentRequestNotificationScheduleProps = {
   Now: DateTime
   DueAt: DateTime
   IsSubsequentRecurringPayment: bool
}

/// Compute notification schedule based on payment due date
let computePaymentRequestNotificationSchedule
   (props: PaymentRequestNotificationScheduleProps)
   : TimeSpan list
   =
   let timeUntilDue = props.DueAt - props.Now
   let daysUntilDue = timeUntilDue.TotalDays

   [
      match props.IsSubsequentRecurringPayment, daysUntilDue < 3. with
      | false, _
      | true, true -> TimeSpan.Zero
      | true, false -> ()

      if daysUntilDue >= 60. then
         timeUntilDue - TimeSpan.FromDays 30.
      elif daysUntilDue >= 30. then
         timeUntilDue - TimeSpan.FromDays 15.
      elif daysUntilDue >= 15. then
         timeUntilDue - TimeSpan.FromDays 7.
      elif daysUntilDue >= 3. then
         timeUntilDue - TimeSpan.FromDays 3.

      if timeUntilDue.TotalHours > 24. then
         timeUntilDue - TimeSpan.FromDays 1.
   ]
