module RecurringPaymentSchedule

open System

open Lib.Time

type RecurrenceScheduleId =
   | RecurrenceScheduleId of Guid

   member x.Value = let (RecurrenceScheduleId id) = x in id

[<RequireQualifiedAccess>]
type OccurrenceOfDayInMonth =
   | First
   | Second
   | Third
   | Fourth
   | Last

[<RequireQualifiedAccess>]
type RecurrenceIntervalMonthly =
   | DayOfMonth of int
   /// Ex: (Second, Wednesday) of month
   | OccurrenceOfDay of OccurrenceOfDayInMonth * DayOfWeek

[<RequireQualifiedAccess>]
type RecurrenceInterval =
   | Monthly of RecurrenceIntervalMonthly
   | Weekly of DayOfWeek

type RecurrencePattern = {
   RepeatEvery: int
   Interval: RecurrenceInterval
} with

   member x.Display =
      match x.Interval with
      | RecurrenceInterval.Weekly dayOfWeek ->
         let timeUnit =
            match x.RepeatEvery with
            | 1 -> "Weekly"
            | num -> $"Every {num} weeks"

         $"{timeUnit} on {DateTime.dayOfWeekDisplay dayOfWeek}"
      | RecurrenceInterval.Monthly interval ->
         let timeUnit =
            match x.RepeatEvery with
            | 1 -> "Monthly"
            | num -> $"Every {num} months"

         match interval with
         | RecurrenceIntervalMonthly.DayOfMonth day ->
            $"{timeUnit} on the {DateTime.dayWithOrdinal day}"
         | RecurrenceIntervalMonthly.OccurrenceOfDay(occurrence, dayOfWeek) ->
            let occurrence = (string occurrence).ToLower()
            let dayOfWeek = DateTime.dayOfWeekDisplayShort dayOfWeek
            $"{timeUnit} on the {occurrence} {dayOfWeek}"

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

let private shiftToTargetDayOfWeek
   (date: DateTime)
   (dayOfWeek: DayOfWeek)
   // Ex: Find the third occurrence of Wednesday in the month.
   (skip: int)
   =
   let skip = max 0 skip
   let mutable occurrences = 0

   let rec shift (date: DateTime) (dayOfWeek: DayOfWeek) =
      if date.DayOfWeek = dayOfWeek then
         occurrences <- occurrences + 1

      let skipSatisfied = occurrences = skip + 1

      if date.DayOfWeek = dayOfWeek && skipSatisfied then
         date
      else
         shift (date.AddDays 1) dayOfWeek

   shift date dayOfWeek

let nextPaymentDueDate (settings: RecurrenceSettings) (dueAt: DateTime) =
   match settings.Pattern.Interval with
   | RecurrenceInterval.Weekly dayOfWeek ->
      let date = shiftToTargetDayOfWeek dueAt dayOfWeek 0
      date.AddDays(float (settings.Pattern.RepeatEvery * 7))
   | RecurrenceInterval.Monthly interval ->
      match interval with
      | RecurrenceIntervalMonthly.DayOfMonth day ->
         let daysInMonth = DateTime.DaysInMonth(dueAt.Year, dueAt.Month)

         // Protect from day out of bounds (< 1 or > daysInMonth)
         let day = max day 1 |> min daysInMonth

         let date = DateTime(dueAt.Year, dueAt.Month, day)
         date.AddMonths settings.Pattern.RepeatEvery
      | RecurrenceIntervalMonthly.OccurrenceOfDay(occurrence, dayOfWeek) ->
         let nextMonth = dueAt.AddMonths settings.Pattern.RepeatEvery
         let year = nextMonth.Year
         let month = nextMonth.Month

         let firstOfMonth = DateTime(year, month, 1)
         let shift = shiftToTargetDayOfWeek firstOfMonth dayOfWeek

         match occurrence with
         | OccurrenceOfDayInMonth.First -> shift 0
         | OccurrenceOfDayInMonth.Second -> shift 1
         | OccurrenceOfDayInMonth.Third -> shift 2
         | OccurrenceOfDayInMonth.Fourth -> shift 3
         | OccurrenceOfDayInMonth.Last ->
            let daysInMonth =
               DateTime.DaysInMonth(nextMonth.Year, nextMonth.Month)

            let lastOfMonth = DateTime(year, month, daysInMonth)

            let lastWeekOfMonth =
               if lastOfMonth.DayOfWeek = dayOfWeek then
                  lastOfMonth
               else
                  lastOfMonth.AddDays -7

            shiftToTargetDayOfWeek lastWeekOfMonth dayOfWeek 0


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
   (previousDueDate: DateTime)
   : DateTime option
   =
   if canSendAnotherPaymentRequest settings previousDueDate then
      Some(nextPaymentDueDate settings previousDueDate)
   else
      None

let hasNextPaymentDueIn
   (settings: RecurrenceSettings)
   (previousDueDate: DateTime)
   : TimeSpan option
   =
   hasNextPaymentDueDate settings previousDueDate
   |> Option.map (fun next -> next - previousDueDate)

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
