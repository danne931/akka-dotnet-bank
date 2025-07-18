module Bank.Account.Forms.RecurringPaymentForm

open Feliz
open Fable.Form.Simple
open Fable.Form.Simple.Pico
open System
open RecurringPaymentSchedule
open Lib.Time
open Lib.SharedTypes

[<RequireQualifiedAccess>]
type SelectedUnit =
   | Weekly
   | Monthly

type RecurrenceValues = {
   IsRecurring: bool
   Interval: int
   Unit: SelectedUnit
   PatternType: string
   DayOfMonth: int
   WeekOrdinal: int
   DayOfWeek: DayOfWeek
   TerminationType: string
   EndDate: string
   MaxPayments: int
}

let private recurrenceRepeatEveryForm
   : Form.Form<RecurrenceValues, SelectedUnit * int, IReactProperty> =
   let repeatEveryField (selectedUnit: SelectedUnit) =
      let maxInterval =
         match selectedUnit with
         | SelectedUnit.Weekly -> 52
         | SelectedUnit.Monthly -> 12

      Form.selectField {
         Parser = int >> Ok
         Value = _.Interval >> string
         Update = fun newValue values -> { values with Interval = int newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Repeat every:"
            Placeholder = ""
            Options = [ for i in 1..maxInterval -> string i, string i ]
         }
      }

   let unitField =
      Form.selectField {
         Parser =
            function
            | "Weekly" -> Ok SelectedUnit.Weekly
            | "Monthly" -> Ok SelectedUnit.Monthly
            | _ -> Error "Invalid unit"
         Value = fun values -> string values.Unit
         Update =
            fun newValue values -> {
               values with
                  Unit =
                     if newValue = "Weekly" then
                        SelectedUnit.Weekly
                     else
                        SelectedUnit.Monthly
            }
         Error = fun _ -> None
         Attributes = {
            Label = "Unit:"
            Placeholder = "Select unit"
            Options = [ "Monthly", "months"; "Weekly", "weeks" ]
         }
      }

   Form.succeed (fun repeatEvery unit -> unit, repeatEvery)
   |> Form.append (Form.meta (_.Unit >> repeatEveryField))
   |> Form.append unitField
   |> Form.group

let private dayOfWeekField =
   Form.selectField {
      Parser =
         function
         | "1" -> Ok DayOfWeek.Monday
         | "2" -> Ok DayOfWeek.Tuesday
         | "3" -> Ok DayOfWeek.Wednesday
         | "4" -> Ok DayOfWeek.Thursday
         | "5" -> Ok DayOfWeek.Friday
         | _ -> Error "Invalid day of week"
      Value = _.DayOfWeek >> string
      Update =
         fun newValue values ->
            let day =
               match newValue with
               | "1" -> DayOfWeek.Monday
               | "2" -> DayOfWeek.Tuesday
               | "3" -> DayOfWeek.Wednesday
               | "4" -> DayOfWeek.Thursday
               | "5" -> DayOfWeek.Friday
               | _ -> values.DayOfWeek

            { values with DayOfWeek = day }
      Error = fun _ -> None
      Attributes = {
         Label = "Day of week:"
         Placeholder = ""
         Options = [
            "1", "Monday"
            "2", "Tuesday"
            "3", "Wednesday"
            "4", "Thursday"
            "5", "Friday"
         ]
      }
   }

let private recurrenceIntervalMonthlyForm
   : Form.Form<RecurrenceValues, RecurrenceIntervalMonthly, IReactProperty> =
   let dayOfMonthField =
      let date = DateTime.Now
      let daysInMonth = DateTime.DaysInMonth(date.Year, date.Month)

      Form.selectField {
         Parser =
            fun day ->
               let day = int day

               if day >= 1 && day <= daysInMonth then
                  Ok(RecurrenceIntervalMonthly.DayOfMonth day)
               else
                  Error $"Day of month must be between 1 and {daysInMonth}"
         Value = _.DayOfMonth >> string
         Update =
            fun newValue values -> {
               values with
                  DayOfMonth = int newValue
            }
         Error = fun _ -> None
         Attributes = {
            Label = "Repeats on day:"
            Placeholder = ""
            Options = [ for day in 1..daysInMonth -> string day, string day ]
         }
      }

   let weekOrdinalField =
      Form.selectField {
         Parser =
            function
            | "0" -> WeekOfMonth.First
            | "1" -> WeekOfMonth.Second
            | "2" -> WeekOfMonth.Third
            | "3" -> WeekOfMonth.Fourth
            | _ -> WeekOfMonth.Last
            >> Ok
         Value = _.WeekOrdinal >> string
         Update =
            fun newValue values -> {
               values with
                  WeekOrdinal = int newValue
            }
         Error = fun _ -> None
         Attributes = {
            Label = "On the:"
            Placeholder = ""
            Options = [
               "0", "1st"
               "1", "2nd"
               "2", "3rd"
               "3", "4th"
               "4", "Last"
            ]
         }
      }

   let weekAndDayForm =
      Form.succeed (fun week day ->
         RecurrenceIntervalMonthly.WeekAndDay(week, day))
      |> Form.append weekOrdinalField
      |> Form.append dayOfWeekField

   let patternTypeField =
      Form.selectField {
         Parser = Ok
         Value = _.PatternType
         Update = fun newValue values -> { values with PatternType = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Repeats on:"
            Placeholder = ""
            Options = [
               "day_of_month", "Specific day of month (e.g., 15th)"
               "day_of_week", "Specific day of week (e.g., 2nd Wednesday)"
            ]
         }
      }

   patternTypeField
   |> Form.andThen (function
      | "day_of_month" -> dayOfMonthField
      | "day_of_week" -> weekAndDayForm
      | _ -> dayOfMonthField)
   |> Form.group

let private recurrenceTerminationForm =
   let lastDueDateByField =
      Form.dateField {
         Parser =
            CustomDateInterpreter.validate
               CustomDateInterpreter.DateSignifier.Single
            >> Result.bind (
               snd
               >> Lib.Validators.dateInFutureValidator "End date"
               >> validationErrorsHumanFriendly
            )
            >> Result.map RecurrenceTerminationCondition.EndDate
         Value = _.EndDate
         Update = fun newValue values -> { values with EndDate = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "No more payments due after:"
            Placeholder = ""
            HtmlAttributes = []
         }
      }

   let maxPaymentsField =
      Form.selectField {
         Parser = int >> RecurrenceTerminationCondition.MaxPayments >> Ok
         Value = _.MaxPayments >> string
         Update =
            fun newValue values -> {
               values with
                  MaxPayments = int newValue
            }
         Error = fun _ -> None
         Attributes = {
            Label = "Number of payments:"
            Placeholder = ""
            Options = [ for num in 2..99 -> string num, string num ]
         }
      }

   let terminationTypeField =
      Form.selectField {
         Parser = Ok
         Value = _.TerminationType
         Update =
            fun newValue values -> {
               values with
                  TerminationType = newValue
            }
         Error = fun _ -> None
         Attributes = {
            Label = "Ends:"
            Placeholder = ""
            Options = [
               "lastDueDateBy", "After a specific date"
               "maxPayments", "After a number of payments"
               "never", "Never (until cancelled)"
            ]
         }
      }

   terminationTypeField
   |> Form.andThen (function
      | "lastDueDateBy" -> lastDueDateByField
      | "maxPayments" -> maxPaymentsField
      | _ -> Form.succeed RecurrenceTerminationCondition.Never)
   |> Form.group

let recurringPaymentForm
   (values: RecurrenceValues)
   : Form.Form<RecurrenceValues, RecurrenceSettings option, IReactProperty>
   =
   if values.IsRecurring then
      Form.succeed (fun (pattern: RecurrencePattern) termination ->
         Some {
            Id = RecurrenceScheduleId(Guid.NewGuid())
            Pattern = pattern
            Termination = termination
            PaymentsRequestedCount = 1
         })
      |> Form.append (
         recurrenceRepeatEveryForm
         |> Form.andThen (fun (unit: SelectedUnit, repeatEvery: int) ->
            match unit with
            | SelectedUnit.Monthly ->
               Form.succeed (fun interval -> {
                  RepeatEvery = repeatEvery
                  Interval = RecurrenceInterval.Monthly interval
               })
               |> Form.append recurrenceIntervalMonthlyForm
            | SelectedUnit.Weekly ->
               Form.succeed (fun dayOfWeek -> {
                  RepeatEvery = repeatEvery
                  Interval = RecurrenceInterval.Weekly dayOfWeek
               })
               |> Form.append dayOfWeekField)
      )
      |> Form.append recurrenceTerminationForm
   else
      Form.succeed None

let defaultRecurringPaymentValues = {
   IsRecurring = false
   Interval = 1
   Unit = SelectedUnit.Monthly
   PatternType = "day_of_month"
   DayOfMonth = 1
   WeekOrdinal = 1
   DayOfWeek = DayOfWeek.Monday
   TerminationType = "never"
   EndDate = DateTime.Now.AddMonths 12 |> DateTime.format
   MaxPayments = 12
}

/// Recurring payment form conditionally displayed
/// based on "Repeat this payment" checkbox.
let recurringPaymentFormOptional
   : Form.Form<RecurrenceValues, RecurrenceSettings option, IReactProperty> =
   Form.checkboxField {
      Parser = Ok
      Value = _.IsRecurring
      Update = fun newValue values -> { values with IsRecurring = newValue }
      Error = fun _ -> None
      Attributes = { Text = "Repeat payment" }
   }
   |> Form.andThen (fun isRecurring ->
      recurringPaymentForm {
         defaultRecurringPaymentValues with
            IsRecurring = isRecurring
      })
