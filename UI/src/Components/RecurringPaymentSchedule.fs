[<RequireQualifiedAccess>]
module RecurringPaymentScheduleComponent

open RecurringPaymentSchedule
open Lib.Time

open System
open Feliz

let private numberOfPaymentsToDisplayAtMost = 12

let render
   (props:
      {|
         Settings: RecurrenceSettings
         DueAt: DateTime
         PaymentAmount: decimal
      |})
   =
   let scheduleRows =
      RecurringPaymentSchedule.computePaymentSchedule {
         Settings = props.Settings
         DueAt = props.DueAt
         MaxPayments =
            match props.Settings.Termination with
            | RecurrenceTerminationCondition.MaxPayments num ->
               min num numberOfPaymentsToDisplayAtMost
            | _ -> numberOfPaymentsToDisplayAtMost
      }
      |> List.mapi (fun ind v -> ind + 1, v)
      |> List.chunkBySize 6

   Html.details [
      attr.name "payment schedule"
      attr.isOpen true
      attr.classes [ "payment-schedule" ]
      attr.children [
         Html.summary "Payment Schedule"

         for row in scheduleRows do
            classyNode Html.div [ "grid" ] [
               for paymentOrder, dueAt in row do
                  Html.article [
                     Html.small [
                        attr.classes [ "payment-ordering" ]
                        attr.text paymentOrder
                     ]

                     Html.p (DateTime.formatShortWithDayOfWeek dueAt)
                     Html.p [
                        attr.text (Money.format props.PaymentAmount)
                        attr.classes [ "success" ]
                     ]
                  ]
            ]

         Html.br []

         Html.small (
            match props.Settings.Termination with
            | RecurrenceTerminationCondition.Never -> "No end date"
            | RecurrenceTerminationCondition.MaxPayments num ->
               $"Ends after {num} payments"
            | RecurrenceTerminationCondition.EndDate date ->
               $"Ends by {DateTime.format date}"
         )

         Html.br []
      ]
   ]
