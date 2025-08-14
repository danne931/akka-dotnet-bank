[<RequireQualifiedAccess>]
module RecurringPaymentScheduleComponent

open RecurringPaymentSchedule
open Lib.Time

open System
open Feliz

let render
   (props:
      {|
         Settings: RecurrenceSettings
         DueAt: DateTime
         PaymentAmount: decimal
         MaxPaymentsToDisplay: int
         MaxColumns: int
      |})
   =
   let maxColumns = max 1 props.MaxColumns
   let maxPaymentsToDisplay = max 1 props.MaxPaymentsToDisplay

   let scheduleRows =
      RecurringPaymentSchedule.computePaymentDueDateSchedule {
         Settings = props.Settings
         DueAt = props.DueAt
         MaxPayments =
            match props.Settings.Termination with
            | RecurrenceTerminationCondition.MaxPayments num ->
               min num maxPaymentsToDisplay
            | _ -> maxPaymentsToDisplay
      }
      |> List.mapi (fun ind v -> ind + 1, v)
      |> List.chunkBySize maxColumns

   Html.details [
      attr.name "payment due dates"
      attr.isOpen true
      attr.classes [ "payment-schedule" ]
      attr.children [
         Html.summary "Payment Due Date Schedule"

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
               $"No payments due after {DateTime.format date}"
         )

         Html.br []
      ]
   ]
