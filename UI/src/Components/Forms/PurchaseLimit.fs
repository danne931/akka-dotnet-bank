module Bank.Employee.Forms.PurchaseLimitForm

open Feliz
open Fable.Form.Simple

open Bank.Employee.Domain
open UIDomain.Employee
open FormContainer

type Values = { Duration: string; Amount: string }

let durationSelect =
   Form.selectField {
      Parser = Ok
      Value = fun values -> values.Duration
      Update = fun newValue values -> { values with Duration = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Duration:"
         Placeholder = ""
         Options = [ "daily", "Daily"; "monthly", "Monthly" ]
      }
   }

[<ReactComponent>]
let PurchaseLimitFormComponent
   (session: UserSession)
   (notifyParentOnSubmit: EmployeeCommandReceipt -> unit)
   (card: Card)
   (employee: Employee)
   =
   let initiatedBy = session.AsInitiator

   let form =
      durationSelect
      |> Form.andThen (function
         | "daily" ->
            Form.succeed (
               DailyPurchaseLimitForm.onSubmit card employee initiatedBy
            )
            |> Form.append (
               DailyPurchaseLimitForm.dailyPurchaseLimitField
               |> Form.mapValues {
                  Value = fun a -> { Amount = a.Amount }
                  Update = fun a b -> { b with Amount = a.Amount }
               }
            )
         | _ ->
            Form.succeed (
               MonthlyPurchaseLimitForm.onSubmit card employee initiatedBy
            )
            |> Form.append (
               MonthlyPurchaseLimitForm.monthlyPurchaseLimitField
               |> Form.mapValues {
                  Value = fun a -> { Amount = a.Amount }
                  Update = fun a b -> { b with Amount = a.Amount }
               }
            ))

   EmployeeFormContainer {|
      InitialValues = { Amount = ""; Duration = "daily" }
      Form = form
      Action = None
      OnSubmit = notifyParentOnSubmit
   |}
