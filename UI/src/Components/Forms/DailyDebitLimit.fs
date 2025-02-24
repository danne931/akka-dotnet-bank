module Bank.Employee.Forms.DailyPurchaseLimitForm

open Feliz
open Fable.Form.Simple
open Validus.Operators

open Bank.Employee.Domain
open UIDomain.Employee
open Lib.Validators
open Lib.SharedTypes
open FormContainer

type Values = { Amount: string }

let dailyPurchaseLimitField =
   Form.textField {
      Parser =
         (parseDecimal >=> Card.dailyPurchaseLimitValidator)
            "Daily purchase limit"
         >> validationErrorsHumanFriendly
      Value = fun values -> values.Amount
      Update = fun newValue values -> { values with Amount = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Daily Purchase Limit:"
         Placeholder = Money.format Constants.DAILY_PURCHASE_LIMIT_DEFAULT
         HtmlAttributes = []
      }
   }

let onSubmit (card: Card) (employee: Employee) initiatedBy amount =
   let cmd =
      LimitDailyDebitsCommand.create employee.CompositeId initiatedBy {
         CardId = card.CardId
         CardNumberLast4 = card.CardNumberLast4
         PriorLimit = card.DailyPurchaseLimit
         DebitLimit = amount
      }
      |> EmployeeCommand.LimitDailyDebits

   Msg.Submit(employee, cmd, Started)

[<ReactComponent>]
let DailyPurchaseLimitFormComponent
   (session: UserSession)
   (notifyParentOnSubmit: EmployeeCommandReceipt -> unit)
   (card: Card)
   (employee: Employee)
   =
   let form =
      Form.succeed (onSubmit card employee session.AsInitiator)
      |> Form.append dailyPurchaseLimitField

   EmployeeFormContainer {|
      InitialValues = { Amount = "" }
      Form = form
      Action = None
      OnSubmit = notifyParentOnSubmit
   |}
