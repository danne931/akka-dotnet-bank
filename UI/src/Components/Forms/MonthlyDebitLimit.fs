module Bank.Employee.Forms.MonthlyPurchaseLimitForm

open Feliz
open Fable.Form.Simple
open Validus.Operators

open Bank.Employee.Domain
open UIDomain.Employee
open Lib.Validators
open Lib.SharedTypes
open Bank.Forms.FormContainer

type Values = { Amount: string }

let monthlyPurchaseLimitField =
   Form.textField {
      Parser =
         (parseDecimal >=> Card.monthlyPurchaseLimitValidator)
            "Monthly purchase limit"
         >> validationErrorsHumanFriendly
      Value = fun values -> values.Amount
      Update = fun newValue values -> { values with Amount = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Monthly Purchase Limit:"
         Placeholder = Money.format Constants.MONTHLY_PURCHASE_LIMIT_DEFAULT
         HtmlAttributes = []
      }
   }

let onSubmit (card: Card) (employee: Employee) initiatedBy amount =
   let cmd =
      LimitMonthlyDebitsCommand.create employee.CompositeId initiatedBy {
         CardId = card.CardId
         CardNumberLast4 = card.CardNumberLast4
         PriorLimit = card.MonthlyPurchaseLimit
         DebitLimit = amount
      }
      |> EmployeeCommand.LimitMonthlyDebits
      |> FormCommand.Employee

   Msg.Submit(FormEntity.Employee employee, cmd, Started)

[<ReactComponent>]
let MonthlyPurchaseLimitFormComponent
   (session: UserSession)
   (notifyParentOnSubmit: EmployeeCommandReceipt -> unit)
   (card: Card)
   (employee: Employee)
   =
   let form =
      Form.succeed (onSubmit card employee session.AsInitiator)
      |> Form.append monthlyPurchaseLimitField

   FormContainer {|
      InitialValues = { Amount = "" }
      Form = form
      Action = None
      OnSubmit =
         function
         | FormSubmitReceipt.Employee receipt -> notifyParentOnSubmit receipt
         | _ -> ()
      Session = session
      ComponentName = "MonthlyPurchaseLimitForm"
      UseEventSubscription = None
   |}
