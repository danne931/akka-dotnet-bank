module Bank.Employee.Forms.PurchaseLimitForm

open Feliz
open Fable.Form.Simple
open Validus.Operators

open Bank.Employee.Domain
open UIDomain.Employee
open Bank.Forms.FormContainer
open Lib.Validators
open Lib.SharedTypes

type PurchaseLimitValue = { Amount: string }

let dailyPurchaseLimitField
   : Form.Form<PurchaseLimitValue, decimal, IReactProperty> =
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

let monthlyPurchaseLimitField
   : Form.Form<PurchaseLimitValue, decimal, IReactProperty> =
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

type Values = {
   DailyLimit: string
   MonthlyLimit: string
}

let onSubmit
   (card: Card)
   (employee: Employee)
   initiatedBy
   dailyLimit
   monthlyLimit
   =
   let cmd =
      ConfigureRollingPurchaseLimitCommand.create
         employee.CompositeId
         initiatedBy
         {
            CardId = card.CardId
            CardNumberLast4 = card.CardNumberLast4
            PriorDailyLimit = card.DailyPurchaseLimit
            PriorMonthlyLimit = card.MonthlyPurchaseLimit
            DailyLimit = dailyLimit
            MonthlyLimit = monthlyLimit
         }
      |> EmployeeCommand.ConfigureRollingPurchaseLimit
      |> FormCommand.Employee

   Msg.Submit(FormEntity.Employee employee, cmd, Started)

let form
   card
   employee
   initiatedBy
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   Form.succeed (onSubmit card employee initiatedBy)
   |> Form.append (
      dailyPurchaseLimitField
      |> Form.mapValues {
         Value = fun a -> { Amount = a.DailyLimit }
         Update = fun a b -> { b with DailyLimit = a.Amount }
      }
   )
   |> Form.append (
      monthlyPurchaseLimitField
      |> Form.mapValues {
         Value = fun a -> { Amount = a.MonthlyLimit }
         Update = fun a b -> { b with MonthlyLimit = a.Amount }
      }
   )

[<ReactComponent>]
let PurchaseLimitFormComponent
   (session: UserSession)
   (notifyParentOnSubmit: EmployeeCommandReceipt -> unit)
   (card: Card)
   (employee: Employee)
   =
   FormContainer {|
      InitialValues = {
         DailyLimit = string card.DailyPurchaseLimit
         MonthlyLimit = string card.MonthlyPurchaseLimit
      }
      Form = form card employee session.AsInitiator
      Action = None
      OnSubmit =
         function
         | FormSubmitReceipt.Employee receipt -> notifyParentOnSubmit receipt
         | _ -> ()
      Session = session
      ComponentName = "PurchaseLimitForm"
      UseEventSubscription = None
   |}
