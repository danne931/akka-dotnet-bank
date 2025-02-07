module Bank.Employee.Forms.PurchaseForm

open Feliz
open Fable.Form.Simple
open System

open Fable.Form.Simple.Pico
open Bank.Account.Domain
open Bank.Employee.Domain
open UIDomain.Employee
open Lib.Validators
open Lib.SharedTypes
open FormContainer

type Values = { Amount: string; Merchant: string }

let form
   (account: Account)
   (employee: Employee)
   (selectedCardId: CardId)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let amountField =
      Form.textField {
         Parser =
            amountValidatorFromString "Purchase amount"
            >> validationErrorsHumanFriendly
         Value = fun (values: Values) -> values.Amount
         Update = fun newValue values -> { values with Amount = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Purchase Amount:"
            Placeholder = "1"
            HtmlAttributes = []
         }
      }

   let merchantField =
      Form.textField {
         Parser = merchantValidator >> validationErrorsHumanFriendly
         Value = fun (values: Values) -> values.Merchant
         Update = fun newValue values -> { values with Merchant = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Merchant:"
            Placeholder = "Trader Joe's"
            HtmlAttributes = []
         }
      }

   let onSubmit amount merchant =
      let cmd =
         PurchasePendingCommand.create employee.CompositeId {
            CardId = selectedCardId
            CardNumberLast4 = employee.Cards[selectedCardId].CardNumberLast4
            AccountId = account.AccountId
            Amount = amount
            Merchant = merchant
            Reference = None
            Date = DateTime.UtcNow
         }
         |> EmployeeCommand.PurchasePending

      Msg.Submit(employee, cmd, Started)

   Form.succeed onSubmit |> Form.append amountField |> Form.append merchantField

[<ReactComponent>]
let PurchaseFormComponent
   (onSubmit: EmployeeCommandReceipt -> unit)
   (account: Account)
   (selectedCardId: CardId)
   (employee: Employee)
   =
   EmployeeFormContainer {|
      InitialValues = { Amount = ""; Merchant = "" }
      Form = form account employee selectedCardId
      Action = None
      OnSubmit = onSubmit
   |}
