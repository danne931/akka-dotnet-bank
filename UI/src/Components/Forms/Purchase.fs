module Bank.Employee.Forms.PurchaseForm

open Feliz
open Fable.Form.Simple
open System

open Fable.Form.Simple.Pico
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open UIDomain.Employee
open Lib.Validators
open Lib.SharedTypes
open Bank.Forms.FormContainer

type Values = { Amount: string; Merchant: string }

let form
   (accounts: Map<AccountId, Account>)
   (employee: Employee)
   (selectedCardId: CardId)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let card = employee.Cards[selectedCardId]
   let account = accounts[card.AccountId]

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
         PurchaseIntentCommand.create {
            CorrelationId = CorrelationId(Guid.NewGuid())
            InitiatedBy = {
               Id = InitiatedById employee.EmployeeId
               Name = employee.Name
            }
            OrgId = employee.OrgId
            ParentAccountId = account.ParentAccountId
            EmployeeId = employee.EmployeeId
            EmployeeName = employee.Name
            EmployeeEmail = employee.Email
            CardId = selectedCardId
            CardNumberLast4 = card.CardNumberLast4
            CardNickname = card.CardNickname
            AccountId = account.AccountId
            Amount = amount
            Merchant = merchant
            Reference = None
            Date = DateTime.UtcNow
            // Represents ID of transaction coming from
            // simulated card network.
            CardNetworkTransactionId = Guid.NewGuid()
         }
         |> EmployeeCommand.PurchaseIntent
         |> FormCommand.Employee

      Msg.Submit(FormEntity.Employee employee, cmd, Started)

   Form.succeed onSubmit |> Form.append amountField |> Form.append merchantField

[<ReactComponent>]
let PurchaseFormComponent
   (org: OrgWithAccountProfiles)
   (session: UserSession)
   (selectedCardId: CardId)
   (employee: Employee)
   (onSubmit: EmployeeCommandReceipt -> unit)
   =
   FormContainer {|
      InitialValues = { Amount = ""; Merchant = "" }
      Form = form org.CheckingAccounts employee selectedCardId
      Action = None
      OnSubmit =
         function
         | FormSubmitReceipt.Employee receipt -> onSubmit receipt
         | _ -> ()
      Session = session
      ComponentName = "PurchaseForm"
      UseEventSubscription =
         Some [
            SignalREventProvider.EventType.Employee
            SignalREventProvider.EventType.Account
         ]
   |}
