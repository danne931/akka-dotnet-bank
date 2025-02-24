module Bank.Account.Forms.AccountCreateForm

open Feliz
open Fable.Form.Simple
open System

open Fable.Form.Simple.Pico
open Bank.Account.Domain
open Bank.Employee.Domain
open UIDomain.Account
open Lib.Validators
open FormContainer
open Lib.SharedTypes

type Values = {
   AccountName: string
   AccountDepository: string
}

let private form
   (session: UserSession)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let fieldAccountDepository =
      Form.selectField {
         Parser =
            fun depository ->
               let depository =
                  match depository with
                  | "checking" -> AccountDepository.Checking
                  | "savings" -> AccountDepository.Savings
                  | other ->
                     failwith $"Not implemented account depository {other}"

               Ok depository
         Value = fun (values: Values) -> values.AccountDepository
         Update =
            fun newValue values -> {
               values with
                  AccountDepository = newValue
            }
         Error = fun _ -> None
         Attributes = {
            Label = "Account Type"
            Placeholder = ""
            Options = [ "checking", "Checking"; "savings", "Savings" ]
         }
      }

   let fieldAccountName =
      Form.textField {
         Parser = accountNameValidator >> validationErrorsHumanFriendly
         Value = fun (values: Values) -> values.AccountName
         Update = fun newValue values -> { values with AccountName = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Account Name:"
            Placeholder = "Account Name"
            HtmlAttributes = []
         }
      }

   let onSubmit depository name =
      let cmd =
         CreateAccountCommand.create {
            Name = name
            Depository = depository
            AccountNumber = AccountNumber.generate ()
            OrgId = session.OrgId
            AccountId = Guid.NewGuid() |> AccountId
            Currency = Currency.USD
            InitiatedBy = session.AsInitiator
         }
         |> AccountCommand.CreateAccount

      Msg.Submit(Account.empty, cmd, Started)

   Form.succeed onSubmit
   |> Form.append fieldAccountDepository
   |> Form.append fieldAccountName

[<ReactComponent>]
let AccountCreateFormComponent
   (session: UserSession)
   (onSubmit: AccountCommandReceipt -> unit)
   =
   AccountFormContainer {|
      InitialValues = {
         AccountName = ""
         AccountDepository = "checking"
      }
      Form = form session
      Action = None
      OnSubmit = onSubmit
   |}
