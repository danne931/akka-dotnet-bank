module Bank.Account.Forms.DepositForm

open Feliz
open Fable.Form.Simple

open Lib.Validators
open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Org.Domain
open UIDomain.Account
open FormContainer
open Bank.Employee.Forms.AccountProfileForm

type Values = {
   DestinationAccountId: string
   Amount: string
}

let fieldDestinationAccount accounts =
   accountSelect None accounts
   |> Form.mapValues {
      Value = fun a -> { AccountId = a.DestinationAccountId }
      Update =
         fun a b -> {
            b with
               DestinationAccountId = a.AccountId
         }
   }

let form
   (accounts: Map<AccountId, Account>)
   (initiatedBy: Initiator)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let amountField =
      Form.textField {
         Parser =
            amountValidatorFromString "Deposit amount"
            >> validationErrorsHumanFriendly
         Value = fun values -> values.Amount
         Update = fun newValue values -> { values with Amount = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Deposit Cash:"
            Placeholder = "100"
            HtmlAttributes = []
         }
      }

   let onSubmit accountId amount =
      let account = accounts[accountId]

      let command =
         DepositCashCommand.create account.CompositeId initiatedBy {
            Amount = amount
            Origin = Some "ATM"
         }
         |> AccountCommand.DepositCash

      Msg.Submit(account, command, Started)

   Form.succeed onSubmit
   |> Form.append (fieldDestinationAccount accounts)
   |> Form.append amountField

[<ReactComponent>]
let DepositFormComponent
   (session: UserSession)
   (org: OrgWithAccountProfiles)
   (onSubmit: AccountCommandReceipt -> unit)
   =
   AccountFormContainer {|
      InitialValues = {
         DestinationAccountId = ""
         Amount = ""
      }
      Form = form org.CheckingAccounts session.AsInitiator
      Action = None
      OnSubmit = onSubmit
      Session = session
   |}
