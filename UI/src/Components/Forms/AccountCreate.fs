module Bank.Account.Forms.AccountCreateForm

open Feliz
open Fable.Form.Simple
open System

open Fable.Form.Simple.Pico
open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Org.Domain
open UIDomain.Account
open Lib.Validators
open Bank.Forms.FormContainer
open Lib.SharedTypes

type Values = {
   AccountName: string
   AccountDepository: string
}

let private form
   (session: UserSession)
   (org: Org)
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
         CreateVirtualAccountCommand.create {
            Name = name
            Depository = depository
            AccountNumber = AccountNumber.generate ()
            OrgId = session.OrgId
            ParentAccountId = org.ParentAccountId
            AccountId = Guid.NewGuid() |> AccountId
            Currency = Currency.USD
            InitiatedBy = session.AsInitiator
         }
         |> AccountCommand.CreateVirtualAccount
         |> FormCommand.Account

      Msg.Submit(FormEntity.Account Account.empty, cmd, Started)

   Form.succeed onSubmit
   |> Form.append fieldAccountDepository
   |> Form.append fieldAccountName

[<ReactComponent>]
let AccountCreateFormComponent
   (session: UserSession)
   (org: Org)
   (onSubmit: AccountCommandReceipt -> unit)
   =
   FormContainer {|
      InitialValues = {
         AccountName = ""
         AccountDepository = "checking"
      }
      Form = form session org
      Action = None
      OnSubmit =
         function
         | FormSubmitReceipt.Account receipt -> onSubmit receipt
         | _ -> ()
      Session = session
      ComponentName = "AccountCreateForm"
      UseEventSubscription = Some [ SignalREventProvider.EventType.Account ]
   |}
