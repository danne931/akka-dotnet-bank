module Bank.Org.Forms.RegisterTransferRecipientForm

open Feliz
open Fable.Form.Simple
open System

open Fable.Form.Simple.Pico
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Employee.Domain
open UIDomain.Account
open Lib.Validators
open Bank.Forms.FormContainer
open Lib.SharedTypes

type Values = {
   AccountEnvironment: string
   FirstName: string
   LastName: string
   SelectedInternalAccountId: string
   AccountNumber: string
   RoutingNumber: string
   AccountDepository: string
   PaymentNetwork: string
}

let domesticRecipientForm
   (org: Org)
   (editingDomesticRecipient: DomesticTransferRecipient option)
   (initiatedBy: Initiator)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let fieldFirstName =
      Form.textField {
         Parser = firstNameValidator >> validationErrorsHumanFriendly
         Value = fun (values: Values) -> values.FirstName
         Update = fun newValue values -> { values with FirstName = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "First Name:"
            Placeholder = "First Name"
            HtmlAttributes = []
         }
      }

   let fieldLastName =
      Form.textField {
         Parser = lastNameValidator >> validationErrorsHumanFriendly
         Value = fun (values: Values) -> values.LastName
         Update = fun newValue values -> { values with LastName = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Last Name:"
            Placeholder = "Last Name"
            HtmlAttributes = []
         }
      }

   let fieldAccountNumber =
      Form.textField {
         Parser =
            AccountNumber.fromString "Account Number"
            >> validationErrorsHumanFriendly
            >> Result.map string
         Value = fun (values: Values) -> values.AccountNumber
         Update =
            fun newValue values -> { values with AccountNumber = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Account Number:"
            Placeholder = "Account Number"
            HtmlAttributes = []
         }
      }

   let fieldRoutingNumber =
      Form.textField {
         Parser =
            RoutingNumber.fromString "Routing Number"
            >> validationErrorsHumanFriendly
            >> Result.map string
         Value = fun (values: Values) -> values.RoutingNumber
         Update =
            fun newValue values -> { values with RoutingNumber = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Routing Number:"
            Placeholder = "Routing Number"
            HtmlAttributes = []
         }
      }

   let fieldAccountDepository =
      Form.selectField {
         Parser =
            fun depository ->
               let depository =
                  match depository with
                  | "checking" -> DomesticRecipientAccountDepository.Checking
                  | "savings" -> DomesticRecipientAccountDepository.Savings
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

   let fieldPaymentNetwork =
      Form.selectField {
         Parser =
            fun pay ->
               let pay =
                  match pay with
                  | "ach" -> PaymentNetwork.ACH
                  | other -> failwith $"Not implemented payment network {other}"

               Ok pay
         Value = fun (values: Values) -> values.PaymentNetwork
         Update =
            fun newValue values -> {
               values with
                  PaymentNetwork = newValue
            }
         Error = fun _ -> None
         Attributes = {
            Label = "Payment Network"
            Placeholder = ""
            Options = [ "ach", "ACH" ]
         }
      }

   let onSubmit paymentNetwork depository name accountNum routingNum =
      let first, last = name

      let cmd =
         match editingDomesticRecipient with
         | None ->
            RegisterDomesticTransferRecipientCommand.create initiatedBy {
               AccountId = AccountId <| Guid.NewGuid()
               LastName = last
               FirstName = first
               AccountNumber = accountNum
               RoutingNumber = routingNum
               Depository = depository
               PaymentNetwork = paymentNetwork
               Sender = {|
                  OrgId = org.OrgId
                  ParentAccountId = org.ParentAccountId
               |}
            }
            |> ParentAccountCommand.RegisterDomesticTransferRecipient
         | Some recipient ->
            EditDomesticTransferRecipientCommand.create
               org.ParentAccountId
               org.OrgId
               initiatedBy
               {
                  LastName = last
                  FirstName = first
                  AccountNumber = accountNum
                  RoutingNumber = routingNum
                  Depository = depository
                  PaymentNetwork = paymentNetwork
                  RecipientWithoutAppliedUpdates = recipient
               }
            |> ParentAccountCommand.EditDomesticTransferRecipient

      Msg.Submit(
         FormEntity.ParentAccount,
         FormCommand.ParentAccount cmd,
         Started
      )

   Form.succeed onSubmit
   |> Form.append fieldPaymentNetwork
   |> Form.append fieldAccountDepository
   |> Form.append (
      Form.succeed (fun first last -> first, last)
      |> Form.append fieldFirstName
      |> Form.append fieldLastName
      |> Form.group
   )
   |> Form.append fieldAccountNumber
   |> Form.append fieldRoutingNumber

let form
   (org: Org)
   (editingDomesticRecipient: DomesticTransferRecipient option)
   (initiatedBy: Initiator)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let fieldAccountEnvironment =
      Form.selectField {
         Parser =
            fun envId ->
               let env =
                  match envId with
                  | "domestic" -> RecipientAccountEnvironment.Domestic
                  | other ->
                     failwith $"Not implemented account environment {other}"

               Ok env
         Value = fun (values: Values) -> values.AccountEnvironment
         Update =
            fun newValue values -> {
               values with
                  AccountEnvironment = newValue
            }
         Error = fun _ -> None
         Attributes = {
            Label = ""
            Placeholder = ""
            Options = [ "domestic", "Domestic Account" ]
         }
      }

   fieldAccountEnvironment
   |> Form.andThen (fun _ ->
      domesticRecipientForm org editingDomesticRecipient initiatedBy)

[<ReactComponent>]
let RegisterTransferRecipientFormComponent
   (session: UserSession)
   (org: OrgWithAccountProfiles)
   (recipientIdForEdit: AccountId option)
   (onSubmit: ParentAccountCommandReceipt -> unit)
   =
   let transfersToRetry, setTransfersToRetry =
      React.useState<Deferred<DomesticTransfer list option>> Deferred.Idle

   let recipient =
      recipientIdForEdit
      |> Option.bind (fun accountId ->
         Map.tryFind accountId org.DomesticTransferRecipients)

   React.useEffectOnce (fun () ->
      match recipient with
      | Some r ->
         async {
            let! res =
               AccountService.getDomesticTransfersRetryableUponRecipientEdit
                  r.RecipientAccountId

            match res with
            | Error err ->
               Log.error $"Error fetching retryable domestic transfers {err}"
            | Ok transfersOpt ->
               setTransfersToRetry (Deferred.Resolved transfersOpt)
         }
         |> Async.StartImmediate
      | None -> setTransfersToRetry (Deferred.Resolved None))


   let formProps: Values = {
      AccountEnvironment = "domestic"
      AccountDepository = "checking"
      PaymentNetwork = "ach"
      FirstName = ""
      LastName = ""
      SelectedInternalAccountId = ""
      AccountNumber = ""
      RoutingNumber = ""
   }

   let formProps =
      match recipient with
      | Some recipient -> {
         formProps with
            FirstName = recipient.FirstName
            LastName = recipient.LastName
            AccountNumber = string recipient.AccountNumber
            RoutingNumber = string recipient.RoutingNumber
        }
      | _ -> formProps

   React.fragment [
      match transfersToRetry with
      | Deferred.Resolved transfersOpt ->
         match transfersOpt with
         | None -> ()
         | Some transfers ->
            let count = transfers.Length
            let msg = "will be retried upon editing recipient info."

            let msg =
               match count with
               | 0 -> None
               | 1 -> Some $"1 failed transfer {msg}"
               | count -> Some $"{count} failed transfers {msg}"

            match msg with
            | Some msg ->
               Html.div [
                  Html.ins [
                     attr.text msg
                     attr.style [ style.color "var(--primary)" ]
                  ]
               ]

               Html.br []
            | None -> ()

         FormContainer {|
            InitialValues = formProps
            Form = form org.Org recipient session.AsInitiator
            Action = None
            OnSubmit =
               function
               | FormSubmitReceipt.ParentAccount receipt -> onSubmit receipt
               | _ -> ()
            Session = session
            ComponentName = "RegisterTransferRecipientForm"
            UseEventSubscription =
               Some [
                  // Listen for recipient registered/edited
                  SignalREventProvider.EventType.ParentAccount
               ]
         |}
      | _ -> Html.progress []
   ]
