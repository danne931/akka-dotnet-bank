module Bank.Account.Forms.PaymentRequest

open Feliz
open Fable.Form.Simple
open System

open Fable.Form.Simple.Pico
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Payment.Domain
open Bank.Employee.Domain
open UIDomain.Account
open Lib.Validators
open Bank.Forms.FormContainer
open Lib.SharedTypes
open Lib.Time

type Values = {
   Amount: string
   PayeeAccountId: string
   PayerOrgId: string
   PayerName: string
   PayerEmail: string
   Memo: string
   Expiration: string
}

let amountField =
   Form.textField {
      Parser =
         amountValidatorFromString "Payment amount"
         >> validationErrorsHumanFriendly
      Value = _.Amount
      Update = fun newValue values -> { values with Amount = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Amount:"
         Placeholder = "1300"
         HtmlAttributes = []
      }
   }

let memoField =
   Form.textField {
      Parser = Ok
      Value = _.Memo
      Update = fun newValue values -> { values with Memo = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Memo:"
         Placeholder = "Reason for Payment Request"
         HtmlAttributes = []
      }
   }

let expirationField =
   Form.dateField {
      Parser =
         CustomDateInterpreter.validate
            CustomDateInterpreter.DateSignifier.Single
         >> Result.bind (
            snd
            >> dateInFutureValidator "Expiration"
            >> validationErrorsHumanFriendly
         )
      Value = _.Expiration
      Update = fun newValue values -> { values with Expiration = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Expiration:"
         Placeholder = "Payment request expires on"
         HtmlAttributes = []
      }
   }

let expirationForm = Form.succeed id |> Form.append expirationField

let fieldPayeeAccountSelect
   (payeeDestinationAccounts: Map<AccountId, Account>)
   =
   let payeeAccountOptions =
      payeeDestinationAccounts
      |> Map.toList
      |> List.map (fun (accountId, a) ->
         string accountId, $"{a.Name} ({Money.format a.AvailableBalance})")
      |> List.sortBy snd

   Form.selectField {
      Parser = Ok
      Value = _.PayeeAccountId
      Update =
         fun newValue values -> {
            values with
               PayeeAccountId = newValue
         }
      Error = fun _ -> None
      Attributes = {
         Label = "Payee Destination Account:"
         Placeholder = "No account selected"
         Options = payeeAccountOptions
      }
   }

let formPlatformPayment
   (payeeOrg: Org)
   (payeeDestinationAccounts: Map<AccountId, Account>)
   (orgs: Org list)
   (initiatedBy: Initiator)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let orgPayerOptions =
      orgs
      |> List.choose (fun org ->
         org.FeatureFlags.SocialTransferDiscoveryPrimaryAccountId
         |> Option.map (fun _ -> string org.OrgId, org.Name))
      |> List.sortBy snd

   let fieldOrgPayerSelect =
      Form.selectField {
         Parser = Ok
         Value = _.PayerOrgId
         Update = fun newValue values -> { values with PayerOrgId = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Payer Organization:"
            Placeholder = "No organization selected"
            Options = orgPayerOptions
         }
      }

   let onSubmit
      (selectedPayerOrgId: string)
      (amount: decimal)
      (memo: string)
      (expiration: DateTime)
      (selectedDestinationAccountId: string)
      =
      let payerOrg =
         orgs |> List.find (fun o -> string o.OrgId = selectedPayerOrgId)

      let payeeAccountId =
         selectedDestinationAccountId |> Guid.Parse |> AccountId

      let payeeAccount = payeeDestinationAccounts[payeeAccountId]

      let info =
         PaymentRequested.Platform {
            Payer = {
               OrgId = payerOrg.OrgId
               OrgName = payerOrg.Name
               ParentAccountId = payerOrg.ParentAccountId
            }
            SharedDetails = {
               Id = Guid.NewGuid() |> PaymentRequestId
               Amount = amount
               Expiration = expiration
               Payee = {
                  OrgId = payeeOrg.OrgId
                  OrgName = payeeOrg.Name
                  AccountId = payeeAccountId
                  ParentAccountId = payeeAccount.ParentAccountId
               }
               Memo = memo
            }
         }

      let cmd =
         RequestPaymentCommand.create initiatedBy info
         |> AccountCommand.RequestPayment
         |> FormCommand.Account

      Msg.GetAndSubmit(FormEntityId.Account payeeAccountId, cmd)

   Form.succeed onSubmit
   |> Form.append fieldOrgPayerSelect
   |> Form.append amountField
   |> Form.append memoField
   |> Form.append expirationForm
   |> Form.append (fieldPayeeAccountSelect payeeDestinationAccounts)

let formThirdPartyPayment
   (payeeOrg: Org)
   (payeeDestinationAccounts: Map<AccountId, Account>)
   (initiatedBy: Initiator)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let fieldPayerName =
      Form.textField {
         Parser = Ok
         Value = _.PayerName
         Update = fun newValue values -> { values with PayerName = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Payer Name:"
            Placeholder = "Payer Name"
            HtmlAttributes = []
         }
      }

   let fieldPayerEmail =
      Form.textField {
         Parser = Email.ofString "Payer Email" >> validationErrorsHumanFriendly
         Value = _.PayerEmail
         Update = fun newValue values -> { values with PayerEmail = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Payer Email:"
            Placeholder = "Payer Email"
            HtmlAttributes = []
         }
      }

   let onSubmit
      amount
      memo
      expiration
      (selectedDestinationAccountId: string)
      payerName
      payerEmail
      =
      let payeeAccountId =
         selectedDestinationAccountId |> Guid.Parse |> AccountId

      let payeeAccount = payeeDestinationAccounts[payeeAccountId]

      let info =
         PaymentRequested.ThirdParty {
            Payer = { Name = payerName; Email = payerEmail }
            ShortId = PaymentPortalShortId.create ()
            SharedDetails = {
               Id = Guid.NewGuid() |> PaymentRequestId
               Amount = amount
               Expiration = expiration
               Memo = memo
               Payee = {
                  OrgId = payeeOrg.OrgId
                  OrgName = payeeOrg.Name
                  AccountId = payeeAccountId
                  ParentAccountId = payeeAccount.ParentAccountId
               }
            }
         }

      let cmd =
         RequestPaymentCommand.create initiatedBy info
         |> AccountCommand.RequestPayment
         |> FormCommand.Account

      Msg.GetAndSubmit(FormEntityId.Account payeeAccountId, cmd)

   Form.succeed onSubmit
   |> Form.append amountField
   |> Form.append memoField
   |> Form.append expirationForm
   |> Form.append (fieldPayeeAccountSelect payeeDestinationAccounts)
   |> Form.append fieldPayerName
   |> Form.append fieldPayerEmail

[<ReactComponent>]
let PaymentRequestFormComponent
   (session: UserSession)
   (payeeOrg: Org)
   (payeeDestinationAccounts: Map<AccountId, Account>)
   (onSubmit: AccountCommandReceipt -> unit)
   =
   let payeeDestinationAccounts =
      payeeDestinationAccounts
      |> Map.filter (fun _ a -> a.Depository = AccountDepository.Checking)

   let defaultDestinationAccount =
      payeeDestinationAccounts
      |> Map.toSeq
      |> Seq.tryHead
      |> Option.map (snd >> _.AccountId >> string)
      |> Option.defaultValue ""

   let initValues = {
      Amount = ""
      PayeeAccountId = defaultDestinationAccount
      PayerOrgId = ""
      PayerName = ""
      PayerEmail = ""
      Memo = ""
      Expiration = DateTime.format (DateTime.Now.AddMonths 1)
   }

   let initiatedBy = session.AsInitiator

   let selectedPaymentType, setSelectedPaymentType =
      React.useState PaymentRequestType.Platform

   React.fragment [
      Html.select [
         attr.onChange (
            PaymentRequestType.fromStringUnsafe >> setSelectedPaymentType
         )
         attr.value (string selectedPaymentType)

         attr.children [
            Html.option [
               attr.value (string PaymentRequestType.Platform)
               attr.text "To org on the platform"
            ]

            Html.option [
               attr.value (string PaymentRequestType.ThirdParty)
               attr.text "To org/person outside the platform"
            ]
         ]
      ]

      let useEventSubscription = Some [ SignalREventProvider.EventType.Account ]
      let componentName = "RequestPaymentForm"

      let onSubmit =
         function
         | FormSubmitReceipt.Account receipt -> onSubmit receipt
         | _ -> ()

      match selectedPaymentType with
      | PaymentRequestType.Platform ->
         OrgSocialTransferDiscovery.OrgSearchComponent
            session.OrgId
            (fun searchInput orgs ->
               match orgs with
               | Deferred.InProgress -> Html.progress []
               | Deferred.Resolved(Ok(Some orgs)) ->
                  let initValues =
                     orgs
                     |> List.tryHead
                     |> Option.map (fun o -> {
                        initValues with
                           PayerOrgId = string o.OrgId
                     })
                     |> Option.defaultValue initValues

                  FormContainer {|
                     InitialValues = initValues
                     Form =
                        formPlatformPayment
                           payeeOrg
                           payeeDestinationAccounts
                           orgs
                           initiatedBy
                     Action = None
                     OnSubmit = onSubmit
                     Session = session
                     ComponentName = componentName
                     UseEventSubscription = useEventSubscription
                  |}
               | Deferred.Resolved(Ok None) ->
                  Html.p $"No orgs found by search query {searchInput}."
               | _ -> Html.none)
      | PaymentRequestType.ThirdParty ->
         FormContainer {|
            InitialValues = initValues
            Form =
               formThirdPartyPayment
                  payeeOrg
                  payeeDestinationAccounts
                  initiatedBy
            Action = None
            OnSubmit = onSubmit
            Session = session
            ComponentName = componentName
            UseEventSubscription = useEventSubscription
         |}
   ]
