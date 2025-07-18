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
open RecurringPaymentSchedule
open RecurringPaymentForm

type Values = {
   Amount: string
   PayeeAccountId: string
   PayerOrgId: string
   PayerName: string
   PayerEmail: string
   Memo: string
   DueAt: string
   IsRecurringPayment: bool
   RecurrenceValues: RecurrenceValues
}

let amountParser =
   amountValidatorFromString "Payment amount" >> validationErrorsHumanFriendly

let amountField =
   Form.textField {
      Parser = amountParser
      Value = _.Amount
      Update = fun newValue values -> { values with Amount = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Amount:"
         Placeholder = "1337"
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

let dateParser =
   CustomDateInterpreter.validate CustomDateInterpreter.DateSignifier.Single
   >> Result.bind (
      snd >> dateInFutureValidator "Due on" >> validationErrorsHumanFriendly
   )

let dueAtField =
   Form.dateField {
      Parser = dateParser
      Value = _.DueAt
      Update = fun newValue values -> { values with DueAt = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Due on:"
         Placeholder = "Payment due on"
         HtmlAttributes = []
      }
   }

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
      (amount, dueAt)
      (memo: string)
      (selectedDestinationAccountId: string)
      (recurrenceSettings: RecurrenceSettings option)
      =
      let payerOrg =
         orgs |> List.find (fun o -> string o.OrgId = selectedPayerOrgId)

      let payeeAccountId =
         selectedDestinationAccountId |> Guid.Parse |> AccountId

      let payeeAccount = payeeDestinationAccounts[payeeAccountId]

      let paymentId = Guid.NewGuid() |> PaymentRequestId

      let info =
         PaymentRequested.Platform {
            Payer = {
               OrgId = payerOrg.OrgId
               OrgName = payerOrg.Name
               ParentAccountId = payerOrg.ParentAccountId
            }
            SharedDetails = {
               Id = paymentId
               Amount = amount
               DueAt = dueAt
               Payee = {
                  OrgId = payeeOrg.OrgId
                  OrgName = payeeOrg.Name
                  AccountId = payeeAccountId
                  ParentAccountId = payeeAccount.ParentAccountId
               }
               Memo = memo
            }
            RecurringPaymentReference =
               recurrenceSettings
               |> Option.map (fun settings -> {
                  Settings = settings
                  OriginPaymentId = paymentId
               })
         }

      let cmd =
         RequestPaymentCommand.create initiatedBy info
         |> AccountCommand.RequestPayment
         |> FormCommand.Account

      Msg.GetAndSubmit(FormEntityId.Account payeeAccountId, cmd)

   Form.succeed onSubmit
   |> Form.append fieldOrgPayerSelect
   |> Form.append (
      Form.succeed (fun amount dueAt -> amount, dueAt)
      |> Form.append amountField
      |> Form.append dueAtField
      |> Form.group
   )
   |> Form.append memoField
   |> Form.append (fieldPayeeAccountSelect payeeDestinationAccounts)
   |> Form.append (
      recurringPaymentFormOptional
      |> Form.mapValues {
         Value = _.RecurrenceValues
         Update =
            fun (a: RecurrenceValues) (b: Values) -> {
               b with
                  RecurrenceValues = a
            }
      }
   )

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
      (amount, dueAt)
      memo
      (selectedDestinationAccountId: string)
      payerName
      payerEmail
      (recurrenceSettings: RecurrenceSettings option)
      =
      let payeeAccountId =
         selectedDestinationAccountId |> Guid.Parse |> AccountId

      let payeeAccount = payeeDestinationAccounts[payeeAccountId]

      let paymentId = Guid.NewGuid() |> PaymentRequestId

      let info =
         PaymentRequested.ThirdParty {
            Payer = { Name = payerName; Email = payerEmail }
            ShortId = PaymentPortalShortId.create ()
            RecurringPaymentReference =
               recurrenceSettings
               |> Option.map (fun settings -> {
                  Settings = settings
                  OriginPaymentId = paymentId
               })
            SharedDetails = {
               Id = paymentId
               Amount = amount
               DueAt = dueAt
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
   |> Form.append (
      Form.succeed (fun amount dueAt -> amount, dueAt)
      |> Form.append amountField
      |> Form.append dueAtField
      |> Form.group
   )
   |> Form.append memoField
   |> Form.append (fieldPayeeAccountSelect payeeDestinationAccounts)
   |> Form.append fieldPayerName
   |> Form.append fieldPayerEmail
   |> Form.append (
      recurringPaymentFormOptional
      |> Form.mapValues {
         Value = _.RecurrenceValues
         Update =
            fun (a: RecurrenceValues) (b: Values) -> {
               b with
                  RecurrenceValues = a
            }
      }
   )

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
      DueAt = DateTime.format (DateTime.Now.AddMonths 1)
      IsRecurringPayment = false
      RecurrenceValues = defaultRecurringPaymentValues
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

      let customAction (model: Form.View.Model<Values>) =
         Form.View.Action.Custom(fun state _ ->
            let parsedSettings =
               Form.fill
                  recurringPaymentFormOptional
                  model.Values.RecurrenceValues

            let parsedDueDate = dateParser model.Values.DueAt
            let parsedAmount = amountParser model.Values.Amount

            React.fragment [
               match parsedSettings.Result, parsedDueDate, parsedAmount with
               | Ok(Some settings), Ok dueAt, Ok paymentAmount ->
                  RecurringPaymentScheduleComponent.render {|
                     Settings = settings
                     DueAt = dueAt
                     PaymentAmount = paymentAmount
                  |}
               | _ -> ()

               Form.View.submitButton "Submit Payment Request" state
            ])

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
                     Action = Some customAction
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
            Action = Some customAction
            OnSubmit = onSubmit
            Session = session
            ComponentName = componentName
            UseEventSubscription = useEventSubscription
         |}
   ]
