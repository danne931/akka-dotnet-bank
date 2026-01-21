module Bank.Account.Forms.PaymentRequest

open Feliz
open Fable.Form.Simple
open Fable.Form.Simple.Pico
open Fable.Form.Simple.Field.FileField
open System

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
open InvoiceForm
open Email

type Values = {
   Amount: string
   PayeeAccountId: string
   PayerOrgId: string
   PayerName: string
   PayerEmail: string
   Memo: string
   DueAt: string
   InvoiceValues: InvoiceValues
   IsRecurringPayment: bool
   RecurrenceValues: RecurrenceValues
   ParsedInvoice: Deferred<Result<ParsedInvoice, Err>>
}

let moneyParser =
   amountValidatorFromString "Payment amount" >> validationErrorsHumanFriendly

let paymentAmountField =
   Form.textField {
      Parser = moneyParser
      Value =
         fun values ->
            Form.Util.formattedMoney values.Amount
            |> Option.defaultValue values.Amount
      Update = fun newValue values -> { values with Amount = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Amount:"
         Placeholder = ""
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

let fieldFormUpload (onFileSelect: Browser.Types.File -> unit) =
   Form.View.fileField {|
      Value = [||]
      Disabled = false
      OnChange =
         fun files ->
            if files.Length > 0 then
               onFileSelect files.[0]
      Error = None
      ShowError = false
      Attributes = {
         Label = "Upload Invoice (optional)"
         InputLabel = "Choose file"
         Accept =
            FileType.Specific [ "image/jpeg"; "image/png"; "application/pdf" ]
         FileIconClassName = FileIconClassName.Default
         Multiple = false
      }
   |}

let invoiceForm: Form.Form<Values, decimal * Invoice option, _> =
   Form.succeed (fun (invoice: Invoice) -> invoice.Total, Some invoice)
   |> Form.append (
      invoiceForm
      |> Form.mapValues {
         Value = _.InvoiceValues
         Update =
            fun (a: InvoiceValues) (b: Values) -> {
               b with
                  InvoiceValues = a
                  Amount = string (parsedInvoiceFromInvoiceValues a).Total
            }
      }
   )

let formPlatformPayment
   (payeeOrg: Org)
   (payeeDestinationAccounts: Map<AccountId, Account>)
   (payerOrg: SocialTransferDiscoveryCandidate)
   (initiatedBy: Initiator)
   (asInvoice: bool)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let onSubmit
      (amount: decimal, invoiceOpt: Invoice option)
      (dueAt: DateTime)
      (memo: string)
      (selectedDestinationAccountId: string)
      (recurrenceSettings: RecurrenceSettings option)
      =
      let payeeAccountId =
         selectedDestinationAccountId |> Guid.Parse |> AccountId

      let payeeAccount = payeeDestinationAccounts[payeeAccountId]

      let paymentId = Guid.NewGuid() |> PaymentRequestId

      let info =
         PaymentRequested.Platform {
            Payer = {
               OrgId = payerOrg.OrgId
               OrgName = payerOrg.OrgName
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
            Invoice = invoiceOpt
         }

      let cmd =
         RequestPaymentCommand.create initiatedBy info
         |> AccountCommand.RequestPayment
         |> FormCommand.Account

      Msg.GetAndSubmit(FormEntityId.Account payeeAccountId, cmd)

   Form.succeed onSubmit
   |> Form.append (
      if asInvoice then
         invoiceForm
      else
         Form.succeed (fun amount -> amount, None)
         |> Form.append paymentAmountField
   )
   |> Form.append dueAtField
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
   (asInvoice: bool)
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
      (amount, invoiceOpt: Invoice option)
      (dueAt: DateTime)
      (memo: string)
      (selectedDestinationAccountId: string)
      (payerName: string)
      (payerEmail: Email)
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
            Invoice = invoiceOpt
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
      if asInvoice then
         invoiceForm
      else
         Form.succeed (fun amount -> amount, None)
         |> Form.append paymentAmountField
   )
   |> Form.append dueAtField
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

let private parsedInvoiceDataToFormValues
   (parsedData: ParsedInvoice)
   (currentValues: Values)
   : Values
   =
   let lineItemValues =
      parsedData.LineItems
      |> List.map (fun item -> {
         Name = item.Description
         Quantity =
            item.Quantity |> Option.map string |> Option.defaultValue "1"
         UnitPrice =
            item.UnitPrice
            |> Option.map Money.format
            |> Option.defaultValue "0"
      })

   let lineItems =
      if lineItemValues.IsEmpty then
         defaultInvoiceValues.LineItems
      else
         lineItemValues

   {
      currentValues with
         Amount =
            parsedData.Total
            |> Option.map Money.format
            |> Option.defaultValue currentValues.Amount
         DueAt =
            parsedData.DueDate
            |> Option.map DateTime.formatShort
            |> Option.defaultValue currentValues.DueAt
         Memo =
            parsedData.VendorName
            |> Option.map (fun v -> $"Invoice from {v}")
            |> Option.defaultValue currentValues.Memo
         InvoiceValues = {
            LineItems = lineItems
            TaxPercent =
               parsedData.TaxPercent
               |> Option.map string
               |> Option.defaultValue "0"
            SubTotal =
               parsedData.SubTotal
               |> Option.map Money.format
               |> Option.defaultValue "0"
            Total =
               parsedData.Total
               |> Option.map Money.format
               |> Option.defaultValue "0"
         }
         ParsedInvoice = Deferred.Resolved(Ok parsedData)
   }

type private InvoiceSelect =
   | Upload
   | ManualEntry

let private renderInvoiceSelectButtons
   (onClick: InvoiceSelect option -> unit)
   (selected: InvoiceSelect option)
   =
   let text option =
      match option with
      | Some InvoiceSelect.Upload -> "Upload Invoice"
      | Some InvoiceSelect.ManualEntry -> "Enter Invoice Manually"
      | None -> "No Invoice"

   let button option =
      let styles = [
         style.backgroundColor "var(--form-element-background-color)"
         if option = selected then
            style.color "var(--primary)"
            style.borderColor "var(--primary)"
      ]

      Html.button [
         attr.text (text option)
         attr.onClick (fun _ -> onClick option)
         attr.classes [ "outline"; "secondary" ]
         attr.style styles
      ]

   React.fragment [
      button None
      button (Some InvoiceSelect.Upload)
      button (Some InvoiceSelect.ManualEntry)
   ]

let private renderPaymentRequestForm
   setInvoiceSelect
   invoiceSelect
   onInvoiceFileSelect
   (form: ReactElement)
   (parsedInvoice: Deferred<Result<ParsedInvoice, Err>>)
   =
   React.fragment [
      classyNode Html.div [ "grid" ] [
         renderInvoiceSelectButtons setInvoiceSelect invoiceSelect
      ]

      match invoiceSelect with
      | None -> form
      | Some InvoiceSelect.ManualEntry -> form
      | Some InvoiceSelect.Upload ->
         match parsedInvoice with
         | Deferred.Idle -> fieldFormUpload onInvoiceFileSelect
         | Deferred.InProgress ->
            Html.progress []
            Html.p "Uploading and parsing invoice..."
         | Deferred.Resolved(Error _) -> Html.p "Error parsing invoice."
         | Deferred.Resolved(Ok parsedData) ->
            if parsedData.LineItems.Length = 0 then
               Html.p "No line items found in invoice."
            else
               form
   ]

let private customAction (model: Form.View.Model<Values>) =
   Form.View.Action.Custom(fun state _ ->
      let parsedSettings =
         Form.fill recurringPaymentFormOptional model.Values.RecurrenceValues

      let parsedDueDate = dateParser model.Values.DueAt
      let parsedAmount = moneyParser model.Values.Amount

      React.fragment [
         match parsedSettings.Result, parsedDueDate, parsedAmount with
         | Ok(Some settings), Ok dueAt, Ok paymentAmount ->
            RecurringPaymentScheduleComponent.render {|
               Settings = settings
               DueAt = dueAt
               PaymentAmount = paymentAmount
               MaxPaymentsToDisplay = 12
               MaxColumns = 6
            |}
         | _ -> ()

         Form.View.submitButton "Submit Payment Request" state
      ])

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

   let invoiceSelect, setInvoiceSelect =
      React.useState<InvoiceSelect option> None

   let initiatedBy = session.AsInitiator

   let selectedPaymentType, setSelectedPaymentType =
      React.useState PaymentRequestType.Platform

   let signalRConnection = React.useContext SignalRConnectionProvider.context

   let defaultFormValues = {
      Amount = "1337"
      PayeeAccountId = defaultDestinationAccount
      PayerOrgId = ""
      PayerName = ""
      PayerEmail = ""
      Memo = ""
      DueAt = DateTime.format (DateTime.Now.AddMonths 1)
      IsRecurringPayment = false
      InvoiceValues = defaultInvoiceValues
      RecurrenceValues = defaultRecurringPaymentValues
      ParsedInvoice = Deferred.Idle
   }

   // Invoice values may be set automatically after an invoice is
   // uploaded and a SignalR realtime event arrives.
   let formValues, setFormValues = React.useState<Values> defaultFormValues

   let invoiceUploadId, setInvoiceUploadId = React.useState None

   // SignalR listener for Azure Doc Intelligence invoice parsing
   React.useEffect (
      (fun () ->
         signalRConnection
         |> Option.iter (fun conn ->
            PaymentService.listenForInvoiceParsed
               (fun evt ->
                  // Verify this event is for the current upload
                  match invoiceUploadId with
                  | Some uploadId when uploadId = string evt.DraftId ->
                     let formValues =
                        parsedInvoiceDataToFormValues
                           evt.ParsedData
                           formValues

                     setFormValues formValues
                  | _ -> ())
               conn)

         React.createDisposable (fun () ->
            signalRConnection
            |> Option.iter PaymentService.removeInvoiceParseListener)),
      [| box invoiceUploadId |]
   )

   let onInvoiceFileSelect (file: Browser.Types.File) =
      async {
         let formValues = {
            formValues with
               ParsedInvoice = Deferred.InProgress
         }

         setFormValues formValues

         let! result = PaymentService.uploadInvoice session.OrgId file

         match result with
         | Ok id -> setInvoiceUploadId (Some(string id))
         | Error err ->
            setFormValues {
               formValues with
                  ParsedInvoice = Deferred.Resolved(Error err)
            }

            Log.error $"Failed to upload invoice: {err}"
      }
      |> Async.StartImmediate

   let renderPaymentRequestForm =
      renderPaymentRequestForm
         setInvoiceSelect
         invoiceSelect
         onInvoiceFileSelect

   let onSubmit =
      function
      | FormSubmitReceipt.Account receipt -> onSubmit receipt
      | _ -> ()

   let useEventSubscription = Some [ SignalREventProvider.EventType.Account ]
   let componentName = "RequestPaymentForm"

   // TODO:
   // Use a multistep component to represent this longer payment request form.
   // Either wait for one to be included in Fable.Form library (https://github.com/MangelMaxime/Fable.Form/issues/62)
   // or go with something like this (https://www.npmjs.com/package/react-multistep)
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

      match selectedPaymentType with
      | PaymentRequestType.Platform ->
         OrgSocialTransferDiscovery.OrgSearchSelectComponent
            session.OrgId
            "Payer Organization:"
            (fun searchInput orgs ->
               match orgs with
               | Deferred.InProgress -> Html.progress []
               | Deferred.Resolved(Ok(Some org)) ->
                  let initValues = {
                     formValues with
                        PayerOrgId = string org.OrgId
                  }

                  let form =
                     FormContainer {|
                        InitialValues = initValues
                        Form =
                           formPlatformPayment
                              payeeOrg
                              payeeDestinationAccounts
                              org
                              initiatedBy
                              invoiceSelect.IsSome
                        Action = Some customAction
                        OnSubmit = onSubmit
                        Session = session
                        ComponentName = componentName
                        UseEventSubscription = useEventSubscription
                     |}

                  renderPaymentRequestForm form initValues.ParsedInvoice
               | Deferred.Resolved(Ok None) ->
                  Html.p $"No orgs found by search query {searchInput}."
               | _ -> Html.none)
      | PaymentRequestType.ThirdParty ->
         let form =
            FormContainer {|
               InitialValues = formValues
               Form =
                  formThirdPartyPayment
                     payeeOrg
                     payeeDestinationAccounts
                     initiatedBy
                     invoiceSelect.IsSome
               Action = Some customAction
               OnSubmit = onSubmit
               Session = session
               ComponentName = componentName
               UseEventSubscription = useEventSubscription
            |}

         renderPaymentRequestForm form formValues.ParsedInvoice
   ]
