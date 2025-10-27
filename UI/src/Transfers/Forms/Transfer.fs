module Bank.Account.Forms.TransferForm

open Feliz
open Feliz.Router
open Fable.Form.Simple
open System

open Fable.Form.Simple.Pico
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Employee.Domain
open UIDomain.Account
open UIDomain.Org
open Lib.Validators
open Bank.Forms.FormContainer
open Lib.SharedTypes
open Bank.Employee.Forms.AccountProfileForm
open CommandApproval

type Values = {
   Amount: string
   SenderId: string
   RecipientId: string
   Memo: string
   ScheduledAt: string
}

let scheduledAtField =
   Form.dateField {
      Parser =
         CustomDateInterpreter.validate
            CustomDateInterpreter.DateSignifier.Single
         >> Result.bind (
            snd
            >> datePresentOrFutureValidator "Scheduled date"
            >> validationErrorsHumanFriendly
         )
      Value = _.ScheduledAt
      Update = fun newValue values -> { values with ScheduledAt = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Scheduled Date:"
         Placeholder = "Transfer scheduled for"
         HtmlAttributes = []
      }
   }

let amountField (availableBalance: decimal option) =
   Form.textField {
      Parser =
         amountValidatorFromString "Transfer amount"
         >> validationErrorsHumanFriendly
         >> Result.bind (fun amt ->
            match availableBalance with
            | Some balance ->
               if balance - amt < 0m then
                  Error $"Insufficient Balance ${balance}"
               else
                  Ok amt
            | None -> Ok amt)
      Value = _.Amount
      Update = fun newValue values -> { values with Amount = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Transfer Amount:"
         Placeholder = "25"
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
         Placeholder = "Reason for Transfer"
         HtmlAttributes = []
      }
   }

let memoForm = Form.succeed id |> Form.append memoField |> Form.optional

let fieldSenderSelect accounts =
   accountSelect (Some "Move money from account:") accounts
   |> Form.mapValues {
      Value = fun a -> { AccountId = a.SenderId }
      Update = fun a b -> { b with SenderId = a.AccountId }
   }

let formInternalWithinOrg
   (accounts: Map<AccountId, Account>)
   (initiatedBy: Initiator)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let fieldRecipientSelect accounts =
      accountSelect (Some "Move money to account:") accounts
      |> Form.mapValues {
         Value = fun a -> { AccountId = a.RecipientId }
         Update = fun a b -> { b with RecipientId = a.AccountId }
      }

   let onSubmit (sender: Account) (recipient: Account) (amount: decimal) =
      let orgId = sender.OrgId
      let parentAccountId = sender.ParentAccountId

      let transfer: InternalTransferWithinOrgInput = {
         Memo = None
         ScheduledDateSeedOverride = None
         Amount = amount
         Sender = {
            OrgId = orgId
            ParentAccountId = parentAccountId
            AccountId = sender.AccountId
            Name = sender.Name
         }
         Recipient = {
            OrgId = orgId
            ParentAccountId = parentAccountId
            AccountId = recipient.AccountId
            Name = recipient.Name
         }
         OriginatedFromSchedule = false
      }

      let msg =
         InternalTransferWithinOrgCommand.create initiatedBy transfer
         |> AccountCommand.InternalTransfer
         |> FormCommand.Account

      Msg.Submit(FormEntity.Account sender, msg, Started)

   Form.meta (fun values ->
      let senderOptions =
         accounts
         |> Map.filter (fun acctId _ -> string acctId <> values.RecipientId)

      let recipientOptions =
         accounts
         |> Map.filter (fun acctId _ -> string acctId <> values.SenderId)

      Form.succeed (fun (props: Account * AccountId * decimal) ->
         let sender, recipientId, amount = props
         let recipient = accounts[recipientId]
         onSubmit sender recipient amount)
      |> Form.append (
         fieldSenderSelect senderOptions
         |> Form.andThen (fun senderId ->
            let sender = accounts[senderId]

            Form.succeed (fun recipientId amount ->
               sender, recipientId, amount)
            |> Form.append (fieldRecipientSelect recipientOptions)
            |> Form.append (amountField (Some sender.AvailableBalance)))
      ))

let formInternalBetweenOrgs
   (senderAccounts: Map<AccountId, Account>)
   (destinations: SocialTransferDiscoveryCandidate list)
   (initiatedBy: Initiator)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let fieldOrgSelect =
      Form.selectField {
         Parser = Ok
         Value = _.RecipientId
         Update = fun newValue values -> { values with RecipientId = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Transfer to organization:"
            Placeholder = "No organization selected"
            Options =
               destinations
               |> List.map (fun candidate ->
                  string candidate.PrimaryReceivingAccountId, candidate.OrgName)
               |> List.sortBy snd
         }
      }

   let onSubmit
      (selectedRecipientId: AccountId)
      (sender: Account)
      (amount: decimal)
      (memo: string option)
      (scheduledAt: DateTime)
      =
      let memo =
         memo
         |> Option.bind (fun memo ->
            if String.IsNullOrWhiteSpace memo then None else Some memo)

      let recipient =
         destinations
         |> List.find (fun o ->
            o.PrimaryReceivingAccountId = selectedRecipientId)

      let transfer: InternalTransferBetweenOrgsInput = {
         ScheduledDateSeedOverride = None
         Amount = amount
         Sender = {
            OrgId = sender.OrgId
            ParentAccountId = sender.ParentAccountId
            AccountId = sender.AccountId
            Name = sender.Name
         }
         Recipient = {
            OrgId = recipient.OrgId
            ParentAccountId = recipient.ParentAccountId
            AccountId = selectedRecipientId
            Name = recipient.OrgName
         }
         Memo = memo
         OriginatedFromSchedule =
            scheduledAt.ToUniversalTime() <> DateTime.Today.ToUniversalTime()
         OriginatedFromPaymentRequest = None
      }

      let cmd =
         if transfer.OriginatedFromSchedule then
            ScheduleInternalTransferBetweenOrgsCommand.create initiatedBy {
               ScheduledDate = scheduledAt.ToUniversalTime()
               TransferInput = transfer
            }
            |> AccountCommand.ScheduleInternalTransferBetweenOrgs
         else
            InternalTransferBetweenOrgsCommand.create initiatedBy transfer
            |> AccountCommand.InternalTransferBetweenOrgs

      Msg.Submit(FormEntity.Account sender, FormCommand.Account cmd, Started)

   Form.succeed (fun (recipientId: string) props ->
      let sender, amount, memo, scheduledAt = props
      let recipientId = recipientId |> Guid.Parse |> AccountId
      onSubmit recipientId sender amount memo scheduledAt)
   |> Form.append fieldOrgSelect
   |> Form.append (
      fieldSenderSelect senderAccounts
      |> Form.andThen (fun senderId ->
         let sender = senderAccounts[senderId]

         Form.succeed (fun amount memo scheduledAt ->
            sender, amount, memo, scheduledAt)
         |> Form.append (amountField (Some sender.AvailableBalance))
         |> Form.append memoForm
         |> Form.append scheduledAtField)
   )

type private DomesticAccountInterchange =
   | Internal of Account
   | External of Counterparty

let formDomestic
   (org: OrgWithAccountProfiles)
   (initiatedBy: Initiator)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let internalAccounts = org.CheckingAccounts

   let internalAccountOptions =
      internalAccounts
      |> Map.toList
      |> List.map (fun (accountId, account) ->
         string accountId,
         $"{account.Name} ({Money.format account.AvailableBalance})")

   let counterpartyOptions (cps: Map<CounterpartyId, Counterparty>) =
      Map.toList cps
      |> List.map (fun (recipientId, recipient) ->
         let name = recipient.Nickname |> Option.defaultValue recipient.Name

         string recipientId, $"{name} **{recipient.AccountNumber.Last4}")

   let externalFundingSourceOptions =
      counterpartyOptions org.ExternalFundingSources

   let externalRecipientOptions =
      counterpartyOptions org.ExternalTradingPartners

   let fieldSenderSelect values =
      Form.selectField {
         Parser = Ok
         Value = _.SenderId
         Update =
            fun newValue values ->
               let internalCount =
                  internalAccountOptions
                  |> List.filter (fun (id, _) ->
                     id = values.RecipientId || id = newValue)
                  |> List.length

               let externalCount =
                  externalFundingSourceOptions
                  |> List.filter (fun (id, _) ->
                     id = values.RecipientId || id = newValue)
                  |> List.length

               // Resetting the recipient ensures that the transfer flows from an
               // internal account to an external account or vice versa.
               // Reset if:
               //    1. Sender is already selected as the recipient.
               //    2. Both sender & recipient are internal accounts.
               //    3. Both sender & recipient are external accounts.
               let resetRecipient =
                  internalCount = 2
                  || externalCount = 2
                  || newValue = values.RecipientId

               {
                  values with
                     SenderId = newValue
                     RecipientId =
                        if resetRecipient then "" else values.RecipientId
               }
         Error = fun _ -> None
         Attributes = {
            Label = "Sender:"
            Placeholder = "No account selected"
            Options =
               internalAccountOptions @ externalFundingSourceOptions
               |> List.sortBy snd
         }
      }

   let fieldRecipientSelect values =
      // Restrict recipient to accounts of the opposite type to the sender
      // (internal accounts can only send to external accounts and vice versa)
      let recipientOptions =
         let senderIsInternal =
            Guid.parseOptional values.SenderId
            |> Option.exists (AccountId >> internalAccounts.ContainsKey)

         if String.IsNullOrWhiteSpace values.SenderId then
            internalAccountOptions
            @ externalRecipientOptions
            @ externalFundingSourceOptions
         elif senderIsInternal then
            externalFundingSourceOptions @ externalRecipientOptions
         else
            internalAccountOptions

      Form.selectField {
         Parser = Ok
         Value = _.RecipientId
         Update = fun newValue values -> { values with RecipientId = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Recipient:"
            Placeholder = "No recipient selected"
            Options = recipientOptions |> List.sortBy snd
         }
      }

   let onSubmit
      (amount: decimal)
      (memo: string option)
      (scheduledAt: DateTime)
      (originator: Account)
      (counterparty: Counterparty)
      (moneyFlow: MoneyFlow)
      =
      let memo =
         memo
         |> Option.bind (fun memo ->
            if String.IsNullOrWhiteSpace memo then None else Some memo)

      let scheduledAt = scheduledAt.ToUniversalTime()

      let transfer: DomesticTransferInput = {
         Amount = amount
         Originator = {
            Name = originator.Name
            OrgId = originator.OrgId
            ParentAccountId = originator.ParentAccountId
            AccountId = originator.AccountId
         }
         Counterparty = counterparty
         Memo = memo
         MoneyFlow = moneyFlow
         ScheduledDateSeedOverride = None
         OriginatedFromSchedule =
            scheduledAt <> DateTime.Today.ToUniversalTime()
      }

      let cmd =
         if transfer.OriginatedFromSchedule then
            ScheduleDomesticTransferCommand.create initiatedBy {
               ScheduledDate = scheduledAt
               TransferInput = transfer
            }
            |> AccountCommand.ScheduleDomesticTransfer
         else
            DomesticTransferCommand.create
               (Guid.NewGuid() |> CorrelationId)
               initiatedBy
               transfer
            |> AccountCommand.DomesticTransfer

      Msg.Submit(
         FormEntity.Account originator,
         FormCommand.Account cmd,
         Started
      )

   let interchange (id: Guid) =
      internalAccounts.TryFind(AccountId id)
      |> Option.map DomesticAccountInterchange.Internal
      |> Option.orElseWith (fun () ->
         org.Counterparties.TryFind(CounterpartyId id)
         |> Option.map DomesticAccountInterchange.External)

   Form.meta (fun values ->
      let availableBalance =
         Guid.parseOptional values.SenderId
         |> Option.bind (AccountId >> internalAccounts.TryFind)
         |> Option.map _.AvailableBalance

      Form.succeed
         (fun (senderId: string) (recipientId: string) amount memo scheduledAt ->
            let sender = interchange (Guid.Parse senderId)
            let recipient = interchange (Guid.Parse recipientId)

            let onSubmit = onSubmit amount memo scheduledAt

            match sender, recipient with
            | Some(DomesticAccountInterchange.Internal sender),
              Some(DomesticAccountInterchange.External receiver) ->
               onSubmit sender receiver MoneyFlow.Out
            | Some(DomesticAccountInterchange.External sender),
              Some(DomesticAccountInterchange.Internal receiver) ->
               onSubmit receiver sender MoneyFlow.In
            | o -> failwith $"Invalid account interchange: {o}")
      |> Form.append (fieldSenderSelect values)
      |> Form.append (fieldRecipientSelect values)
      |> Form.append (amountField availableBalance)
      |> Form.append memoForm
      |> Form.append scheduledAtField)


[<ReactComponent>]
let TransferInternalWithinOrgComponent
   (session: UserSession)
   (accounts: Map<AccountId, Account>)
   (onSubmit: AccountCommandReceipt -> unit)
   =
   FormContainer {|
      Session = session
      InitialValues = {
         Amount = ""
         SenderId = ""
         RecipientId = ""
         Memo = ""
         ScheduledAt = "TODAY"
      }
      Form = formInternalWithinOrg accounts session.AsInitiator
      Action = None
      OnSubmit =
         function
         | FormSubmitReceipt.Account receipt -> onSubmit receipt
         | _ -> ()
      ComponentName = "TransferInternalWithinOrgForm"
      UseEventSubscription = Some [ SignalREventProvider.EventType.Account ]
   |}

[<ReactComponent>]
let TransferInternalBetweenOrgsComponent
   (destinationCandidates: SocialTransferDiscoveryCandidate list)
   (session: UserSession)
   (senderAccounts: Map<AccountId, Account>)
   (rules: Map<CommandApprovalRuleId, CommandApprovalRule>)
   (employeeAccrual: CommandApprovalDailyAccrual)
   (onSubmit: AccountCommandReceipt -> unit)
   (onSubmitForApproval: CommandApprovalProgress.RequestCommandApproval -> unit)
   =
   let initValues = {
      Amount = ""
      SenderId = ""
      RecipientId = ""
      Memo = ""
      ScheduledAt = "TODAY"
   }

   let initValues =
      destinationCandidates
      |> List.tryHead
      |> Option.map (fun candidate -> {
         initValues with
            RecipientId = string candidate.PrimaryReceivingAccountId
      })
      |> Option.defaultValue initValues

   FormContainer {|
      Session = session
      InitialValues = initValues
      Form =
         formInternalBetweenOrgs
            senderAccounts
            destinationCandidates
            session.AsInitiator
      Action = None
      OnSubmit =
         function
         | FormSubmitReceipt.Account receipt ->
            match receipt.PendingCommand with
            | AccountCommand.ScheduleInternalTransferBetweenOrgs _ ->
               onSubmit receipt
            | AccountCommand.InternalTransferBetweenOrgs cmd ->
               let cmd =
                  cmd
                  |> InternalTransferBetweenOrgs
                  |> ApprovableCommand.AmountBased

               let requiresApproval =
                  CommandApprovalRule.commandRequiresApproval
                     cmd
                     employeeAccrual
                     rules

               match requiresApproval with
               | None -> onSubmit receipt
               | Some rule ->
                  CommandApprovalProgress.RequestCommandApproval.fromApprovableCommand
                     session
                     rule
                     cmd
                  |> onSubmitForApproval
            | _ -> ()
         | _ -> ()
      ComponentName = "TransferInternalBetweenOrgsForm"
      UseEventSubscription =
         Some [
            // Listen for internal transfer between orgs
            SignalREventProvider.EventType.Account
            // Listen for command approval request
            SignalREventProvider.EventType.Org
         ]
   |}

[<ReactComponent>]
let TransferDomesticFormComponent
   (session: UserSession)
   (org: OrgWithAccountProfiles)
   (employeeAccrual: CommandApprovalDailyAccrual)
   (selectedRecipient: (RecipientAccountEnvironment * CounterpartyId) option)
   (onSubmit: AccountCommandReceipt -> unit)
   (onSubmitForApproval: CommandApprovalProgress.RequestCommandApproval -> unit)
   =
   let defaultRecipientId =
      match selectedRecipient with
      | Some(env, id) when env = RecipientAccountEnvironment.Domestic ->
         string id
      | _ -> ""

   FormContainer {|
      Session = session
      InitialValues = {
         Amount = ""
         SenderId = ""
         RecipientId = defaultRecipientId
         Memo = ""
         ScheduledAt = "TODAY"
      }
      Form = formDomestic org session.AsInitiator
      Action = None
      OnSubmit =
         function
         | FormSubmitReceipt.Account receipt ->
            match receipt.PendingCommand with
            | AccountCommand.ScheduleDomesticTransfer _ -> onSubmit receipt
            | AccountCommand.DomesticTransfer cmd ->
               let cmd =
                  cmd |> DomesticTransfer |> ApprovableCommand.AmountBased

               let requiresApproval =
                  CommandApprovalRule.commandRequiresApproval
                     cmd
                     employeeAccrual
                     org.Org.CommandApprovalRules

               match requiresApproval with
               | None -> onSubmit receipt
               | Some rule ->
                  CommandApprovalProgress.RequestCommandApproval.fromApprovableCommand
                     session
                     rule
                     cmd
                  |> onSubmitForApproval
            | _ -> ()
         | _ -> ()
      ComponentName = "DomesticTransferForm"
      UseEventSubscription =
         Some [
            // Listen for domestic transfer
            SignalREventProvider.EventType.Account
            // Listen for command approval request
            SignalREventProvider.EventType.Org
         ]
   |}

[<ReactComponent>]
let TransferFormComponent
   (session: UserSession)
   (org: OrgWithAccountProfiles)
   (selectedRecipient: (RecipientAccountEnvironment * CounterpartyId) option)
   (onSubmit: AccountCommandReceipt -> unit)
   (onSubmitForApproval: CommandApprovalProgress.RequestCommandApproval -> unit)
   =
   let dailyAccrual, setDailyAccrual =
      React.useState<Deferred<Result<CommandApprovalDailyAccrual, Err>>>
         Deferred.InProgress

   React.useEffectOnce (fun () ->
      async {
         let! res =
            OrgService.getTodaysAccrualMetricsByInitiatedBy
               session.OrgId
               session.AsInitiator.Id

         match res with
         | Error e -> Log.error $"Error getting employee accrual metrics {e}"
         | _ -> ()

         setDailyAccrual (Deferred.Resolved res)
      }
      |> Async.StartImmediate)

   let initialDestinationEnv =
      selectedRecipient
      |> Option.map fst
      |> Option.defaultValue RecipientAccountEnvironment.InternalWithinOrg

   let destinationAccountEnv, setDestinationAccountEnv =
      React.useState initialDestinationEnv

   React.fragment [
      Html.select [
         attr.onChange (
            RecipientAccountEnvironment.fromStringUnsafe
            >> setDestinationAccountEnv
         )
         attr.value (string destinationAccountEnv)

         attr.children [
            Html.option [
               attr.value (string RecipientAccountEnvironment.InternalWithinOrg)
               attr.text "Internal transfer within organization"
            ]

            Html.option [
               attr.value (
                  string RecipientAccountEnvironment.InternalBetweenOrgs
               )
               attr.text "Internal transfer to another organization"
            ]

            Html.option [
               attr.value (string RecipientAccountEnvironment.Domestic)
               attr.text "Domestic transfer"
            ]
         ]
      ]

      match destinationAccountEnv with
      | RecipientAccountEnvironment.InternalBetweenOrgs ->
         OrgSocialTransferDiscovery.OrgSearchComponent
            session.OrgId
            (fun searchInput destinationCandidates ->
               match destinationCandidates with
               | Deferred.Resolved(Ok(Some destinations)) ->
                  match dailyAccrual with
                  | Deferred.Resolved(Ok accrual) ->
                     TransferInternalBetweenOrgsComponent
                        destinations
                        session
                        org.CheckingAccounts
                        org.Org.CommandApprovalRules
                        accrual
                        onSubmit
                        onSubmitForApproval
                  | _ -> Html.progress []
               | Deferred.Resolved(Ok None) ->
                  Html.p $"No orgs found by search query {searchInput}."
               | _ -> Html.none)
      | RecipientAccountEnvironment.InternalWithinOrg ->
         TransferInternalWithinOrgComponent session org.Accounts onSubmit
      | RecipientAccountEnvironment.Domestic ->
         if org.Counterparties.Count = 0 then
            Html.button [
               attr.classes [ "outline" ]
               attr.text "No recipients.  Click here to create."
               attr.onClick (fun _ ->
                  {
                     TransactionBrowserQuery.empty with
                        Action = Some AccountActionView.RegisterCounterparty
                  }
                  |> Routes.TransactionsUrl.queryPath
                  |> Router.navigate)
            ]
         else
            match dailyAccrual with
            | Deferred.Resolved(Ok accrual) ->
               TransferDomesticFormComponent
                  session
                  org
                  accrual
                  selectedRecipient
                  onSubmit
                  onSubmitForApproval
            | _ -> Html.progress []
   ]
