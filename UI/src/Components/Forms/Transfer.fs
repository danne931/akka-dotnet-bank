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
open FormContainer
open Lib.SharedTypes
open Bank.Employee.Forms.AccountProfileForm

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

let amountField (account: Account) =
   Form.textField {
      Parser =
         amountValidatorFromString "Transfer amount"
         >> validationErrorsHumanFriendly
         >> Result.bind (fun amt ->
            if account.Balance - amt < 0m then
               Result.Error $"Insufficient Balance ${account.Balance}"
            else
               Ok amt)
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

let fieldRecipientSelect accounts =
   accountSelect (Some "Move money to account:") accounts
   |> Form.mapValues {
      Value = fun a -> { AccountId = a.RecipientId }
      Update = fun a b -> { b with RecipientId = a.AccountId }
   }

let formInternalWithinOrg
   (accounts: Map<AccountId, Account>)
   (initiatedBy: InitiatedById)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let onSubmit (sender: Account) (recipient: Account) (amount: decimal) =
      let transfer: InternalTransferInput = {
         Memo = None
         ScheduledDateSeedOverride = None
         Amount = amount
         Recipient = {
            OrgId = recipient.OrgId
            AccountId = recipient.AccountId
            Name = recipient.Name
         }
         Sender = {
            Name = sender.Name
            AccountId = sender.AccountId
            OrgId = sender.OrgId
         }
      }

      let msg =
         InternalTransferWithinOrgCommand.create
            sender.CompositeId
            initiatedBy
            transfer
         |> AccountCommand.InternalTransfer

      Msg.Submit(sender, msg, Started)

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
         (fieldSenderSelect senderOptions)
         |> Form.andThen (fun senderId ->
            let sender = accounts[senderId]

            Form.succeed (fun recipientId amount ->
               sender, recipientId, amount)
            |> Form.append (fieldRecipientSelect recipientOptions)
            |> Form.append (amountField sender))
      ))

let formInternalBetweenOrgs
   (senderAccounts: Map<AccountId, Account>)
   (destinationOrgs: Org list)
   (initiatedBy: InitiatedById)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let fieldOrgSelect =
      Form.selectField {
         Parser = Ok
         Value = fun values -> values.RecipientId
         Update = fun newValue values -> { values with RecipientId = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Transfer to organization:"
            Placeholder = "No organization selected"
            Options =
               destinationOrgs
               |> List.choose (fun org ->
                  org.FeatureFlags.SocialTransferDiscoveryPrimaryAccountId
                  |> Option.map (fun id -> string id, org.Name))
               |> List.sortBy snd
         }
      }

   let onSubmit
      (recipientId: AccountId)
      (sender: Account)
      (amount: decimal)
      (memo: string option)
      (scheduledAt: DateTime)
      =
      let memo =
         memo
         |> Option.bind (fun memo ->
            if String.IsNullOrWhiteSpace memo then None else Some memo)

      let org =
         destinationOrgs
         |> List.find (fun o ->
            match o.FeatureFlags.SocialTransferDiscoveryPrimaryAccountId with
            | Some accountId -> accountId = recipientId
            | None -> false)

      let transfer: InternalTransferInput = {
         ScheduledDateSeedOverride = None
         Amount = amount
         Recipient = {
            OrgId = org.OrgId
            AccountId = recipientId
            Name = org.Name
         }
         Sender = {
            Name = sender.Name
            AccountId = sender.AccountId
            OrgId = sender.OrgId
         }
         Memo = memo
      }

      let scheduledTransfer: ScheduleInternalTransferInput = {
         ScheduledDate = scheduledAt.ToUniversalTime()
         TransferInput = transfer
      }

      let cmd =
         if
            scheduledTransfer.ScheduledDate = DateTime.Today.ToUniversalTime()
         then
            InternalTransferBetweenOrgsCommand.create
               sender.CompositeId
               initiatedBy
               transfer
            |> AccountCommand.InternalTransferBetweenOrgs
         else
            ScheduleInternalTransferBetweenOrgsCommand.create
               sender.CompositeId
               initiatedBy
               scheduledTransfer
            |> AccountCommand.ScheduleInternalTransferBetweenOrgs

      Msg.Submit(sender, cmd, Started)

   Form.succeed (fun (recipientId: string) props ->
      let senderId, amount, memo, scheduledAt = props
      let recipientId = recipientId |> Guid.Parse |> AccountId
      onSubmit recipientId senderId amount memo scheduledAt)
   |> Form.append fieldOrgSelect
   |> Form.append (
      fieldSenderSelect senderAccounts
      |> Form.andThen (fun senderId ->
         let sender = senderAccounts[senderId]

         Form.succeed (fun amount memo scheduledAt ->
            sender, amount, memo, scheduledAt)
         |> Form.append (amountField sender)
         |> Form.append memoForm
         |> Form.append scheduledAtField)
   )

let formDomestic
   (org: Org)
   (senderAccounts: Map<AccountId, Account>)
   (initiatedBy: InitiatedById)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let fieldDomesticSelect =
      Form.selectField {
         Parser = Ok
         Value = fun values -> values.RecipientId
         Update = fun newValue values -> { values with RecipientId = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Recipient:"
            Placeholder = "No selection"
            Options =
               org.DomesticTransferRecipients
               |> Map.toList
               |> List.map (fun (recipientId, recipient) ->
                  let name =
                     recipient.Nickname |> Option.defaultValue recipient.Name

                  string recipientId,
                  $"{name} **{recipient.AccountNumber.Last4}")
               |> List.sortBy snd
         }
      }

   let onSubmit
      (recipient: DomesticTransferRecipient)
      (sender: Account)
      (amount: decimal)
      (memo: string option)
      (scheduledAt: DateTime)
      =
      let memo =
         memo
         |> Option.bind (fun memo ->
            if String.IsNullOrWhiteSpace memo then None else Some memo)

      let transfer: DomesticTransferInput = {
         Amount = amount
         Sender = {
            Name = sender.Name
            AccountNumber = sender.AccountNumber
            RoutingNumber = sender.RoutingNumber
            OrgId = sender.OrgId
            AccountId = sender.AccountId
         }
         Recipient = recipient
         Memo = memo
         ScheduledDateSeedOverride = None
      }

      let scheduledTransfer: ScheduleDomesticTransferInput = {
         ScheduledDate = scheduledAt.ToUniversalTime()
         TransferInput = transfer
      }

      let cmd =
         if
            scheduledTransfer.ScheduledDate = DateTime.Today.ToUniversalTime()
         then
            DomesticTransferCommand.create
               sender.CompositeId
               (Guid.NewGuid() |> CorrelationId)
               initiatedBy
               transfer
            |> AccountCommand.DomesticTransfer
         else
            ScheduleDomesticTransferCommand.create
               sender.CompositeId
               initiatedBy
               scheduledTransfer
            |> AccountCommand.ScheduleDomesticTransfer

      Msg.Submit(sender, cmd, Started)

   Form.succeed (fun props ->
      let sender, recipient, amount, memo, scheduledAt = props
      onSubmit recipient sender amount memo scheduledAt)
   |> Form.append (
      fieldSenderSelect senderAccounts
      |> Form.andThen (fun senderId ->
         let sender = senderAccounts[senderId]

         Form.succeed (fun (recipientId: string) amount memo scheduledAt ->
            let recipientId = recipientId |> Guid.Parse |> AccountId
            let recipient = org.DomesticTransferRecipients[recipientId]
            sender, recipient, amount, memo, scheduledAt)
         |> Form.append fieldDomesticSelect
         |> Form.append (amountField sender)
         |> Form.append memoForm
         |> Form.append scheduledAtField)
   )

[<ReactComponent>]
let TransferInternalWithinOrgComponent
   (session: UserSession)
   (accounts: Map<AccountId, Account>)
   (onSubmit: AccountCommandReceipt -> unit)
   =
   let initiatedBy = InitiatedById session.EmployeeId

   AccountFormContainer {|
      InitialValues = {
         Amount = ""
         SenderId = ""
         RecipientId = ""
         Memo = ""
         ScheduledAt = "TODAY"
      }
      Form = formInternalWithinOrg accounts initiatedBy
      Action = None
      OnSubmit = onSubmit
   |}

[<ReactComponent>]
let TransferInternalBetweenOrgsComponent
   (destinationOrgs: Org list)
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
      destinationOrgs
      |> List.tryHead
      |> Option.map (fun o ->
         o.FeatureFlags.SocialTransferDiscoveryPrimaryAccountId)
      |> Option.map (fun accountId -> {
         initValues with
            RecipientId = string accountId
      })
      |> Option.defaultValue initValues

   AccountFormContainer {|
      InitialValues = initValues
      Form =
         formInternalBetweenOrgs
            senderAccounts
            destinationOrgs
            (InitiatedById session.EmployeeId)
      Action = None
      OnSubmit =
         fun receipt ->
            match receipt.PendingCommand with
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
   |}

[<ReactComponent>]
let TransferDomesticFormComponent
   (session: UserSession)
   (org: Org)
   (senderAccounts: Map<AccountId, Account>)
   (employeeAccrual: CommandApprovalDailyAccrual)
   (selectedRecipient: (RecipientAccountEnvironment * AccountId) option)
   (onSubmit: AccountCommandReceipt -> unit)
   (onSubmitForApproval: CommandApprovalProgress.RequestCommandApproval -> unit)
   =
   let initiatedBy = InitiatedById session.EmployeeId

   let defaultRecipientId =
      match selectedRecipient with
      | Some(env, accountId) when env = RecipientAccountEnvironment.Domestic ->
         string accountId
      | _ ->
         org.DomesticTransferRecipients.Values
         |> Seq.tryHead
         |> Option.map (_.RecipientAccountId >> string)
         |> Option.defaultValue ""

   AccountFormContainer {|
      InitialValues = {
         Amount = ""
         SenderId = ""
         RecipientId = defaultRecipientId
         Memo = ""
         ScheduledAt = "TODAY"
      }
      Form = formDomestic org senderAccounts initiatedBy
      Action = None
      OnSubmit =
         fun receipt ->
            match receipt.PendingCommand with
            | AccountCommand.DomesticTransfer cmd ->
               let cmd =
                  cmd |> DomesticTransfer |> ApprovableCommand.AmountBased

               let requiresApproval =
                  CommandApprovalRule.commandRequiresApproval
                     cmd
                     employeeAccrual
                     org.CommandApprovalRules

               match requiresApproval with
               | None -> onSubmit receipt
               | Some rule ->
                  CommandApprovalProgress.RequestCommandApproval.fromApprovableCommand
                     session
                     rule
                     cmd
                  |> onSubmitForApproval
            | _ -> ()
   |}

[<ReactComponent>]
let TransferFormComponent
   (session: UserSession)
   (org: OrgWithAccountProfiles)
   (selectedRecipient: (RecipientAccountEnvironment * AccountId) option)
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
               (InitiatedById session.EmployeeId)

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
            (fun searchInput destinationOrgs ->
               match destinationOrgs with
               | Deferred.Resolved(Ok(Some destinationOrgs)) ->
                  match dailyAccrual with
                  | Deferred.Resolved(Ok accrual) ->
                     TransferInternalBetweenOrgsComponent
                        destinationOrgs
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
         if org.Org.DomesticTransferRecipients.Count = 0 then
            Html.button [
               attr.classes [ "outline" ]
               attr.text "No recipients.  Click here to create."
               attr.onClick (fun _ ->
                  {
                     AccountBrowserQuery.empty with
                        Action =
                           Some AccountActionView.RegisterTransferRecipient
                  }
                  |> Routes.TransactionsUrl.queryPath
                  |> Router.navigate)
            ]
         else
            match dailyAccrual with
            | Deferred.Resolved(Ok accrual) ->
               TransferDomesticFormComponent
                  session
                  org.Org
                  org.CheckingAccounts
                  accrual
                  selectedRecipient
                  onSubmit
                  onSubmitForApproval
            | _ -> Html.progress []
   ]
