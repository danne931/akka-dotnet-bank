module UIDomain.Account

open System

open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes
open Lib.NetworkQuery
open Lib.Time
open Transaction

type AccountProfilesMaybe = Result<Map<AccountId, AccountProfile> option, Err>

type AccountMaybe = Result<Account option, Err>

type TransactionsMaybe = Result<Map<TransactionId, Transaction.T> option, Err>

type PaymentsMaybe = Result<PaymentSummary option, Err>

type AccountCommandReceipt = {
   PendingCommand: AccountCommand
   PendingEvent: AccountEvent
   PendingState: Account
   Envelope: Envelope
}

type SelectedCard = { Display: string; CardId: CardId }

type SelectedAccount = {
   Display: string
   AccountId: AccountId
}

[<RequireQualifiedAccess>]
type AccountActionView =
   | Purchase
   | Deposit
   | Transfer of (RecipientAccountEnvironment * AccountId) option
   | RegisterTransferRecipient
   | EditTransferRecipient of AccountId

   member x.Display =
      match x with
      | Purchase -> "Purchase"
      | Deposit -> "Deposit"
      | Transfer _ -> "Transfer"
      | RegisterTransferRecipient -> "Add a Transfer Recipient"
      | EditTransferRecipient _ -> "Edit a Transfer Recipient"

type AccountBrowserQuery = {
   Accounts: (SelectedAccount list) option
   MoneyFlow: MoneyFlow option
   Category: CategoryFilter option
   Amount: AmountFilter option
   Date: DateFilter option
   Action: AccountActionView option
   Transaction: TransactionId option
   SelectedCards: (SelectedCard list) option
   SelectedInitiatedBy: (UIDomain.Employee.SelectedEmployee list) option
   EventType: (TransactionGroupFilter list) option
} with

   member x.ChangeDetection =
      Serialization.serialize {|
         MoneyFlow = x.MoneyFlow
         Category = x.Category
         Amount = x.Amount
         Date = x.Date
         AccountIds = x.Accounts
         CardIds = x.SelectedCards
         InitiatedByIds = x.SelectedInitiatedBy
         EventType = x.EventType
      |}

module AccountBrowserQuery =
   let toQueryParams (query: AccountBrowserQuery) : (string * string) list =
      let agg =
         query.Amount
         |> Option.map AmountFilter.toQuery
         |> Option.defaultValue []

      let agg =
         match query.MoneyFlow with
         | None -> agg
         | Some flow -> ("moneyFlow", string flow) :: agg

      // If custom date range selected, date query param will consist
      // of a start & end date.  Otherwise it will be something like
      // date=Last30Days; date=LastMonth; etc.
      let agg =
         match query.Date with
         | None -> agg
         | Some(DateFilter.Custom(startDate, endDate)) ->
            ("date", DateTime.rangeAsQueryString startDate endDate) :: agg
         | Some filter -> ("date", string filter) :: agg

      let agg =
         match query.Category with
         | Some(CategoryFilter.CategoryIds catIds) ->
            ("categoryIds", listToQueryString catIds) :: agg
         | Some(CategoryFilter.IsCategorized isCat) ->
            ("isCategorized", string isCat) :: agg
         | None -> agg

      let agg =
         match query.Action with
         | Some(AccountActionView.EditTransferRecipient accountId) ->
            [
               "action", "EditTransferRecipient"
               "transferRecipient", string accountId
            ]
            @ agg
         | Some(AccountActionView.Transfer qParamsOpt) ->
            match qParamsOpt with
            | None -> ("action", "Transfer") :: agg
            | Some(accountEnvironment, recipientId) ->
               [
                  "action", "Transfer"
                  "accountEnvironment", string accountEnvironment
                  "transferRecipient", string recipientId
               ]
               @ agg
         | Some view -> ("action", string view) :: agg
         | None -> agg

      let agg =
         match query.Accounts with
         | Some accounts ->
            ("accounts", Serialization.serialize accounts) :: agg
         | None -> agg

      let agg =
         match query.SelectedCards with
         | None -> agg
         | Some cards -> ("cards", Serialization.serialize cards) :: agg

      let agg =
         match query.SelectedInitiatedBy with
         | None -> agg
         | Some initiatedBy ->
            ("initiatedBy", Serialization.serialize initiatedBy) :: agg

      let agg =
         match query.EventType with
         | None -> agg
         | Some filters -> ("events", listToQueryString filters) :: agg

      match query.Transaction with
      | Some txnId -> ("transaction", string txnId) :: agg
      | None -> agg

   let fromQueryParams
      (queryParams: (string * string) list)
      : AccountBrowserQuery
      =
      let queryParams = Map.ofList queryParams

      {
         Category =
            Map.tryFind "categoryIds" queryParams
            |> Option.bind CategoryFilter.categoryFromQueryString
            |> Option.orElse (
               Map.tryFind "isCategorized" queryParams
               |> Option.map (bool.Parse >> CategoryFilter.IsCategorized)
            )
         Amount = AmountFilter.fromQueryString queryParams
         MoneyFlow =
            Map.tryFind "moneyFlow" queryParams
            |> Option.bind MoneyFlow.fromString
         Date =
            Map.tryFind "date" queryParams |> Option.bind DateFilter.fromString
         Action =
            Map.tryFind "action" queryParams
            |> Option.bind (function
               | "Deposit" -> Some AccountActionView.Deposit
               | "Purchase" -> Some AccountActionView.Purchase
               | "Transfer" ->
                  let envOpt =
                     Map.tryFind "accountEnvironment" queryParams
                     |> Option.bind RecipientAccountEnvironment.fromString

                  let recipientIdOpt =
                     Map.tryFind "transferRecipient" queryParams
                     |> Option.bind Guid.parseOptional

                  Option.map2
                     (fun env recipientId ->
                        AccountActionView.Transfer(
                           Some(env, AccountId recipientId)
                        ))
                     envOpt
                     recipientIdOpt
                  |> Option.orElse (Some(AccountActionView.Transfer None))
               | "RegisterTransferRecipient" ->
                  Some AccountActionView.RegisterTransferRecipient
               | "EditTransferRecipient" ->
                  Map.tryFind "transferRecipient" queryParams
                  |> Option.map (
                     Guid.Parse
                     >> AccountId
                     >> AccountActionView.EditTransferRecipient
                  )
               | view ->
                  Log.error $"Account action view not implemented: {view}"
                  None)
         Transaction =
            Map.tryFind "transaction" queryParams
            |> Option.bind (
               Guid.parseOptional >> Option.map (CorrelationId >> TransactionId)
            )
         Accounts =
            Map.tryFind "accounts" queryParams
            |> Option.bind (
               Serialization.deserialize<SelectedAccount list>
               >> Result.toOption
            )
         SelectedCards =
            queryParams
            |> Map.tryFind "cards"
            |> Option.bind (
               Serialization.deserialize<SelectedCard list> >> Result.toOption
            )
         SelectedInitiatedBy =
            Map.tryFind "initiatedBy" queryParams
            |> Option.bind UIDomain.Employee.parseEmployees
         EventType =
            Map.tryFind "events" queryParams
            |> Option.bind TransactionGroupFilter.fromQueryString
      }

   let empty: AccountBrowserQuery = {
      Category = None
      MoneyFlow = None
      Amount = None
      Date = None
      Action = None
      Accounts = None
      Transaction = None
      SelectedCards = None
      SelectedInitiatedBy = None
      EventType = None
   }

type TransactionUIFriendly = {
   Name: string
   Date: string
   Amount: string option
   Info: string
   MoneyFlow: MoneyFlow option
   Source: string option
   Destination: string option
}

let getMerchantAlias
   (merchant: string)
   (merchants: Map<string, Merchant>)
   : string option
   =
   merchants |> Map.tryFind (merchant.ToLower()) |> Option.bind _.Alias

let debitWithMerchantAlias
   (evt: BankEvent<DebitedAccount>)
   (merchants: Map<string, Merchant>)
   =
   {
      evt with
         Data.Merchant =
            getMerchantAlias evt.Data.Merchant merchants
            |> Option.defaultValue evt.Data.Merchant
   }

let autoTransferRuleDisplay (rule: AutomaticTransfer.AutomaticTransferRule) =
   match rule with
   | AutomaticTransfer.AutomaticTransferRule.ZeroBalance _ ->
      "Maintain a zero balance."
   | AutomaticTransfer.AutomaticTransferRule.TargetBalance rule ->
      let targetBalance =
         rule.TargetAccountBalance |> PositiveAmount.get |> Money.formatShort

      let msg = $"Restore balance to {targetBalance}."

      match rule.TargetBalanceRange with
      | None -> msg
      | Some range ->
         let low = range.LowerBound |> PositiveAmount.get |> Money.formatShort

         let upper = range.UpperBound |> PositiveAmount.get |> Money.formatShort

         $"{msg} Balance out of range ({low}, {upper})"
   | AutomaticTransfer.AutomaticTransferRule.PercentDistribution rule ->
      let rule = AutomaticTransfer.PercentDistributionRule.get rule
      $"Maintain a zero balance by distributing it between {rule.DestinationAccounts.Length} accounts."

let transactionUIFriendlyFromAccountEvent
   (org: OrgWithAccountProfiles)
   (txn: AccountEvent)
   : TransactionUIFriendly
   =
   let _, envelope = AccountEnvelope.unwrap txn

   let props = {
      Name = envelope.EventName
      Date = DateTime.dateUIFriendlyWithSeconds envelope.Timestamp
      Amount = None
      Info = ""
      MoneyFlow = None
      Source = None
      Destination = None
   }

   let domesticRecipientName (recipientFromEvt: DomesticTransferRecipient) =
      org.Org.DomesticTransferRecipients
      |> Map.tryFind recipientFromEvt.RecipientAccountId
      |> Option.map _.FullName
      |> Option.defaultValue recipientFromEvt.FullName

   let accountName =
      org.AccountProfiles.TryFind(AccountId.fromEntityId envelope.EntityId)
      |> Option.map (fun a -> a.Account.Name)
      |> Option.defaultValue "Account"

   match txn with
   | CreatedAccount _ -> { props with Info = "Created Account" }
   | DepositedCash evt -> {
      props with
         Name = "Deposit Received"
         Info = $"Deposited money into {accountName}."
         MoneyFlow = Some MoneyFlow.In
         Source = Some evt.Data.Origin
         Destination = Some accountName
         Amount = Some <| Money.format evt.Data.Amount
     }
   | DebitedAccount evt ->
      let employee = evt.Data.EmployeePurchaseReference

      let employee =
         $"{employee.EmployeeName}**{employee.EmployeeCardNumberLast4}"

      {
         props with
            Name = "Purchase"
            Info =
               $"Purchase from {evt.Data.Merchant} by {employee} ({accountName})"
            Amount = Some <| Money.format evt.Data.Amount
            MoneyFlow = Some MoneyFlow.Out
            Source = Some $"{accountName} via {employee}"
            Destination = Some evt.Data.Merchant
      }
   | MaintenanceFeeDebited evt -> {
      props with
         Info = "Maintenance Fee"
         Amount = Some <| Money.format evt.Data.Amount
         MoneyFlow = Some MoneyFlow.Out
     }
   | MaintenanceFeeSkipped _ -> {
      props with
         Info = "Skipped Maintenance Fee"
     }
   | InternalTransferWithinOrgPending evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Internal Transfer"
            Info =
               $"Moved money from {info.Sender.Name} to {info.Recipient.Name}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.Out
            Source = Some accountName
            Destination = Some info.Recipient.Name
      }
   | InternalTransferWithinOrgCompleted evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Internal Transfer Completed"
            Info =
               $"Completed money movement from {info.Sender.Name} to {info.Recipient.Name}"
            Amount = Some <| Money.format info.Amount
      }
   | InternalTransferWithinOrgFailed evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Internal Transfer Failed"
            Info =
               $"Failed money movement from {info.Sender.Name} to {info.Recipient.Name} 
              - Reason: {evt.Data.Reason} 
              - Account refunded"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.In
      }
   | InternalTransferBetweenOrgsPending evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Transfer Between Orgs"
            Info = $"Transfer from {accountName} to {info.Recipient.Name}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.Out
            Source = Some accountName
            Destination = Some info.Recipient.Name
      }
   | InternalTransferBetweenOrgsScheduled evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Transfer Between Orgs Scheduled"
            Info =
               $"Transfer from {accountName} to {info.Recipient.Name} scheduled for {DateTime.formatShort info.ScheduledDate}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = None
            Source = Some accountName
            Destination = Some info.Recipient.Name
      }
   | InternalTransferBetweenOrgsCompleted evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Transfer Between Orgs Completed"
            Info =
               $"Completed transfer from {accountName} to {info.Recipient.Name}"
            Amount = Some <| Money.format info.Amount
      }
   | InternalTransferBetweenOrgsFailed evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Transfer Between Orgs Failed"
            Info =
               $"Failed transfer from {accountName} to {info.Recipient.Name} 
              - Reason: {evt.Data.Reason} 
              - Account refunded"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.In
      }
   | DomesticTransferPending evt ->
      let info = evt.Data.BaseInfo
      let recipientName = domesticRecipientName info.Recipient
      let payNetwork = info.Recipient.PaymentNetwork

      {
         props with
            Name = "Domestic Transfer"
            Info =
               $"{payNetwork} transfer processing from {info.Sender.Name} to {recipientName}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.Out
            Source = Some accountName
            Destination = Some recipientName
      }
   | DomesticTransferScheduled evt ->
      let info = evt.Data.BaseInfo
      let recipientName = domesticRecipientName info.Recipient
      let payNetwork = info.Recipient.PaymentNetwork

      {
         props with
            Name = "Domestic Transfer"
            Info =
               $"{payNetwork} transfer from {info.Sender.Name} to {recipientName} scheduled for {DateTime.formatShort info.ScheduledDate}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = None
            Source = Some accountName
            Destination = Some recipientName
      }
   | DomesticTransferCompleted evt ->
      let info = evt.Data.BaseInfo
      let payNetwork = info.Recipient.PaymentNetwork

      {
         props with
            Name = "Domestic Transfer Completed"
            Info =
               $"{payNetwork} transfer completed from {info.Sender.Name} to {domesticRecipientName info.Recipient}"
            Amount = Some <| Money.format info.Amount
      }
   | DomesticTransferFailed evt ->
      let info = evt.Data.BaseInfo

      let recipientName = domesticRecipientName info.Recipient
      let payNetwork = info.Recipient.PaymentNetwork

      {
         props with
            Name = "Domestic Transfer Failed"
            Info =
               $"{payNetwork} transfer from {accountName} to {recipientName} failed
               - Reason: {evt.Data.Reason.Display}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.In
            Source = Some accountName
            Destination = Some recipientName
      }
   | DomesticTransferProgress evt ->
      let info = evt.Data.BaseInfo
      let payNetwork = info.Recipient.PaymentNetwork

      {
         props with
            Info =
               $"Progress update received for {payNetwork} transfer 
                 to {domesticRecipientName info.Recipient} from {accountName}
                 - {evt.Data.InProgressInfo}"
            Amount = Some <| Money.format info.Amount
      }
   | InternalTransferWithinOrgDeposited evt ->
      let info = evt.Data.BaseInfo
      let sender = info.Sender.Name

      {
         props with
            Name = "Transfer Deposit Within Org"
            Info = $"{accountName} received transfer from {sender}."
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.In
            Source = Some sender
            Destination = Some accountName
      }
   | InternalTransferBetweenOrgsDeposited evt ->
      let info = evt.Data.BaseInfo
      let sender = info.Sender.Name

      {
         props with
            Name = "Transfer Deposit Between Orgs"
            Info = $"{accountName} received transfer from {sender}."
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.In
            Source = Some sender
            Destination = Some accountName
      }
   | BillingCycleStarted _ -> {
      props with
         Info = "New billing cycle.."
     }
   | AccountClosed evt -> {
      props with
         Info = $"Closed Account - Reference: {evt.Data.Reference}"
     }
   | PlatformPaymentRequested evt ->
      let p = evt.Data.BaseInfo

      {
         props with
            Name = "Payment Requested"
            Info =
               $"Requested payment from {p.Payer.OrgName} into {accountName}"
            Amount = Some <| Money.format p.Amount
            MoneyFlow = None
            Source = Some p.Payer.OrgName
            Destination = Some accountName
      }
   | PlatformPaymentPaid evt ->
      let p = evt.Data.BaseInfo

      {
         props with
            Name = "Payment Fulfilled"
            Info = $"Fulfilled payment to {p.Payee.OrgName} from {accountName}."
            Amount = Some <| Money.format p.Amount
            MoneyFlow = Some MoneyFlow.Out
            Source = Some accountName
            Destination = Some p.Payee.OrgName
      }
   | PlatformPaymentDeposited evt ->
      let p = evt.Data.BaseInfo

      {
         props with
            Name = "Payment"
            Info = $"{accountName} received payment from {p.Payer.OrgName}."
            Amount = Some <| Money.format p.Amount
            MoneyFlow = Some MoneyFlow.In
            Source = Some p.Payer.OrgName
            Destination = Some accountName
      }
   | PlatformPaymentCancelled evt ->
      let p = evt.Data.BaseInfo

      {
         props with
            Name = "Payment Cancelled"
            Info = $"Cancelled payment request to {p.Payer.OrgName}"
            Amount = Some <| Money.format p.Amount
            MoneyFlow = None
      }
   | PlatformPaymentDeclined evt ->
      let p = evt.Data.BaseInfo

      {
         props with
            Name = "Payment Declined"
            Info = $"{p.Payer.OrgName} declined payment request"
            Amount = Some <| Money.format p.Amount
            MoneyFlow = None
      }
   | AutoTransferRuleConfigured evt -> {
      props with
         Name = "Auto Transfer Rule Configured"
         Info = $"Created auto transfer rule {evt.Data.Config.Info.Display}"
     }
   | AutoTransferRuleDeleted _ -> {
      props with
         Name = "Auto Transfer Rule Deleted"
         Info = $"Deleted auto transfer rule"
     }
   | InternalAutomatedTransferPending evt ->
      let info = evt.Data.BaseInfo
      let reason = autoTransferRuleDisplay evt.Data.Rule

      {
         props with
            Name = "Auto Balance Management"
            Info =
               $"Automatically moved money from {info.Sender.Name} to {info.Recipient.Name}
               - Reason: {reason}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.Out
            Source = Some accountName
            Destination = Some info.Recipient.Name
      }
   | InternalAutomatedTransferCompleted evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Internal Automated Transfer Completed"
            Info =
               $"Auto Balance Management: completed transfer from {info.Sender.Name} to {info.Recipient.Name}"
            Amount = Some <| Money.format info.Amount
      }
   | InternalAutomatedTransferFailed evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Internal Automated Transfer Failed"
            Info =
               $"Auto Balance Management: failed transfer from {info.Sender.Name} to {info.Recipient.Name} 
              - Reason: {evt.Data.Reason} 
              - Account refunded"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.In
      }
   | InternalAutomatedTransferDeposited evt ->
      let info = evt.Data.BaseInfo
      let sender = info.Sender.Name

      {
         props with
            Name = "Internal Automated Transfer Deposit"
            Info =
               $"Auto Balance Management: {accountName} received transfer from {sender}."
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.In
            Source = Some sender
            Destination = Some accountName
      }

let transactionUIFriendly
   (org: OrgWithAccountProfiles)
   (txn: Transaction.T)
   : TransactionUIFriendly
   =
   let props = {
      Name = ""
      Date = DateTime.dateUIFriendly txn.Timestamp
      Amount = Some(Money.format txn.Amount)
      Info = ""
      MoneyFlow = None
      Source = None
      Destination = None
   }

   let domesticRecipientName (recipientFromEvt: DomesticTransferRecipient) =
      org.Org.DomesticTransferRecipients
      |> Map.tryFind recipientFromEvt.RecipientAccountId
      |> Option.map _.FullName
      |> Option.defaultValue recipientFromEvt.FullName

   let accountName accountId =
      org.AccountProfiles.TryFind(accountId)
      |> Option.map (fun a -> a.Account.FullName)
      |> Option.defaultValue "Account"

   match txn.Type with
   | TransactionType.Deposit ->
      let origin, recipientAccount =
         txn.Events
         |> List.tryLast
         |> Option.bind (function
            | AccountEvent.DepositedCash e ->
               Some(
                  e.Data.Origin,
                  accountName (AccountId.fromEntityId e.EntityId)
               )
            | _ -> None)
         |> Option.defaultValue ("Unknown", "Unknown")

      {
         props with
            Name = "Deposit Received"
            Info = $"Deposited money into {recipientAccount}."
            MoneyFlow =
               match txn.Status with
               | TransactionStatus.Complete
               | TransactionStatus.InProgress -> Some MoneyFlow.In
               | TransactionStatus.Failed -> None
            Source = Some origin
            Destination = Some recipientAccount
      }
   | TransactionType.Purchase ->
      let employee, merchant, accountName =
         txn.Events
         |> List.tryLast
         |> Option.bind (function
            | AccountEvent.DebitedAccount e ->
               let em = e.Data.EmployeePurchaseReference

               Some(
                  $"{em.EmployeeName}**{em.EmployeeCardNumberLast4}",
                  e.Data.Merchant,
                  accountName (AccountId.fromEntityId e.EntityId)
               )
            | _ -> None)
         |> Option.defaultValue ("Unknown", "Unknown", "Unknown")

      {
         props with
            Name = "Purchase"
            Info = $"Purchase from {merchant} by {employee} ({accountName})"
            MoneyFlow =
               match txn.Status with
               | TransactionStatus.Complete
               | TransactionStatus.InProgress -> Some MoneyFlow.Out
               | TransactionStatus.Failed -> None
            Source = Some $"{accountName} via {employee}"
            Destination = Some merchant
      }
   | TransactionType.InternalTransferWithinOrg ->
      let sender, recipient =
         txn.Events
         |> List.tryPick (function
            | AccountEvent.InternalTransferWithinOrgPending e ->
               let i = e.Data.BaseInfo
               Some(i.Sender.Name, i.Recipient.Name)
            | _ -> None)
         |> Option.defaultValue ("Unknown", "Unknown")

      {
         props with
            Name = "Move Money Between Accounts"
            Info = $"Moved money from {sender} to {recipient}"
            Source = Some sender
            Destination = Some recipient
      }
   | TransactionType.InternalTransferBetweenOrgs ->
      let sender, recipient =
         txn.Events
         |> List.tryPick (function
            | AccountEvent.InternalTransferBetweenOrgsPending e ->
               let sender = accountName (AccountId.fromEntityId e.EntityId)
               Some(sender, e.Data.BaseInfo.Recipient.Name)
            | _ -> None)
         |> Option.defaultValue ("Unknown", "Unknown")

      {
         props with
            Name = "Transfer Between Orgs"
            Info = $"Transfer {txn.Status.Display} from {sender} to {recipient}"
            MoneyFlow =
               match txn.Status with
               | TransactionStatus.Complete
               | TransactionStatus.InProgress -> Some MoneyFlow.Out
               | TransactionStatus.Failed -> None
            Source = Some sender
            Destination = Some recipient
      }
   | TransactionType.DomesticTransfer ->
      let sender, recipient, payNetwork =
         txn.Events
         |> List.tryLast
         |> Option.bind (function
            | AccountEvent.DomesticTransferPending e ->
               let i = e.Data.BaseInfo

               Some(
                  i.Sender.Name,
                  domesticRecipientName i.Recipient,
                  string i.Recipient.PaymentNetwork
               )
            | _ -> None)
         |> Option.defaultValue ("Unknown", "Unknown", "Unknown")

      {
         props with
            Name = "Domestic Transfer"
            Info =
               $"{payNetwork} transfer {txn.Status.Display} from {sender} to {recipient}"
            MoneyFlow =
               match txn.Status with
               | TransactionStatus.Complete
               | TransactionStatus.InProgress -> Some MoneyFlow.Out
               | TransactionStatus.Failed -> None
            Source = Some sender
            Destination = Some recipient
      }
   | TransactionType.Payment ->
      let sender, recipient, moneyFlow =
         txn.Events
         |> List.tryPick (function
            | AccountEvent.PlatformPaymentDeposited e when
               e.OrgId = org.Org.OrgId
               ->
               let i = e.Data.BaseInfo

               Some(
                  i.Payer.OrgName,
                  accountName i.Payee.AccountId,
                  Some MoneyFlow.In
               )
            | AccountEvent.PlatformPaymentPaid e when e.OrgId = org.Org.OrgId ->
               let i = e.Data.BaseInfo

               Some(
                  accountName (AccountId.fromEntityId e.EntityId),
                  i.Payee.OrgName,
                  Some MoneyFlow.Out
               )
            | _ -> None)
         |> Option.defaultValue ("Unknown", "Unknown", None)

      {
         props with
            Name = "Payment"
            Info = $"Payment from {sender} to {recipient}"
            MoneyFlow =
               match txn.Status with
               | TransactionStatus.Complete
               | TransactionStatus.InProgress -> moneyFlow
               | TransactionStatus.Failed -> None
            Source = Some sender
            Destination = Some recipient
      }
   | TransactionType.InternalAutomatedTransfer ->
      let sender, recipient, reason =
         txn.Events
         |> List.tryPick (function
            | AccountEvent.InternalAutomatedTransferPending e ->
               Some(
                  e.Data.BaseInfo.Sender.Name,
                  e.Data.BaseInfo.Recipient.Name,
                  autoTransferRuleDisplay e.Data.Rule
               )
            | _ -> None)
         |> Option.defaultValue ("Unknown", "Unknown", "Unknown")

      {
         props with
            Name = "Auto Balance Management"
            Info =
               $"Automatically moved money from {sender} to {recipient}
               - Reason: {reason}"
            Source = Some sender
            Destination = Some recipient
      }
