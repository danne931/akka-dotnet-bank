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

type TransactionsMaybe = Result<Transaction.T list option, Err>

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
   Amount: string
   Info: string
   MoneyFlow: MoneyFlow option
   Source: string
   Destination: string
}

let getMerchant
   (merchantName: string)
   (merchants: Map<string, Merchant>)
   : Merchant option
   =
   merchants |> Map.tryFind (merchantName.ToLower())

let debitWithMerchantAlias
   (evt: BankEvent<DebitedAccount>)
   (merchants: Map<string, Merchant>)
   =
   {
      evt with
         Data.Merchant =
            getMerchant evt.Data.Merchant merchants
            |> Option.bind _.Alias
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

let transactionUIFriendly
   (merchants: Map<string, Merchant>)
   (org: OrgWithAccountProfiles)
   (txn: Transaction.T)
   : TransactionUIFriendly
   =
   let props = {
      Name = ""
      Date = DateTime.dateUIFriendly txn.Timestamp
      Amount = Money.format txn.Amount
      Info = ""
      MoneyFlow = None
      Source = ""
      Destination = ""
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
         |> List.tryPick (function
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
            Source = origin
            Destination = recipientAccount
      }
   | TransactionType.Purchase ->
      let employee, card, merchant, accountName =
         txn.Events
         |> List.tryPick (function
            | AccountEvent.DebitedAccount e ->
               let em = e.Data.EmployeePurchaseReference

               let merchant =
                  getMerchant e.Data.Merchant merchants
                  |> Option.bind _.Alias
                  |> Option.defaultValue e.Data.Merchant

               Some(
                  em.EmployeeName,
                  $"**{em.EmployeeCardNumberLast4}",
                  merchant,
                  accountName (AccountId.fromEntityId e.EntityId)
               )
            | _ -> None)
         |> Option.defaultValue ("Unknown", "Unknown", "Unknown", "Unknown")

      {
         props with
            Name = "Purchase"
            Info = $"Purchase from {merchant} with card {card} ({accountName})"
            MoneyFlow =
               match txn.Status with
               | TransactionStatus.Complete
               | TransactionStatus.InProgress -> Some MoneyFlow.Out
               | TransactionStatus.Failed -> None
            Source = $"{accountName} via {employee} **{card}"
            Destination = merchant
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
            Source = sender
            Destination = recipient
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
            Source = sender
            Destination = recipient
      }
   | TransactionType.DomesticTransfer ->
      let sender, recipient, payNetwork =
         txn.Events
         |> List.tryPick (function
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
            Source = sender
            Destination = recipient
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
            Source = sender
            Destination = recipient
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
            Source = sender
            Destination = recipient
      }

let private matchesTransactionGroupFilter
   (event: AccountEvent)
   (filter: TransactionGroupFilter)
   =
   match filter with
   | TransactionGroupFilter.Purchase ->
      match event with
      | AccountEvent.DebitedAccount _ -> true
      | _ -> false
   | TransactionGroupFilter.Deposit ->
      match event with
      | AccountEvent.DepositedCash _ -> true
      | _ -> false
   | TransactionGroupFilter.InternalTransferWithinOrg ->
      match event with
      | AccountEvent.InternalTransferWithinOrgPending _
      | AccountEvent.InternalTransferWithinOrgCompleted _
      | AccountEvent.InternalTransferWithinOrgFailed _
      | AccountEvent.InternalTransferWithinOrgDeposited _ -> true
      | _ -> false
   | TransactionGroupFilter.InternalTransferBetweenOrgs ->
      match event with
      | AccountEvent.InternalTransferBetweenOrgsPending _
      | AccountEvent.InternalTransferBetweenOrgsCompleted _
      | AccountEvent.InternalTransferBetweenOrgsFailed _
      | AccountEvent.InternalTransferBetweenOrgsDeposited _ -> true
      | _ -> false
   | TransactionGroupFilter.InternalAutomatedTransfer ->
      match event with
      | AccountEvent.InternalAutomatedTransferPending _
      | AccountEvent.InternalAutomatedTransferCompleted _
      | AccountEvent.InternalAutomatedTransferFailed _
      | AccountEvent.InternalAutomatedTransferDeposited _ -> true
      | _ -> false
   | TransactionGroupFilter.DomesticTransfer ->
      match event with
      | AccountEvent.DomesticTransferPending _
      | AccountEvent.DomesticTransferCompleted _
      | AccountEvent.DomesticTransferFailed _
      | AccountEvent.DomesticTransferProgress _ -> true
      | _ -> false
   | TransactionGroupFilter.PlatformPayment ->
      match event with
      | AccountEvent.PlatformPaymentRequested _
      | AccountEvent.PlatformPaymentPaid _
      | AccountEvent.PlatformPaymentDeposited _
      | AccountEvent.PlatformPaymentDeclined _
      | AccountEvent.PlatformPaymentCancelled _ -> true
      | _ -> false

/// Apply the selected filter logic to events arriving via SignalR.
/// Events fetched from the network query will be filtered via the
/// database query but still need to apply an in-browser filter for
/// events arriving via SignalR.
let keepRealtimeEventsCorrespondingToSelectedFilter
   (query: TransactionQuery)
   (evt: AccountEvent)
   =
   let _, envelope = AccountEnvelope.unwrap evt

   let qualifiedDate =
      match query.DateRange with
      | None -> true
      | Some(start, finish) ->
         let timestamp = envelope.Timestamp.ToLocalTime()
         timestamp >= start && timestamp <= finish

   let qualifiedAccount =
      match query.AccountIds with
      | None -> true
      | Some accounts ->
         accounts
         |> List.exists (fun accountId ->
            accountId = AccountId.fromEntityId envelope.EntityId)

   let qualifiedCard =
      match query.CardIds, evt with
      | Some cards, AccountEvent.DebitedAccount e ->
         cards
         |> List.exists (fun cardId ->
            cardId = e.Data.EmployeePurchaseReference.CardId)
      | _ -> true

   let qualifiedInitiator =
      match query.InitiatedByIds with
      | None -> true
      | Some initiators ->
         initiators
         |> List.exists (fun initiatedById ->
            initiatedById = envelope.InitiatedBy.Id)

   let amount, flow, _ = AccountEvent.moneyTransaction evt

   let qualifiedMoneyFlow =
      match query.MoneyFlow, flow with
      | Some queryFlow, Some eventFlow -> queryFlow = eventFlow
      | _ -> true

   let qualifiedAmount =
      match query.Amount, amount with
      | Some amountFilter, Some eventAmount ->
         AmountFilter.isAmountQualified eventAmount amountFilter
      | _ -> true

   // NOTE:
   // A realtime account event may correspond to a transaction
   // that is indeed categorized.  However, we currently don't have
   // that data available in the UI outside of the TransactionDetail
   // component.  It's not worth the effort right now to make it available
   // outside the TransactionDetail component so we will simply
   // discard the realtime event if a CategoryFilter is selected.
   let qualifiedCategory =
      match query.Category with
      | None -> true
      | Some(CategoryFilter.IsCategorized isCat) -> not isCat
      | Some(CategoryFilter.CategoryIds _) -> false

   let qualifiedEventType =
      match query.EventType with
      | None -> true
      | Some filters ->
         filters |> List.exists (matchesTransactionGroupFilter evt)

   qualifiedDate
   && qualifiedAccount
   && qualifiedInitiator
   && qualifiedCard
   && qualifiedMoneyFlow
   && qualifiedAmount
   && qualifiedCategory
   && qualifiedEventType
