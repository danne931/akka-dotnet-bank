module Bank.Account.UIDomain

open System

open Bank.Account.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes
open Lib.TransactionQuery

type AccountProfilesMaybe = Result<Map<AccountId, AccountProfile> option, Err>

type AccountAndTransactionsMaybe =
   Result<(Account * AccountEvent list) option, Err>

type TransactionsMaybe = Result<AccountEvent list option, Err>

type TransactionUIFriendly = {
   Name: string
   DateNaked: DateTime
   Date: string
   AmountNaked: decimal option
   Amount: string option
   Sign: string
   Info: string
   MoneyFlow: MoneyFlow option
   Source: string option
   Destination: string option
}

let dateUIFriendly (date: DateTime) =
   let dayAndMonth = date.ToLongDateString().Split(string date.Year)[0]
   $"{dayAndMonth} {date.ToShortTimeString()}"

let debitWithMerchantAlias
   (evt: BankEvent<DebitedAccount>)
   (merchants: Map<string, Merchant>)
   =
   {
      evt with
         Data.Origin =
            merchants
            |> Map.tryFind (evt.Data.Origin.ToLower())
            |> Option.bind _.Alias
            |> Option.defaultValue evt.Data.Origin
   }

let eventWithMerchantAlias
   (evt: AccountEvent)
   (merchants: Map<string, Merchant>)
   =
   match evt with
   | AccountEvent.DebitedAccount e ->
      AccountEvent.DebitedAccount(debitWithMerchantAlias e merchants)
   | _ -> evt

let nameAndNicknamePair
   (account: Account)
   (recipientId: AccountId)
   : string * string option
   =
   let recipient = Map.tryFind recipientId account.TransferRecipients

   match recipient with
   | Some(TransferRecipient.Internal recipient) ->
      recipient.Name, recipient.Nickname
   | Some(TransferRecipient.Domestic recipient) ->
      recipient.Name, recipient.Nickname
   | None -> "", None

let transactionUIFriendly
   (account: Account)
   (txn: AccountEvent)
   : TransactionUIFriendly
   =
   let _, envelope = AccountEnvelope.unwrap txn

   let props = {
      Name = envelope.EventName
      DateNaked = envelope.Timestamp
      Date = dateUIFriendly envelope.Timestamp
      AmountNaked = None
      Amount = None
      Sign = ""
      Info = ""
      MoneyFlow = None
      Source = None
      Destination = None
   }

   let accountIdLast4 (num: AccountId) = num |> string |> _.Substring(-4)

   let accountName = account.Name + " **" + account.AccountNumber.Last4

   let recipientName (recipientId: AccountId) =
      let name, nickname = nameAndNicknamePair account recipientId
      nickname |> Option.defaultValue name

   let recipientNameAndAccountNumber
      (recipientId: AccountId)
      (recipientAccountNum: AccountNumber)
      =
      let name = recipientName recipientId
      name + " **" + recipientAccountNum.Last4

   match txn with
   | CreatedAccount _ -> { props with Info = "Created Account" }
   | DepositedCash evt -> {
      props with
         Name = "Deposit Received"
         Info = "Deposit Received"
         MoneyFlow = Some MoneyFlow.In
         Source = Some evt.Data.Origin
         Destination = Some accountName
         AmountNaked = Some evt.Data.Amount
         Amount = Some <| Money.format evt.Data.Amount
     }
   | DebitedAccount evt ->
      let employee = evt.Data.EmployeePurchaseReference
      // TODO: rename Origin field as Merchant
      let employee =
         $"{employee.EmployeeName}**{employee.EmployeeCardNumberLast4}"

      {
         props with
            Name = "Purchase"
            Info = $"Purchase from {evt.Data.Origin} by {employee}"
            Amount = Some <| Money.format evt.Data.Amount
            AmountNaked = Some evt.Data.Amount
            MoneyFlow = Some MoneyFlow.Out
            Source = Some $"{accountName} via {employee}"
            Destination = Some evt.Data.Origin
      }
   | MaintenanceFeeDebited evt -> {
      props with
         Info = "Maintenance Fee"
         AmountNaked = Some evt.Data.Amount
         Amount = Some <| Money.format evt.Data.Amount
         MoneyFlow = Some MoneyFlow.Out
     }
   | MaintenanceFeeSkipped _ -> {
      props with
         Info = "Skipped Maintenance Fee"
     }
   | InternalTransferRecipient evt -> {
      props with
         Name = "Created Internal Recipient"
         Info =
            $"Created internal recipient: {recipientName evt.Data.Recipient.AccountId}"
     }
   | DomesticTransferRecipient evt ->
      let recipientName =
         recipientNameAndAccountNumber
            evt.Data.Recipient.AccountId
            evt.Data.Recipient.AccountNumber

      {
         props with
            Name = "Created Domestic Recipient"
            Info = $"Created domestic recipient: {recipientName}"
      }
   | EditedDomesticTransferRecipient evt ->
      let recipientName =
         recipientNameAndAccountNumber
            evt.Data.Recipient.AccountId
            evt.Data.Recipient.AccountNumber

      {
         props with
            Name = "Edited Domestic Recipient"
            Info = $"Edited recipient: {recipientName}"
      }
   | InternalSenderRegistered evt -> {
      props with
         Info =
            $"{evt.Data.Sender.Name} configured this account as a transfer recipient."
     }
   | InternalRecipientDeactivated evt -> {
      props with
         Info =
            $"Recipient {recipientName evt.Data.RecipientId} closed their account."
     }
   | InternalTransferPending evt -> {
      props with
         Name = "Internal Transfer Pending"
         Info = $"Internal transfer to {recipientName evt.Data.RecipientId}"
         AmountNaked = Some evt.Data.Amount
         Amount = Some <| Money.format evt.Data.Amount
         MoneyFlow = Some MoneyFlow.Out
         Source = Some accountName
         Destination = Some <| recipientName evt.Data.RecipientId
     }
   | InternalTransferApproved evt -> {
      props with
         Name = "Internal Transfer Approved"
         Info =
            $"Internal transfer approved to {recipientName evt.Data.RecipientId}"
         AmountNaked = Some evt.Data.Amount
         Amount = Some <| Money.format evt.Data.Amount
     }
   | InternalTransferRejected evt -> {
      props with
         Name = "Internal Transfer Rejected"
         Info =
            $"Internal transfer declined to {recipientName evt.Data.RecipientId} 
              - Reason: {evt.Data.Reason} 
              - Account refunded"
         AmountNaked = Some evt.Data.Amount
         Amount = Some <| Money.format evt.Data.Amount
         MoneyFlow = Some MoneyFlow.In
     }
   | DomesticTransferPending evt ->
      let info = evt.Data.BaseInfo

      let recipientName =
         recipientNameAndAccountNumber
            info.Recipient.AccountId
            info.Recipient.AccountNumber

      {
         props with
            Name = "Domestic Transfer Pending"
            Info = $"Domestic transfer processing to {recipientName}"
            AmountNaked = Some info.Amount
            Amount = Some <| Money.format evt.Data.BaseInfo.Amount
            MoneyFlow = Some MoneyFlow.Out
            Source = Some accountName
            Destination = Some recipientName
      }
   | DomesticTransferApproved evt ->
      let info = evt.Data.BaseInfo

      let recipientName =
         recipientNameAndAccountNumber
            info.Recipient.AccountId
            info.Recipient.AccountNumber

      {
         props with
            Name = "Domestic Transfer Approved"
            Info = $"Domestic transfer approved to {recipientName}"
            AmountNaked = Some info.Amount
            Amount = Some <| Money.format evt.Data.BaseInfo.Amount
      }
   | DomesticTransferRejected evt ->
      let info = evt.Data.BaseInfo

      let recipientName =
         recipientNameAndAccountNumber
            info.Recipient.AccountId
            info.Recipient.AccountNumber

      {
         props with
            Name = "Domestic Transfer Declined"
            Info =
               $"Domestic transfer declined to {recipientName} 
                 - Reason {evt.Data.Reason} 
                 - Account refunded"
            AmountNaked = Some info.Amount
            Amount = Some <| Money.format evt.Data.BaseInfo.Amount
            MoneyFlow = Some MoneyFlow.In
            Source = Some accountName
            Destination = Some recipientName
      }
   | DomesticTransferProgress evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Info =
               $"Progress update received for domestic transfer 
                 to {recipientName info.Recipient.AccountId}
                 - Status {evt.Data.Status}"
            AmountNaked = Some info.Amount
            Amount = Some <| Money.format evt.Data.BaseInfo.Amount
      }
   | TransferDeposited evt ->
      let sender =
         account.InternalTransferSenders
         |> Map.tryFind evt.Data.Origin
         |> Option.map _.Name
         |> Option.defaultValue (accountIdLast4 evt.Data.Origin)

      {
         props with
            Name = "Transfer Received"
            Info = $"Transfer received from {sender}."
            AmountNaked = Some evt.Data.Amount
            Amount = Some <| Money.format evt.Data.Amount
            MoneyFlow = Some MoneyFlow.In
            Source = Some sender
            Destination = Some accountName
      }
   | BillingCycleStarted _ -> {
      props with
         Info =
            "New billing cycle. Transactions consolidated into billing statement."
     }
   | AccountClosed evt -> {
      props with
         Info = $"Closed Account - Reference: {evt.Data.Reference}"
     }
   | RecipientNicknamed evt -> {
      props with
         Info =
            match evt.Data.Nickname with
            | None -> "Removed recipient nickname."
            | Some name -> $"Updated recipient nickname to {name}"
     }

type PotentialInternalTransferRecipients =
   private | PotentialInternalTransferRecipients of
      Map<AccountId, AccountProfile>

module PotentialInternalTransferRecipients =
   let create (account: Account) (accounts: Map<AccountId, AccountProfile>) =
      let potentialRecipients =
         accounts
         |> Map.filter (fun accountId _ ->
            let accountInRecipients =
               account.InternalTransferRecipients |> Map.containsKey accountId

            accountId <> account.AccountId && not accountInRecipients)

      PotentialInternalTransferRecipients potentialRecipients

   let value (PotentialInternalTransferRecipients recipients) = recipients

[<RequireQualifiedAccess>]
type DateFilter =
   | Custom of dateStart: DateTime * dateEnd: DateTime
   | Last30Days
   | CurrentMonth
   | LastMonth
   | CurrentYear
   | LastYear

module DateFilter =
   let toDateRange (filter: DateFilter) =
      let endOfToday = DateTime.Today.AddDays(1).AddMilliseconds(-1)

      match filter with
      | DateFilter.Custom(startDate, endDate) -> startDate, endDate
      | DateFilter.Last30Days -> DateTime.Today.AddDays(-30), endOfToday
      | DateFilter.CurrentMonth ->
         DateTime(DateTime.Today.Year, DateTime.Today.Month, 1), endOfToday
      | DateFilter.LastMonth ->
         let start = DateTime.Today.AddMonths -1

         let endDate =
            DateTime(
               start.Year,
               start.Month,
               DateTime.DaysInMonth(start.Year, start.Month)
            )

         DateTime(start.Year, start.Month, 1),
         endDate.AddDays(1).AddMilliseconds(-1)
      | DateFilter.CurrentYear ->
         DateTime(DateTime.Today.Year, 1, 1), endOfToday
      | DateFilter.LastYear ->
         DateTime(DateTime.Today.AddYears(-1).Year, 1, 1),
         DateTime(DateTime.Today.Year, 1, 1).AddMilliseconds(-1)

[<RequireQualifiedAccess>]
type AccountActionView =
   | Debit
   | Deposit
   | Transfer
   | RegisterTransferRecipient
   | EditTransferRecipient of AccountId
   | DailyDebitLimit
   | CardAccess

type AccountBrowserQuery = {
   MoneyFlow: MoneyFlow option
   Category: CategoryFilter option
   Amount: AmountFilter option
   Date: DateFilter option
   Action: AccountActionView option
   Transaction: EventId option
}

module AccountBrowserQuery =
   let toQueryParams (query: AccountBrowserQuery) : (string * string) list =
      let agg =
         match query.Amount with
         | None -> []
         | Some(AmountFilter.GreaterThanOrEqualTo amount) -> [
            "amountMin", string amount
           ]
         | Some(AmountFilter.LessThanOrEqualTo amount) -> [
            "amountMax", string amount
           ]
         | Some(AmountFilter.Between(min, max)) -> [
            "amountMin", string min
            "amountMax", string max
           ]

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
            ("categoryIds", TransactionQuery.categoryListToQueryString catIds)
            :: agg
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
         | Some view -> ("action", string view) :: agg
         | None -> agg

      match query.Transaction with
      | Some txnId -> ("transaction", string txnId) :: agg
      | None -> agg

   let fromQueryParams
      (queryParams: (string * string) list)
      : AccountBrowserQuery
      =
      let queryParams = Map.ofList queryParams

      let findAmount key =
         Map.tryFind key queryParams |> Option.map decimal

      let min = findAmount "amountMin"
      let max = findAmount "amountMax"

      let amountOpt =
         match min, max with
         | Some min, None -> Some(AmountFilter.GreaterThanOrEqualTo min)
         | None, Some max -> Some(AmountFilter.LessThanOrEqualTo max)
         | Some min, Some max -> Some(AmountFilter.Between(min, max))
         | _ -> None

      {
         Category =
            Map.tryFind "categoryIds" queryParams
            |> Option.map TransactionQuery.categoryFromQueryString
            |> Option.orElse (
               Map.tryFind "isCategorized" queryParams
               |> Option.map (bool.Parse >> CategoryFilter.IsCategorized)
            )
         Amount = amountOpt
         MoneyFlow =
            Map.tryFind "moneyFlow" queryParams
            |> Option.bind MoneyFlow.fromString
         Date =
            Map.tryFind "date" queryParams
            |> Option.bind (function
               | "Last30Days" -> Some DateFilter.Last30Days
               | "CurrentMonth" -> Some DateFilter.CurrentMonth
               | "LastMonth" -> Some DateFilter.LastMonth
               | "CurrentYear" -> Some DateFilter.CurrentYear
               | "LastYear" -> Some DateFilter.LastYear
               | str ->
                  TransactionQuery.dateRangeFromQueryString str
                  |> Option.map DateFilter.Custom)
         Action =
            Map.tryFind "action" queryParams
            |> Option.bind (function
               | "Deposit" -> Some AccountActionView.Deposit
               | "Debit" -> Some AccountActionView.Debit
               | "Transfer" -> Some AccountActionView.Transfer
               | "RegisterTransferRecipient" ->
                  Some AccountActionView.RegisterTransferRecipient
               | "EditTransferRecipient" ->
                  Map.tryFind "transferRecipient" queryParams
                  |> Option.map (
                     Guid.Parse
                     >> AccountId
                     >> AccountActionView.EditTransferRecipient
                  )
               | "DailyDebitLimit" -> Some AccountActionView.DailyDebitLimit
               | "CardAccess" -> Some AccountActionView.CardAccess
               | view ->
                  Log.error $"Account action view not implemented: {view}"
                  None)
         Transaction =
            Map.tryFind "transaction" queryParams
            |> Option.bind (Guid.parseOptional >> Option.map EventId)
      }

/// May edit transfer recipient if domestic and status is not Closed.
let canEditTransferRecipient
   (account: Account)
   (evt: AccountEvent)
   : DomesticTransferRecipient option
   =
   let recipientIdOpt =
      match evt with
      | AccountEvent.DomesticTransferPending evt ->
         Some evt.Data.BaseInfo.Recipient.AccountId
      | AccountEvent.DomesticTransferRecipient evt ->
         Some evt.Data.Recipient.AccountId
      | AccountEvent.DomesticTransferRejected evt ->
         Some evt.Data.BaseInfo.Recipient.AccountId
      | _ -> None

   recipientIdOpt
   |> Option.bind (fun recipientId ->
      Map.tryFind recipientId account.DomesticTransferRecipients)
   |> Option.bind (fun recipient ->
      if recipient.Status <> RecipientRegistrationStatus.Closed then
         Some recipient
      else
         None)
