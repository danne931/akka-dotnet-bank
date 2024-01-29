namespace Bank.Account.Domain

open System
open Npgsql.FSharp

open Lib.Types
open MaintenanceFee
open Bank.Transfer.Domain

module AccountFields =
   let entityId = "id"
   let email = "email"
   let firstName = "first_name"
   let lastName = "last_name"
   let currency = "currency"
   let status = "status"
   let balance = "balance"
   let dailyDebitLimit = "daily_debit_limit"
   let dailyDebitAccrued = "daily_debit_accrued"
   let lastDebitDate = "last_debit_date"
   let transferRecipients = "transfer_recipients"
   let events = "events"

   let maintenanceFeeQualifyingDepositFound =
      "maintenance_fee_qualifying_deposit_found"

   let maintenanceFeeDailyBalanceThreshold =
      "maintenance_fee_daily_balance_threshold"

   let inProgressTransfers = "in_progress_transfers"
   let inProgressTransfersCount = "in_progress_transfers_count"

module AccountSqlReader =
   let entityId (read: RowReader) = read.uuid AccountFields.entityId

   let email (read: RowReader) =
      read.string AccountFields.email |> Email.deserialize

   let firstName (read: RowReader) = read.string AccountFields.firstName
   let lastName (read: RowReader) = read.string AccountFields.lastName

   let currency (read: RowReader) =
      read.string AccountFields.currency
      |> sprintf "\"%s\""
      |> Serialization.deserializeUnsafe<Currency>

   let status (read: RowReader) =
      read.string AccountFields.status
      |> sprintf "\"%s\""
      |> Serialization.deserializeUnsafe<AccountStatus>

   let balance (read: RowReader) = read.decimal AccountFields.balance

   let dailyDebitLimit (read: RowReader) =
      read.decimal AccountFields.dailyDebitLimit

   let dailyDebitAccrued (read: RowReader) =
      read.decimal AccountFields.dailyDebitAccrued

   let lastDebitDate (read: RowReader) =
      read.dateTimeOrNone AccountFields.lastDebitDate

   let transferRecipients (read: RowReader) =
      read.text AccountFields.transferRecipients
      |> Serialization.deserializeUnsafe<Map<string, TransferRecipient>>

   let maintenanceFeeCriteria (read: RowReader) = {
      QualifyingDepositFound =
         read.bool AccountFields.maintenanceFeeQualifyingDepositFound
      DailyBalanceThreshold =
         read.bool AccountFields.maintenanceFeeDailyBalanceThreshold
   }

   let events (read: RowReader) =
      read.text AccountFields.events
      |> Serialization.deserializeUnsafe<AccountEvent list>

   let inProgressTransfers (read: RowReader) =
      read.text AccountFields.inProgressTransfers
      |> Serialization.deserializeUnsafe<Map<string, TransferTransaction>>

   let account (read: RowReader) : AccountState = {
      EntityId = entityId read
      Email = email read
      FirstName = firstName read
      LastName = lastName read
      Currency = currency read
      Status = status read
      Balance = balance read
      DailyDebitLimit = dailyDebitLimit read
      DailyDebitAccrued = dailyDebitAccrued read
      LastDebitDate = lastDebitDate read
      TransferRecipients = transferRecipients read
      MaintenanceFeeCriteria = maintenanceFeeCriteria read
      Events = events read
      InProgressTransfers = inProgressTransfers read
   }

module AccountSqlWriter =
   let entityId = Sql.uuid
   let email (email: Email) = Sql.string <| string email
   let firstName = Sql.string
   let lastName = Sql.string
   let balance = Sql.money
   let currency (currency: Currency) = Sql.string <| string currency
   let status (status: AccountStatus) = Sql.string <| string status
   let dailyDebitLimit = Sql.decimal
   let dailyDebitAccrued = Sql.decimal
   let lastDebitDate (date: DateTime option) = Sql.dateOrNone date

   let transferRecipients (recipients: Map<string, TransferRecipient>) =
      Sql.jsonb <| Serialization.serialize recipients

   let events (events: AccountEvent list) =
      Sql.jsonb <| Serialization.serialize events

   let maintenanceFeeQualifyingDepositFound = Sql.bool
   let maintenanceFeeDailyBalanceThreshold = Sql.bool

   let inProgressTransfers
      (inProgressTransfers: Map<string, TransferTransaction>)
      =
      Sql.jsonb <| Serialization.serialize inProgressTransfers

   let inProgressTransfersCount = Sql.int