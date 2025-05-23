module AccountSqlMapper

open System

open Lib.SharedTypes
open MaintenanceFee
open Bank.Account.Domain
open OrganizationSqlMapper
open AutomaticTransfer

let table = "account"

module AccountTypeCast =
   let depository = "account_depository"
   let status = "account_status"
   let autoTransferRuleFrequency = "auto_transfer_rule_frequency"

module AccountFields =
   let accountId = "account_id"
   let orgId = OrgFields.orgId
   let parentAccountId = "parent_account_id"
   let accountNumber = "account_number"
   let routingNumber = "routing_number"
   let name = "account_name"
   let depository = "depository"
   let currency = "currency"
   let status = "status"
   let balance = "balance"
   let lastBillingCycleDate = "last_billing_cycle_at"

   let maintenanceFeeQualifyingDepositFound =
      "maintenance_fee_qualifying_deposit_found"

   let maintenanceFeeDailyBalanceThreshold =
      "maintenance_fee_daily_balance_threshold"

   let autoTransferRule = "auto_transfer_rule"

   let autoTransferRuleFrequency = "auto_transfer_rule_frequency"

module AccountSqlReader =
   let accountId (read: RowReader) =
      AccountFields.accountId |> read.uuid |> AccountId

   let orgId = OrgSqlReader.orgId

   let parentAccountId (read: RowReader) =
      read.uuid AccountFields.parentAccountId |> ParentAccountId

   let accountNumber (read: RowReader) =
      read.int64 AccountFields.accountNumber |> AccountNumber

   let routingNumber (read: RowReader) =
      read.int AccountFields.routingNumber |> RoutingNumber

   let name (read: RowReader) = read.string AccountFields.name

   let depository (read: RowReader) =
      read.string AccountFields.depository |> AccountDepository.fromStringUnsafe

   let currency (read: RowReader) =
      read.string AccountFields.currency
      |> sprintf "\"%s\""
      |> Serialization.deserializeUnsafe<Currency>

   let status (read: RowReader) =
      read.string AccountFields.status |> AccountStatus.fromStringUnsafe

   let balance (read: RowReader) = read.decimal AccountFields.balance

   let lastBillingCycleDate (read: RowReader) =
      read.dateTimeOrNone AccountFields.lastBillingCycleDate

   let maintenanceFeeCriteria (read: RowReader) = {
      QualifyingDepositFound =
         read.bool AccountFields.maintenanceFeeQualifyingDepositFound
      DailyBalanceThreshold =
         read.bool AccountFields.maintenanceFeeDailyBalanceThreshold
   }

   let autoTransferRule (read: RowReader) : AutomaticTransferConfig option =
      read.textOrNone AccountFields.autoTransferRule
      |> Option.map Serialization.deserializeUnsafe<AutomaticTransferConfig>

   let account (read: RowReader) : Account = {
      AccountId = accountId read
      OrgId = orgId read
      ParentAccountId = parentAccountId read
      AccountNumber = accountNumber read
      RoutingNumber = routingNumber read
      Name = name read
      Depository = depository read
      Currency = currency read
      Status = status read
      Balance = balance read
      LastBillingCycleDate = lastBillingCycleDate read
      MaintenanceFeeCriteria = maintenanceFeeCriteria read
      AutoTransferRule = autoTransferRule read
   }

module AccountSqlWriter =
   let accountId = AccountId.get >> Sql.uuid
   let orgId = OrgSqlWriter.orgId
   let parentAccountId (ParentAccountId id) = Sql.uuid id

   let accountNumber (num: AccountNumber) =
      let (AccountNumber acctNum) = num
      Sql.int64 acctNum

   let routingNumber (num: RoutingNumber) =
      let (RoutingNumber routingNum) = num
      Sql.int routingNum

   let depository (dep: AccountDepository) = dep |> string |> Sql.string
   let name = Sql.string
   let balance = Sql.money
   let currency (currency: Currency) = Sql.string <| string currency
   let status (status: AccountStatus) = status |> string |> Sql.string

   let lastBillingCycleDate (date: DateTime option) = Sql.timestamptzOrNone date
   let maintenanceFeeQualifyingDepositFound = Sql.bool
   let maintenanceFeeDailyBalanceThreshold = Sql.bool

   let autoTransferRule (rule: AutomaticTransferConfig option) =
      rule |> Option.map Serialization.serialize |> Sql.jsonbOrNone

   let autoTransferRuleCount = Sql.int

   let autoTransferRuleFrequency
      (frequency: AutomaticTransfer.Frequency option)
      =
      frequency |> Option.map string |> Sql.stringOrNone
