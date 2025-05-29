module BillingSqlMapper

open BillingStatement
open AccountSqlMapper
open OrganizationSqlMapper

let table = "billing_statement"

module BillingFields =
   let transactions = "transactions"
   let month = "month"
   let year = "year"
   let balance = "balance"
   let accountName = "account_name"
   let accountId = AccountFields.accountId
   let parentAccountId = AccountFields.parentAccountId
   let orgId = OrgFields.orgId

module BillingSqlReader =
   let transactions (read: RowReader) =
      read.text BillingFields.transactions
      |> Serialization.deserializeUnsafe<BillingTransaction list>

   let month (read: RowReader) = read.int BillingFields.month
   let year (read: RowReader) = read.int BillingFields.year

   let balance (read: RowReader) = read.decimal BillingFields.balance

   let accountName (read: RowReader) = read.text BillingFields.accountName

   let accountId = AccountSqlReader.accountId

   let parentAccountId = AccountSqlReader.parentAccountId

   let orgId = OrgSqlReader.orgId

module BillingSqlWriter =
   let transactions (txns: BillingTransaction list) =
      txns |> Serialization.serialize |> Sql.jsonb

   let month = Sql.int
   let year = Sql.int
   let balance = Sql.money
   let accountName = Sql.text
   let accountId = AccountSqlWriter.accountId
   let parentAccountId = AccountSqlWriter.parentAccountId
   let orgId = OrgSqlWriter.orgId
