module Bank.BillingCycle.Api

open System
open Lib.SharedTypes
open FsToolkit.ErrorHandling

open Lib.Postgres
open BillingStatement
open BillingSqlMapper

let getBillingStatement () =
   pgQuery<BillingStatement> $"SELECT * FROM {BillingSqlMapper.table}" None
   <| fun (read: RowReader) -> {
      Transactions = BillingSqlReader.transactions read
      Month = BillingSqlReader.month read
      Year = BillingSqlReader.year read
      Balance = BillingSqlReader.balance read
      AccountName = BillingSqlReader.accountName read
      AccountId = BillingSqlReader.accountId read
      ParentAccountId = BillingSqlReader.parentAccountId read
      OrgId = BillingSqlReader.orgId read
   }

/// Get AccountEvents for a past billing cycle.
let getBillingTransactions
   (accountId: AccountId)
   (page: int)
   : TaskResultOption<BillingTransaction list, Err>
   =
   pgQuerySingle<BillingTransaction list>
      $"""
      SELECT {BillingFields.transactions}
      FROM {BillingSqlMapper.table}
      WHERE {BillingFields.accountId} = @accountId
      ORDER BY created_at DESC
      LIMIT 1
      OFFSET @offset
      """
      (Some [
         "@accountId", BillingSqlWriter.accountId accountId
         "@offset", Sql.int <| Math.Max(page, 1) - 1
      ])
      BillingSqlReader.transactions
