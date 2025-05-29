module PartnerBankSqlMapper

open System

open Lib.SharedTypes
open Bank.Account.Domain

let table = "partner_bank_parent_account"

module TypeCast =
   let status = "parent_account_status"

module Fields =
   let orgId = "org_id"
   let parentAccountId = "parent_account_id"
   let routingNumber = "partner_bank_routing_number"
   let accountNumber = "partner_bank_account_number"
   let lastBillingCycleDate = "last_billing_cycle_at"
   let status = "status"

module SqlReader =
   let orgId (read: RowReader) = Fields.orgId |> read.uuid |> OrgId

   let parentAccountId (read: RowReader) =
      Fields.parentAccountId |> read.uuid |> ParentAccountId

   let accountNumber (read: RowReader) =
      read.int64 Fields.accountNumber |> AccountNumber

   let routingNumber (read: RowReader) =
      read.int Fields.routingNumber |> RoutingNumber

   let lastBillingCycleDate (read: RowReader) =
      read.dateTimeOrNone Fields.lastBillingCycleDate

   let status (read: RowReader) =
      read.string Fields.status |> ParentAccountStatus.fromStringUnsafe

module SqlWriter =
   let orgId (OrgId id) = Sql.uuid id

   let parentAccountId (ParentAccountId id) = Sql.uuid id

   let accountNumber (AccountNumber num) = Sql.int64 num

   let routingNumber (RoutingNumber num) = Sql.int num

   let lastBillingCycleDate (date: DateTime option) = Sql.timestamptzOrNone date

   let status (status: ParentAccountStatus) = status |> string |> Sql.string
