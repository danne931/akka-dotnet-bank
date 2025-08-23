module PartnerBankSqlMapper

open System

open Lib.SharedTypes
open Bank.Account.Domain
open PartnerBank.Service.Domain

let table = "partner_bank_parent_account"

module TypeCast =
   let status = "parent_account_status"

module Fields =
   let orgId = "org_id"
   let parentAccountId = "parent_account_id"
   let partnerBankRoutingNumber = "partner_bank_routing_number"
   let partnerBankAccountNumber = "partner_bank_account_number"
   let partnerBankAccountId = "partner_bank_account_id"
   let partnerBankLegalEntityId = "partner_bank_legal_entity_id"
   let lastBillingCycleDate = "last_billing_cycle_at"
   let status = "status"

module SqlReader =
   let orgId (read: RowReader) = Fields.orgId |> read.uuid |> OrgId

   let parentAccountId (read: RowReader) =
      Fields.parentAccountId |> read.uuid |> ParentAccountId

   let partnerBankAccountNumber (read: RowReader) =
      read.int64 Fields.partnerBankAccountNumber
      |> AccountNumber
      |> PartnerBankAccountNumber

   let partnerBankRoutingNumber (read: RowReader) =
      read.int Fields.partnerBankRoutingNumber
      |> RoutingNumber
      |> PartnerBankRoutingNumber

   let partnerBankAccountId (read: RowReader) =
      read.string Fields.partnerBankAccountId |> PartnerBankAccountId

   let partnerBankLegalEntityId (read: RowReader) =
      read.string Fields.partnerBankLegalEntityId |> PartnerBankLegalEntityId

   let lastBillingCycleDate (read: RowReader) =
      read.dateTimeOrNone Fields.lastBillingCycleDate

   let status (read: RowReader) =
      read.string Fields.status |> ParentAccountStatus.fromStringUnsafe

module SqlWriter =
   let orgId (OrgId id) = Sql.uuid id

   let parentAccountId (ParentAccountId id) = Sql.uuid id

   let partnerBankAccountNumber (PartnerBankAccountNumber(AccountNumber num)) =
      Sql.int64 num

   let partnerBankRoutingNumber (PartnerBankRoutingNumber(RoutingNumber num)) =
      Sql.int num

   let partnerBankAccountId (PartnerBankAccountId id) = Sql.string id

   let partnerBankLegalEntityId (PartnerBankLegalEntityId id) = Sql.string id

   let lastBillingCycleDate (date: DateTime option) = Sql.timestamptzOrNone date

   let status (status: ParentAccountStatus) = status |> string |> Sql.string
