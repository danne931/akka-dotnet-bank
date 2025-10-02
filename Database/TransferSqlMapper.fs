module TransferSqlMapper

open System

open Lib.SharedTypes
open OrganizationSqlMapper
open AccountSqlMapper
open CounterpartySqlMapper
open Bank.Transfer.Domain

module Table =
   let transfer = "transfer"
   let internalTransferWithinOrg = "transfer_internal_within_org"
   let internalTransferBetweenOrgs = "transfer_internal_between_orgs"
   let domesticTransfer = "transfer_domestic"

module TransferTypeCast =
   let domesticTransferStatus = "domestic_transfer_status"
   let moneyFlow = "money_flow"

   let internalTransferBetweenOrgsStatus =
      "internal_transfer_between_orgs_status"

   let internalTransferWithinOrgStatus = "internal_transfer_within_org_status"
   let transferCategory = "transfer_category"

module TransferFields =
   let transferId = "transfer_id"
   let initiatedById = "initiated_by_id"
   let amount = "amount"
   let transferCategory = "transfer_category"
   let scheduledAt = "scheduled_at"
   let senderOrgId = "sender_org_id"
   let senderAccountId = "sender_account_id"
   let memo = "memo"
   let createdAt = "created_at"

   // Internal transfer fields
   module InternalWithinOrg =
      let isAutomated = "is_automated"
      let status = "status"
      let statusDetail = "status_detail"
      let recipientOrgId = "recipient_org_id"
      let recipientAccountId = "recipient_account_id"

   module InternalBetweenOrgs =
      let status = "transfer_status"
      let statusDetail = "transfer_status_detail"
      let recipientOrgId = "recipient_org_id"
      let recipientAccountId = "recipient_account_id"

   // Specific to domestic_transfer table
   module Domestic =
      let status = "transfer_status"
      let statusDetail = "transfer_status_detail"
      let counterpartyId = "counterparty_id"
      let expectedSettlementDate = "expected_settlement_date"
      let moneyFlow = "money_flow"

module TransferSqlReader =
   let transferId (read: RowReader) =
      TransferFields.transferId |> read.uuid |> TransferId

   let initiatedById (read: RowReader) =
      TransferFields.initiatedById |> read.uuid |> EmployeeId |> InitiatedById

   let amount (read: RowReader) = TransferFields.amount |> read.decimal

   let transferCategory (read: RowReader) : TransferCategory =
      TransferFields.transferCategory
      |> read.string
      |> TransferCategory.fromStringUnsafe

   let scheduledAt (read: RowReader) =
      TransferFields.scheduledAt |> read.dateTime

   let senderOrgId (read: RowReader) =
      TransferFields.senderOrgId |> read.uuid |> OrgId

   let senderAccountId (read: RowReader) =
      TransferFields.senderAccountId |> read.uuid |> AccountId

   let memo (read: RowReader) = TransferFields.memo |> read.textOrNone

   let createdAt (read: RowReader) = read.dateTime TransferFields.createdAt

   module InternalWithinOrg =
      let isAutomated (read: RowReader) =
         read.bool TransferFields.InternalWithinOrg.isAutomated

      let status (read: RowReader) =
         TransferFields.InternalWithinOrg.statusDetail
         |> read.text
         |> Serialization.deserializeUnsafe<InternalTransferWithinOrgStatus>

      let recipientOrgId (read: RowReader) =
         TransferFields.InternalWithinOrg.recipientOrgId |> read.uuid |> OrgId

      let recipientAccountId (read: RowReader) =
         TransferFields.InternalWithinOrg.recipientAccountId
         |> read.uuid
         |> AccountId

   module InternalBetweenOrgs =
      let status (read: RowReader) =
         TransferFields.InternalBetweenOrgs.statusDetail
         |> read.text
         |> Serialization.deserializeUnsafe<InternalTransferBetweenOrgsStatus>

      let recipientOrgId (read: RowReader) =
         TransferFields.InternalBetweenOrgs.recipientOrgId |> read.uuid |> OrgId

      let recipientAccountId (read: RowReader) =
         TransferFields.InternalBetweenOrgs.recipientAccountId
         |> read.uuid
         |> AccountId

   module Domestic =
      let status (read: RowReader) =
         TransferFields.Domestic.statusDetail
         |> read.text
         |> Serialization.deserializeUnsafe<DomesticTransferProgress>

      let counterpartyId (read: RowReader) =
         TransferFields.Domestic.counterpartyId |> read.uuid |> CounterpartyId

      let moneyFlow (read: RowReader) =
         TransferFields.Domestic.moneyFlow
         |> read.string
         |> MoneyFlow.fromString
         |> _.Value

      let originator (read: RowReader) : DomesticTransferOriginator = {
         Name = AccountSqlReader.name read
         AccountNumber = read.int64 AccountFields.accountNumber |> AccountNumber
         RoutingNumber =
            read.string AccountFields.routingNumber |> RoutingNumber
         OrgId = senderOrgId read
         ParentAccountId =
            read.uuid AccountFields.parentAccountId |> ParentAccountId
         PartnerBankAccountId =
            read.string PartnerBankSqlMapper.Fields.partnerBankAccountId
            |> PartnerBankAccountId
         AccountId = AccountSqlReader.accountId read
      }

      let expectedSettlementDate (read: RowReader) =
         read.dateTime TransferFields.Domestic.expectedSettlementDate

      let transfer (read: RowReader) : DomesticTransfer = {
         TransferId = transferId read
         InitiatedBy = {
            Id = initiatedById read
            Name = read.string "initiated_by_name"
         }
         Memo = memo read
         Status = status read
         Originator = originator read
         Counterparty = CounterpartyReader.counterparty read
         Amount = amount read
         MoneyFlow = moneyFlow read
         ScheduledDate = scheduledAt read
         ExpectedSettlementDate = expectedSettlementDate read
      }

module TransferSqlWriter =
   let transferId (TransferId id) = Sql.uuid id
   let initiatedById (InitiatedById employeeId) = Sql.uuid employeeId.Value
   let amount = Sql.decimal
   let transferCategory (cat: TransferCategory) = Sql.string (string cat)
   let scheduledAt (date: DateTime) = Sql.timestamptz date
   let orgId = OrgSqlWriter.orgId
   let accountId = AccountSqlWriter.accountId
   let memo = Sql.textOrNone
   let createdAt (date: DateTime) = Sql.timestamptz date

   module InternalWithinOrg =
      let isAutomated = Sql.bool

      let status (status: InternalTransferWithinOrgStatus) =
         match status with
         | InternalTransferWithinOrgStatus.Pending -> "Pending"
         | InternalTransferWithinOrgStatus.Settled -> "Settled"
         |> Sql.string

      let statusDetail (status: InternalTransferWithinOrgStatus) =
         Serialization.serialize status |> Sql.jsonb

   module InternalBetweenOrgs =
      let status =
         function
         | InternalTransferBetweenOrgsStatus.Scheduled -> "Scheduled"
         | InternalTransferBetweenOrgsStatus.Pending -> "Pending"
         | InternalTransferBetweenOrgsStatus.Deposited -> "Deposited"
         | InternalTransferBetweenOrgsStatus.Settled -> "Settled"
         | InternalTransferBetweenOrgsStatus.Failed _ -> "Failed"
         >> Sql.string

      let statusDetail (status: InternalTransferBetweenOrgsStatus) =
         Serialization.serialize status |> Sql.jsonb

   module Domestic =
      let status =
         function
         | DomesticTransferProgress.Scheduled -> "Scheduled"
         | DomesticTransferProgress.ProcessingAccountDeduction ->
            "ProcessingAccountDeduction"
         | DomesticTransferProgress.WaitingForTransferServiceAck ->
            "WaitingForTransferServiceAck"
         | DomesticTransferProgress.Settled -> "Settled"
         | DomesticTransferProgress.PartnerBank _ -> "PartnerBankProcessing"
         | DomesticTransferProgress.Failed _ -> "Failed"
         >> Sql.string

      let statusDetail (status: DomesticTransferProgress) =
         status |> Serialization.serialize |> Sql.jsonb

      let expectedSettlementDate (date: DateTime) = Sql.timestamptz date

      let counterpartyId (CounterpartyId id) = Sql.uuid id

      let moneyFlow (flow: MoneyFlow) = Sql.string (string flow)
