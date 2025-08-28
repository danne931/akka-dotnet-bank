module TransferSqlMapper

open System

open Lib.SharedTypes
open OrganizationSqlMapper
open AccountSqlMapper
open Bank.Transfer.Domain

module Table =
   let transfer = "transfer"
   let internalTransferWithinOrg = "transfer_internal_within_org"
   let internalTransferBetweenOrgs = "transfer_internal_between_orgs"
   let domesticTransfer = "transfer_domestic"
   let counterparty = "counterparty"

module TransferTypeCast =
   let paymentNetwork = "payment_network"

   let counterpartyAccountDepository = "counterparty_account_depository"

   let counterpartyStatus = "counterparty_status"
   let domesticTransferStatus = "domestic_transfer_status"

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

   // Specific to counterparty table
   module Counterparty =
      let counterpartyId = "counterparty_id"
      let orgId = "org_id"
      let firstName = "first_name"
      let lastName = "last_name"
      let nickname = "nickname"
      let routingNumber = "routing_number"
      let accountNumber = "account_number"
      let status = "counterparty_status"
      let depository = "depository"
      let paymentNetwork = "payment_network"

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

   module Counterparty =
      let id (read: RowReader) =
         TransferFields.Counterparty.counterpartyId |> read.uuid |> AccountId

      let orgId (read: RowReader) =
         TransferFields.Counterparty.orgId |> read.uuid |> OrgId

      let firstName (read: RowReader) =
         TransferFields.Counterparty.firstName |> read.string

      let lastName (read: RowReader) =
         TransferFields.Counterparty.lastName |> read.string

      let nickname (read: RowReader) =
         TransferFields.Counterparty.nickname |> read.stringOrNone

      let routingNumber = AccountSqlReader.routingNumber
      let accountNumber = AccountSqlReader.accountNumber

      let status (read: RowReader) =
         TransferFields.Counterparty.status
         |> read.string
         |> CounterpartyRegistrationStatus.fromStringUnsafe

      let depository (read: RowReader) =
         TransferFields.Counterparty.depository
         |> read.string
         |> CounterpartyAccountDepository.fromStringUnsafe

      let paymentNetwork (read: RowReader) =
         TransferFields.Counterparty.paymentNetwork
         |> read.string
         |> PaymentNetwork.fromStringUnsafe

      let counterparty (read: RowReader) : Counterparty = {
         FirstName = firstName read
         LastName = lastName read
         Nickname = nickname read
         AccountNumber = accountNumber read
         RoutingNumber = routingNumber read
         Status = status read
         CounterpartyId = id read
         OrgId = orgId read
         Depository = depository read
         PaymentNetwork = paymentNetwork read
         CreatedAt = createdAt read
      }

   module Domestic =
      let status (read: RowReader) =
         TransferFields.Domestic.statusDetail
         |> read.text
         |> Serialization.deserializeUnsafe<DomesticTransferProgress>

      let counterpartyId (read: RowReader) =
         TransferFields.Domestic.counterpartyId |> read.uuid |> AccountId

      let originator (read: RowReader) : DomesticTransferOriginator = {
         Name = AccountSqlReader.name read
         AccountNumber = read.int64 AccountFields.accountNumber |> AccountNumber
         RoutingNumber = read.int AccountFields.routingNumber |> RoutingNumber
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
         Counterparty = Counterparty.counterparty read
         Amount = amount read
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
         | DomesticTransferProgress.ThirdParty _ -> "ThirdPartyProcessing"
         | DomesticTransferProgress.Failed _ -> "Failed"
         >> Sql.string

      let statusDetail (status: DomesticTransferProgress) =
         status |> Serialization.serialize |> Sql.jsonb

      let expectedSettlementDate (date: DateTime) = Sql.timestamptz date

   module Counterparty =
      let counterpartyId = AccountSqlWriter.accountId
      let orgId = OrgSqlWriter.orgId
      let firstName = Sql.string
      let lastName = Sql.string
      let nickname = Sql.stringOrNone
      let routingNumber = AccountSqlWriter.routingNumber
      let accountNumber = AccountSqlWriter.accountNumber

      let status (status: CounterpartyRegistrationStatus) =
         Sql.string (string status)

      let depository (status: CounterpartyAccountDepository) =
         Sql.string (string status)

      let paymentNetwork (network: PaymentNetwork) = Sql.string (string network)

module Query =
   let domesticTransfer =
      let employeeId = EmployeeSqlMapper.EmployeeFields.employeeId

      let employeeName =
         $"employee.{EmployeeSqlMapper.EmployeeFields.firstName} || ' ' ||
           employee.{EmployeeSqlMapper.EmployeeFields.lastName}"

      $"""
      SELECT
         dt.{TransferFields.Domestic.counterpartyId},
         dt.{TransferFields.Domestic.status},
         dt.{TransferFields.Domestic.statusDetail},
         t.{TransferFields.transferId},
         t.{TransferFields.initiatedById},
         t.{TransferFields.scheduledAt},
         t.{TransferFields.senderOrgId},
         t.{TransferFields.senderAccountId},
         t.{TransferFields.amount},
         t.{TransferFields.memo},
         cp.{TransferFields.Counterparty.firstName},
         cp.{TransferFields.Counterparty.lastName},
         cp.{TransferFields.Counterparty.nickname},
         cp.{TransferFields.Counterparty.accountNumber},
         cp.{TransferFields.Counterparty.routingNumber},
         cp.{TransferFields.Counterparty.depository},
         cp.{TransferFields.Counterparty.paymentNetwork},
         cp.{TransferFields.Counterparty.status},
         cp.{TransferFields.createdAt},
         a.{AccountFields.name},
         a.{AccountFields.accountNumber} as sender_account_number,
         a.{AccountFields.routingNumber} as sender_routing_number,
         a.{AccountFields.accountId},
         a.{AccountFields.parentAccountId},
         pba.{PartnerBankSqlMapper.Fields.partnerBankAccountId},
         {employeeName} as initiated_by_name
      FROM {Table.domesticTransfer} dt
      JOIN {Table.counterparty} cp using({TransferFields.Domestic.counterpartyId})
      JOIN {Table.transfer} t using({TransferFields.transferId})
      JOIN {AccountSqlMapper.table} a
         ON t.{TransferFields.senderAccountId} = a.{AccountFields.accountId}
      JOIN {PartnerBankSqlMapper.table} pba
         ON a.{AccountFields.parentAccountId} = pba.{PartnerBankSqlMapper.Fields.parentAccountId}
      JOIN {EmployeeSqlMapper.table} employee
         ON t.{TransferFields.initiatedById} = employee.{employeeId}
      """

   let counterparty =
      $"""
      SELECT
         cp.{TransferFields.Counterparty.counterpartyId},
         cp.{TransferFields.Counterparty.orgId},
         cp.{TransferFields.Counterparty.firstName},
         cp.{TransferFields.Counterparty.lastName},
         cp.{TransferFields.Counterparty.nickname},
         cp.{TransferFields.Counterparty.accountNumber},
         cp.{TransferFields.Counterparty.routingNumber},
         cp.{TransferFields.Counterparty.depository},
         cp.{TransferFields.Counterparty.paymentNetwork},
         cp.{TransferFields.Counterparty.status},
         cp.{TransferFields.createdAt}
      FROM {Table.counterparty} cp
      """
