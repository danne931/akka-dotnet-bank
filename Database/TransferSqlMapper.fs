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
   let domesticRecipient = "transfer_domestic_recipient"

module TransferTypeCast =
   let paymentNetwork = "payment_network"

   let domesticRecipientAccountDepository =
      "domestic_transfer_recipient_account_depository"

   let domesticRecipientStatus = "domestic_transfer_recipient_status"
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
      let recipientAccountId = "recipient_account_id"

   // Specific to domestic_transfer_recipient table
   module DomesticRecipient =
      let recipientAccountId = "recipient_account_id"
      let senderOrgId = "sender_org_id"
      let firstName = "first_name"
      let lastName = "last_name"
      let nickname = "nickname"
      let routingNumber = "routing_number"
      let accountNumber = "account_number"
      let status = "recipient_status"
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

   module DomesticRecipient =
      let recipientAccountId (read: RowReader) =
         TransferFields.DomesticRecipient.recipientAccountId
         |> read.uuid
         |> AccountId

      let senderOrgId (read: RowReader) =
         TransferFields.DomesticRecipient.senderOrgId |> read.uuid |> OrgId

      let firstName (read: RowReader) =
         TransferFields.DomesticRecipient.firstName |> read.string

      let lastName (read: RowReader) =
         TransferFields.DomesticRecipient.lastName |> read.string

      let nickname (read: RowReader) =
         TransferFields.DomesticRecipient.nickname |> read.stringOrNone

      let routingNumber = AccountSqlReader.routingNumber
      let accountNumber = AccountSqlReader.accountNumber

      let status (read: RowReader) =
         TransferFields.DomesticRecipient.status
         |> read.string
         |> RecipientRegistrationStatus.fromStringUnsafe

      let depository (read: RowReader) =
         TransferFields.DomesticRecipient.depository
         |> read.string
         |> DomesticRecipientAccountDepository.fromStringUnsafe

      let paymentNetwork (read: RowReader) =
         TransferFields.DomesticRecipient.paymentNetwork
         |> read.string
         |> PaymentNetwork.fromStringUnsafe

      let recipient (read: RowReader) : DomesticTransferRecipient = {
         FirstName = firstName read
         LastName = lastName read
         Nickname = nickname read
         AccountNumber = accountNumber read
         RoutingNumber = routingNumber read
         Status = status read
         RecipientAccountId = recipientAccountId read
         SenderOrgId = senderOrgId read
         Depository = depository read
         PaymentNetwork = paymentNetwork read
         CreatedAt = createdAt read
      }

   module Domestic =
      let status (read: RowReader) =
         TransferFields.Domestic.statusDetail
         |> read.text
         |> Serialization.deserializeUnsafe<DomesticTransferProgress>

      let recipientAccountId (read: RowReader) =
         TransferFields.Domestic.recipientAccountId |> read.uuid |> AccountId

      let sender (read: RowReader) : DomesticTransferSender = {
         Name = AccountSqlReader.name read
         AccountNumber = read.int64 AccountFields.accountNumber |> AccountNumber
         RoutingNumber = read.int AccountFields.routingNumber |> RoutingNumber
         OrgId = senderOrgId read
         ParentAccountId = Guid.NewGuid() |> ParentAccountId
         AccountId = AccountSqlReader.accountId read
      }

      let transfer (read: RowReader) : DomesticTransfer = {
         TransferId = transferId read
         InitiatedBy = {
            Id = initiatedById read
            Name = read.string "initiated_by_name"
         }
         Memo = memo read
         Status = status read
         Sender = sender read
         Recipient = DomesticRecipient.recipient read
         Amount = amount read
         ScheduledDate = scheduledAt read
      }

module TransferSqlWriter =
   let transferId = TransferId.get >> Sql.uuid
   let initiatedById = InitiatedById.get >> Sql.uuid
   let amount = Sql.decimal
   let transferCategory (cat: TransferCategory) = Sql.string (string cat)
   let scheduledAt (date: DateTime) = Sql.timestamptz date
   let senderOrgId = OrgSqlWriter.orgId
   let senderAccountId = AccountSqlWriter.accountId
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

      let recipientOrgId = OrgSqlWriter.orgId
      let recipientAccountId = AccountSqlWriter.accountId

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

      let recipientOrgId = OrgSqlWriter.orgId
      let recipientAccountId = AccountSqlWriter.accountId

   module Domestic =
      let status =
         function
         | DomesticTransferProgress.Scheduled -> "Scheduled"
         | DomesticTransferProgress.ProcessingSenderAccountDeduction ->
            "ProcessingSenderAccountDeduction"
         | DomesticTransferProgress.WaitingForTransferServiceAck ->
            "WaitingForTransferServiceAck"
         | DomesticTransferProgress.Settled -> "Settled"
         | DomesticTransferProgress.ThirdParty _ -> "ThirdPartyProcessing"
         | DomesticTransferProgress.Failed _ -> "Failed"
         >> Sql.string

      let statusDetail (status: DomesticTransferProgress) =
         status |> Serialization.serialize |> Sql.jsonb

      let recipientAccountId = AccountSqlWriter.accountId

   module DomesticRecipient =
      let recipientAccountId = AccountSqlWriter.accountId
      let senderOrgId = OrgSqlWriter.orgId
      let firstName = Sql.string
      let lastName = Sql.string
      let nickname = Sql.stringOrNone
      let routingNumber = AccountSqlWriter.routingNumber
      let accountNumber = AccountSqlWriter.accountNumber

      let status (status: RecipientRegistrationStatus) =
         Sql.string (string status)

      let depository (status: DomesticRecipientAccountDepository) =
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
         dt.{TransferFields.Domestic.recipientAccountId},
         dt.{TransferFields.Domestic.status},
         dt.{TransferFields.Domestic.statusDetail},
         t.{TransferFields.transferId},
         t.{TransferFields.initiatedById},
         t.{TransferFields.scheduledAt},
         t.{TransferFields.senderOrgId},
         t.{TransferFields.senderAccountId},
         t.{TransferFields.amount},
         t.{TransferFields.memo},
         dr.{TransferFields.DomesticRecipient.firstName},
         dr.{TransferFields.DomesticRecipient.lastName},
         dr.{TransferFields.DomesticRecipient.nickname},
         dr.{TransferFields.DomesticRecipient.accountNumber},
         dr.{TransferFields.DomesticRecipient.routingNumber},
         dr.{TransferFields.DomesticRecipient.depository},
         dr.{TransferFields.DomesticRecipient.paymentNetwork},
         dr.{TransferFields.DomesticRecipient.status},
         dr.{TransferFields.createdAt},
         a.{AccountFields.name},
         a.{AccountFields.accountNumber} as sender_account_number,
         a.{AccountFields.routingNumber} as sender_routing_number,
         a.{AccountFields.accountId},
         {employeeName} as initiated_by_name
      FROM {Table.domesticTransfer} dt
      JOIN {Table.domesticRecipient} dr using({TransferFields.Domestic.recipientAccountId})
      JOIN {Table.transfer} t using({TransferFields.transferId})
      JOIN {AccountSqlMapper.table} a
         ON t.{TransferFields.senderAccountId} = a.{AccountFields.accountId}
      JOIN {EmployeeSqlMapper.table} employee
         ON t.{TransferFields.initiatedById} = employee.{employeeId}
      """

   let domesticTransferRecipient =
      $"""
      SELECT
         dr.{TransferFields.DomesticRecipient.recipientAccountId},
         dr.{TransferFields.DomesticRecipient.senderOrgId},
         dr.{TransferFields.DomesticRecipient.firstName},
         dr.{TransferFields.DomesticRecipient.lastName},
         dr.{TransferFields.DomesticRecipient.nickname},
         dr.{TransferFields.DomesticRecipient.accountNumber},
         dr.{TransferFields.DomesticRecipient.routingNumber},
         dr.{TransferFields.DomesticRecipient.depository},
         dr.{TransferFields.DomesticRecipient.paymentNetwork},
         dr.{TransferFields.DomesticRecipient.status},
         dr.{TransferFields.createdAt}
      FROM {Table.domesticRecipient} dr
      """
