module Bank.Transfer.Api

open System.Threading.Tasks
open FsToolkit.ErrorHandling

open Lib.Postgres
open Lib.SharedTypes
open Bank.Transfer.Domain

open TransferSqlMapper
open AccountSqlMapper
open CounterpartySqlMapper

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
         dt.{TransferFields.Domestic.counterpartyId},
         t.{TransferFields.transferId},
         t.{TransferFields.initiatedById},
         t.{TransferFields.scheduledAt},
         t.{TransferFields.senderOrgId},
         t.{TransferFields.senderAccountId},
         t.{TransferFields.amount},
         t.{TransferFields.memo},
         cp.{CounterpartyFields.firstName},
         cp.{CounterpartyFields.lastName},
         cp.{CounterpartyFields.nickname},
         cp.{CounterpartyFields.accountNumber},
         cp.{CounterpartyFields.routingNumber},
         cp.{CounterpartyFields.depository},
         cp.{CounterpartyFields.paymentNetwork},
         cp.{CounterpartyFields.address},
         cp.{CounterpartyFields.partnerBankCounterpartyId},
         cp.{CounterpartyFields.counterpartyKind},
         created_at,
         a.{AccountFields.name},
         a.{AccountFields.accountNumber} as sender_account_number,
         a.{AccountFields.routingNumber} as sender_routing_number,
         a.{AccountFields.accountId},
         a.{AccountFields.parentAccountId},
         pba.{PartnerBankSqlMapper.Fields.partnerBankAccountId},
         {employeeName} as initiated_by_name
      FROM {Table.domesticTransfer} dt
      JOIN {CounterpartySqlMapper.table} cp using({TransferFields.Domestic.counterpartyId})
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
         cp.{CounterpartyFields.counterpartyId},
         cp.{CounterpartyFields.counterpartyKind},
         cp.{CounterpartyFields.partnerBankCounterpartyId},
         cp.{CounterpartyFields.orgId},
         cp.{CounterpartyFields.firstName},
         cp.{CounterpartyFields.lastName},
         cp.{CounterpartyFields.nickname},
         cp.{CounterpartyFields.accountNumber},
         cp.{CounterpartyFields.routingNumber},
         cp.{CounterpartyFields.depository},
         cp.{CounterpartyFields.paymentNetwork},
         cp.{CounterpartyFields.address},
         cp.created_at
      FROM {table} cp
      """

let getDomesticTransferRecipients
   (orgId: OrgId)
   : Result<Counterparty list option, Err> Task
   =
   pgQuery<Counterparty>
      $"""
      {Query.counterparty}
      WHERE cp.{CounterpartyFields.orgId} = @orgId
      """
      (Some [ "orgId", CounterpartyWriter.orgId orgId ])
      CounterpartyReader.counterparty

let getFailedDomesticTransfersByCounterparty
   (counterpartyId: CounterpartyId)
   : Task<Result<DomesticTransfer list option, Err>>
   =
   let query =
      $"""
      {Query.domesticTransfer}
      WHERE
         {CounterpartyFields.counterpartyId} = @counterpartyId
         AND {TransferFields.Domestic.status} = 'Failed'
      """

   pgQuery<DomesticTransfer>
      query
      (Some [
         "counterpartyId", CounterpartyWriter.counterpartyId counterpartyId
      ])
      TransferSqlReader.Domestic.transfer

// Failed domestic transfers will be retried upon editing the counterparty.
let getDomesticTransfersRetryableUponCounterpartyCorrection
   (counterpartyId: CounterpartyId)
   : Task<Result<DomesticTransfer list option, Err>>
   =
   taskResultOption {
      let! transfers = getFailedDomesticTransfersByCounterparty counterpartyId

      let retryableStatus =
         DomesticTransferPartnerBankFailReason.CounterpartyAccountInvalidInfo
         |> DomesticTransferFailReason.PartnerBank
         |> DomesticTransferProgress.Failed

      return transfers |> List.filter (fun t -> t.Status = retryableStatus)
   }
