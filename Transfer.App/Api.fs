module Bank.Transfer.Api

open System.Threading.Tasks
open FsToolkit.ErrorHandling

open Lib.Postgres
open Lib.SharedTypes
open Bank.Transfer.Domain
open PaymentSqlMapper

module Reader = PaymentSqlReader

let getPayments (orgId: OrgId) : Result<PaymentSummary option, Err> Task = taskResultOption {
   let query =
      $"""
      SELECT
         {Table.payment}.*,
         payeeOrg.{OrganizationSqlMapper.OrgFields.name} as payee_org_name,
         payerOrg.{OrganizationSqlMapper.OrgFields.name} as payer_org_name,
         {Table.platformPayment}.{PaymentFields.Platform.status},
         {PaymentFields.Platform.payerOrgId},
         {PaymentFields.Platform.payByAccount}
      FROM {Table.payment} 
      JOIN {Table.platformPayment} using({PaymentFields.paymentId})
      JOIN {OrganizationSqlMapper.table} payeeOrg ON payeeOrg.org_id = {PaymentFields.payeeOrgId}
      JOIN {OrganizationSqlMapper.table} payerOrg ON payerOrg.org_id = {PaymentFields.Platform.payerOrgId}
      """

   let query =
      $"""
      SELECT * FROM ({query})
      WHERE {PaymentFields.payeeOrgId} = @payeeOrgId

      UNION ALL

      SELECT * FROM ({query})
      WHERE {PaymentFields.Platform.payerOrgId} = @payeeOrgId
      """

   let! payments =
      pgQuery<PlatformPayment option>
         query
         (Some [ "payeeOrgId", PaymentSqlWriter.payeeOrgId orgId ])
         (fun read ->
            let baseInfo = {
               Id = Reader.paymentId read
               InitiatedBy = Reader.initiatedById read
               Amount = Reader.amount read
               Type = Reader.paymentType read
               Payee = {
                  OrgId = Reader.payeeOrgId read
                  OrgName = read.string "payee_org_name"
                  AccountId = Reader.payeeAccountId read
               }
               CreatedAt = Reader.createdAt read
               Expiration = Reader.expiration read
               Memo = Reader.memo read
            }

            match baseInfo.Type with
            | PaymentType.Platform ->
               Some {
                  BaseInfo = baseInfo
                  Status = Reader.Platform.status read
                  Payer = {
                     OrgName = read.string "payer_org_name"
                     OrgId = Reader.Platform.payerOrgId read
                  }
                  PaidBy =
                     Reader.Platform.payByAccount read
                     |> Option.map PaymentMethod.Platform
               }
            | PaymentType.ThirdParty -> None) // Not implemented yet

   let paymentSummary =
      payments
      |> List.choose id
      |> List.fold
         (fun acc payment ->
            if payment.BaseInfo.Payee.OrgId = orgId then
               {
                  acc with
                     OutgoingRequests =
                        (Payment.Platform payment) :: acc.OutgoingRequests
               }
            else
               {
                  acc with
                     IncomingRequests = payment :: acc.IncomingRequests
               })
         {
            IncomingRequests = []
            OutgoingRequests = []
         }

   return paymentSummary
}

open TransferSqlMapper

let getFailedDomesticTransfersByRecipient
   (recipientAccountId: AccountId)
   : Task<Result<DomesticTransfer list option, Err>>
   =
   let query =
      $"""
      {Query.domesticTransfer}
      WHERE
         {TransferFields.Domestic.recipientAccountId} = @recipientId
         AND {TransferFields.Domestic.status} = 'Failed'
      """

   pgQuery<DomesticTransfer>
      query
      (Some [
         "recipientId",
         TransferSqlWriter.DomesticRecipient.recipientAccountId
            recipientAccountId
      ])
      TransferSqlReader.Domestic.transfer

// Get domestic transfers which may be retried if recipient
// (with InvalidAccountInfo status) info edited via
// EditDomesticTransferRecipient command.
// Any recipient with InvalidAccountInfo status will have their failed domestic
// transfers retried upon editing the recipient data.
let getDomesticTransfersRetryableUponRecipientCorrection
   (recipientAccountId: AccountId)
   : Task<Result<DomesticTransfer list option, Err>>
   =
   taskResultOption {
      let! transfers = getFailedDomesticTransfersByRecipient recipientAccountId

      let retryableStatus =
         DomesticTransferFailReason.InvalidAccountInfo
         |> DomesticTransferProgress.Failed

      return transfers |> List.filter (fun t -> t.Status = retryableStatus)
   }
