module Bank.Transfer.Api

open System.Threading.Tasks
open FsToolkit.ErrorHandling

open Lib.Postgres
open Lib.SharedTypes
open Bank.Transfer.Domain

open TransferSqlMapper

let getDomesticTransferRecipients
   (orgId: OrgId)
   : Result<Counterparty list option, Err> Task
   =
   pgQuery<Counterparty>
      $"""
      {Query.counterparty}
      WHERE cp.{TransferFields.Counterparty.orgId} = @orgId
      """
      (Some [ "orgId", TransferSqlWriter.Counterparty.orgId orgId ])
      TransferSqlReader.Counterparty.counterparty

let getFailedDomesticTransfersByRecipient
   (recipientAccountId: AccountId)
   : Task<Result<DomesticTransfer list option, Err>>
   =
   let query =
      $"""
      {Query.domesticTransfer}
      WHERE
         {TransferFields.Counterparty.counterpartyId} = @counterpartyId
         AND {TransferFields.Domestic.status} = 'Failed'
      """

   pgQuery<DomesticTransfer>
      query
      (Some [
         "counterpartyId",
         TransferSqlWriter.Counterparty.counterpartyId recipientAccountId
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
         DomesticTransferThirdPartyFailReason.CounterpartyAccountInvalidInfo
         |> DomesticTransferFailReason.ThirdParty
         |> DomesticTransferProgress.Failed

      return transfers |> List.filter (fun t -> t.Status = retryableStatus)
   }
