namespace PartnerBank.Service.Domain

open System
open FsToolkit.ErrorHandling
open Lib.Validators

open Lib.SharedTypes
open Lib.ActivePatterns
open Bank.Transfer.Domain
open Bank.Account.Domain

[<RequireQualifiedAccess>]
type DomesticTransferServiceAction =
   | TransferAck
   | ProgressCheck

type PartnerBankDomesticTransferRequest = {
   Action: DomesticTransferServiceAction
   OriginatingAccountId: PartnerBankAccountId
   Recipient: Counterparty
   Amount: decimal
   PaymentNetwork: PaymentNetwork
   Date: DateTime
   Status: DomesticTransferProgress
   TransferId: TransferId
   SagaMetadata: PartnerBankSagaMetadata
} with

   member x.AsDTO = {|
      idempotency_key = string x.TransferId
      action =
         match x.Action with
         | DomesticTransferServiceAction.TransferAck -> "TransferRequest"
         | DomesticTransferServiceAction.ProgressCheck -> "ProgressCheck"
      data = {|
         originating_account_id = string x.OriginatingAccountId
         recipient = {|
            name = x.Recipient.Name
            account_number = string x.Recipient.AccountNumber
            routing_number = string x.Recipient.RoutingNumber
            depository =
               match x.Recipient.Depository with
               | CounterpartyAccountDepository.Checking -> "checking"
               | CounterpartyAccountDepository.Savings -> "savings"
         |}
         amount = x.Amount
         date = x.Date
         payment_network =
            match x.Recipient.PaymentNetwork with
            | PaymentNetwork.ACH -> "ach"
      |}
   |}

type private InfraFailReason = DomesticTransferInfraFailReason
type private FailReason = DomesticTransferPartnerBankFailReason

type PartnerBankDomesticTransferResponse = {
   Ok: bool
   Status: string
   ExpectedSettlementDate: DateTime
   Reason: string
   TransactionId: string
} with

   member x.Progress: DomesticTransferPartnerBankUpdate =
      if x.Ok then
         match x.Status with
         | "Complete" -> DomesticTransferPartnerBankUpdate.Settled
         | "ReceivedRequest" ->
            DomesticTransferPartnerBankUpdate.ServiceAckReceived
         | status ->
            DomesticTransferPartnerBankUpdate.ProgressDetail {
               Detail = status
               ExpectedSettlementDate = x.ExpectedSettlementDate
            }
      else
         x.Reason
         |> PartnerBankDomesticTransferResponse.FailReasonFromErrorResponse
         |> DomesticTransferPartnerBankUpdate.Failed

   member x.NewProgressToSave
      (existingStatus: DomesticTransferProgress)
      : DomesticTransferPartnerBankUpdate option
      =
      let fresh = x.Progress

      match existingStatus with
      | DomesticTransferProgress.WaitingForTransferServiceAck -> Some fresh
      // Don't save a new progress update if there has been no change.
      | DomesticTransferProgress.PartnerBank existing when existing <> fresh ->
         Some fresh
      | _ -> None

   static member FailReasonFromErrorResponse
      (err: string)
      : DomesticTransferPartnerBankFailReason
      =
      match err with
      | Contains "CorruptData" -> FailReason.Infra InfraFailReason.CorruptData
      | Contains "InvalidAction" ->
         FailReason.Infra InfraFailReason.InvalidAction
      | Contains "InvalidAmount" -> FailReason.InvalidAmount
      | Contains "InvalidAccountInfo" ->
         FailReason.CounterpartyAccountInvalidInfo
      | Contains "InvalidPaymentNetwork" ->
         FailReason.Infra InfraFailReason.InvalidPaymentNetwork
      | Contains "InvalidDepository" ->
         FailReason.Infra InfraFailReason.InvalidDepository
      | Contains "InactiveAccount" -> FailReason.CounterpartyAccountNotActive
      | Contains "NoTransferProcessing" -> FailReason.NoTransferFound
      | e -> FailReason.Infra(InfraFailReason.Unknown e)

type PartnerBankDomesticTransferResponseDTO = {
   ok: bool
   status: string
   expected_settlement_date: DateTime
   reason: string
   idempotency_key: string
} with

   member x.AsEntity = {
      Ok = x.ok
      Status = x.status
      ExpectedSettlementDate = x.expected_settlement_date
      Reason = x.reason
      TransactionId = x.idempotency_key
   }

/// Moves money between accounts in the partner bank instantaneously.
type PartnerBankSyncTransferBetweenOrgs = {
   Amount: decimal
   From: PartnerBankInternalAccountLink
   To: PartnerBankInternalAccountLink
   SagaMetadata: PartnerBankSagaMetadata
} with

   member x.AsDTO = {|
      idempotency_key = string x.SagaMetadata.CorrelationId
      action = "BookTransfer"
      data = {|
         sender_bank_account_id = string x.From.PartnerBankAccountId
         receiver_bank_account_id = string x.To.PartnerBankAccountId
         amount = x.Amount
         currency_code = "USD"
      |}
   |}

[<RequireQualifiedAccess>]
type BookTransferStatus =
   | Rejected
   | Completed

   static member fromString(status: string) : Result<BookTransferStatus, Err> =
      match status with
      | "REJECTED" -> Ok BookTransferStatus.Rejected
      | "COMPLETED" -> Ok BookTransferStatus.Completed
      | _ -> Error(Err.SerializationError $"Invalid status: {status}")

type PartnerBankSyncTransferBetweenOrgsResponse = {
   ConfirmationId: Guid
   IdempotencyKey: CorrelationId
   Amount: decimal
   SenderBankAccountId: PartnerBankAccountId
   RecipientBankAccountId: PartnerBankAccountId
   Status: BookTransferStatus
}

type PartnerBankSyncTransferBetweenOrgsResponseDTO = {
   confirmation_id: string
   idempotency_key: string
   amount: decimal
   sender_bank_account_id: string
   receiver_bank_account_id: string
   status: string
} with

   member x.AsEntity = result {
      let! status = BookTransferStatus.fromString x.status

      let! idempotencyKey =
         parseGuid "idempotency_key" x.idempotency_key
         |> Result.mapError Err.ValidationError

      let! confirmationId =
         parseGuid "confirmation_id" x.confirmation_id
         |> Result.mapError Err.ValidationError

      return {
         ConfirmationId = confirmationId
         IdempotencyKey = CorrelationId idempotencyKey
         Amount = x.amount
         SenderBankAccountId = PartnerBankAccountId x.sender_bank_account_id
         RecipientBankAccountId =
            PartnerBankAccountId x.receiver_bank_account_id
         Status = status
      }
   }
