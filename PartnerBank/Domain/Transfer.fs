namespace PartnerBank.Service.Domain

open System

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
   Recipient: DomesticTransferRecipient
   Amount: decimal
   PaymentNetwork: PaymentNetwork
   Date: DateTime
   Status: DomesticTransferProgress
   TransferId: TransferId
   SagaMetadata: PartnerBankSagaMetadata
} with

   member x.AsDTO = {|
      transaction_id = string x.TransferId
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
               | DomesticRecipientAccountDepository.Checking -> "checking"
               | DomesticRecipientAccountDepository.Savings -> "savings"
         |}
         amount = x.Amount
         date = x.Date
         payment_network =
            match x.Recipient.PaymentNetwork with
            | PaymentNetwork.ACH -> "ach"
      |}
   |}

type private InfraFailReason = DomesticTransferInfraFailReason
type private FailReason = DomesticTransferThirdPartyFailReason

type PartnerBankDomesticTransferResponse = {
   Ok: bool
   Status: string
   ExpectedSettlementDate: DateTime
   Reason: string
   TransactionId: string
} with

   member x.Progress: DomesticTransferThirdPartyUpdate =
      if x.Ok then
         match x.Status with
         | "Complete" -> DomesticTransferThirdPartyUpdate.Settled
         | "ReceivedRequest" ->
            DomesticTransferThirdPartyUpdate.ServiceAckReceived
         | status ->
            DomesticTransferThirdPartyUpdate.ProgressDetail {
               Detail = status
               ExpectedSettlementDate = x.ExpectedSettlementDate
            }
      else
         x.Reason
         |> PartnerBankDomesticTransferResponse.FailReasonFromErrorResponse
         |> DomesticTransferThirdPartyUpdate.Failed

   member x.NewProgressToSave
      (existingStatus: DomesticTransferProgress)
      : DomesticTransferThirdPartyUpdate option
      =
      let fresh = x.Progress

      match existingStatus with
      | DomesticTransferProgress.WaitingForTransferServiceAck -> Some fresh
      // Don't save a new progress update if there has been no change.
      | DomesticTransferProgress.ThirdParty existing when existing <> fresh ->
         Some fresh
      | _ -> None

   static member FailReasonFromErrorResponse
      (err: string)
      : DomesticTransferThirdPartyFailReason
      =
      match err with
      | Contains "CorruptData" -> FailReason.Infra InfraFailReason.CorruptData
      | Contains "InvalidAction" ->
         FailReason.Infra InfraFailReason.InvalidAction
      | Contains "InvalidAmount" -> FailReason.InvalidAmount
      | Contains "InvalidAccountInfo" -> FailReason.RecipientAccountInvalidInfo
      | Contains "InvalidPaymentNetwork" ->
         FailReason.Infra InfraFailReason.InvalidPaymentNetwork
      | Contains "InvalidDepository" ->
         FailReason.Infra InfraFailReason.RecipientAccountInvalidDepository
      | Contains "InactiveAccount" -> FailReason.RecipientAccountInvalidInfo
      | Contains "NoTransferProcessing" -> FailReason.NoTransferFound
      | e -> FailReason.Infra(InfraFailReason.Unknown e)

type PartnerBankDomesticTransferResponseDTO = {
   ok: bool
   status: string
   expected_settlement_date: DateTime
   reason: string
   transaction_id: string
} with

   member x.AsEntity = {
      Ok = x.ok
      Status = x.status
      ExpectedSettlementDate = x.expected_settlement_date
      Reason = x.reason
      TransactionId = x.transaction_id
   }

type PartnerBankSyncTransferBetweenOrgs = {
   Amount: decimal
   From: PartnerBankInternalAccountLink
   To: PartnerBankInternalAccountLink
   SagaMetadata: PartnerBankSagaMetadata
}

type PartnerBankSyncTransferBetweenOrgsResponse = { ConfirmationId: Guid }
