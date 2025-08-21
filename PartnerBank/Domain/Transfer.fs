namespace PartnerBank.Service.Domain

open System

open Lib.SharedTypes
open Lib.ActivePatterns
open Bank.Transfer.Domain

[<RequireQualifiedAccess>]
type DomesticTransferServiceAction =
   | TransferAck
   | ProgressCheck

type DomesticTransferServiceSender = {
   Name: string
   AccountNumber: string
   RoutingNumber: string
}

type DomesticTransferServiceRecipient = {
   Name: string
   AccountNumber: string
   RoutingNumber: string
   Depository: string
}

type PartnerBankDomesticTransfer = {
   Action: DomesticTransferServiceAction
   Transfer: DomesticTransfer
   SagaMetadata: PartnerBankSagaMetadata
}

type PartnerBankDomesticTransferRequest = {
   Action: DomesticTransferServiceAction
   Sender: DomesticTransferSender
   Recipient: DomesticTransferRecipient
   Amount: decimal
   Date: DateTime
   TransactionId: TransferId
   PaymentNetwork: PaymentNetwork
} with

   member x.AsDTO = {|
      Action =
         match x.Action with
         | DomesticTransferServiceAction.TransferAck -> "TransferRequest"
         | DomesticTransferServiceAction.ProgressCheck -> "ProgressCheck"
      Sender = PartnerBankDomesticTransferRequest.networkSender x.Sender
      Recipient =
         PartnerBankDomesticTransferRequest.networkRecipient x.Recipient
      Amount = x.Amount
      Date = x.Date
      TransactionId = string x.TransactionId
      PaymentNetwork =
         match x.Recipient.PaymentNetwork with
         | PaymentNetwork.ACH -> "ach"
   |}

   static member networkSender
      (sender: DomesticTransferSender)
      : DomesticTransferServiceSender
      =
      {
         Name = sender.Name
         AccountNumber = string sender.AccountNumber
         RoutingNumber = string sender.RoutingNumber
      }

   static member networkRecipient
      (recipient: DomesticTransferRecipient)
      : DomesticTransferServiceRecipient
      =
      {
         Name = recipient.Name
         AccountNumber = string recipient.AccountNumber
         RoutingNumber = string recipient.RoutingNumber
         Depository =
            match recipient.Depository with
            | DomesticRecipientAccountDepository.Checking -> "checking"
            | DomesticRecipientAccountDepository.Savings -> "savings"
      }

type private InfraFailReason = DomesticTransferInfraFailReason
type private FailReason = DomesticTransferThirdPartyFailReason

type PartnerBankDomesticTransferResponse = {
   Sender: DomesticTransferServiceSender
   Recipient: DomesticTransferServiceRecipient
   Ok: bool
   Status: string
   ExpectedSettlementDate: DateTime option
   Reason: string
   TransactionId: string
} with

   member x.Progress
      (defaultExpectedSettlementDate: DateTime)
      : DomesticTransferThirdPartyUpdate
      =
      if x.Ok then
         match x.Status with
         | "Complete" -> DomesticTransferThirdPartyUpdate.Settled
         | "ReceivedRequest" ->
            DomesticTransferThirdPartyUpdate.ServiceAckReceived
         | status ->
            let expectedSettlementDate =
               x.ExpectedSettlementDate
               |> Option.defaultValue defaultExpectedSettlementDate

            DomesticTransferThirdPartyUpdate.ProgressDetail {
               Detail = status
               ExpectedSettlementDate = expectedSettlementDate
            }
      else
         x.Reason
         |> PartnerBankDomesticTransferResponse.FailReasonFromErrorResponse
         |> DomesticTransferThirdPartyUpdate.Failed

   member x.NewProgressToSave
      (transfer: DomesticTransfer)
      : DomesticTransferThirdPartyUpdate option
      =
      let fresh = x.Progress transfer.ExpectedSettlementDate

      match transfer.Status with
      | DomesticTransferProgress.WaitingForTransferServiceAck -> Some fresh
      // Don't save a new progress update if there has been no change.
      | DomesticTransferProgress.ThirdParty existingProgressUpdate when
         existingProgressUpdate <> fresh
         ->
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

type PartnerBankSyncTransferBetweenOrgs = {
   Amount: decimal
   From: PartnerBankAccountLink
   To: PartnerBankAccountLink
   SagaMetadata: PartnerBankSagaMetadata
}

type PartnerBankSyncTransferBetweenOrgsResponse = { ConfirmationId: Guid }
