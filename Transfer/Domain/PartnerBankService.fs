module PartnerBank.Service.Domain

open System

open Lib.SharedTypes
open Bank.Transfer.Domain

type PartnerBankMetadata = {
   OrgId: OrgId
   CorrelationId: CorrelationId
}

type PartnerBankAccountLink = {
   AccountNumber: ParentAccountNumber
   RoutingNumber: ParentRoutingNumber
}

type PartnerBankAccountLinking = {
   LegalBusinessName: string
   EmployerIdentificationNumber: string
   Metadata: PartnerBankMetadata
}

type AccountLinkResponse = {
   Accepted: bool
   Link: PartnerBankAccountLink
}

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

type PartnerBankDomesticTransferRequest = {
   Action: DomesticTransferServiceAction
   Transfer: DomesticTransfer
   Metadata: PartnerBankMetadata
}

type PartnerBankDomesticTransferResponse = {
   Sender: DomesticTransferServiceSender
   Recipient: DomesticTransferServiceRecipient
   Ok: bool
   Status: string
   ExpectedSettlementDate: DateTime option
   Reason: string
   TransactionId: string
}

type PartnerBankSyncPurchase = {
   Amount: decimal
   Account: PartnerBankAccountLink
   Metadata: PartnerBankMetadata
}

type PartnerBankSyncPurchaseResponse = { ConfirmationId: Guid }

type PartnerBankSyncTransferBetweenOrgs = {
   Amount: decimal
   From: PartnerBankAccountLink
   To: PartnerBankAccountLink
   Metadata: PartnerBankMetadata
}

type PartnerBankSyncTransferBetweenOrgsResponse = { ConfirmationId: Guid }

[<RequireQualifiedAccess>]
type PartnerBankServiceMessage =
   | LinkAccount of PartnerBankAccountLinking
   | TransferDomestic of PartnerBankDomesticTransferRequest
   | TransferBetweenOrganizations of PartnerBankSyncTransferBetweenOrgs
   | Purchase of PartnerBankSyncPurchase

   member x.Metadata =
      match x with
      | LinkAccount req -> req.Metadata
      | TransferBetweenOrganizations req -> req.Metadata
      | TransferDomestic req -> req.Metadata
      | Purchase req -> req.Metadata

[<RequireQualifiedAccess>]
type PartnerBankResponse =
   | LinkAccount of AccountLinkResponse
   | TransferBetweenOrganizations of PartnerBankSyncTransferBetweenOrgsResponse
   | TransferDomestic of PartnerBankDomesticTransferResponse
   | Purchase of PartnerBankSyncPurchaseResponse
