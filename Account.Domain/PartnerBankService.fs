module PartnerBank.Service.Domain

open System

open Bank.Account.Domain
open Lib.SharedTypes

(*
[<RequireQualifiedAccess>]
type PartnerBankServiceFailReason =
   | CorruptData
   | InvalidAction
   | InvalidPaymentNetwork
   | InvalidDepository
   | InvalidAmount
   | AccountClosed
   | InvalidAccountInfo
   | Unknown of string
type DomesticTransferServiceResponse = {
   Sender: DomesticTransferServiceSender
   Recipient: DomesticTransferServiceRecipient
   Ok: bool
   Status: string
   Reason: string
   TransactionId: string
}
*)

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

type PartnerBankSyncPurchase = {
   Amount: decimal
   Account: PartnerBankAccountLink
   Metadata: PartnerBankMetadata
}

[<RequireQualifiedAccess>]
type TransferSagaReplyTo =
   | PlatformTransfer
   | PlatformPayment

type PartnerBankSyncTransferBetweenOrgs = {
   Amount: decimal
   From: PartnerBankAccountLink
   To: PartnerBankAccountLink
   Metadata: PartnerBankMetadata
   ReplyTo: TransferSagaReplyTo
}

[<RequireQualifiedAccess>]
type PartnerBankServiceMessage =
   | LinkAccount of PartnerBankAccountLinking
   | TransferBetweenOrganizations of PartnerBankSyncTransferBetweenOrgs
   | Purchase of PartnerBankSyncPurchase

   member x.Metadata =
      match x with
      | LinkAccount req -> req.Metadata
      | TransferBetweenOrganizations req -> req.Metadata
      | Purchase req -> req.Metadata

type AccountLinkResponse = {
   Accepted: bool
   Link: PartnerBankAccountLink
}

type PartnerBankSyncTransferBetweenOrgsResponse = { ConfirmationId: Guid }

type PartnerBankSyncPurchaseResponse = { ConfirmationId: Guid }

[<RequireQualifiedAccess>]
type PartnerBankResponse =
   | LinkAccount of AccountLinkResponse
   | TransferBetweenOrganizations of PartnerBankSyncTransferBetweenOrgsResponse
   | Purchase of PartnerBankSyncPurchaseResponse
