namespace PartnerBank.Service.Domain

open System

open Lib.SharedTypes

type PartnerBankAccountLink = {
   AccountNumber: ParentAccountNumber
   RoutingNumber: ParentRoutingNumber
}

type PartnerBankAccountLinking = {
   LegalBusinessName: string
   EmployerIdentificationNumber: string
   SagaMetadata: PartnerBankSagaMetadata
}

type AccountLinkResponse = {
   Accepted: bool
   Link: PartnerBankAccountLink
}

// TODO: Remove - will be handled by Lithic
type PartnerBankSyncPurchase = {
   Amount: decimal
   Account: PartnerBankAccountLink
   SagaMetadata: PartnerBankSagaMetadata
}

type PartnerBankSyncPurchaseResponse = { ConfirmationId: Guid }
