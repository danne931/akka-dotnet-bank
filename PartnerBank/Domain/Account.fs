namespace PartnerBank.Service.Domain

open System
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Bank.Account.Domain

type InternalAccountCreateRequest = {
   LegalEntityId: PartnerBankLegalEntityId
   AccountName: string
   SagaMetadata: PartnerBankSagaMetadata
} with

   member x.AsDTO = {|
      legal_entity_id = x.LegalEntityId
      description = x.AccountName
   |}

type InternalAccountCreateResponse = PartnerBankInternalAccountLink

type InternalAccountCreateResponseDTO = {
   account_number: string
   routing_number: string
   account_id: string
} with

   member x.AsEntity = result {
      let preface = "Partner Bank Internal Account"

      let! accountNumber =
         AccountNumber.fromString $"{preface} Account number" x.account_number
         |> Result.mapError Err.ValidationError

      let! routingNumber =
         RoutingNumber.fromString $"{preface} Routing number" x.routing_number
         |> Result.mapError Err.ValidationError

      return {
         AccountNumber = PartnerBankAccountNumber accountNumber
         RoutingNumber = PartnerBankRoutingNumber routingNumber
         PartnerBankAccountId = PartnerBankAccountId x.account_id
      }
   }

// TODO: Remove - will be handled by Lithic
type SyncPurchase = {
   Amount: decimal
   Account: PartnerBankInternalAccountLink
   SagaMetadata: PartnerBankSagaMetadata
}

type SyncPurchaseResponse = { ConfirmationId: Guid }
