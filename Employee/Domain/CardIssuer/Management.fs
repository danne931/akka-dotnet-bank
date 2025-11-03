namespace CardIssuer.Service.Domain

open Lib.SharedTypes
open Bank.Employee.Domain

[<RequireQualifiedAccess>]
type CardIssuerMetadata = {
   OrgId: OrgId
   CorrelationId: CorrelationId
}

type CardIssuerCreateCardRequest = {
   CardType: CardType
   CardNickname: string option
   Expiration: CardExpiration
   Metadata: CardIssuerMetadata
}

type CardCreateResponse = {
   CardIssuerCardId: CardIssuerCardId
   CardNumberLast4: string
   CardIssuerName: CardIssuerName
}

type CardIssuerCloseCardRequest = {
   CardIssuerCardId: CardIssuerCardId
   Metadata: CardIssuerMetadata
}

type CardGetResponse = {
   CardIssuerCardId: CardIssuerCardId
   NumberLast4: string
   Number: string
   Expiration: CardExpiration
}

[<RequireQualifiedAccess>]
type CardIssuerMessage =
   | CreateCard of CardIssuerCreateCardRequest
   | CloseCard of CardIssuerCloseCardRequest

   member x.Metadata =
      match x with
      | CreateCard req -> req.Metadata
      | CloseCard req -> req.Metadata

type CardCloseResponse = {
   Customer: obj
   CardIssuerCardId: CardIssuerCardId
}

type SimulatePurchaseRequest = {
   Amount: decimal
   Descriptor: string
   CardNumber: string
   MerchantCurrency: Currency
   Metadata: CardIssuerMetadata
}

type SimulatePurchaseResponse = {
   CardIssuerTransactionId: CardIssuerTransactionId
}

[<RequireQualifiedAccess>]
type CardIssuerResponse =
   | CreateCard of CardCreateResponse
   | CloseCard of CardCloseResponse
