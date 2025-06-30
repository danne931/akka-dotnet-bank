module CardIssuer.Service.Domain

open Lib.SharedTypes
open Bank.Employee.Domain

[<RequireQualifiedAccess>]
type SagaReplyTo =
   | EmployeeOnboard
   | CardSetup

[<RequireQualifiedAccess>]
type CardIssuerMetadata = {
   OrgId: OrgId
   CorrelationId: CorrelationId
}

type CardIssuerCreateCardRequest = {
   CardType: CardType
   CardHolderName: string
   Metadata: CardIssuerMetadata
   ReplyTo: SagaReplyTo
}

type CardIssuerCloseCardRequest = {
   ProviderCardId: ThirdPartyProviderCardId
   Metadata: CardIssuerMetadata
}

[<RequireQualifiedAccess>]
type CardIssuerMessage =
   | CreateCard of CardIssuerCreateCardRequest
   | CloseCard of CardIssuerCloseCardRequest

   member x.Metadata =
      match x with
      | CreateCard req -> req.Metadata
      | CloseCard req -> req.Metadata

type CardCreateResponse = {
   ProviderCardId: ThirdPartyProviderCardId
}

type CardCloseResponse = {
   Customer: objnull
   ProviderCardId: ThirdPartyProviderCardId
}

[<RequireQualifiedAccess>]
type CardIssuerResponse =
   | CreateCard of CardCreateResponse
   | CloseCard of CardCloseResponse
