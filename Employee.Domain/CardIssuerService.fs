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
   ReplyTo: SagaReplyTo
}

type CardIssuerRequest = {
   CardType: CardType
   CardHolderName: string
   Metadata: CardIssuerMetadata
}

[<RequireQualifiedAccess>]
type CardIssuerMessage =
   | CreateCard of CardIssuerRequest

   member x.Metadata =
      match x with
      | CreateCard req -> req.Metadata

type CardCreateResponse = {
   ProviderCardId: ThirdPartyProviderCardId
}

[<RequireQualifiedAccess>]
type CardIssuerResponse = CreateCard of CardCreateResponse
