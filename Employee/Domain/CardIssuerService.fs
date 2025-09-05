module CardIssuer.Service.Domain

open System

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
} with

   member x.AsDTO: obj =
      let dto = {|
         ``type`` = "VIRTUAL"
         exp_year = string x.Expiration.Year
         exp_month = sprintf "%02i" x.Expiration.Month
      |}

      match x.CardNickname with
      | None -> dto
      | Some name -> {| dto with memo = name |}

type CardCreateResponse = {
   ProviderCardId: ThirdPartyProviderCardId
   CardNumberLast4: string
}

type CardCreateResponseDTO = {
   token: Guid
   state: string
   ``type``: string
   last_four: string
   exp_month: string
   exp_year: string
} with

   member x.AsEntity = {
      ProviderCardId = ThirdPartyProviderCardId x.token
      CardNumberLast4 = x.last_four
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

type CardCloseResponse = {
   Customer: obj
   ProviderCardId: ThirdPartyProviderCardId
}

[<RequireQualifiedAccess>]
type CardIssuerResponse =
   | CreateCard of CardCreateResponse
   | CloseCard of CardCloseResponse
