namespace PartnerBank.Service.Domain

open Lib.SharedTypes
open System.Text.Json.Serialization

type PartnerBankSagaMetadata = {
   OrgId: OrgId
   CorrelationId: CorrelationId
}

type AddressDTO = {
   city: string
   country_code: string
   line_1: string
   line_2: string
   postal_code: string
   state: string
} with

   [<JsonIgnore>]
   member x.AsEntity = {
      City = x.city
      CountryCode = x.country_code
      Line1 = x.line_1
      Line2 = x.line_2
      PostalCode = x.postal_code
      State = x.state
   }

   static member fromAddress(addr: Address) = {
      city = addr.City
      country_code = addr.CountryCode
      line_1 = addr.Line1
      line_2 = addr.Line2
      postal_code = addr.PostalCode
      state = addr.State
   }
