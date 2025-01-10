[<RequireQualifiedAccess>]
module OrgService

open Fable.SimpleHttp
open FsToolkit.ErrorHandling

open Bank.Org.Domain
open Lib.SharedTypes
open RoutePaths

let private serviceName = "OrgService"

let getOrgAndAccountProfiles
   (orgId: OrgId)
   : Async<Result<OrgWithAccountProfiles option, Err>>
   =
   async {
      let path = OrgPath.get orgId

      let! (code, responseText) = Http.get path

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<OrgWithAccountProfiles>
            |> Result.map Some
   }

let searchOrgTransferSocialDiscovery
   (orgId: OrgId)
   (nameQuery: string)
   : Async<Result<Org list option, Err>>
   =
   async {
      let path = OrgPath.search orgId nameQuery

      let! (code, responseText) = Http.get path

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<Org list>
            |> Result.map Some
   }

let getMerchants (orgId: OrgId) : Async<Result<Map<string, Merchant>, Err>> = async {
   let! (code, responseText) = Http.get <| OrgPath.merchants orgId

   if code = 404 then
      return Ok Map.empty
   elif code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return
         responseText
         |> Serialization.deserialize<Merchant list>
         |> Result.map (List.map (fun o -> o.Name, o) >> Map.ofList)
}

let updateMerchant (merchant: Merchant) : Async<Result<int, Err>> = async {
   let! res =
      Http.postJson
         (OrgPath.merchants merchant.OrgId)
         (Serialization.serialize merchant)

   let code = res.statusCode

   if code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return Serialization.deserialize<int> res.responseText
}
