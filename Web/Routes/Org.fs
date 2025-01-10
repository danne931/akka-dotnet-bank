module Bank.Org.Routes

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder

open Bank.Org.Domain
open Bank.Org.Api
open RoutePaths
open Lib.SharedTypes
open Bank.UserSession.Middleware

let startOrgRoutes (app: WebApplication) =
   app
      .MapGet(
         OrgPath.Get,
         Func<Guid, Task<IResult>>(fun orgId ->
            getOrgAndAccountProfiles (OrgId orgId)
            |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.GetOrgAndAccountProfiles)
   |> ignore

   app
      .MapGet(
         OrgPath.Search,
         Func<Guid, string, Task<IResult>>(fun orgId searchQuery ->
            searchOrgTransferSocialDiscovery (OrgId orgId) searchQuery
            |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.GetOrgAndAccountProfiles)
   |> ignore

   app
      .MapGet(
         OrgPath.Merchants,
         Func<Guid, Task<IResult>>(fun orgId ->
            getMerchants (OrgId orgId) |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.GetMerchants)
   |> ignore

   app
      .MapPost(
         OrgPath.Merchants,
         Func<Merchant, Task<IResult>>(fun merchant ->
            upsertMerchant merchant |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageMerchants)
   |> ignore
