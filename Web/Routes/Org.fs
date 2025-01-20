module Bank.Org.Routes

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Akka.Actor

open Bank.Org.Domain
open Bank.Org.Api
open Bank.CommandApproval.Api
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

   app.MapGet(
      OrgPath.GetCommandApprovals,
      Func<Guid, Task<IResult>>(fun orgId ->
         getCommandApprovals (OrgId orgId) |> RouteUtil.unwrapTaskResultOption)
   )
   |> ignore

   app.MapGet(
      OrgPath.GetCommandApprovalRules,
      Func<Guid, Task<IResult>>(fun orgId ->
         getApprovalRules (OrgId orgId) |> RouteUtil.unwrapTaskResultOption)
   )
   |> ignore

   app
      .MapPost(
         OrgPath.ConfigureCommandApprovalRule,
         Func<
            ActorSystem,
            CommandApprovalRule.ConfigureApprovalRuleCommand,
            Task<IResult>
          >
            (fun sys cmd ->
               processCommand sys (OrgCommand.ConfigureApprovalRule cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageCommandApprovalRule)
   |> ignore

   app
      .MapPost(
         OrgPath.DeleteCommandApprovalRule,

         Func<
            ActorSystem,
            CommandApprovalRule.DeleteApprovalRuleCommand,
            Task<IResult>
          >
            (fun sys cmd ->
               processCommand sys (OrgCommand.DeleteApprovalRule cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageCommandApprovalRule)
   |> ignore

   app
      .MapPost(
         OrgPath.AcquireCommandApproval,
         Func<
            ActorSystem,
            CommandApprovalProgress.AcquireCommandApproval,
            Task<IResult>
          >
            (fun sys cmd ->
               processCommand sys (OrgCommand.AcquireCommandApproval cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageCommandApprovalProgress)
   |> ignore

   app
      .MapPost(
         OrgPath.DeclineCommandApproval,
         Func<
            ActorSystem,
            CommandApprovalProgress.DeclineCommandApproval,
            Task<IResult>
          >
            (fun sys cmd ->
               processCommand sys (OrgCommand.DeclineCommandApproval cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageCommandApprovalProgress)
   |> ignore
