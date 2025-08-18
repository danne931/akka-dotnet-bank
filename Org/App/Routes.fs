module Bank.Routes.Org

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder

open Bank.Org.Domain
open CommandApproval
open Bank.Org.Api
open Bank.CommandApproval.Api
open RoutePaths
open Lib.SharedTypes
open Bank.UserSession.Middleware
open BankActorRegistry

let start (app: WebApplication) getDomesticTransferRecipients =
   app
      .MapGet(
         OrgPath.Get,
         Func<Guid, Task<IResult>>(fun orgId ->
            getOrgAndAccountProfiles
               (OrgId orgId)
               getDomesticTransferRecipients
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
            BankActorRegistry,
            CommandApprovalRule.ConfigureApprovalRuleCommand,
            Task<IResult>
          >
            (fun registry cmd ->
               processCommand registry (OrgCommand.ConfigureApprovalRule cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageCommandApprovalRule)
   |> ignore

   app
      .MapPost(
         OrgPath.DeleteCommandApprovalRule,

         Func<
            BankActorRegistry,
            CommandApprovalRule.DeleteApprovalRuleCommand,
            Task<IResult>
          >
            (fun registry cmd ->
               processCommand registry (OrgCommand.DeleteApprovalRule cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageCommandApprovalRule)
   |> ignore

   app
      .MapPost(
         OrgPath.RequestCommandApproval,
         Func<
            BankActorRegistry,
            CommandApprovalProgress.RequestCommandApproval,
            Task<IResult>
          >
            (fun registry cmd ->
               processCommand registry (OrgCommand.RequestCommandApproval cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageCommandApprovalProgress)
   |> ignore

   app
      .MapPost(
         OrgPath.AcquireCommandApproval,
         Func<
            BankActorRegistry,
            CommandApprovalProgress.AcquireCommandApproval,
            Task<IResult>
          >
            (fun registry cmd ->
               processCommand registry (OrgCommand.AcquireCommandApproval cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageCommandApprovalProgress)
   |> ignore

   app
      .MapPost(
         OrgPath.DeclineCommandApproval,
         Func<
            BankActorRegistry,
            CommandApprovalProgress.DeclineCommandApproval,
            Task<IResult>
          >
            (fun registry cmd ->
               processCommand registry (OrgCommand.DeclineCommandApproval cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageCommandApprovalProgress)
   |> ignore

   app.MapGet(
      OrgPath.CommandApprovalDailyAccrual,
      Func<BankActorRegistry, Guid, Guid, Task<IResult>>
         (fun registry orgId initiatedById ->
            getTodaysCommandApprovalDailyAccrualByInitiatedBy
               registry
               (OrgId orgId)
               (InitiatedById(EmployeeId initiatedById))
            |> RouteUtil.unwrapTask)
   )
   |> ignore
