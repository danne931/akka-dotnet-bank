module Bank.Routes.Billing

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder

open Bank.BillingCycle.Api
open RoutePaths
open Lib.SharedTypes
open Bank.UserSession.Middleware

let start (app: WebApplication) =
   app
      .MapGet(
         BillingPath.BillingStatement,
         Func<Guid, int, Task<IResult>>(fun accountId page ->
            getBillingTransactions (AccountId accountId) page
            |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.BillingStatement)
   |> ignore
