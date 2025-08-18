module Bank.Routes.Account

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder

open Bank.Account.Domain
open Bank.Account.Api
open RoutePaths
open Lib.SharedTypes
open Bank.UserSession.Middleware
open BankActorRegistry

let start (app: WebApplication) =
   app
      .MapGet(
         AccountPath.Account,
         Func<Guid, Task<IResult>>(fun id ->
            getAccount (AccountId id) |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.GetVirtualAccount)
   |> ignore

   app
      .MapPost(
         AccountPath.Base,
         Func<BankActorRegistry, CreateVirtualAccountCommand, Task<IResult>>
            (fun registry cmd ->
               processCommand
                  registry
                  (AccountCommand.CreateVirtualAccount cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.CreateVirtualAccount)
   |> ignore

   app
      .MapPost(
         AccountPath.Deposit,
         Func<BankActorRegistry, DepositCashCommand, Task<IResult>>
            (fun registry cmd ->
               processCommand registry (AccountCommand.DepositCash cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.Deposit)
   |> ignore

   app
      .MapPost(
         AccountPath.CloseAccount,
         Func<BankActorRegistry, CloseAccountCommand, Task<IResult>>
            (fun registry cmd ->
               processCommand registry (AccountCommand.CloseAccount cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.CloseAccount)
   |> ignore
