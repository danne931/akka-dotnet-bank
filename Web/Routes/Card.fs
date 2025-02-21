module Bank.Card.Routes

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Akka.Actor
open Akkling
open FsToolkit.ErrorHandling

open Bank.Org.Domain
open Bank.Employee.Domain
open Bank.Employee.Api
open CommandApproval
open RoutePaths
open Lib.SharedTypes
open Bank.UserSession.Middleware
open Lib.NetworkQuery

let startCardRoutes (app: WebApplication) =
   app.MapGet(
      CardPath.Get,
      Func<
         Guid,
         string,
         Nullable<decimal>,
         Nullable<decimal>,
         string,
         string,
         Task<IResult>
       >
         (fun
              orgId
              ([<FromQuery>] createdAt)
              ([<FromQuery>] amountMin)
              ([<FromQuery>] amountMax)
              ([<FromQuery>] accountIds)
              ([<FromQuery>] employeeIds) ->
            let query = {
               Amount = AmountFilter.fromQuery amountMin amountMax
               CreatedAtDateRange = dateRangeFromQueryString createdAt
               AccountIds = CardQuery.accountIdsFromQueryString accountIds
               EmployeeIds = CardQuery.employeeIdsFromQueryString employeeIds
            }

            getCards (OrgId orgId) query |> RouteUtil.unwrapTaskResultOption)
   )
   |> ignore

   app
      .MapPost(
         CardPath.Base,
         Func<ActorSystem, CreateCardCommand, Task<IResult>>(fun sys cmd ->
            processCommand sys (EmployeeCommand.CreateCard cmd)
            |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.CreateCard)
   |> ignore

   app
      .MapPost(
         CardPath.Purchase,
         Func<ActorSystem, PurchasePendingCommand, Task<IResult>>(fun sys cmd ->
            processCommand sys (EmployeeCommand.PurchasePending cmd)
            |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.DebitRequest)
   |> ignore

   app
      .MapPost(
         CardPath.DailyPurchaseLimit,
         Func<ActorSystem, LimitDailyDebitsCommand, Task<IResult>>
            (fun sys cmd ->
               processCommand sys (EmployeeCommand.LimitDailyDebits cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.UpdatePurchaseLimit)
   |> ignore

   app
      .MapPost(
         CardPath.MonthlyPurchaseLimit,
         Func<ActorSystem, LimitMonthlyDebitsCommand, Task<IResult>>
            (fun sys cmd ->
               processCommand sys (EmployeeCommand.LimitMonthlyDebits cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.UpdatePurchaseLimit)
   |> ignore

   app
      .MapPost(
         CardPath.LockCard,
         Func<ActorSystem, LockCardCommand, Task<IResult>>(fun sys cmd ->
            processCommand sys (EmployeeCommand.LockCard cmd)
            |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.LockCard)
   |> ignore

   app
      .MapPost(
         CardPath.UnlockCard,
         Func<ActorSystem, UnlockCardCommand, Task<IResult>>(fun sys cmd ->
            taskResult {
               let validation =
                  cmd
                  |> UnlockCardCommand.toEvent
                  |> Result.map EmployeeEnvelope.get

               let! res = validation |> Result.mapError Err.ValidationError

               let msg =
                  cmd
                  |> UnlockCard
                  |> ApprovableCommand.PerCommand
                  |> OrgMessage.ApprovableRequest

               (OrgActor.get sys cmd.OrgId) <! msg
               return res
            }
            |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.UnlockCard)
   |> ignore

   app
      .MapPost(
         CardPath.UpdateNickname,
         Func<ActorSystem, EditCardNicknameCommand, Task<IResult>>
            (fun sys cmd ->
               processCommand sys (EmployeeCommand.EditCardNickname cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.EditCardNickname)
   |> ignore
