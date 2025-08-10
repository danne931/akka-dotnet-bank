module Bank.Routes.Card

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
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
open BankActorRegistry

let start (app: WebApplication) =
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
         Func<BankActorRegistry, CreateCardCommand, Task<IResult>>
            (fun registry cmd ->
               processCommand registry (EmployeeCommand.CreateCard cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.CreateCard)
   |> ignore

   app
      .MapPost(
         CardPath.Purchase,
         Func<BankActorRegistry, PurchaseIntentCommand, Task<IResult>>
            (fun registry cmd ->
               processCommand registry (EmployeeCommand.PurchaseIntent cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.DebitRequest)
   |> ignore

   app
      .MapPost(
         CardPath.PurchaseLimit,
         Func<
            BankActorRegistry,
            ConfigureRollingPurchaseLimitCommand,
            Task<IResult>
          >
            (fun registry cmd ->
               processCommand
                  registry
                  (EmployeeCommand.ConfigureRollingPurchaseLimit cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.UpdatePurchaseLimit)
   |> ignore

   app
      .MapPost(
         CardPath.LockCard,
         Func<BankActorRegistry, LockCardCommand, Task<IResult>>
            (fun registry cmd ->
               processCommand registry (EmployeeCommand.LockCard cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.LockCard)
   |> ignore

   app
      .MapPost(
         CardPath.UnlockCard,
         Func<BankActorRegistry, UnlockCardCommand, Task<IResult>>
            (fun registry cmd ->
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
                     |> GuaranteedDelivery.message (OrgId.get cmd.OrgId)

                  let registry: IOrgGuaranteedDeliveryActor = registry
                  registry.OrgGuaranteedDeliveryActor() <! msg

                  return res
               }
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.UnlockCard)
   |> ignore

   app
      .MapPost(
         CardPath.UpdateNickname,
         Func<BankActorRegistry, EditCardNicknameCommand, Task<IResult>>
            (fun registry cmd ->
               processCommand registry (EmployeeCommand.EditCardNickname cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.EditCardNickname)
   |> ignore
