module Bank.Routes.Diagnostic

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Akkling
open FSharp.Control

open Lib.SharedTypes
open Bank.Account.Domain
open RoutePaths
open Bank.UserSession.Middleware
open Lib.CircuitBreaker
open BankActorRegistry
open SagaDTO
open Lib.NetworkQuery
open Lib.Time

let start (app: WebApplication) =
   app
      .MapGet(
         DiagnosticPath.Account,
         Func<BankActorRegistry, Guid, Task<IResult>>(fun registry id ->
            let ref =
               (registry :> IAccountActor).AccountActor(ParentAccountId id)

            let askTask: ParentAccountSnapshot option Task =
               ref.Ask(
                  AccountMessage.GetAccount,
                  Some(TimeSpan.FromSeconds 3.)
               )
               |> Async.toTask

            RouteUtil.unwrapTaskOption askTask)
      )
      .RBAC(Permissions.Diagnostic)
   |> ignore

   app
      .MapGet(
         DiagnosticPath.CircuitBreaker,
         Func<BankActorRegistry, Task<IResult>>(fun registry ->
            let ref = (registry :> ICircuitBreakerActor).CircuitBreakerActor()

            let lookup: CircuitBreakerState Task =
               ref <? CircuitBreakerMessage.Lookup |> Async.toTask

            lookup |> RouteUtil.unwrapTask)
      )
      .RBAC(Permissions.Diagnostic)
   |> ignore

   app
      .MapGet(
         DiagnosticPath.Sagas,
         Func<
            Guid,
            string,
            string,
            string,
            Nullable<int>,
            Nullable<Guid>,
            string,
            Task<IResult>
          >
            (fun
                 orgId
                 ([<FromQuery>] date)
                 ([<FromQuery>] status)
                 ([<FromQuery>] kind)
                 ([<FromQuery>] pageLimit)
                 ([<FromQuery>] cursorSagaId)
                 ([<FromQuery>] cursorCreatedAt) ->
               let query = {
                  DateRange = dateRangeFromQueryString date
                  Status = SagaDTOStatus.fromQueryString status
                  SagaKind = SagaKind.fromQueryString kind
                  PageLimit =
                     if pageLimit.HasValue then pageLimit.Value else 50
                  Cursor =
                     match
                        cursorSagaId.HasValue,
                        DateTime.parseOptional cursorCreatedAt
                     with
                     | true, Some ts ->
                        Some {
                           CreatedAt = ts
                           SagaId = CorrelationId cursorSagaId.Value
                        }
                     | _ -> None
               }

               SagaApi.getAllSagas (OrgId orgId) query
               |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.Diagnostic)
   |> ignore


   app
      .MapPost(
         DiagnosticPath.RetrySagaActivity,
         Func<BankActorRegistry, Guid, Guid, string, Task<IResult>>
            (fun registry orgId sagaId activity -> task {
               let activity =
                  SagaDTO.ActivityRecoverableByHumanInTheLoop.fromString
                     activity

               match activity with
               | None -> return Results.BadRequest "Unknown Activity"
               | Some activity ->
                  let corrId = CorrelationId sagaId
                  let aref = (registry :> ISagaActor).SagaActor corrId

                  match activity with
                  | ActivityRecoverableByHumanInTheLoop.DomesticTransferServiceDevelopmentFix ->
                     let evt =
                        DomesticTransferSaga.DomesticTransferSagaEvent.RetryTransferServiceRequest
                           None

                     let msg =
                        AppSaga.Message.domesticTransfer
                           (OrgId orgId)
                           corrId
                           evt

                     aref <! msg

                  return Results.Ok()
            })
      )
      .RBAC(Permissions.Diagnostic)
   |> ignore
