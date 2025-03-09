module Bank.Transaction.Routes

open System
open System.IO
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder

open Bank.Account.Domain
open Bank.Transaction.Api
open RoutePaths
open Lib.NetworkQuery
open Lib.SharedTypes
open Lib.Time
open Bank.UserSession.Middleware

let startTransactionRoutes (app: WebApplication) =
   app
      .MapGet(
         TransactionPath.Transactions,
         Func<
            Guid,
            string,
            Nullable<int>,
            string,
            Nullable<Guid>,
            string,
            Nullable<bool>,
            string,
            string,
            string,
            Nullable<decimal>,
            Nullable<decimal>,
            string,
            string,
            Task<IResult>
          >
            (fun
                 ([<FromRoute>] orgId: Guid)
                 ([<FromQuery>] accountIds: string)
                 ([<FromQuery>] pageLimit: Nullable<int>)
                 ([<FromQuery>] cursorTimestamp: string)
                 ([<FromQuery>] cursorTransactionId: Nullable<Guid>)
                 ([<FromQuery>] moneyFlow: string)
                 ([<FromQuery>] isCategorized: Nullable<bool>)
                 ([<FromQuery>] categoryIds: string)
                 ([<FromQuery>] cardIds: string)
                 ([<FromQuery>] initiatedByIds: string)
                 ([<FromQuery>] amountMin: Nullable<decimal>)
                 ([<FromQuery>] amountMax: Nullable<decimal>)
                 ([<FromQuery>] events: string)
                 ([<FromQuery>] date: string) ->
               let moneyFlowOpt = MoneyFlow.fromString moneyFlow
               let dateOpt = dateRangeFromQueryString date

               let categories =
                  CategoryFilter.categoryFromQueryString categoryIds

               let categoryOpt =
                  if categories.IsSome then
                     categories
                  else if isCategorized.HasValue then
                     Some(CategoryFilter.IsCategorized isCategorized.Value)
                  else
                     None

               let query = {
                  OrgId = OrgId orgId
                  AccountIds =
                     TransactionQuery.accountIdsFromQueryString accountIds
                  PageLimit =
                     if pageLimit.HasValue then pageLimit.Value else 30
                  Cursor =
                     match
                        cursorTransactionId.HasValue,
                        DateTime.parseOptional cursorTimestamp
                     with
                     | true, Some ts ->
                        Some {
                           TransactionId =
                              cursorTransactionId.Value
                              |> CorrelationId
                              |> TransactionId
                           Timestamp = ts
                        }
                     | _ -> None
                  MoneyFlow = moneyFlowOpt
                  Category = categoryOpt
                  Amount = AmountFilter.fromQuery amountMin amountMax
                  DateRange = dateOpt
                  CardIds = TransactionQuery.cardIdsFromQueryString cardIds
                  InitiatedByIds =
                     TransactionQuery.initiatedByIdsFromQueryString
                        initiatedByIds
                  EventType = TransactionGroupFilter.fromQueryString events
               }

               getTransactions query |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.GetTransactions)
   |> ignore

   app
      .MapGet(
         TransactionPath.Categories,
         Func<Task<IResult>>(getCategories >> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.GetCategories)
   |> ignore

   app
      .MapGet(
         TransactionPath.TransactionInfo,
         Func<Guid, Task<IResult>>(fun txnId ->
            getTransactionInfo (TransactionId(CorrelationId txnId))
            |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.GetTransactionInfo)
   |> ignore

   app
      .MapGet(
         TransactionPath.TransactionConfirmation,
         Func<Guid, Task<IResult>>(fun correlationId ->
            isEventPersistenceConfirmed (CorrelationId correlationId)
            |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.GetTransactionInfo)
   |> ignore

   app
      .MapPost(
         TransactionPath.Category,
         Func<Guid, int, Task<IResult>>(fun txnId categoryId ->
            upsertTransactionCategory
               (TransactionId(CorrelationId txnId))
               categoryId
            |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageTransactionCategory)
   |> ignore

   app
      .MapDelete(
         TransactionPath.CategoryDelete,
         Func<Guid, Task<IResult>>(fun txnId ->
            deleteTransactionCategory (TransactionId(CorrelationId txnId))
            |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageTransactionCategory)
   |> ignore

   app
      .MapPost(
         TransactionPath.Note,
         Func<Guid, Stream, Task<IResult>>(fun txnId body -> task {
            try
               use reader = new StreamReader(body)
               let! note = reader.ReadToEndAsync()

               return!
                  upsertTransactionNote
                     (TransactionId(CorrelationId txnId))
                     note
                  |> RouteUtil.unwrapTaskResult
            with e ->
               return Results.Problem e.Message
         })
      )
      .RBAC(Permissions.ManageTransactionNotes)
   |> ignore
