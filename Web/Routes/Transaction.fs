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
open Bank.UserSession.Middleware

/// Parse TransactionQuery-compliant query params from network request and
/// pass along to dotnet minimal api route handler.
let withQueryParams<'t> (func: TransactionQuery -> 't) =
   Func<
      Guid,
      string,
      int,
      string,
      Nullable<bool>,
      string,
      string,
      string,
      Nullable<decimal>,
      Nullable<decimal>,
      string,
      string,
      't
    >
      (fun
           ([<FromRoute>] orgId: Guid)
           ([<FromQuery>] accountIds: string)
           ([<FromQuery>] page: int)
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
         let categories = CategoryFilter.categoryFromQueryString categoryIds

         let categoryOpt =
            if categories.IsSome then
               categories
            else if isCategorized.HasValue then
               Some(CategoryFilter.IsCategorized isCategorized.Value)
            else
               None

         func {
            OrgId = OrgId orgId
            AccountIds = TransactionQuery.accountIdsFromQueryString accountIds
            Page = page
            MoneyFlow = moneyFlowOpt
            Category = categoryOpt
            Amount = AmountFilter.fromQuery amountMin amountMax
            DateRange = dateOpt
            CardIds = TransactionQuery.cardIdsFromQueryString cardIds
            InitiatedByIds =
               TransactionQuery.initiatedByIdsFromQueryString initiatedByIds
            EventType = TransactionGroupFilter.fromQueryString events
         })

let startTransactionRoutes (app: WebApplication) =
   app
      .MapGet(
         TransactionPath.Transactions,
         withQueryParams<Task<IResult>> (
            getTransactions >> RouteUtil.unwrapTaskResultOption
         )
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
            getCorrelatedTransactionConfirmations (CorrelationId correlationId)
            |> RouteUtil.unwrapTaskResultOption)
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
