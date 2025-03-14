[<RequireQualifiedAccess>]
module TransactionService

open Fable.SimpleHttp
open Feliz.Router

open Bank.Account.Domain
open SignalRBroadcast
open UIDomain
open UIDomain.Account
open Lib.SharedTypes
open RoutePaths
open Lib.NetworkQuery

let private serviceName = "TransactionService"

let transactionQueryFromAccountBrowserQuery
   (orgId: OrgId)
   (query: AccountBrowserQuery)
   : TransactionQuery
   =
   {
      OrgId = orgId
      AccountIds = query.Accounts |> Option.map (List.map _.AccountId)
      PageLimit = 30
      Cursor = None
      Category = query.Category
      MoneyFlow = query.MoneyFlow
      Amount = query.Amount
      DateRange = query.Date |> Option.map DateFilter.toDateRange
      CardIds = query.SelectedCards |> Option.map (List.map _.CardId)
      InitiatedByIds =
         query.SelectedInitiatedBy
         |> Option.map (List.map (_.Id >> InitiatedById))
      EventType = query.EventType
   }

let transactionQueryParams (query: TransactionQuery) : (string * string) list =
   let queryParams =
      ("pageLimit", string query.PageLimit)
      :: AccountBrowserQuery.toQueryParams {
         Amount = query.Amount
         Category = query.Category
         MoneyFlow = query.MoneyFlow
         EventType = query.EventType
         Accounts = None
         SelectedCards = None
         SelectedInitiatedBy = None
         Date = None
         Action = None
         Transaction = None
      }

   let queryParams =
      match query.Cursor with
      | Some cursor ->
         [
            "cursorTimestamp", DateTime.toISOString cursor.Timestamp
            "cursorTransactionId", string cursor.TransactionId
         ]
         @ queryParams
      | None -> queryParams

   let queryParams =
      match query.AccountIds with
      | None -> queryParams
      | Some accountIds ->
         ("accountIds", listToQueryString accountIds) :: queryParams

   let queryParams =
      match query.CardIds with
      | None -> queryParams
      | Some cardIds -> ("cardIds", listToQueryString cardIds) :: queryParams

   let queryParams =
      match query.InitiatedByIds with
      | None -> queryParams
      | Some ids -> ("initiatedByIds", listToQueryString ids) :: queryParams

   match query.DateRange with
   | None -> queryParams
   | Some(startDate, endDate) ->
      ("date", DateTime.rangeAsQueryString startDate endDate) :: queryParams

open Transaction

let getTransactions (query: TransactionQuery) : Async<TransactionsMaybe> = async {
   let basePath = TransactionPath.transactions query.OrgId

   let queryParams = Router.encodeQueryString <| transactionQueryParams query

   let path = basePath + queryParams

   let! (code, responseText) = Http.get path

   if code = 404 then
      return Ok None
   elif code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return
         responseText
         |> Serialization.deserialize<Transaction.T list>
         |> Result.map Some
}

let getCategories () : Async<Result<Map<int, TransactionCategory>, Err>> = async {
   let! (code, responseText) = Http.get TransactionPath.Categories

   if code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return
         responseText
         |> Serialization.deserialize<TransactionCategory list>
         |> Result.map (List.map (fun o -> o.Id, o) >> Map.ofList)
}

let getTransactionInfo
   (txnId: TransactionId)
   : Async<Result<TransactionWithAncillaryInfo option, Err>>
   =
   async {
      let! (code, responseText) =
         Http.get (TransactionPath.transactionInfo txnId)

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<TransactionWithAncillaryInfo>
            |> Result.map Some
   }

let isEventPersistenceConfirmed
   (correlationId: CorrelationId)
   : Async<Result<bool, Err>>
   =
   async {
      let! (code, responseText) =
         Http.get (TransactionPath.transactionConfirmation correlationId)

      if code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return responseText |> Serialization.deserialize<bool>
   }

let updateCategory
   (txnId: TransactionId)
   (categoryId: int)
   : Async<Result<int, Err>>
   =
   async {
      let! code, responseText =
         Http.post
            (TransactionPath.category txnId categoryId)
            (string categoryId)

      if code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return Serialization.deserialize<int> responseText
   }

let deleteCategory (txnId: TransactionId) : Async<Result<int, Err>> = async {
   let! code, responseText = Http.delete (TransactionPath.categoryDelete txnId)

   if code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return Serialization.deserialize<int> responseText
}

let updateNote (txnId: TransactionId) (note: string) : Async<Result<int, Err>> = async {
   let! code, responseText = Http.post (TransactionPath.note txnId) note

   if code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return Serialization.deserialize<int> responseText
}
