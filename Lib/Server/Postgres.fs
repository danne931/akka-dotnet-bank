module Lib.Postgres

open System.Threading.Tasks
open Npgsql.FSharp
open FsToolkit.ErrorHandling

open Lib.SharedTypes

let private connString = Env.config.ConnectionStrings.Postgres

type SqlParameter = string * SqlValue
type SqlTransactionStatement = string * SqlParameter list list

let pgQuerySingle<'t>
   (query: string)
   (parameters: SqlParameter list option)
   (mapper: RowReader -> 't)
   : Result<'t option, Err> Task
   =
   connString
   |> Sql.connect
   |> Sql.query query
   |> Sql.parameters (Option.defaultValue [] parameters)
   |> Sql.executeRowAsync mapper
   |> Task.map Some
   |> TaskResult.ofTask
   |> TaskResult.catch id
   |> TaskResult.orElseWith (fun e ->
      if e.Message.Contains("NoResultsException") then
         Task.FromResult(Ok None)
      else
         Task.FromResult(Error(DatabaseError e)))

let pgQuery<'t>
   (query: string)
   (parameters: SqlParameter list option)
   (mapper: RowReader -> 't)
   : Result<'t list option, Err> Task
   =
   connString
   |> Sql.connect
   |> Sql.query query
   |> Sql.parameters (Option.defaultValue [] parameters)
   |> Sql.executeAsync mapper
   |> Task.map (fun res -> if res.IsEmpty then None else Some res)
   |> TaskResult.ofTask
   |> TaskResult.catch DatabaseError

let pgPersist
   (query: string)
   (parameters: SqlParameter list)
   : Result<int, Err> Task
   =
   connString
   |> Sql.connect
   |> Sql.query query
   |> Sql.parameters parameters
   |> Sql.executeNonQueryAsync
   |> TaskResult.ofTask
   |> TaskResult.catch DatabaseError

let pgTransaction
   (txn: SqlTransactionStatement list)
   : Result<int list, Err> Task
   =
   connString
   |> Sql.connect
   |> Sql.executeTransactionAsync txn
   |> TaskResult.ofTask
   |> TaskResult.catch DatabaseError

let pgProcedure
   (name: string)
   (parameters: SqlParameter list option)
   : Result<int, Err> Task
   =
   connString
   |> Sql.connect
   |> Sql.func name
   |> Sql.parameters (Option.defaultValue [] parameters)
   |> Sql.executeNonQueryAsync
   |> TaskResult.ofTask
   |> TaskResult.catch DatabaseError

/// Used to ensure that expected parameters are present for an UPDATE statement.
/// Otherwise, sets missing expected parameters to NULL.
/// Typically used with update statements containing COALESCE, where a consumer
/// provides a value for a few parameters to update, but not all, and expects
/// to update columns for their provided parameters, while leaving other columns
/// unchanged.
/// Ex:
///    UPDATE card
///    SET
///       status = COALESCE(@status::{CardTypeCast.status}, status),
///       daily_purchase_limit = COALESCE(@dailyPurchaseLimit, daily_purchase_limit),
///       last_purchase_at = COALESCE(@lastPurchaseAt, last_purchase_at)
///    WHERE card_id = @cardId;
let addCoalescableParamsForUpdate
   (expectedParamNames: string list)
   (providedParams: SqlParameter list)
   =
   let providedMap = Map.ofList providedParams

   let missingParams =
      expectedParamNames
      |> List.filter (fun name -> not (Map.containsKey name providedMap))
      |> List.map (fun name -> name, Sql.dbnull)

   providedParams @ missingParams
