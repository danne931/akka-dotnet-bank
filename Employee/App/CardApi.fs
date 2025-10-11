module Bank.Card.Api

open FsToolkit.ErrorHandling

open Lib.Postgres
open Lib.SharedTypes
open Bank.Employee.Domain
open Lib.NetworkQuery

let table = CardSqlMapper.table
module Fields = CardSqlMapper.CardFields
module Reader = CardSqlMapper.CardSqlReader
module Writer = CardSqlMapper.CardSqlWriter

let getCards (orgId: OrgId) (query: CardQuery) = taskResultOption {
   let employeeTable = EmployeeSqlMapper.table
   let cardIssuerMappingTable = CardIssuerMappingSqlMapper.table

   let dpaView = AccountEventSqlMapper.Views.dailyPurchaseAccruedByCard

   let mpaView = AccountEventSqlMapper.Views.monthlyPurchaseAccruedByCard

   let agg = [ "orgId", Writer.orgId orgId ], $"{table}.{Fields.orgId} = @orgId"

   let agg =
      Option.fold
         (fun (queryParams, where) ids ->
            [ "eIds", EmployeeSqlMapper.EmployeeSqlWriter.employeeIds ids ]
            @ queryParams,
            $"{where} AND {table}.{Fields.employeeId} = ANY(@eIds)")
         agg
         query.EmployeeIds

   let agg =
      Option.fold
         (fun (queryParams, where) ids ->
            [ "aIds", Writer.accountIds ids ] @ queryParams,
            $"{where} AND {table}.{Fields.accountId} = ANY(@aIds)")
         agg
         query.AccountIds

   let agg =
      Option.fold
         (fun (queryParams, where) (startDate, endDate) ->
            [
               "start", Writer.createdAt startDate
               "end", Writer.createdAt endDate
            ]
            @ queryParams,
            $"{where} AND {table}.{Fields.createdAt} >= @start 
              AND {table}.{Fields.createdAt} <= @end")
         agg
         query.CreatedAtDateRange

   let monthlyAccrued = $"{mpaView}.amount_accrued"

   let agg =
      Option.fold
         (fun (queryParams, where) amountFilter ->
            let where, amountParams =
               match amountFilter with
               | AmountFilter.LessThanOrEqualTo max ->
                  $"{where} AND {monthlyAccrued} <= @max",
                  [ "max", Sql.decimal max ]
               | AmountFilter.GreaterThanOrEqualTo min ->
                  $"{where} AND {monthlyAccrued} >= @min",
                  [ "min", Sql.decimal min ]
               | AmountFilter.Between(min, max) ->
                  $"{where} AND {monthlyAccrued} >= @min AND {monthlyAccrued} <= @max",
                  [ "min", Sql.decimal min; "max", Sql.decimal max ]

            amountParams @ queryParams, where)
         agg
         query.Amount

   let queryParams, where = agg

   let query =
      $"SELECT
           {table}.{Fields.cardNumberLast4},
           {table}.{Fields.cardNickname},
           {table}.{Fields.dailyPurchaseLimit},
           {table}.{Fields.monthlyPurchaseLimit},
           {table}.{Fields.cardId},
           {table}.{Fields.accountId},
           {table}.{Fields.cardType},
           {table}.{Fields.isVirtual},
           {table}.{Fields.statusDetail},
           {table}.{Fields.expYear},
           {table}.{Fields.expMonth},
           {table}.{Fields.lastPurchaseAt},
           {mpaView}.amount_accrued as mpa,
           {dpaView}.amount_accrued as dpa,
           {employeeTable}.*
        FROM {table}
        JOIN {employeeTable} using({Fields.employeeId})
        LEFT JOIN {dpaView} using({Fields.cardId})
        LEFT JOIN {mpaView} using({Fields.cardId})
        WHERE {where}
        ORDER BY {table}.{Fields.lastPurchaseAt} desc"

   let! cardsWithMetrics =
      pgQuery<CardWithMetrics> query (Some queryParams) (fun read -> {
         Card = Reader.card read
         DailyPurchaseAccrued =
            read.decimalOrNone "dpa" |> Option.defaultValue 0m
         MonthlyPurchaseAccrued =
            read.decimalOrNone "mpa" |> Option.defaultValue 0m
         Employee = EmployeeSqlMapper.EmployeeSqlReader.employee read
      })

   let cardsGroupedByEmployeeId =
      cardsWithMetrics
      |> List.groupBy _.Employee.EmployeeId
      |> List.map (fun (emId, cards) ->
         emId, cards |> List.map (fun x -> x.Card.CardId, x.Card) |> Map.ofList)
      |> Map.ofList

   return
      cardsWithMetrics
      |> List.map (fun c -> {
         c with
            Employee = {
               c.Employee with
                  Cards = cardsGroupedByEmployeeId[c.Employee.EmployeeId]
            }
      })
}

module Fields = CardIssuerMappingSqlMapper.Fields

let getInternalIdsFromIssuerCardId (cardId: CardIssuerCardId) = task {
   let query =
      $"SELECT {Fields.internalCardId}, {Fields.employeeId}
        FROM {CardIssuerMappingSqlMapper.table}
        WHERE {Fields.issuerCardId} = @cardIssuerCardId AND {Fields.closedReason} IS NULL"

   let! res =
      pgQuerySingle<CardId * EmployeeId>
         query
         (Some [
            "cardIssuerCardId",
            CardIssuerMappingSqlMapper.Writer.issuerCardId cardId
         ])
         (fun read ->
            CardIssuerMappingSqlMapper.Reader.internalCardId read,
            CardIssuerMappingSqlMapper.Reader.employeeId read)

   let err =
      Err.UnexpectedError
         $"No active issuer mapping found for issuer card id {cardId}"

   return res |> Result.unwrapOption err
}

let getIssuerCardIdFromInternalCardId (cardId: CardId) = task {
   let query =
      $"SELECT {Fields.issuerCardId}
        FROM {CardIssuerMappingSqlMapper.table}
        WHERE {Fields.internalCardId} = @cardId AND {Fields.closedReason} IS NULL"

   let! res =
      pgQuerySingle<CardIssuerCardId>
         query
         (Some [
            "cardId", CardIssuerMappingSqlMapper.Writer.internalCardId cardId
         ])
         (fun read -> CardIssuerMappingSqlMapper.Reader.issuerCardId read)

   let err =
      Err.UnexpectedError
         $"No active issuer mapping found for internal card id {cardId}"

   return res |> Result.unwrapOption err
}
