module SagaApi

open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.Postgres
open AppSagaSqlMapper
open SagaDTO

let getAllSagas
   (orgId: OrgId)
   (query: SagaQuery)
   : TaskResultOption<SagaDTO list, Err>
   =
   let whereClause = $"{Fields.orgId} = @orgId"

   let qParams = [
      "orgId", Writer.orgId orgId
      "limit", Sql.int query.PageLimit
   ]

   let agg = qParams, whereClause

   let agg =
      Option.fold
         (fun (queryParams, where) (startDate, endDate) ->
            let queryParams =
               [
                  "start", Writer.timestamp startDate
                  "end", Writer.timestamp endDate
               ]
               @ queryParams

            let ts = Fields.createdAt
            let timestampQuery = $"{ts} >= @start AND {ts} <= @end"

            queryParams, $"{where} AND {timestampQuery}")
         agg
         query.DateRange

   let agg =
      Option.fold
         (fun (queryParams, where) filters ->
            [
               "status",
               filters
               |> List.map Writer.fromSagaDTOStatus
               |> List.toArray
               |> Sql.stringArray
            ]
            @ queryParams,
            $"{where} AND {Fields.status}::text = ANY(@status)")
         agg
         query.Status

   let agg =
      Option.fold
         (fun (queryParams, where) (cursor: SagaCursor) ->
            let queryParams =
               [
                  "createdAt", Sql.timestamptz cursor.CreatedAt
                  "sagaId", Writer.id cursor.SagaId
               ]
               @ queryParams

            let cursorWhere =
               // NOTE:
               // Use of date.toISOString() browser API causes the timestamp
               // to lose a wee bit of specificity.
               // Ex: 2025-02-27T13:17:57.06234Z -> 2025-02-27T13:17:57.062Z
               // Postgres has .06234 but our equivalence query has .062 so it
               // will not match unless we truncate the Postgres value via the
               // date_trunc function below.  I reckon this will be plenty
               // sufficient for now.
               let ts = $"date_trunc('milliseconds', {Fields.createdAt})"
               $"({ts} < @createdAt OR ({ts} = @createdAt AND {Fields.id} < @sagaId))"

            queryParams, $"{where} AND {cursorWhere}")
         agg
         query.Cursor

   let qParams, whereClause = agg

   let query =
      $"""
      SELECT {Fields.sagaState}, {Fields.createdAt}
      FROM {table}
      WHERE {whereClause}
      ORDER BY {Fields.createdAt} DESC, {Fields.id} DESC
      LIMIT @limit
      """

   pgQuery query (Some qParams) Reader.sagaDTO
