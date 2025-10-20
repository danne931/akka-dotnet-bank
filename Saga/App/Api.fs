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
   let qParams = [ "orgId", Writer.orgId orgId ]
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

   let qParams, whereClause = agg

   let query =
      $"""
      SELECT {Fields.sagaState}
      FROM {table}
      WHERE {whereClause}
      ORDER BY {Fields.createdAt} DESC
      LIMIT 50
      """

   pgQuery query (Some qParams) Reader.sagaDTO
