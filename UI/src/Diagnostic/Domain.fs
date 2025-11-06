module UIDomain.Diagnostic

open SagaDTO
open Lib.NetworkQuery
open Lib.SharedTypes

type SagaBrowserQuery = {
   Date: UIDomain.DateFilter option
   Status: (SagaDTOStatus list) option
   SagaKind: (SagaKind list) option
   SagaId: CorrelationId option
} with

   /// Avoid triggering rehydration of saga data when saga expanded view opens
   /// by omitting SagaId from change detection.
   member x.ChangeDetection =
      Serialization.serialize {|
         Date = x.Date
         Status = x.Status
         SagaKind = x.SagaKind
      |}

module SagaBrowserQuery =
   let toQueryParams (query: SagaBrowserQuery) : (string * string) list =
      let agg = []

      let agg =
         match query.Status with
         | None -> agg
         | Some filters -> ("status", listToQueryString filters) :: agg

      let agg =
         match query.SagaKind with
         | None -> agg
         | Some filters -> ("kind", listToQueryString filters) :: agg

      // If custom date range selected, date query param will consist
      // of a start & end date.  Otherwise it will be something like
      // date=Last30Days; date=LastMonth; etc.
      let agg =
         match query.Date with
         | None -> agg
         | Some(UIDomain.DateFilter.Custom(startDate, endDate)) ->
            ("date", DateTime.rangeAsQueryString startDate endDate) :: agg
         | Some filter -> ("date", string filter) :: agg

      let agg =
         match query.SagaId with
         | None -> agg
         | Some sagaId -> ("sagaId", string sagaId) :: agg

      agg

   let fromQueryParams
      (queryParams: (string * string) list)
      : SagaBrowserQuery
      =
      let queryParams = Map.ofList queryParams

      {
         Date =
            Map.tryFind "date" queryParams
            |> Option.bind UIDomain.DateFilter.fromString
         Status =
            Map.tryFind "status" queryParams
            |> Option.bind SagaDTOStatus.fromQueryString
         SagaKind =
            Map.tryFind "kind" queryParams
            |> Option.bind SagaKind.fromQueryString
         SagaId =
            Map.tryFind "sagaId" queryParams
            |> Option.bind Guid.parseOptional
            |> Option.map CorrelationId
      }

   let toNetworkQuery (browserQuery: SagaBrowserQuery) : SagaQuery = {
      PageLimit = 50
      Cursor = None
      DateRange =
         browserQuery.Date |> Option.map UIDomain.DateFilter.toDateRange
      Status = browserQuery.Status
      SagaKind = browserQuery.SagaKind
   }

   let empty: SagaBrowserQuery = {
      Date = None
      Status = None
      SagaKind = None
      SagaId = None
   }
