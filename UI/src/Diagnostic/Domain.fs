module UIDomain.Diagnostic

open SagaDTO
open Lib.NetworkQuery

type SagaBrowserQuery = {
   Date: UIDomain.DateFilter option
   Status: (SagaDTOStatus list) option
}

module SagaBrowserQuery =
   let toQueryParams (query: SagaBrowserQuery) : (string * string) list =
      let agg = []

      let agg =
         match query.Status with
         | None -> agg
         | Some filters -> ("status", listToQueryString filters) :: agg

      // If custom date range selected, date query param will consist
      // of a start & end date.  Otherwise it will be something like
      // date=Last30Days; date=LastMonth; etc.
      let agg =
         match query.Date with
         | None -> agg
         | Some(UIDomain.DateFilter.Custom(startDate, endDate)) ->
            ("date", DateTime.rangeAsQueryString startDate endDate) :: agg
         | Some filter -> ("date", string filter) :: agg

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
      }

   let toNetworkQuery (browserQuery: SagaBrowserQuery) : SagaQuery = {
      DateRange =
         browserQuery.Date |> Option.map UIDomain.DateFilter.toDateRange
      Status = browserQuery.Status
   }

   let empty: SagaBrowserQuery = { Date = None; Status = None }
