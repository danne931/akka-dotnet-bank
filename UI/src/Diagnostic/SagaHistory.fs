module SagaHistory

open Feliz
open Feliz.UseElmish
open Feliz.Router
open Elmish
open System

open Lib.SharedTypes
open SagaDTO
open Bank.Employee.Domain
open UIDomain
open UIDomain.Diagnostic
open TableControlPanel
open Pagination

[<RequireQualifiedAccess>]
type SagaFilterView =
   | Date
   | Status

[<RequireQualifiedAccess>]
type SagaFilter =
   | Date of DateFilter option
   | Status of SagaDTOStatus list option

type Msg =
   | UpdateFilter of SagaFilter
   | PaginationMsg of Pagination.Msg<SagaDTO>

type State = {
   Query: SagaQuery
   Pagination: Pagination.State<SagaCursor, SagaDTO>
}

let init (browserQuery: SagaBrowserQuery) () =
   let paginationState, cmd = Pagination.init<SagaCursor, SagaDTO> ()

   {
      Query = SagaBrowserQuery.toNetworkQuery browserQuery
      Pagination = paginationState
   },
   Cmd.map PaginationMsg cmd

let handlePaginationMsg orgId (state: State) (msg: Pagination.Msg<SagaDTO>) =
   let config = {
      PageLimit = state.Query.PageLimit
      loadPage =
         fun cursor -> async {
            let query = { state.Query with Cursor = cursor }
            return! DiagnosticsService.getSagaHistory orgId query
         }
      getCursor =
         fun saga -> {
            CreatedAt = saga.CreatedAt
            SagaId = saga.Id
         }
      onLoadError =
         fun page err ->
            Log.error $"Error loading history for page {page}: {err}"
   }

   let paginationState, paginationCmd =
      Pagination.update config msg state.Pagination

   {
      state with
         Pagination = paginationState
   },
   Cmd.map PaginationMsg paginationCmd

let update (session: UserSession) msg (state: State) =
   match msg with
   | PaginationMsg msg -> handlePaginationMsg session.OrgId state msg
   | UpdateFilter filter ->
      let browserQuery = Routes.IndexUrl.diagnosticBrowserQuery ()

      let browserQuery =
         match filter with
         | SagaFilter.Date filter -> { browserQuery with Date = filter }
         | SagaFilter.Status filter -> { browserQuery with Status = filter }

      let browserQueryParams =
         browserQuery
         |> SagaBrowserQuery.toQueryParams
         |> Router.encodeQueryString

      state, Cmd.navigate (Routes.DiagnosticUrl.BasePath, browserQueryParams)

let renderSaga (saga: SagaDTO) =
   classyNode Html.div [ "saga-history-item" ] [
      Html.small saga.Name

      classyNode Html.div [ "saga-activities"; "grid" ] [
         for activity in saga.LifeCycle do
            let date =
               match activity.End with
               | Some finishedAt -> finishedAt
               | None -> activity.Start
               |> DateTime.dateUIFriendlyWithSecondsShort

            Html.div [
               attr.style [
                  style.backgroundColor (
                     match activity.Status with
                     | SagaActivityDTOStatus.InProgress -> Style.color.primary
                     | SagaActivityDTOStatus.Completed -> Style.color.moneyIn
                     | SagaActivityDTOStatus.Failed -> Style.color.alert
                     | SagaActivityDTOStatus.Aborted -> Style.color.secondary
                  )
               ]
               attr.custom ("data-tooltip", $"{activity.Name} {date}")
               attr.custom ("data-placement", "right")
            ]
      ]
   ]

let renderPagination state dispatch =
   Pagination.render state.Pagination (PaginationMsg >> dispatch)

let renderTableControlPanel
   (state: State)
   dispatch
   (session: UserSession)
   (browserQuery: SagaBrowserQuery)
   =
   TableControlPanelComponent {|
      FilterViewOptions = [
         SagaFilterView.Date, "Created At"
         SagaFilterView.Status, "Status"
      ]
      RenderFilterViewOnSelect =
         fun view ->
            match view with
            | SagaFilterView.Date ->
               DateFilter.DateFilterComponent
                  browserQuery.Date
                  (SagaFilter.Date >> Msg.UpdateFilter >> dispatch)
            | SagaFilterView.Status ->
               let selectableFilters = SagaDTOStatus.All

               CheckboxFieldset.render {|
                  Options =
                     selectableFilters
                     |> List.map (fun o -> { Id = o; Display = o.Display })
                  SelectedItems = browserQuery.Status
                  OnChange =
                     fun filters ->
                        SagaFilter.Status filters
                        |> Msg.UpdateFilter
                        |> dispatch
               |}
      FilterPills = [
         {
            View = SagaFilterView.Date
            OnDelete =
               fun () -> SagaFilter.Date None |> Msg.UpdateFilter |> dispatch
            Content =
               state.Query.DateRange |> Option.map DateFilter.dateRangeDisplay
         }
         {
            View = SagaFilterView.Status
            OnDelete =
               fun () -> SagaFilter.Status None |> Msg.UpdateFilter |> dispatch
            Content =
               state.Query.Status
               |> Option.map (fun selected ->
                  if selected.Length > 3 then
                     $"{selected.Length} Statuses Selected"
                  else
                     SagaDTOStatus.listToDisplay selected)
         }
      ]
      SubsequentChildren = Some [ renderPagination state dispatch ]
   |}

[<ReactComponent>]
let SagaHistoryComponent (url: Routes.DiagnosticUrl) (session: UserSession) =
   let browserQuery = Routes.IndexUrl.diagnosticBrowserQuery ()

   let state, dispatch =
      React.useElmish (
         init browserQuery,
         update session,
         [| box browserQuery |]
      )

   let signalRConnection = React.useContext SignalRConnectionProvider.context

   (*
   React.useEffect (
      fun () ->
         match signalRConnection with
         | Some conn ->
            DiagnosticsService.listenForSagaHistory
               (SagaHistoryReceived >> dispatch)
               conn
         | _ -> ()
         ()
      , [| box signalRConnection |]
   )
   *)

   let sagas = Map.tryFind state.Pagination.Page state.Pagination.Items

   Html.div [
      Html.h6 [
         attr.text "Saga History"
         attr.style [ style.marginBottom (10) ]
      ]

      classyNode Html.div [ "saga-history-container" ] [
         renderTableControlPanel state dispatch session browserQuery

         match sagas with
         | Some(Deferred.Resolved(Ok sagaHistoryOpt)) ->
            Html.progress [ attr.value 100 ]

            match sagaHistoryOpt with
            | None
            | Some [] -> Html.small "No saga history."
            | Some sagas ->
               classyNode Html.div [ "grid"; "saga-history" ] [
                  for saga in sagas do
                     renderSaga saga
               ]

               renderPagination state dispatch
         | _ -> Html.progress []
      ]
   ]
