module SagaHistory

open Feliz
open Feliz.UseElmish
open Feliz.Router
open Elmish

open Lib.SharedTypes
open DiagnosticsService
open SagaDTO
open Bank.Employee.Domain
open UIDomain
open UIDomain.Diagnostic
open TableControlPanel

[<RequireQualifiedAccess>]
type SagaFilterView =
   | Date
   | Status

[<RequireQualifiedAccess>]
type SagaFilter =
   | Date of DateFilter option
   | Status of SagaDTOStatus list option

type Msg =
   | Load of AsyncOperationStatus<Result<SagaDTO list, Err>>
   | UpdateFilter of SagaFilter

type State = {
   Query: SagaQuery
   Sagas: Deferred<Result<SagaDTO list, Err>>
}

let init (browserQuery: SagaBrowserQuery) () =
   {
      Sagas = Deferred.Idle
      Query = SagaBrowserQuery.toNetworkQuery browserQuery
   },
   Cmd.ofMsg <| Load Started

let update (session: UserSession) msg (state: State) =
   match msg with
   | Load Started ->
      let loadInitStatus = async {
         let! res = getSagaHistory session.OrgId state.Query
         return Load(Finished res)
      }

      {
         state with
            Sagas = Deferred.InProgress
      },
      Cmd.fromAsync loadInitStatus
   | Load(Finished(Ok sagas)) ->
      {
         state with
            Sagas = Deferred.Resolved(Ok sagas)
      },
      Cmd.none
   | Load(Finished(Error err)) ->
      Log.error $"Error loading saga history {err}"

      {
         state with
            Sagas = Deferred.Resolved(Error err)
      },
      Alerts.toastCommand err
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
      SubsequentChildren = None
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

   Html.div [
      Html.h6 [
         attr.text "Saga History"
         attr.style [ style.marginBottom (10) ]
      ]

      Html.progress [
         match state.Sagas with
         | Deferred.Resolved _ -> attr.value 100
         | _ -> ()
      ]

      renderTableControlPanel state dispatch session browserQuery

      classyNode Html.div [ "saga-history-container" ] [
         match state.Sagas with
         | Deferred.Resolved(Ok sagas) ->
            classyNode Html.div [ "grid"; "saga-history" ] [
               if sagas.IsEmpty then
                  Html.small "No saga history."
               else
                  for saga in sagas do
                     renderSaga saga
            ]
         | _ -> ()
      ]
   ]
