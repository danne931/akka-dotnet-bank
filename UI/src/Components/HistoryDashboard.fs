module HistoryDashboard

open Feliz
open Feliz.UseElmish
open Elmish
open Feliz.Router

open Bank.Employee.Domain
open Bank.Org.Domain
open Bank.Account.Domain
open UIDomain
open UIDomain.Employee
open History
open TableControlPanel
open EmployeeSearch
open Lib.SharedTypes

[<RequireQualifiedAccess>]
type HistoryFilterView =
   | Date
   | EventType
   | InitiatedBy

[<RequireQualifiedAccess>]
type HistoryFilter =
   | Date of DateFilter option
   | EventFilter of
      {|
         Employee: (EmployeeEventGroupFilter list) option
         Account: (TransactionGroupFilter list) option
         Org: (OrgEventGroupFilter list) option
      |}
   | InitiatedBy of (SelectedEmployee list) option

type State = {
   Query: HistoryQuery
   History: Map<int, Deferred<HistoryMaybe>>
}

type Msg =
   | LoadHistory of HistoryQuery * AsyncOperationStatus<HistoryMaybe>
   | RealtimeEventReceived of History
   | UpdateFilter of HistoryFilter
   | ResetPageIndex

let init (browserQuery: HistoryBrowserQuery) () =
   let query = OrgService.networkQueryFromHistoryBrowserQuery browserQuery

   {
      Query = query
      History = Map [ 1, Deferred.Idle ]
   },
   Cmd.ofMsg (LoadHistory(query, Started))

let update orgId msg state =
   match msg with
   | LoadHistory(query, Started) ->
      let load = async {
         let! res = OrgService.getHistory orgId query
         return LoadHistory(query, Finished res)
      }

      {
         Query = query
         History = state.History |> Map.add query.Page Deferred.InProgress
      },
      Cmd.fromAsync load
   | LoadHistory(query, Finished(Ok(history))) ->
      let resolvedHistory = Deferred.Resolved(Ok history)

      {
         state with
            History =
               state.History
               |> Map.change query.Page (Option.map (fun _ -> resolvedHistory))
      },
      Cmd.none
   | LoadHistory(query, Finished(Error err)) ->
      Log.error $"Error loading history for Page {query.Page}: {err}"

      {
         state with
            History =
               Map.change
                  query.Page
                  (Option.map (fun _ -> Deferred.Resolved(Error err)))
                  state.History
      },
      Cmd.none
   | RealtimeEventReceived evt ->
      let resolved = Deferred.Resolved << Ok << Some

      {
         state with
            History =
               state.History
               |> Map.change
                     // NOTE: If the user is on page 3 and then goes back to
                     // page 1 they will be able to see the realtime event that
                     // came in while they were not on page 1.
                     1
                     (Option.map (fun history ->
                        match history with
                        | Deferred.Resolved(Ok(Some history)) ->
                           resolved (evt :: history)
                        | _ -> resolved [ evt ]))
      },
      Cmd.none
   | UpdateFilter filter ->
      let browserQuery = Routes.IndexUrl.historyBrowserQuery ()

      let browserQuery =
         match filter with
         | HistoryFilter.Date filter -> { browserQuery with Date = filter }
         | HistoryFilter.EventFilter filter -> {
            browserQuery with
               EmployeeEventType = filter.Employee
               AccountEventType = filter.Account
               OrgEventType = filter.Org
           }
         | HistoryFilter.InitiatedBy selected -> {
            browserQuery with
               SelectedInitiatedBy = selected
           }

      let browserQueryParams =
         browserQuery
         |> HistoryBrowserQuery.toQueryParams
         |> Router.encodeQueryString

      state, Cmd.navigate (Routes.HistoryUrl.BasePath, browserQueryParams)
   | ResetPageIndex ->
      {
         state with
            Query = {
               state.Query with
                  HistoryQuery.Page = 1
            }
      },
      Cmd.none

let renderTableRow (org: OrgWithAccountProfiles) (history: History) =
   let display = historyUIFriendly org history

   let mayRedirectToTransaction =
      match history with
      | History.Account h ->
         AccountEnvelope.unwrap h.Event
         |> snd
         |> _.CorrelationId
         |> TransactionId
         |> Some
      | History.Employee h ->
         let _, envelope = EmployeeEnvelope.unwrap h.Event

         match h.Event with
         | EmployeeEvent.PurchasePending _
         | EmployeeEvent.PurchaseConfirmedByAccount _ ->
            Some(TransactionId envelope.CorrelationId)
         | _ -> None
      | History.Org _ -> None

   Html.tr [
      attr.key (string display.Id)

      match mayRedirectToTransaction with
      | Some txnId ->
         attr.onClick (fun e ->
            e.preventDefault ()

            let query = {
               UIDomain.Account.AccountBrowserQuery.empty with
                  Transaction = Some txnId
            }

            Router.navigate (Routes.TransactionsUrl.queryPath query))
      | None -> attr.style [ style.cursor.defaultCursor ]

      attr.children [
         Html.th [ attr.scope "row" ]

         Html.td [
            attr.classes [
               match display.MoneyFlow with
               | None -> ""
               | Some MoneyFlow.In -> "credit"
               | Some MoneyFlow.Out -> "debit"
            ]

            attr.text (display.Amount |> Option.defaultValue "-")
         ]

         Html.td display.Info

         Html.td display.Initiator

         Html.td display.Date
      ]
   ]

let renderTable (org: OrgWithAccountProfiles) (history: History list) =
   Html.table [
      attr.classes [ "clickable-table" ]
      attr.role "grid"
      attr.children [
         Html.thead [
            Html.tr [
               Html.th [ attr.scope "col" ]

               Html.th [ attr.scope "col"; attr.text "Amount" ]

               Html.th [ attr.scope "col"; attr.text "Info" ]

               Html.th [ attr.scope "col"; attr.text "Initiated By" ]

               Html.th [ attr.scope "col"; attr.text "Date" ]
            ]
         ]

         Html.tbody [ for h in history -> renderTableRow org h ]
      ]
   ]

let renderPagination state dispatch =
   Pagination.render {|
      PaginatedResults = state.History
      Page = state.Query.Page
      OnPageChange =
         fun page ->
            dispatch
            <| Msg.LoadHistory({ state.Query with Page = page }, Started)
      OnPageReset = fun () -> dispatch Msg.ResetPageIndex
   |}

let withoutPurchaseFilter =
   function
   | TransactionGroupFilter.Purchase -> false
   | _ -> true

// The EmployeeEventGroupFilter will handle filtering employee events
// & account events for purchase events.
let selectableAccountFilters =
   TransactionGroupFilter.All |> List.filter withoutPurchaseFilter

let private filterCount (filters: ('t list) option) : int =
   filters |> Option.map _.Length |> Option.defaultValue 0

let private renderEventFilterCheckboxes state dispatch browserQuery =
   React.fragment [
      CheckboxFieldset.render {|
         Options =
            EmployeeEventGroupFilter.All
            |> List.map (fun o -> { Id = o; Display = o.Display })
         SelectedItems = browserQuery.EmployeeEventType
         OnChange =
            fun employeeEventFilters ->
               let isPurchaseFilterSelected =
                  match employeeEventFilters with
                  | Some filters ->
                     filters
                     |> List.exists (function
                        | EmployeeEventGroupFilter.Purchase -> true
                        | _ -> false)
                  | None -> false

               let accountEventFilters =
                  match
                     isPurchaseFilterSelected, state.Query.AccountEventType
                  with
                  | true, None -> Some [ TransactionGroupFilter.Purchase ]
                  | true, Some filters ->
                     Some(TransactionGroupFilter.Purchase :: filters)
                  | false, Some filters ->
                     Some(filters |> List.filter withoutPurchaseFilter)
                  | _ -> state.Query.AccountEventType

               HistoryFilter.EventFilter {|
                  Account = accountEventFilters
                  Employee = employeeEventFilters
                  Org = state.Query.OrgEventType
               |}
               |> Msg.UpdateFilter
               |> dispatch
      |}

      CheckboxFieldset.render {|
         Options =
            selectableAccountFilters
            |> List.map (fun o -> { Id = o; Display = o.Display })
         SelectedItems = browserQuery.AccountEventType
         OnChange =
            fun accountEventFilters ->
               HistoryFilter.EventFilter {|
                  Account = accountEventFilters
                  Employee = state.Query.EmployeeEventType
                  Org = state.Query.OrgEventType
               |}
               |> Msg.UpdateFilter
               |> dispatch
      |}

      CheckboxFieldset.render {|
         Options =
            OrgEventGroupFilter.All
            |> List.map (fun o -> { Id = o; Display = o.Display })
         SelectedItems = browserQuery.OrgEventType
         OnChange =
            fun orgEventFilters ->
               HistoryFilter.EventFilter {|
                  Org = orgEventFilters
                  Account = state.Query.AccountEventType
                  Employee = state.Query.EmployeeEventType
               |}
               |> Msg.UpdateFilter
               |> dispatch
      |}
   ]

let renderTableControlPanel
   state
   dispatch
   (session: UserSession)
   (browserQuery: HistoryBrowserQuery)
   =
   TableControlPanelComponent {|
      FilterViewOptions = [
         HistoryFilterView.InitiatedBy, "Initiated By"
         HistoryFilterView.Date, "Date"
         HistoryFilterView.EventType, "Event Group"
      ]
      RenderFilterViewOnSelect =
         fun view ->
            match view with
            | HistoryFilterView.InitiatedBy ->
               EmployeeMultiSelectSearchComponent {|
                  OrgId = session.OrgId
                  Selected = browserQuery.SelectedInitiatedBy
                  OnSelect =
                     HistoryFilter.InitiatedBy >> Msg.UpdateFilter >> dispatch
                  Dependencies = Some [| string view |]
               |}
            | HistoryFilterView.Date ->
               DateFilter.DateFilterComponent
                  browserQuery.Date
                  (HistoryFilter.Date >> Msg.UpdateFilter >> dispatch)
            | HistoryFilterView.EventType ->
               renderEventFilterCheckboxes state dispatch browserQuery
      FilterPills =
         [
            {
               View = HistoryFilterView.Date
               OnDelete =
                  fun () ->
                     HistoryFilter.Date None |> Msg.UpdateFilter |> dispatch
               Content =
                  state.Query.DateRange
                  |> Option.map DateFilter.dateRangeDisplay
            }
            {
               View = HistoryFilterView.EventType
               OnDelete =
                  fun () ->
                     HistoryFilter.EventFilter {|
                        Org = None
                        Account = None
                        Employee = None
                     |}
                     |> Msg.UpdateFilter
                     |> dispatch
               Content =
                  let orgFilters = state.Query.OrgEventType

                  let employeeFilters = state.Query.EmployeeEventType

                  let accountFilters =
                     state.Query.AccountEventType
                     |> Option.bind (fun filters ->
                        // 'Purchase' filter will be shown for the EmployeeEventGroupFilter
                        // so no need to duplicate it.
                        let filters =
                           filters |> List.filter withoutPurchaseFilter

                        if filters.IsEmpty then None else Some filters)

                  let filters =
                     [
                        employeeFilters
                        |> Option.map EmployeeEventGroupFilter.listToDisplay
                        accountFilters
                        |> Option.map TransactionGroupFilter.listToDisplay
                        orgFilters
                        |> Option.map OrgEventGroupFilter.listToDisplay
                     ]
                     |> List.choose id

                  let selectedFilterCount =
                     filterCount orgFilters
                     + filterCount accountFilters
                     + filterCount employeeFilters

                  if selectedFilterCount = 0 then
                     None
                  elif selectedFilterCount > 3 then
                     Some $"{selectedFilterCount} Event Types Selected"
                  else
                     Some(String.concat ", " filters)
            }
         ]
         @ [
            match browserQuery.SelectedInitiatedBy with
            | None -> ()
            | Some selected ->
               for employee in selected ->
                  {
                     View = HistoryFilterView.InitiatedBy
                     OnDelete =
                        fun () ->
                           selected
                           |> List.filter (fun e -> e.Id <> employee.Id)
                           |> fun es ->
                              (if es.Length = 0 then None else Some es)
                              |> HistoryFilter.InitiatedBy
                              |> Msg.UpdateFilter
                              |> dispatch
                     Content = Some $"Initiated By: {employee.Name}"
                  }
         ]
      SubsequentChildren = Some [ renderPagination state dispatch ]
   |}

[<ReactComponent>]
let HistoryDashboardComponent (url: Routes.HistoryUrl) (session: UserSession) =
   let browserQuery = Routes.IndexUrl.historyBrowserQuery ()

   let state, dispatch =
      React.useElmish (
         init browserQuery,
         update session.OrgId,
         [| box browserQuery |]
      )

   let orgCtx = React.useContext OrgProvider.context

   let history = Map.tryFind state.Query.Page state.History

   SignalREventProvider.useEventSubscription {
      ComponentName = "HistoryDashboard"
      OrgId = Some session.OrgId
      EventTypes = [
         SignalREventProvider.EventType.Account
         SignalREventProvider.EventType.Org
         SignalREventProvider.EventType.Employee
      ]
      OnPersist =
         React.useCallbackRef (fun conf ->
            let history =
               match conf with
               | SignalREventProvider.EventPersistedConfirmation.Account conf ->
                  History.Account {
                     Event = conf.EventPersisted
                     InitiatedByName =
                        AccountEnvelope.unwrap conf.EventPersisted
                        |> snd
                        |> _.InitiatedBy.Name
                  }
               | SignalREventProvider.EventPersistedConfirmation.Org conf ->
                  History.Org {
                     Event = conf.EventPersisted
                     InitiatedByName =
                        OrgEnvelope.unwrap conf.EventPersisted
                        |> snd
                        |> _.InitiatedBy.Name
                  }
               | SignalREventProvider.EventPersistedConfirmation.Employee conf ->
                  History.Employee {
                     Event = conf.EventPersisted
                     InitiatedByName =
                        EmployeeEnvelope.unwrap conf.EventPersisted
                        |> snd
                        |> _.InitiatedBy.Name
                     EmployeeName = conf.Employee.Name
                  }

            if
               keepRealtimeEventsCorrespondingToSelectedFilter
                  state.Query
                  history
            then
               dispatch (Msg.RealtimeEventReceived history))
   }

   classyNode Html.div [ "history-dashboard" ] [
      classyNode Html.main [ "container-fluid" ] [
         Html.section [
            Html.h4 "History"

            Html.progress [
               match history with
               | Some(Deferred.Resolved _) -> attr.value 100
               | _ -> ()
            ]

            classyNode Html.figure [ "control-panel-and-table-container" ] [
               renderTableControlPanel state dispatch session browserQuery

               classyNode Html.div [ "history-table" ] [
                  match orgCtx, history with
                  | _, Some(Resolved(Error _)) ->
                     Html.small "Uh oh. Error getting history."
                  | _, Some(Resolved(Ok None)) -> Html.small "No history."
                  | Resolved(Ok(Some org)), Some(Resolved(Ok(Some history))) ->
                     if history.IsEmpty then
                        Html.small "No history."
                     else
                        renderTable org history
                        renderPagination state dispatch
                  | _ -> ()
               ]
            ]
         ]
      ]
   ]
