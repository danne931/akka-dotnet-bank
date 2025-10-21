module HistoryDashboard

open Feliz
open Feliz.UseElmish
open Elmish
open Feliz.Router
open Fable.Core.JsInterop

open Bank.Employee.Domain
open Bank.Org.Domain
open Bank.Account.Domain
open UIDomain
open UIDomain.Employee
open UIDomain.History
open Transaction
open TableControlPanel
open EmployeeSearch
open Lib.SharedTypes
open Pagination
open SignalRBroadcast

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
         Account: (AccountEventGroupFilter list) option
         Org: (OrgEventGroupFilter list) option
         ParentAccount: (ParentAccountEventGroupFilter list) option
      |}
   | InitiatedBy of (SelectedEmployee list) option

type State = {
   Query: HistoryQuery
   Pagination: Pagination.State<HistoryCursor, History>
   RealtimeHistory: History list
}

type Msg =
   | UpdateFilter of HistoryFilter
   | PaginationMsg of Pagination.Msg<History>
   | RealtimeHistoryReceived of History

let init (browserQuery: HistoryBrowserQuery) () =
   let query =
      TransactionService.networkQueryFromHistoryBrowserQuery browserQuery

   let paginationState, cmd = Pagination.init<HistoryCursor, History> ()

   {
      Query = query
      Pagination = paginationState
      RealtimeHistory = []
   },
   Cmd.map PaginationMsg cmd

let handlePaginationMsg orgId (state: State) (msg: Pagination.Msg<History>) =
   let config = {
      PageLimit = state.Query.PageLimit
      loadPage =
         fun cursor -> async {
            let query = { state.Query with Cursor = cursor }
            return! TransactionService.getHistory orgId query
         }
      getCursor =
         fun history -> {
            Timestamp = history.Envelope.Timestamp
            EventId = history.Envelope.Id
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

let update orgId msg state =
   match msg with
   | PaginationMsg msg -> handlePaginationMsg orgId state msg
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
               ParentAccountEventType = filter.ParentAccount
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
   | RealtimeHistoryReceived history ->
      {
         state with
            RealtimeHistory = history :: state.RealtimeHistory
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
         | EmployeeEvent.PurchaseSettled _ ->
            Some(TransactionId envelope.CorrelationId)
         | _ -> None
      | History.ParentAccount _
      | History.Org _ -> None

   Html.tr [
      attr.key (string display.Id)

      match mayRedirectToTransaction with
      | Some txnId ->
         attr.onClick (fun e ->
            e.preventDefault ()

            let query = {
               UIDomain.Account.TransactionBrowserQuery.empty with
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
   Pagination.render state.Pagination (PaginationMsg >> dispatch)

let withoutPurchaseFilter =
   function
   | AccountEventGroupFilter.Purchase -> false
   | _ -> true

// The EmployeeEventGroupFilter will handle filtering employee events
// & account events for purchase events.
let selectableAccountFilters =
   AccountEventGroupFilter.All |> List.filter withoutPurchaseFilter

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
                  employeeEventFilters
                  |> Option.exists (
                     List.exists (function
                        | EmployeeEventGroupFilter.Purchase -> true
                        | _ -> false)
                  )

               let accountEventFilters =
                  match
                     isPurchaseFilterSelected, state.Query.AccountEventType
                  with
                  | true, None -> Some [ AccountEventGroupFilter.Purchase ]
                  | true, Some filters ->
                     Some(AccountEventGroupFilter.Purchase :: filters)
                  | false, Some filters ->
                     Some(filters |> List.filter withoutPurchaseFilter)
                  | _ -> state.Query.AccountEventType

               HistoryFilter.EventFilter {|
                  Account = accountEventFilters
                  Employee = employeeEventFilters
                  Org = state.Query.OrgEventType
                  ParentAccount = state.Query.ParentAccountEventType
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
                  ParentAccount = state.Query.ParentAccountEventType
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
                  ParentAccount = state.Query.ParentAccountEventType
               |}
               |> Msg.UpdateFilter
               |> dispatch
      |}

      CheckboxFieldset.render {|
         Options =
            ParentAccountEventGroupFilter.All
            |> List.map (fun o -> { Id = o; Display = o.Display })
         SelectedItems = browserQuery.ParentAccountEventType
         OnChange =
            fun parentAccountEventFilters ->
               HistoryFilter.EventFilter {|
                  Org = state.Query.OrgEventType
                  Account = state.Query.AccountEventType
                  Employee = state.Query.EmployeeEventType
                  ParentAccount = parentAccountEventFilters
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
                        ParentAccount = None
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

                  let parentAccountFilters = state.Query.ParentAccountEventType

                  let filters =
                     [
                        employeeFilters
                        |> Option.map EmployeeEventGroupFilter.listToDisplay
                        accountFilters
                        |> Option.map AccountEventGroupFilter.listToDisplay
                        orgFilters
                        |> Option.map OrgEventGroupFilter.listToDisplay
                        parentAccountFilters
                        |> Option.map
                              ParentAccountEventGroupFilter.listToDisplay
                     ]
                     |> List.choose id

                  let selectedFilterCount =
                     filterCount orgFilters
                     + filterCount accountFilters
                     + filterCount employeeFilters
                     + filterCount parentAccountFilters

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

   let history = Map.tryFind state.Pagination.Page state.Pagination.Items

   let realtimeHistory =
      state.RealtimeHistory
      |> List.filter (
         keepRealtimeEventsCorrespondingToSelectedFilter state.Query
      )

   SignalREventProvider.useEventSubscription {
      ComponentName = "HistoryDashboard"
      OrgId = Some session.OrgId
      EventTypes = [
         SignalREventProvider.EventType.ParentAccount
         SignalREventProvider.EventType.Account
         SignalREventProvider.EventType.Org
         SignalREventProvider.EventType.Employee
      ]
      OnError = ignore
      OnPersist =
         React.useCallbackRef (fun conf ->
            let history =
               match conf with
               | EventPersistedConfirmation.ParentAccount conf ->
                  History.ParentAccount {
                     Event = conf.EventPersisted
                     InitiatedByName =
                        conf.EventPersisted
                        |> AccountEvent.ParentAccount
                        |> AccountEnvelope.unwrap
                        |> snd
                        |> _.InitiatedBy.Name
                  }
               | EventPersistedConfirmation.Account conf ->
                  History.Account {
                     Event = conf.EventPersisted
                     InitiatedByName =
                        AccountEnvelope.unwrap conf.EventPersisted
                        |> snd
                        |> _.InitiatedBy.Name
                  }
               | EventPersistedConfirmation.Org conf ->
                  History.Org {
                     Event = conf.EventPersisted
                     InitiatedByName =
                        OrgEnvelope.unwrap conf.EventPersisted
                        |> snd
                        |> _.InitiatedBy.Name
                  }
               | EventPersistedConfirmation.Employee conf ->
                  History.Employee {
                     Event = conf.EventPersisted
                     InitiatedByName =
                        EmployeeEnvelope.unwrap conf.EventPersisted
                        |> snd
                        |> _.InitiatedBy.Name
                     EmployeeName = conf.Employee.Name
                  }

            dispatch (Msg.RealtimeHistoryReceived history))
   }

   classyNode Html.div [ "history-dashboard" ] [
      classyNode Html.main [ "container-fluid" ] [
         Html.section [
            Html.h4 "History"

            renderTableControlPanel state dispatch session browserQuery

            Html.progress [
               match history with
               | Some(Deferred.Resolved _) -> attr.value 100
               | _ -> ()
            ]

            Html.div [
               classyNode Html.div [ "history-table" ] [
                  match orgCtx, history with
                  | _, Some(Resolved(Error _)) ->
                     Html.small "Uh oh. Error getting history."
                  | _, Some(Resolved(Ok None)) -> Html.small "No history."
                  | Resolved(Ok(Some org)), Some(Resolved(Ok(Some history))) ->
                     let history =
                        if state.Pagination.Page = 1 then
                           realtimeHistory @ history
                        else
                           history

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

      match orgCtx with
      | Deferred.Resolved(Ok(Some org)) ->
         React.suspense (
            [
               React.lazy' (
                  (fun () -> importDynamic "../Orgs/MetricsComponent"),
                  org
               )
            ],
            Html.progress []
         )
      | _ -> ()
   ]
