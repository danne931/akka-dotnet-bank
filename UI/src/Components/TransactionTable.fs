module TransactionTable

open Feliz
open Feliz.UseElmish
open Feliz.Router
open Elmish
open Fable.FontAwesome

open Lib.SharedTypes
open UIDomain
open UIDomain.Account
open Bank.Account.Domain
open Lib.NetworkQuery
open TableControlPanel

[<RequireQualifiedAccess>]
type TransactionFilterView =
   | Date
   | Amount
   | Category

[<RequireQualifiedAccess>]
type TransactionFilter =
   | Date of DateFilter option
   | MoneyFlow of MoneyFlow option
   | Amount of AmountFilter option
   | Category of CategoryFilter option
   | Cards of (SelectedCard list) option

type State = {
   Transactions: Map<int, Deferred<TransactionsMaybe>>
   Query: TransactionQuery
}

type Msg =
   | ToggleDiagnosticView
   | UpdateFilter of TransactionFilter
   | ResetPageIndex
   | LoadTransactions of
      TransactionQuery *
      AsyncOperationStatus<TransactionsMaybe>
   | RefreshTransactions of TransactionQuery
   | TransactionPropsResolved of Deferred<TransactionsMaybe>
   | ViewTransaction of EventId

let init
   (txnsDeferred: Deferred<TransactionsMaybe>)
   (txnQuery: TransactionQuery)
   ()
   =
   {
      Transactions = Map [ 1, txnsDeferred ]
      Query = txnQuery
   },
   Cmd.none

let update msg state =
   match msg with
   | ToggleDiagnosticView ->
      let query = {
         state.Query with
            Page = 1
            Diagnostic = not state.Query.Diagnostic
      }

      {
         state with
            Query = query
            Transactions = Map [ 1, Deferred.Idle ]
      },
      Cmd.ofMsg (Msg.LoadTransactions(query, Started))
   | UpdateFilter filter ->
      let browserQuery = Routes.IndexUrl.accountBrowserQuery ()

      let browserQuery =
         match filter with
         | TransactionFilter.Date filter -> { browserQuery with Date = filter }
         | TransactionFilter.MoneyFlow direction -> {
            browserQuery with
               MoneyFlow = direction
           }
         | TransactionFilter.Amount amount -> {
            browserQuery with
               Amount = amount
           }
         | TransactionFilter.Category cat -> {
            browserQuery with
               Category = cat
           }
         | TransactionFilter.Cards cards -> {
            browserQuery with
               SelectedCards = cards
           }

      let browserQueryParams =
         browserQuery
         |> AccountBrowserQuery.toQueryParams
         |> Router.encodeQueryString

      state,
      Cmd.navigate (
         Routes.TransactionUrl.BasePath,
         string state.Query.AccountId,
         browserQueryParams
      )
   | ResetPageIndex ->
      let query = {
         state.Query with
            TransactionQuery.Page = 1
      }

      { state with Query = query }, Cmd.none
   | RefreshTransactions query ->
      let load = async {
         let! res = TransactionService.getTransactions query
         return LoadTransactions(query, Finished res)
      }

      {
         state with
            Query = query
            Transactions = Map [ 1, Deferred.InProgress ]
      },
      Cmd.fromAsync load
   | LoadTransactions(query, Started) ->
      let load = async {
         let! res = TransactionService.getTransactions query
         return LoadTransactions(query, Finished res)
      }

      let page = query.Page
      let deferredPageData = state.Transactions.TryFind page
      let state = { state with Query = query }

      match deferredPageData with
      | Some(Deferred.Resolved(Ok _)) when page <> 1 -> state, Cmd.none
      | _ ->
         {
            state with
               Transactions =
                  state.Transactions |> Map.add page Deferred.InProgress
         },
         Cmd.fromAsync load
   | LoadTransactions(query, Finished(Ok txnsOpt)) ->
      let resolvedTxns = Deferred.Resolved(Ok txnsOpt)

      {
         state with
            Transactions =
               state.Transactions
               |> Map.change query.Page (Option.map (fun _ -> resolvedTxns))
      },
      Cmd.none
   | LoadTransactions(query, Finished(Error err)) ->
      Log.error $"Error loading transactions for query {query}: {err}"

      {
         state with
            Transactions =
               state.Transactions
               |> Map.change
                     query.Page
                     (Option.map (fun _ -> Deferred.Resolved(Error err)))
      },
      Cmd.none
   | TransactionPropsResolved txnsDeferred ->
      {
         state with
            Transactions = Map [ 1, txnsDeferred ]
      },
      Cmd.none
   | ViewTransaction(txnId) ->
      let queryString =
         {
            Routes.IndexUrl.accountBrowserQuery () with
               Action = None
               Transaction = Some txnId
         }
         |> AccountBrowserQuery.toQueryParams
         |> Router.encodeQueryString

      state,
      Cmd.navigate (
         Routes.TransactionUrl.BasePath,
         string state.Query.AccountId,
         queryString
      )

let renderControlPanel
   state
   dispatch
   (categories: Map<int, TransactionCategory>)
   =
   let query = Routes.IndexUrl.accountBrowserQuery ()

   TableControlPanelComponent {|
      FilterViewOptions = [
         TransactionFilterView.Date, "Date"
         TransactionFilterView.Amount, "Amount"
         TransactionFilterView.Category, "Categories"
      ]
      RenderFilterViewOnSelect =
         function
         | TransactionFilterView.Category ->
            TransactionCategoryFilter.TransactionCategoryFilterComponent
               query.Category
               categories
               (TransactionFilter.Category >> Msg.UpdateFilter >> dispatch)
         | TransactionFilterView.Amount ->
            React.fragment [
               AmountFilter.renderMoneyFlowFilter
                  query.MoneyFlow
                  (TransactionFilter.MoneyFlow >> Msg.UpdateFilter >> dispatch)

               AmountFilter.AmountFilterComponent
                  query.Amount
                  (TransactionFilter.Amount >> Msg.UpdateFilter >> dispatch)
            ]
         | TransactionFilterView.Date ->
            DateFilter.DateFilterComponent
               query.Date
               (TransactionFilter.Date >> Msg.UpdateFilter >> dispatch)
      FilterPills = [
         {
            View = TransactionFilterView.Date
            OnDelete =
               fun () ->
                  dispatch <| Msg.UpdateFilter(TransactionFilter.Date None)
            Content =
               state.Query.DateRange |> Option.map DateFilter.dateRangeDisplay
         }
         {
            View = TransactionFilterView.Amount
            OnDelete =
               fun () ->
                  dispatch <| Msg.UpdateFilter(TransactionFilter.Amount None)
            Content = query.Amount |> Option.map AmountFilter.display
         }
         {
            View = TransactionFilterView.Amount
            OnDelete =
               fun () ->
                  dispatch <| Msg.UpdateFilter(TransactionFilter.MoneyFlow None)
            Content = query.MoneyFlow |> Option.map MoneyFlow.display
         }
         {
            View = TransactionFilterView.Category
            OnDelete =
               fun () ->
                  dispatch <| Msg.UpdateFilter(TransactionFilter.Category None)
            Content =
               query.Category |> Option.bind (CategoryFilter.display categories)
         }
         {
            View = TransactionFilterView.Date
            OnDelete =
               fun () ->
                  dispatch <| Msg.UpdateFilter(TransactionFilter.Cards None)
            Content =
               query.SelectedCards
               |> Option.map (fun cards ->
                  if cards.Length = 1 then
                     $"Card: {cards |> List.head |> _.Display}"
                  else
                     $"Cards ({cards.Length})")
         }
      ]
      SubsequentChildren =
         Some [
            Pagination.render {|
               PaginatedResults = state.Transactions
               Page = state.Query.Page
               OnPageChange =
                  fun page ->
                     dispatch
                     <| Msg.LoadTransactions(
                        { state.Query with Page = page },
                        Started
                     )
               OnPageReset = fun () -> dispatch ResetPageIndex
            |}

            Html.a [
               attr.children [
                  Fa.i [
                     if state.Query.Diagnostic then
                        Fa.Solid.EyeSlash
                     else
                        Fa.Solid.Eye
                  ] []
               ]

               attr.href ""

               attr.onClick (fun e ->
                  e.preventDefault ()
                  dispatch ToggleDiagnosticView)

               attr.custom (
                  "data-tooltip",
                  if state.Query.Diagnostic then
                     "Hide Diagnostic Events"
                  else
                     "Show Diagnostic Events"
               )
               attr.custom ("data-placement", "left")
            ]
         ]
   |}

let renderTableRow
   (account: Account)
   (evt: AccountEvent)
   (selectedTxnId: EventId option)
   (merchants: Map<string, Merchant>)
   dispatch
   =
   let txn =
      eventWithMerchantAlias evt merchants |> transactionUIFriendly account

   let orDefaultValue opt = opt |> Option.defaultValue "-"
   let _, envelope = AccountEnvelope.unwrap evt

   Html.tr [
      attr.key (string envelope.Id)

      match selectedTxnId with
      | Some txnId when txnId = envelope.Id -> attr.classes [ "selected" ]
      | _ -> ()

      if TransactionDetail.hasRenderImplementation evt then
         attr.onClick (fun _ -> dispatch (Msg.ViewTransaction envelope.Id))
      else
         attr.style [ style.cursor.defaultCursor; style.borderLeftWidth 0 ]

      attr.children [
         Html.th [ attr.scope "row" ]

         Html.td [
            attr.classes [
               match txn.MoneyFlow with
               | None -> ""
               | Some MoneyFlow.In -> "credit"
               | Some MoneyFlow.Out -> "debit"
            ]

            attr.text (txn.Amount |> orDefaultValue)
         ]

         Html.td txn.Info

         Html.td txn.Date
      ]
   ]

let renderTable
   (account: Account)
   (txns: AccountEvent list)
   (selectedTxnId: EventId option)
   (merchants: Map<string, Merchant>)
   dispatch
   =
   Html.table [
      attr.classes [ "clickable-table" ]
      attr.role "grid"
      attr.children [
         Html.thead [
            Html.tr [
               Html.th [ attr.scope "col" ]

               Html.th [ attr.scope "col"; attr.text "Amount" ]

               Html.th [ attr.scope "col"; attr.text "Info" ]

               Html.th [ attr.scope "col"; attr.text "Date" ]
            ]
         ]

         Html.tbody [
            for txn in txns ->
               renderTableRow account txn selectedTxnId merchants dispatch
         ]
      ]
   ]

[<ReactComponent>]
let TransactionTableComponent
   (account: Account)
   (deferred: Deferred<AccountAndTransactionsMaybe>)
   =
   let isInitialMount = React.useRef true
   let txnsDeferred = (Deferred.map << Result.map << Option.map) snd deferred
   let categories = React.useContext TransactionCategoryProvider.context
   let merchants = React.useContext MerchantProvider.stateContext
   let signalRCtx = React.useContext SignalRAccountEventProvider.context
   let browserQuery = Routes.IndexUrl.accountBrowserQuery ()

   let txnQuery =
      TransactionService.transactionQueryFromAccountBrowserQuery
         account.AccountId
         browserQuery

   let state, dispatch =
      React.useElmish (
         init txnsDeferred txnQuery,
         update,
         [| box account.AccountId |]
      )

   React.useEffect (
      fun () ->
         if isInitialMount.current then
            isInitialMount.current <- false
         else
            dispatch (Msg.RefreshTransactions txnQuery)
      , [| box browserQuery.ChangeDetection |]
   )

   let txns = Map.tryFind state.Query.Page state.Transactions

   let q = state.Query

   let noFilterSelected =
      match q.Amount, q.Category, q.MoneyFlow, q.DateRange, q.Page with
      | None, None, None, None, 1 -> true
      | _ -> false

   React.useEffect (
      (fun _ ->
         let txnsResolvedInState =
            Map.tryFind state.Query.Page state.Transactions
            |> Option.map Deferred.resolved

         let txnsResolvedInProps = Deferred.resolved txnsDeferred

         match txnsResolvedInState, txnsResolvedInProps with
         | Some false, true
         | Some true, false ->
            dispatch <| TransactionPropsResolved txnsDeferred
         | _ -> ()),
      [| box deferred |]
   )

   React.fragment [
      Html.progress [
         attr.style [ style.marginBottom 5 ]
         attr.custom ("data-transactions-loader", "")

         match txns with
         | Some(Deferred.Resolved _) -> attr.value 100
         | _ -> ()
      ]

      classyNode Html.figure [ "control-panel-and-table-container" ] [
         renderControlPanel state dispatch categories

         match txns with
         | Some(Resolved(Ok None)) -> Html.p "No transactions found."
         | Some(Resolved(Ok(Some txns))) ->
            // Combine txns matching query with txns received in real-time
            // if no filters applied.
            // TODO: Display real-time events separately & without regard to
            //       applied filters.
            let txns =
               if noFilterSelected then
                  signalRCtx.RealtimeEvents @ txns
                  |> List.distinctBy (AccountEnvelope.unwrap >> snd >> _.Id)
               else
                  txns

            renderTable account txns browserQuery.Transaction merchants dispatch
         | _ -> ()
      ]
   ]
