module TransactionTable

open Feliz
open Feliz.UseElmish
open Feliz.Router
open Elmish

open Lib.SharedTypes
open UIDomain
open UIDomain.Account
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open Lib.NetworkQuery
open TableControlPanel
open EmployeeSearch

[<RequireQualifiedAccess>]
type TransactionFilterView =
   | Date
   | Amount
   | Category
   | InitiatedBy
   | Cards
   | EventType
   | Accounts

[<RequireQualifiedAccess>]
type TransactionFilter =
   | Date of DateFilter option
   | MoneyFlow of MoneyFlow option
   | Amount of AmountFilter option
   | Category of CategoryFilter option
   | Cards of (SelectedCard list) option
   | InitiatedBy of (UIDomain.Employee.SelectedEmployee list) option
   | EventFilter of (TransactionGroupFilter list) option
   | Accounts of (SelectedAccount list) option

type State = {
   Transactions: Map<int, Deferred<TransactionsMaybe>>
   Query: TransactionQuery
}

type Msg =
   | UpdateFilter of TransactionFilter
   | ResetPageIndex
   | LoadTransactions of
      TransactionQuery *
      AsyncOperationStatus<TransactionsMaybe>
   | RefreshTransactions of TransactionQuery
   | ViewTransaction of TransactionId

let init (txnQuery: TransactionQuery) () =
   {
      Transactions = Map [ 1, Deferred.Idle ]
      Query = txnQuery
   },
   Cmd.none

let update msg state =
   match msg with
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
         | TransactionFilter.InitiatedBy selected -> {
            browserQuery with
               SelectedInitiatedBy = selected
           }
         | TransactionFilter.Accounts accounts -> {
            browserQuery with
               Accounts = accounts
           }
         | TransactionFilter.EventFilter filter -> {
            browserQuery with
               EventType = filter
           }

      state, Cmd.navigate (Routes.TransactionsUrl.queryPath browserQuery)
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
            Query = { query with Page = 1 }
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
   | ViewTransaction(txnId) ->
      let path =
         Routes.TransactionsUrl.queryPath {
            Routes.IndexUrl.accountBrowserQuery () with
               Action = None
               Transaction = Some txnId
         }

      state, Cmd.navigate path

let renderPagination state dispatch =
   Pagination.render {|
      PaginatedResults = state.Transactions
      Page = state.Query.Page
      OnPageChange =
         fun page ->
            dispatch
            <| Msg.LoadTransactions({ state.Query with Page = page }, Started)
      OnPageReset = fun () -> dispatch ResetPageIndex
   |}

let renderControlPanel
   state
   dispatch
   (org: OrgWithAccountProfiles)
   (categories: Map<int, TransactionCategory>)
   (session: UserSession)
   =
   let query = Routes.IndexUrl.accountBrowserQuery ()

   TableControlPanelComponent {|
      FilterViewOptions = [
         TransactionFilterView.Date, "Date"
         TransactionFilterView.Amount, "Amount"
         TransactionFilterView.EventType, "Transaction Type"
         TransactionFilterView.Accounts, "Accounts"
         TransactionFilterView.InitiatedBy, "Initiated By"
         TransactionFilterView.Cards, "Cards"
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
         | TransactionFilterView.InitiatedBy ->
            EmployeeMultiSelectSearchComponent {|
               OrgId = session.OrgId
               Selected = query.SelectedInitiatedBy
               OnSelect =
                  TransactionFilter.InitiatedBy >> Msg.UpdateFilter >> dispatch
               Dependencies =
                  Some [| string TransactionFilterView.InitiatedBy |]
            |}
         | TransactionFilterView.Accounts ->
            CheckboxFieldset.render {|
               Options =
                  org.Accounts.Values
                  |> List.ofSeq
                  |> List.map (fun a -> {
                     Id = {
                        AccountId = a.AccountId
                        Display = a.FullName
                     }
                     Display = a.FullName
                  })
               SelectedItems = query.Accounts
               OnChange =
                  TransactionFilter.Accounts >> Msg.UpdateFilter >> dispatch
            |}
         | TransactionFilterView.Cards ->
            EmployeeCardMultiSelectSearchComponent {|
               OrgId = session.OrgId
               Selected = query.SelectedCards
               OnSelect =
                  TransactionFilter.Cards >> Msg.UpdateFilter >> dispatch
               Dependencies = Some [| string TransactionFilterView.Cards |]
            |}
         | TransactionFilterView.EventType ->
            CheckboxFieldset.render {|
               Options =
                  [
                     TransactionGroupFilter.Purchase
                     TransactionGroupFilter.Deposit
                     TransactionGroupFilter.InternalTransferWithinOrg
                     TransactionGroupFilter.InternalTransferBetweenOrgs
                     TransactionGroupFilter.InternalAutomatedTransfer
                     TransactionGroupFilter.DomesticTransfer
                     TransactionGroupFilter.PlatformPayment
                  ]
                  |> List.map (fun o -> { Id = o; Display = o.Display })
               SelectedItems = query.EventType
               OnChange =
                  TransactionFilter.EventFilter >> Msg.UpdateFilter >> dispatch
            |}
      FilterPills =
         [
            {
               View = TransactionFilterView.Date
               OnDelete =
                  fun () ->
                     dispatch <| Msg.UpdateFilter(TransactionFilter.Date None)
               Content =
                  state.Query.DateRange
                  |> Option.map DateFilter.dateRangeDisplay
            }
            {
               View = TransactionFilterView.EventType
               OnDelete =
                  fun () ->
                     TransactionFilter.EventFilter None
                     |> Msg.UpdateFilter
                     |> dispatch
               Content =
                  state.Query.EventType
                  |> Option.map TransactionGroupFilter.listToDisplay
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
                     dispatch
                     <| Msg.UpdateFilter(TransactionFilter.MoneyFlow None)
               Content = query.MoneyFlow |> Option.map MoneyFlow.display
            }
            {
               View = TransactionFilterView.Category
               OnDelete =
                  fun () ->
                     dispatch
                     <| Msg.UpdateFilter(TransactionFilter.Category None)
               Content =
                  query.Category
                  |> Option.bind (CategoryFilter.display categories)
            }
         ]
         @ [
            match query.Accounts with
            | None -> ()
            | Some selected ->
               for account in selected ->
                  {
                     View = TransactionFilterView.Accounts
                     OnDelete =
                        fun () ->
                           selected
                           |> List.filter (fun e ->
                              e.AccountId <> account.AccountId)
                           |> fun es ->
                              (if es.Length = 0 then None else Some es)
                              |> TransactionFilter.Accounts
                              |> Msg.UpdateFilter
                              |> dispatch
                     Content = Some account.Display
                  }

            match query.SelectedInitiatedBy with
            | None -> ()
            | Some selected ->
               for employee in selected ->
                  {
                     View = TransactionFilterView.InitiatedBy
                     OnDelete =
                        fun () ->
                           selected
                           |> List.filter (fun e -> e.Id <> employee.Id)
                           |> fun es ->
                              (if es.Length = 0 then None else Some es)
                              |> TransactionFilter.InitiatedBy
                              |> Msg.UpdateFilter
                              |> dispatch
                     Content = Some $"Initiated By: {employee.Name}"
                  }

            match query.SelectedCards with
            | None -> ()
            | Some selected ->
               for card in selected ->
                  {
                     View = TransactionFilterView.Cards
                     OnDelete =
                        fun () ->
                           selected
                           |> List.filter (fun e -> e.CardId <> card.CardId)
                           |> fun es ->
                              (if es.Length = 0 then None else Some es)
                              |> TransactionFilter.Cards
                              |> Msg.UpdateFilter
                              |> dispatch
                     Content = Some card.Display
                  }
         ]
      SubsequentChildren = Some [ renderPagination state dispatch ]
   |}

let renderTableRow
   (selectedTxnId: TransactionId option)
   (txn: Transaction.T)
   (txnDisplay: TransactionUIFriendly)
   dispatch
   =
   Html.tr [
      attr.key (string txn.Id)

      match txn.Status with
      | Transaction.TransactionStatus.Complete -> ()
      | Transaction.TransactionStatus.InProgress -> attr.classes [ "warning" ]
      | Transaction.TransactionStatus.Failed -> attr.classes [ "error" ]

      match selectedTxnId with
      | Some txnId when txnId = txn.Id -> attr.classes [ "selected" ]
      | _ -> ()

      attr.onClick (fun _ -> dispatch (Msg.ViewTransaction txn.Id))

      attr.children [
         Html.th [ attr.scope "row" ]

         Html.td [
            attr.classes [
               match txnDisplay.MoneyFlow with
               | None -> ()
               | Some MoneyFlow.In -> "credit"
               | Some MoneyFlow.Out -> "debit"
            ]

            attr.text (txnDisplay.Amount |> Option.defaultValue "-")
         ]

         Html.td txnDisplay.Info

         Html.td txnDisplay.Date
      ]
   ]

let renderTable
   (txns: Transaction.T seq)
   (selectedTxnId: TransactionId option)
   (displayTransaction: Transaction.T -> TransactionUIFriendly)
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
               renderTableRow
                  selectedTxnId
                  txn
                  (displayTransaction txn)
                  dispatch
         ]
      ]
   ]

[<ReactComponent>]
let TransactionTableComponent
   (org: OrgWithAccountProfiles)
   (session: UserSession)
   =
   let categories = React.useContext TransactionCategoryProvider.context
   let merchants = React.useContext MerchantProvider.stateContext
   let signalRCtx = React.useContext SignalRAccountEventProvider.context
   let browserQuery = Routes.IndexUrl.accountBrowserQuery ()

   let txnQuery =
      TransactionService.transactionQueryFromAccountBrowserQuery
         org.Org.OrgId
         browserQuery

   let state, dispatch = React.useElmish (init txnQuery, update, [||])

   React.useEffect (
      fun () -> dispatch (Msg.RefreshTransactions txnQuery)
      , [| box org.Org.OrgId; box browserQuery.ChangeDetection |]
   )

   let txns = Map.tryFind state.Query.Page state.Transactions

   let displayTransaction (txn: Transaction.T) =
      let txn = {
         txn with
            Events =
               txn.Events
               |> List.map (function
                  | AccountEvent.DebitedAccount e ->
                     AccountEvent.DebitedAccount(
                        debitWithMerchantAlias e merchants
                     )
                  | evt -> evt)
      }

      transactionUIFriendly org txn

   React.fragment [
      Html.progress [
         attr.style [ style.marginBottom 5 ]
         attr.custom ("data-transactions-loader", "")

         match txns with
         | Some(Deferred.Resolved _) -> attr.value 100
         | _ -> ()
      ]

      classyNode Html.figure [ "control-panel-and-table-container" ] [
         renderControlPanel state dispatch org categories session

         match txns with
         | Some(Resolved(Ok None)) -> Html.p "No transactions found."
         | Some(Resolved(Ok(Some txns))) ->
            let txns =
               signalRCtx.RealtimeEvents
               |> List.filter (
                  keepRealtimeEventsCorrespondingToSelectedFilter state.Query
               )
               |> List.fold Transaction.applyAccountEvent txns
               |> _.Values
               |> Seq.sortByDescending _.Timestamp

            renderTable
               txns
               browserQuery.Transaction
               displayTransaction
               dispatch

            renderPagination state dispatch
         | _ -> ()
      ]
   ]
