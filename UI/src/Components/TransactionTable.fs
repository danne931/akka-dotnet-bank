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
open Pagination

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
   Query: TransactionQuery
   Pagination: Pagination.State<TransactionCursor, Transaction.T>
}

type Msg =
   | UpdateFilter of TransactionFilter
   | PaginationMsg of Pagination.Msg<Transaction.T>
   | RefreshTransactions of TransactionQuery
   | ViewTransaction of TransactionId

let init (txnQuery: TransactionQuery) () =
   let paginationState, cmd =
      Pagination.init<TransactionCursor, Transaction.T> ()

   {
      Query = txnQuery
      Pagination = paginationState
   },
   Cmd.map PaginationMsg cmd

let handlePaginationMsg (state: State) (msg: Pagination.Msg<Transaction.T>) =
   let config = {
      PageLimit = state.Query.PageLimit
      loadPage =
         fun cursor -> async {
            let query = { state.Query with Cursor = cursor }
            return! TransactionService.getTransactions query
         }
      getCursor =
         fun txn -> {
            Timestamp = txn.Timestamp
            TransactionId = txn.Id
         }
      onLoadError =
         fun page err ->
            Log.error $"Error loading transactions for page {page}: {err}"
   }

   let paginationState, paginationCmd =
      Pagination.update config msg state.Pagination

   {
      state with
         Pagination = paginationState
   },
   Cmd.map PaginationMsg paginationCmd

let update msg state =
   match msg with
   | PaginationMsg msg -> handlePaginationMsg state msg
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
   | RefreshTransactions query ->
      let state = { state with Query = query }
      handlePaginationMsg state Reset
   | ViewTransaction(txnId) ->
      let path =
         Routes.TransactionsUrl.queryPath {
            Routes.IndexUrl.accountBrowserQuery () with
               Action = None
               Transaction = Some txnId
         }

      state, Cmd.navigate path

let renderPagination state dispatch =
   Pagination.render state.Pagination (PaginationMsg >> dispatch)

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
   let signalRCtx = React.useContext SignalREventProvider.context
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

   let txns = Map.tryFind state.Pagination.Page state.Pagination.Items

   let displayTransaction (txn: Transaction.T) =
      let txn = {
         txn with
            Events =
               txn.Events
               |> List.map (function
                  | AccountEvent.DebitedAccount e ->
                     debitWithMerchantAlias e merchants
                     |> AccountEvent.DebitedAccount
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
               if state.Pagination.Page = 1 then
                  let txns = [ for txn in txns -> txn.Id, txn ] |> Map.ofList

                  signalRCtx.RealtimeAccountEvents
                  |> List.filter (
                     keepRealtimeEventsCorrespondingToSelectedFilter state.Query
                  )
                  |> List.fold Transaction.applyAccountEvent txns
                  |> _.Values
                  |> List.ofSeq
                  |> List.sortByDescending _.Timestamp
               else
                  txns

            renderTable
               txns
               browserQuery.Transaction
               displayTransaction
               dispatch

            renderPagination state dispatch
         | _ -> ()
      ]
   ]
