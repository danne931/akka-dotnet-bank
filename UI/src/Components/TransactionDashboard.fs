module TransactionDashboard

open Feliz
open Feliz.Router
open Feliz.UseElmish
open Elmish
open Fable.Core.JsInterop
open Fable.FontAwesome

open UIDomain
open UIDomain.Account
open Bank.Account.Forms
open Bank.Org.Forms
open Bank.Employee.Forms
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes
open Lib.NetworkQuery
open EmployeeSearch
open TransactionDetail
open SignalRBroadcast
open Transaction
open Pagination
open TableControlPanel

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
   | EventFilter of (AccountEventGroupFilter list) option
   | Accounts of (SelectedAccount list) option

let navigation (view: AccountActionView option) =
   {
      Routes.IndexUrl.transactionBrowserQuery () with
         Action = view
   }
   |> Routes.TransactionsUrl.queryPath

type State = {
   Query: TransactionQuery
   Pagination: Pagination.State<TransactionCursor, Transaction>
}

type Msg =
   | UpdateFilter of TransactionFilter
   | PaginationMsg of Pagination.Msg<Transaction>
   | RefreshTransactions of TransactionQuery
   | ViewTransaction of TransactionId
   | CommandProcessed of
      commandName: string *
      redirect: AccountActionView option
   | CommandProcessedPendingApproval of
      commandName: string *
      redirect: AccountActionView option
   | Cancel

let init (txnQuery: TransactionQuery) () =
   let paginationState, cmd = Pagination.init<TransactionCursor, Transaction> ()

   {
      Query = txnQuery
      Pagination = paginationState
   },
   Cmd.map PaginationMsg cmd

let handlePaginationMsg (state: State) (msg: Pagination.Msg<Transaction>) =
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
      let browserQuery = Routes.IndexUrl.transactionBrowserQuery ()

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
            Routes.IndexUrl.transactionBrowserQuery () with
               Action = None
               Transaction = Some txnId
         }

      state, Cmd.navigate path
   | CommandProcessed(commandName, redirectTo) ->
      state,
      Cmd.batch [
         Alerts.toastSuccessCommand $"Submitted {commandName}."
         Cmd.navigate (navigation redirectTo)
      ]
   | CommandProcessedPendingApproval(commandName, redirectTo) ->
      state,
      Cmd.batch [
         Alerts.toastSuccessCommand $"Submitted {commandName} for approval."
         Cmd.navigate (navigation redirectTo)
      ]
   | Cancel -> state, Cmd.navigate (navigation None)

let renderPagination state dispatch =
   Pagination.render state.Pagination (PaginationMsg >> dispatch)

let renderControlPanel
   state
   dispatch
   (org: OrgWithAccountProfiles)
   (categories: Map<int, TransactionCategory>)
   (session: UserSession)
   =
   let query = Routes.IndexUrl.transactionBrowserQuery ()

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
                     AccountEventGroupFilter.Purchase
                     AccountEventGroupFilter.Deposit
                     AccountEventGroupFilter.InternalTransferWithinOrg
                     AccountEventGroupFilter.InternalTransferBetweenOrgs
                     AccountEventGroupFilter.InternalAutomatedTransfer
                     AccountEventGroupFilter.DomesticTransfer
                     AccountEventGroupFilter.PlatformPayment
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
                  |> Option.map AccountEventGroupFilter.listToDisplay
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
   (txn: Transaction)
   (txnDisplay: TransactionUIFriendly)
   dispatch
   =
   Html.tr [
      attr.key (string txn.Id)

      match txn.Status with
      | TransactionStatus.Complete -> ()
      | TransactionStatus.Scheduled
      | TransactionStatus.InProgress -> attr.classes [ "warning" ]
      | TransactionStatus.Failed -> attr.classes [ "error" ]

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

            attr.text txnDisplay.Amount
         ]

         Html.td txnDisplay.Info

         Html.td txn.InitiatedBy.Name

         Html.td txnDisplay.Date
      ]
   ]

let renderTable
   (txns: Transaction seq)
   (selectedTxnId: TransactionId option)
   (displayTransaction: Transaction -> TransactionUIFriendly)
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

               Html.th [ attr.scope "col"; attr.text "Initiated By" ]

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

let renderMenuButton (view: AccountActionView) =
   Html.button [
      attr.classes [ "outline"; "grid" ]

      attr.onClick (fun _ -> navigation (Some view) |> Router.navigate)

      attr.children [
         match view with
         | AccountActionView.Deposit ->
            Html.span [ Fa.i [ Fa.Solid.PiggyBank ] [] ]
         | AccountActionView.Purchase ->
            Html.span [ Fa.i [ Fa.Solid.CreditCard ] [] ]
         | AccountActionView.Transfer _ ->
            Html.span [ Fa.i [ Fa.Solid.ArrowsAltH ] [] ]
         | AccountActionView.RegisterTransferRecipient ->
            Html.span [ Fa.i [ Fa.Solid.UserPlus ] [] ]
         | _ -> ()

         Html.span view.Display
      ]
   ]

let renderForm session org (view: AccountActionView) dispatch =
   classyNode Html.article [ "form-wrapper" ] [
      Html.h6 view.Display

      CloseButton.render (fun _ -> dispatch Msg.Cancel)

      match view with
      | AccountActionView.Deposit ->
         DepositForm.DepositFormComponent session org (fun _ ->
            dispatch (Msg.CommandProcessed("Deposit", None)))
      | AccountActionView.RegisterTransferRecipient ->
         RegisterTransferRecipientForm.RegisterTransferRecipientFormComponent
            session
            org
            None
            (fun conf ->
               match conf.PendingEvent with
               | ParentAccountEvent.RegisteredDomesticTransferRecipient e ->
                  let recipientId = e.Data.Recipient.RecipientAccountId

                  let redirectTo =
                     (RecipientAccountEnvironment.Domestic, recipientId)
                     |> Some
                     |> AccountActionView.Transfer
                     |> Some

                  ("Domestic Transfer Recipient", redirectTo)
                  |> Msg.CommandProcessed
                  |> dispatch
               | evt ->
                  Log.error
                     $"Unknown evt {evt} in RegisterTransferRecipient submit handler")
      | AccountActionView.EditTransferRecipient accountId ->
         RegisterTransferRecipientForm.RegisterTransferRecipientFormComponent
            session
            org
            (Some accountId)
            (fun _ ->
               ("Edit Domestic Transfer Recipient", None)
               |> Msg.CommandProcessed
               |> dispatch)
      | AccountActionView.Transfer selectedRecipient ->
         TransferForm.TransferFormComponent
            session
            org
            selectedRecipient
            (fun _ -> dispatch (Msg.CommandProcessed("Transfer", None)))
            (fun _ ->
               dispatch (Msg.CommandProcessedPendingApproval("Transfer", None)))
      | AccountActionView.Purchase ->
         EmployeeCardSelectSearchComponent {|
            OrgId = org.Org.OrgId
            MakeChildrenOnSelect =
               Some
               <| fun card employee -> [
                  PurchaseForm.PurchaseFormComponent
                     org
                     session
                     card.CardId
                     employee
                     (fun _ ->
                        dispatch (Msg.CommandProcessed("Purchase", None)))
               ]
            OnSelect = None
         |}
   ]

// Combine transactions from network query with real-time data
// received via SignalR.
let transactions
   (txnQuery: TransactionQuery)
   (txns: Transaction list)
   (realtimeAccountEvents: AccountEvent list)
   =
   let historyIds =
      txns |> List.collect _.History |> List.map _.Envelope.Id |> Set.ofList

   let txns = [ for txn in txns -> txn.Id, txn ] |> Map.ofList

   let filterRealtimeEvents =
      keepRealtimeEventsCorrespondingToSelectedFilter txnQuery

   // Should this real-time event be added as a new item in the table?
   // Ex: If we receive a DomesticTransferFailed AccountEvent
   // for a transaction which is not displayed then we do not
   // care to display it.  If on the other hand it was a
   // DomesticTransferPending then we will.
   let mayBeDisplayedAsNewTransaction evt =
      match evt with
      | AccountEvent.DepositedCash _
      | AccountEvent.DebitPending _
      | AccountEvent.InternalTransferWithinOrgDeducted _
      | AccountEvent.InternalTransferBetweenOrgsScheduled _
      | AccountEvent.InternalTransferBetweenOrgsPending _
      | AccountEvent.InternalAutomatedTransferDeducted _
      | AccountEvent.DomesticTransferScheduled _
      | AccountEvent.DomesticTransferPending _
      | AccountEvent.PlatformPaymentPending _
      | AccountEvent.PlatformPaymentDeposited _ -> true
      | _ -> false

   realtimeAccountEvents
   |> List.filter filterRealtimeEvents
   |> List.rev
   |> List.fold
         (fun acc evt ->
            let _, envelope = AccountEnvelope.unwrap evt
            let txnId = TransactionId envelope.CorrelationId

            // Real-time event received for a currently displayed transaction
            let correspondsToDisplayedTransaction = txns.TryFind(txnId).IsSome

            // Real-time event received for a currently displayed
            // transaction that was synthesized from a real-time event.
            let correspondsToDisplayedRealtimeEvent =
               not (mayBeDisplayedAsNewTransaction evt)
               && realtimeAccountEvents
                  |> List.exists (fun e ->
                     let _, env = AccountEnvelope.unwrap e

                     env.CorrelationId = envelope.CorrelationId
                     && mayBeDisplayedAsNewTransaction e)

            // Real-time event already exists in a displayed transaction's history
            let eventAlreadyExists = historyIds.Contains envelope.Id

            if
               not eventAlreadyExists
               && (correspondsToDisplayedTransaction
                   || mayBeDisplayedAsNewTransaction evt
                   || correspondsToDisplayedRealtimeEvent)
            then
               let history =
                  History.Account {
                     InitiatedByName = envelope.InitiatedBy.Name
                     Event = evt
                  }

               Transaction.applyHistory acc history
            else
               acc)
         txns
   |> _.Values
   |> List.ofSeq
   |> List.sortByDescending _.Timestamp

[<ReactComponent>]
let TransactionDashboardComponent
   (url: Routes.TransactionsUrl)
   (session: UserSession)
   =
   let browserQuery = Routes.IndexUrl.transactionBrowserQuery ()

   let txnQuery =
      TransactionService.transactionQueryFromAccountBrowserQuery
         session.OrgId
         browserQuery

   let state, dispatch = React.useElmish (init txnQuery, update, [||])

   let orgCtx = React.useContext OrgProvider.context
   let orgDispatch = React.useContext OrgProvider.dispatchContext
   let categories = React.useContext TransactionCategoryProvider.context
   let merchants = React.useContext MerchantProvider.stateContext
   let signalRCtx = React.useContext SignalREventProvider.context

   React.useEffect (
      fun () -> dispatch (Msg.RefreshTransactions txnQuery)
      , [| box browserQuery.ChangeDetection |]
   )

   let txns = Map.tryFind state.Pagination.Page state.Pagination.Items

   let txns =
      React.useMemo (
         fun () ->
            match txns with
            | Some(Resolved(Ok(Some txns))) ->
               let txns =
                  if state.Pagination.Page = 1 then
                     transactions txnQuery txns signalRCtx.RealtimeAccountEvents
                  else
                     txns

               Some(Resolved(Ok(Some txns)))
            | _ -> txns
         , [| box txns; box signalRCtx.RealtimeAccountEvents |]
      )

   SignalREventProvider.useEventSubscription {
      ComponentName = "TransactionDashboard"
      OrgId = Some session.OrgId
      EventTypes = [
         SignalREventProvider.EventType.ParentAccount
         SignalREventProvider.EventType.Account
         SignalREventProvider.EventType.Org
      ]
      OnPersist =
         fun conf ->
            match conf with
            | EventPersistedConfirmation.Account conf ->
               // Update context so OrgSummary component & account selection
               // form inputs are up to date with the latest balance & other
               // metrics info. Ensure we are using current account info when
               // attempting to initiate transactions against an account.
               orgDispatch (OrgProvider.Msg.AccountUpdated conf)
            | EventPersistedConfirmation.Org conf ->
               orgDispatch (OrgProvider.Msg.OrgUpdated conf.Org)
            | EventPersistedConfirmation.ParentAccount conf ->
               // The ParentAccountSnapshot type is currently not fetched from
               // the server but a portion of its data, it's subaccounts
               // and DomesticTransferRecipients are.  This info is
               // located within the OrgWithAccountProfiles type which is
               // fetched on app startup & updated as SignalR events arrive
               // and are propagated up to the Org context provider.
               // While subaccount related updated are dispatched above via
               // Msg.AccountUpdated, other updates such as
               // DomesticTransferRecipient related ones are dispatched here.
               orgDispatch (OrgProvider.Msg.ParentAccountUpdated conf)
            | _ -> ()
      OnError = ignore
   }

   classyNode Html.div [ "transaction-dashboard" ] [
      ServiceHealth.ServiceHealthComponent()

      classyNode Html.main [ "container-fluid" ] [
         classyNode Html.div [ "grid" ] [
            Html.section [
               Html.h4 "Transactions"

               match orgCtx with
               | Deferred.Resolved(Ok(Some org)) ->
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
                     | Some(Resolved(Ok None)) ->
                        Html.p "No transactions found."
                     | Some(Resolved(Ok(Some txns))) ->
                        renderTable
                           txns
                           browserQuery.Transaction
                           (transactionUIFriendly merchants org)
                           dispatch

                        renderPagination state dispatch
                     | _ -> ()
                  ]
               | _ -> Html.progress []
            ]

            match orgCtx, Routes.IndexUrl.transactionBrowserQuery().Action with
            | Deferred.Resolved(Ok(Some org)), Some action ->
               renderForm session org action dispatch |> ScreenOverlay.Portal
            | _ ->
               Html.aside [
                  classyNode Html.div [ "action-menu" ] [
                     classyNode Html.div [ "grid" ] [
                        renderMenuButton AccountActionView.Purchase
                     ]

                     classyNode Html.div [ "grid" ] [
                        renderMenuButton AccountActionView.Deposit
                     ]

                     classyNode Html.div [ "grid" ] [
                        renderMenuButton (AccountActionView.Transfer None)
                     ]

                     classyNode Html.div [ "grid" ] [
                        renderMenuButton
                           AccountActionView.RegisterTransferRecipient
                     ]
                  ]
               ]

            match Routes.TransactionsUrl.transactionIdMaybe url with
            | Some txnId ->
               match orgCtx, txns with
               | Deferred.Resolved(Ok(Some org)),
                 Some(Deferred.Resolved(Ok(Some txns))) ->
                  TransactionDetailComponent
                     session
                     org
                     txnId
                     (txns |> List.tryFind (fun t -> t.Id = txnId))
               | _ -> Html.progress []
               |> ScreenOverlay.Portal
            | None -> ()
         ]
      ]

      match orgCtx with
      | Deferred.Resolved(Ok(Some org)) ->
         React.suspense (
            [ React.lazy' ((fun () -> importDynamic "./OrgSummary"), org) ],
            Html.progress []
         )
      | _ -> ()
   ]
