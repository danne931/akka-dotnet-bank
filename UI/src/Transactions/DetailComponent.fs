module TransactionDetail

open Feliz
open Feliz.UseElmish
open Feliz.Router
open Elmish

open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open UIDomain.Account
open Bank.Transfer.Domain
open Bank.Purchase.Domain
open Lib.SharedTypes
open Dropdown
open Transaction

type private TransactionMaybe =
   Deferred<Result<TransactionWithAncillaryInfo option, Err>>

type State = {
   TransactionId: TransactionId
   Transaction: TransactionMaybe
   // Nickname may refer to external account or
   // merchant of purchase depending on the AccountEvent rendered.
   EditingNickname: bool
   // Sometimes the user clicks on a persisted event in the table
   // before the read model is synced, resulting in a 404 and
   // nothing to display in this view.  Reattempt fetching the
   // transaction info if this is the case.
   GetTxnAttempt: int
}

let private updateTransaction
   (state: State)
   (transform: TransactionWithAncillaryInfo -> TransactionWithAncillaryInfo)
   =
   {
      state with
         Transaction =
            (Deferred.map << Result.map << Option.map)
               transform
               state.Transaction
   }

type Msg =
   | GetTransactionInfo of
      AsyncOperationStatus<Result<TransactionWithAncillaryInfo option, Err>>
   | SaveCategory of
      TransactionCategory option *
      AsyncOperationStatus<Result<int, Err>>
   | SaveNote of note: string * AsyncOperationStatus<Result<int, Err>>
   | ToggleNicknameEdit
   | EditCounterparty of CounterpartyId

let init txnId () =
   {
      TransactionId = txnId
      Transaction = Deferred.Idle
      EditingNickname = false
      GetTxnAttempt = 1
   },
   Cmd.ofMsg (GetTransactionInfo Started)

let update msg state =
   match msg with
   | GetTransactionInfo Started ->
      let getInfo = async {
         let! res = TransactionService.getTransactionInfo state.TransactionId

         return GetTransactionInfo(Finished res)
      }

      {
         state with
            Transaction = Deferred.InProgress
      },
      Cmd.fromAsync getInfo
   | GetTransactionInfo(Finished(Ok(Some res))) ->
      {
         state with
            Transaction = res |> Some |> Ok |> Deferred.Resolved
      },
      Cmd.none
   | GetTransactionInfo(Finished(Error err)) ->
      Log.error $"Error fetching ancillary txn info: {err}"

      {
         state with
            Transaction = Deferred.Resolved(Error err)
      },
      Alerts.toastCommand err
   | GetTransactionInfo(Finished(Ok None)) ->
      let attempt = state.GetTxnAttempt

      if attempt <= 4 then
         let delayedReattempt = async {
            do! Async.Sleep(1500 * attempt)
            Log.info $"Reattempt fetching transaction info {attempt}."
            return GetTransactionInfo Started
         }

         {
            state with
               GetTxnAttempt = attempt + 1
         },
         Cmd.fromAsync delayedReattempt
      else
         let err = Err.NetworkError(exn "Unable to resolve transaction.")

         {
            state with
               Transaction = Deferred.Resolved(Error err)
         },
         Alerts.toastCommand err
   | SaveCategory(category, Started) ->
      let updateCategory = async {
         let! res =
            match category with
            | None -> TransactionService.deleteCategory state.TransactionId
            | Some category ->
               TransactionService.updateCategory state.TransactionId category.Id

         return SaveCategory(category, Finished res)
      }

      state, Cmd.fromAsync updateCategory
   | SaveCategory(category, Finished(Ok _)) ->
      updateTransaction state (fun txn -> { txn with Category = category }),
      Cmd.none
   | SaveCategory(_, Finished(Error err)) ->
      Log.error $"Error selecting txn category: {err}"
      state, Alerts.toastCommand err
   | SaveNote(note, Started) ->
      let updateNote = async {
         let! res = TransactionService.updateNote state.TransactionId note

         return SaveNote(note, Finished res)
      }

      state, Cmd.fromAsync updateNote
   | SaveNote(note, Finished(Ok _)) ->
      updateTransaction state (fun txn -> { txn with Note = Some note }),
      Cmd.none
   | SaveNote(_, Finished(Error err)) ->
      Log.error $"Error updating note: {err}"
      state, Alerts.toastCommand err
   | ToggleNicknameEdit ->
      {
         state with
            EditingNickname = not state.EditingNickname
      },
      Cmd.none
   | EditCounterparty cpId ->
      let browserQuery = Routes.IndexUrl.transactionBrowserQuery ()

      let path =
         {
            browserQuery with
               Transaction = None
               Action = Some(AccountActionView.EditCounterparty cpId)
         }
         |> Routes.TransactionsUrl.queryPath

      state, Cmd.navigate path

let private canEditCounterparty
   (counterparties: Map<CounterpartyId, Counterparty>)
   (txn: Transaction)
   : Counterparty option
   =
   txn.History
   |> List.tryPick (function
      | History.Account accountHistory ->
         match accountHistory.Event with
         | AccountEvent.DomesticTransferPending e ->
            Some e.Data.BaseInfo.Counterparty.CounterpartyId
         | _ -> None
      | _ -> None)
   |> Option.bind (fun cpId -> Map.tryFind cpId counterparties)

let private canAddCategoryAndNotes =
   function
   | TransactionType.InternalTransferBetweenOrgs
   | TransactionType.DomesticTransfer
   | TransactionType.Payment
   | TransactionType.Purchase -> true
   | _ -> false

let private renderLabeledInfo (label: string) (text: string) =
   Html.div [
      Html.small $"{label}:"
      Html.p [
         attr.style [ style.display.inlineBlock; style.marginLeft 10 ]
         attr.text text
      ]
   ]

let private renderTransactionHistory
   (session: UserSession)
   (txn: TransactionUIFriendly)
   (history: History list)
   =
   classyNode Html.section [ "transaction-detail-history" ] [
      Html.small "History:"
      Html.ul [
         for t in history do
            Html.li [
               match t with
               | History.ParentAccount _ -> ()
               | History.Org orgHistory ->
                  match orgHistory.Event with
                  | OrgEvent.CommandApprovalRequested e ->
                     let requester = e.Data.Requester.EmployeeName
                     Html.p $"Request by {requester} pending approval"
                  | OrgEvent.CommandApprovalAcquired e ->
                     let approver = e.Data.ApprovedBy.EmployeeName
                     Html.p $"Approval acquired from {approver}"
                  | OrgEvent.CommandApprovalDeclined e ->
                     Html.p
                        $"Approval declined by {e.Data.DeclinedBy.EmployeeName}"
                  | OrgEvent.CommandApprovalTerminated e ->
                     Html.p $"Approval terminated early due to {e.Data.Reason}"
                  | OrgEvent.CommandApprovalProcessCompleted e ->
                     Html.p
                        $"Approval completed with approval from {e.Data.ApprovedBy.EmployeeName}"
                  | _ -> ()
               | History.Employee employeeHistory ->
                  match employeeHistory.Event with
                  | EmployeeEvent.PurchasePending e ->
                     Html.p
                        $"Reserved {Money.format e.Data.Info.Amount} from card"
                  | EmployeeEvent.PurchaseProgress e ->
                     let status = e.Data.Info.Status

                     for e in e.Data.Info.Events do
                        Html.p (
                           match e.Type with
                           | PurchaseEventType.Auth ->
                              $"Authorization of {Money.format e.Amount} confirmed by card network"
                           | PurchaseEventType.FinancialAuth ->
                              let status =
                                 match status with
                                 | PurchaseStatus.Declined -> "declined"
                                 | _ -> "approved"

                              $"{Money.format e.Amount} {status} by card network"
                           | PurchaseEventType.AuthAdvice ->
                              $"Auth Advice {Money.format e.Amount} received from card network"
                           | PurchaseEventType.Clearing ->
                              $"Clearing of {Money.format e.Amount} confirmed by card network"
                           | PurchaseEventType.AuthExpiry ->
                              "Auth Expiry received from card network"
                           | PurchaseEventType.AuthReversal ->
                              "Merchant reversed authorization"
                           | _ -> $"Unknown - {e.Type}"
                        )
                  | EmployeeEvent.PurchaseSettled e ->
                     Html.p
                        $"Settled deduction of {Money.format e.Data.Info.Amount} from card"
                  | EmployeeEvent.PurchaseRefunded e ->
                     Html.p $"Purchase refunded to card due to {e.Data.Reason}"
                  | EmployeeEvent.PurchaseFailed e ->
                     Html.p
                        $"Funds released from card reserve due to {e.Data.Reason}"
                  | _ -> ()
               | History.Account accountHistory ->
                  match accountHistory.Event with
                  | AccountEvent.DomesticTransferPending e ->
                     Html.p
                        $"Funds reserved from {e.Data.BaseInfo.Originator.Name}"
                  | AccountEvent.DomesticTransferProgress _ ->
                     Html.p $"Partner bank processing transfer"
                  | AccountEvent.DomesticTransferFailed e ->
                     Html.p $"Failed: {e.Data.Reason.Display}"

                     Html.p
                        $"Refunded account: {e.Data.BaseInfo.Originator.Name}"
                  | AccountEvent.DomesticTransferSettled _ ->
                     Html.p "Partner bank settled transfer"
                  | AccountEvent.DomesticTransferScheduled e ->
                     let date =
                        DateTime.dateUIFriendly e.Data.BaseInfo.ScheduledDate

                     Html.p $"Scheduled for {date}"
                  | AccountEvent.PaymentRequested e ->
                     Html.p (
                        if e.OrgId = session.OrgId then
                           $"Payment request sent to {txn.Source}"
                        else
                           $"Received payment request from {txn.Destination}"
                     )
                  | AccountEvent.DepositedCash e ->
                     Html.p $"Deposited money via {e.Data.Origin}"
                  | AccountEvent.DebitPending e ->
                     Html.p
                        $"Reserved {Money.format e.Data.Amount} from account"
                  | AccountEvent.DebitSettled e ->
                     Html.p
                        $"Settled deduction of {Money.format e.Data.Amount} from account"
                  | AccountEvent.DebitFailed _ ->
                     Html.p "Funds released from account reserve"
                  | AccountEvent.DebitRefunded e ->
                     Html.p $"Account refund applied due to {e.Data.Reason}"
                  | AccountEvent.InternalTransferWithinOrgDeducted e ->
                     Html.p $"Funds deducted from {e.Data.BaseInfo.Sender.Name}"
                  | AccountEvent.InternalTransferWithinOrgDeposited e ->
                     Html.p
                        $"Funds deposited to {e.Data.BaseInfo.Recipient.Name}"
                  | AccountEvent.InternalTransferBetweenOrgsScheduled e ->
                     let date =
                        DateTime.dateUIFriendly e.Data.BaseInfo.ScheduledDate

                     Html.p $"Scheduled for {date}"
                  | AccountEvent.InternalTransferBetweenOrgsPending _ ->
                     Html.p $"Transfer processing from {txn.Source}"
                  | AccountEvent.InternalTransferBetweenOrgsDeposited e ->
                     Html.p
                        $"Funds deposited to {e.Data.BaseInfo.Recipient.Name}"
                  | AccountEvent.InternalTransferBetweenOrgsSettled _ ->
                     Html.p "Transfer settled"
                  | AccountEvent.InternalTransferBetweenOrgsFailed e ->
                     Html.p $"Failed: {e.Data.Reason}"
                  | AccountEvent.InternalAutomatedTransferDeducted _ ->
                     Html.p $"Funds deducted from {txn.Source}"
                  | AccountEvent.InternalAutomatedTransferDeposited e ->
                     Html.p
                        $"Funds deposited to {e.Data.BaseInfo.Recipient.Name}"
                  | _ -> Html.p "Unknown"

               Html.small (
                  DateTime.dateUIFriendlyWithSeconds t.Envelope.Timestamp
               )
            ]
      ]
   ]

let renderTransactionInfo
   (org: OrgWithAccountProfiles)
   (txnInfo: TransactionWithAncillaryInfo)
   (isEditingNickname: bool)
   (merchants: Map<string, Merchant>)
   (merchantDispatch: MerchantProvider.Dispatch)
   (session: UserSession)
   dispatch
   =
   let txn = transactionUIFriendly merchants org txnInfo.Transaction

   let merchantNameFromPurchase =
      txnInfo.Transaction.History
      |> List.tryPick (function
         | History.Account accountHistory ->
            match accountHistory.Event with
            | AccountEvent.DebitPending e -> Some e.Data.Merchant
            | _ -> None
         | _ -> None)

   let editableCounterparty =
      canEditCounterparty org.Counterparties txnInfo.Transaction

   let expectedSettlementDate =
      match txnInfo.Transaction.Type with
      | TransactionType.DomesticTransfer ->
         txnInfo.Transaction.History
         |> List.choose (function
            | History.Account h ->
               match h.Event with
               | AccountEvent.DomesticTransferPending e ->
                  Some(e.Timestamp, e.Data.ExpectedSettlementDate)
               | AccountEvent.DomesticTransferProgress e ->
                  e.Data.NewExpectedSettlementDate
                  |> Option.map (fun date -> e.Timestamp, date)
               | _ -> None
            | _ -> None)
         |> List.sortByDescending fst
         |> List.tryHead
         |> Option.map snd
      | _ -> None

   let originatedFromPayment =
      match txnInfo.Transaction.Type with
      | TransactionType.Payment ->
         txnInfo.Transaction.History
         |> List.tryPick (function
            | History.Account h ->
               match h.Event with
               | AccountEvent.InternalTransferBetweenOrgsPending e ->
                  e.Data.BaseInfo.FromPaymentRequest
               | _ -> None
            | _ -> None)
      | _ -> None

   React.fragment [
      Html.h6 txn.Name

      Html.section [
         Html.h3 txn.Amount

         Html.small [
            attr.text txn.Date
            attr.style [ style.color Style.color.primary ]
         ]
      ]

      Html.section [
         renderLabeledInfo "Status" txnInfo.Transaction.Status.Display
         renderLabeledInfo "From" txn.Source

         match
            isEditingNickname,
            txnInfo.Transaction.Type,
            merchantNameFromPurchase,
            editableCounterparty
         with
         | true, TransactionType.DomesticTransfer, _, Some cp ->
            let counterpartyId = cp.CounterpartyId
            let counterparties = org.Counterparties
            let org = org.Org

            let originalName, nickname =
               counterparties
               |> Map.tryFind counterpartyId
               |> Option.map (fun r -> r.Name, r.Nickname)
               |> Option.defaultValue ("", None)

            Nickname.NicknameEditComponent {|
               OriginalName = originalName
               Alias = nickname
               onCancel = fun () -> dispatch Msg.ToggleNicknameEdit
               persistNickname =
                  fun alias -> async {
                     let command =
                        NicknameCounterpartyCommand.create
                           org.OrgId
                           org.ParentAccountId
                           session.AsInitiator
                           {
                              CounterpartyId = counterpartyId
                              Nickname = alias |> Option.map _.Trim()
                           }
                        |> ParentAccountCommand.NicknameCounterparty

                     let! res =
                        AccountService.submitParentAccountCommand command

                     return res |> Result.map ignore
                  }
               onNicknameSaved = ignore
            |}
         | true, TransactionType.Purchase, Some merchant, _ ->
            let merchantAlias =
               getMerchant merchant merchants |> Option.bind _.Alias

            let merchantFromNickname (nickname: string option) = {
               OrgId = session.OrgId
               Name = merchant.ToLower()
               Alias = nickname |> Option.map _.Trim()
            }

            Nickname.NicknameEditComponent {|
               OriginalName = merchant
               Alias = merchantAlias
               onCancel = fun () -> dispatch Msg.ToggleNicknameEdit
               persistNickname =
                  fun alias -> async {
                     let! res =
                        OrgService.updateMerchant (merchantFromNickname alias)

                     return res |> Result.map ignore
                  }
               onNicknameSaved =
                  fun alias ->
                     let merchant = merchantFromNickname alias
                     // Update parent context of merchant aliases via context reducer.
                     MerchantProvider.Action.UpdateMerchantAlias merchant
                     |> merchantDispatch
            |}
         | _ -> renderLabeledInfo "To" txn.Destination

         match expectedSettlementDate with
         | Some date ->
            renderLabeledInfo
               "Funds Will Arrive By"
               (DateTime.dateUIFriendly date)
         | None -> ()

         renderTransactionHistory session txn txnInfo.Transaction.History

         match originatedFromPayment with
         | Some paymentId ->
            Html.button [
               attr.classes [ "outline" ]
               attr.text "View Payment Request"
               attr.onClick (fun _ ->
                  Router.navigate (Routes.PaymentUrl.selectedPath paymentId))
            ]
         | None -> ()
      ]
   ]

let renderCategorySelect
   (categories: Map<int, TransactionCategory>)
   (txnInfo: TransactionWithAncillaryInfo)
   dispatch
   =
   React.fragment [
      Html.label [ Html.text "Category:" ]
      Html.select [
         attr.onChange (fun (catId: string) ->
            Msg.SaveCategory(Map.tryFind (int catId) categories, Started)
            |> dispatch)

         txnInfo.Category
         |> Option.map _.Id
         |> Option.defaultValue 0
         |> attr.value

         attr.children [
            Html.option [ attr.value 0; attr.text "None" ]

            for category in categories.Values do
               Html.option [ attr.value category.Id; attr.text category.Name ]
         ]
      ]
   ]

let renderNoteInput (txnInfo: TransactionMaybe) dispatch =
   React.fragment [
      Html.label [ Html.text "Notes:" ]
      Html.input [
         attr.type' "text"
         attr.placeholder "Add a note"

         match txnInfo with
         | Deferred.Resolved(Ok(Some txnInfo)) ->
            attr.key (string txnInfo.Id)

            attr.defaultValue (txnInfo.Note |> Option.defaultValue "")
         | _ -> attr.disabled true

         attr.onChange (
            throttleUncontrolledInput 2500 (fun note ->
               SaveNote(note, Started) |> dispatch)
         )
      ]
   ]

let renderFooterMenuControls
   (org: OrgWithAccountProfiles)
   (txnInfo: TransactionMaybe)
   (isEditingNickname: bool)
   dispatch
   =
   let dropdownItems =
      match txnInfo with
      | Deferred.Resolved(Ok(Some txn)) ->
         let isEditable = canEditCounterparty org.Counterparties txn.Transaction

         match txn.Transaction.Type with
         | TransactionType.DomesticTransfer ->
            Some(
               [
                  {
                     Text = "Nickname external account"
                     OnClick = fun _ -> dispatch ToggleNicknameEdit
                     IsSelected = isEditingNickname
                  }

               ]
               @ match isEditable with
                 | None -> []
                 | Some cp -> [
                    {
                       Text = "Edit external account"
                       OnClick =
                          fun _ ->
                             cp.CounterpartyId
                             |> Msg.EditCounterparty
                             |> dispatch
                       IsSelected = isEditingNickname
                    }
                   ]
            )
         | TransactionType.Purchase ->
            Some [
               {
                  Text = "Edit merchant nickname"
                  OnClick = fun _ -> dispatch ToggleNicknameEdit
                  IsSelected = isEditingNickname
               }
            ]
         | _ -> None
      | _ -> None

   React.fragment [
      match dropdownItems with
      | None -> ()
      | Some items ->
         DropdownComponent {|
            Direction = DropdownDirection.RTL
            ShowCaret = false
            Button = None
            Items = items
         |}
   ]

[<ReactComponent>]
let TransactionDetailComponent
   (session: UserSession)
   (org: OrgWithAccountProfiles)
   (txnId: TransactionId)
   (txn: Transaction option)
   =
   let state, dispatch = React.useElmish (init txnId, update, [| box txnId |])

   let categories = React.useContext TransactionCategoryProvider.context
   let merchants = React.useContext MerchantProvider.stateContext
   let merchantProvider = React.useContext MerchantProvider.dispatchContext

   // If we arrived at this TransactionDetail component by clicking on a
   // transaction table row then we want it to display immediately
   // while fetching the ancillary info (category & note) in the background.
   // If we arrived at this TransactionDetail component by a link from
   // somewhere else in the app or elsewhere then the Transaction data
   // will not be immediately available and we will display loading progress
   // while retrieving the TransactionWithAncillaryInfo.
   let txnFromTable =
      match txn with
      | Some txn ->
         {
            Id = txnId
            Transaction = txn
            Category = None
            Note = None
         }
         |> Some
         |> Ok
         |> Deferred.Resolved
      | None -> Deferred.Idle

   let txnFromQueryOrTable =
      match state.Transaction, txnFromTable with
      | Deferred.Resolved(Ok(Some fromQuery)),
        Deferred.Resolved(Ok(Some fromTable)) ->
         // Ensure incoming real-time events are displayed
         if
            fromQuery.Transaction.History.Length < fromTable.Transaction.History.Length
         then
            {
               fromQuery with
                  Transaction = fromTable.Transaction
            }
            |> Some
            |> Ok
            |> Deferred.Resolved
         else
            state.Transaction
      | _, Deferred.Resolved(Ok(Some _)) -> txnFromTable
      | _ -> state.Transaction

   classyNode Html.article [ "transaction-detail" ] [
      CloseButton.render (fun _ ->
         {
            Routes.IndexUrl.transactionBrowserQuery () with
               Transaction = None
         }
         |> Routes.TransactionsUrl.queryPath
         |> Router.navigate)

      match txnFromQueryOrTable with
      | Deferred.Resolved(Ok(Some txn)) ->
         renderTransactionInfo
            org
            txn
            state.EditingNickname
            merchants
            merchantProvider
            session
            dispatch
      | Deferred.Resolved(Ok None) -> Html.p "No transaction found."
      | _ -> Html.progress []

      match state.Transaction with
      | Deferred.Resolved(Ok(Some txnInfo)) ->
         if canAddCategoryAndNotes txnInfo.Transaction.Type then
            Html.section [
               renderCategorySelect categories txnInfo dispatch
               renderNoteInput state.Transaction dispatch
            ]
      | Deferred.InProgress ->
         // Show progress of fetching TransactionWithAncillaryInfo while
         // showing transaction data immediately available from table.
         if txn.IsSome then
            Html.progress []
      | _ -> ()

      Html.section [
         attr.style [ style.position.relative ]

         attr.children [
            renderFooterMenuControls
               org
               state.Transaction
               state.EditingNickname
               dispatch
         ]
      ]
   ]
