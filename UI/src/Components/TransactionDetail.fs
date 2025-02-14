module TransactionDetail

open Feliz
open Feliz.UseElmish
open Feliz.Router
open Elmish
open System

open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open UIDomain.Account
open UIDomain.Org
open Bank.Transfer.Domain
open Lib.SharedTypes
open Dropdown
open Transaction

type private TransactionMaybe =
   Deferred<Result<TransactionWithAncillaryInfo option, Err>>

type State = {
   TransactionId: TransactionId
   Transaction: TransactionMaybe
   // Nickname may refer to transfer recipient or merchant
   // depending on the AccountEvent rendered.
   EditingNickname: bool
   NicknamePersistence: Deferred<Result<EventId, Err>>
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

type RecipientNicknameEditMsg = {
   CommandInput: NicknamedDomesticTransferRecipient
   Org: Org
   InitiatedBy: InitiatedById
}

type Msg =
   | GetTransactionInfo of
      AsyncOperationStatus<Result<TransactionWithAncillaryInfo option, Err>>
   | SaveCategory of
      TransactionCategory option *
      AsyncOperationStatus<Result<int, Err>>
   | SaveNote of note: string * AsyncOperationStatus<Result<int, Err>>
   | ToggleNicknameEdit
   | SaveRecipientNickname of
      RecipientNicknameEditMsg *
      AsyncOperationStatus<Result<OrgCommandReceipt, Err>>
   | SaveMerchantNickname of Merchant * AsyncOperationStatus<Result<int, Err>>
   | EditTransferRecipient of recipientId: AccountId

let init txnId () =
   {
      TransactionId = txnId
      Transaction = Deferred.Idle
      EditingNickname = false
      NicknamePersistence = Deferred.Idle
      GetTxnAttempt = 1
   },
   Cmd.ofMsg (GetTransactionInfo Started)

let update (merchantDispatch: MerchantProvider.Dispatch) msg state =
   match msg with
   | GetTransactionInfo Started ->
      let getInfo = async {
         let! res = TransactionService.getTransactionInfo state.TransactionId

         return GetTransactionInfo(Finished res)
      }

      state, Cmd.fromAsync getInfo
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
   | SaveRecipientNickname(edit, Started) ->
      let command =
         NicknameDomesticTransferRecipientCommand.create
            edit.Org.OrgId
            edit.InitiatedBy
            edit.CommandInput
         |> OrgCommand.NicknameDomesticTransferRecipient

      let submitCommand = async {
         let! res = OrgService.submitCommand edit.Org command
         return Msg.SaveRecipientNickname(edit, Finished res)
      }

      {
         state with
            NicknamePersistence = Deferred.InProgress
      },
      Cmd.fromAsync submitCommand
   | SaveRecipientNickname(_, Finished(Ok _)) ->
      {
         state with
            EditingNickname = false
            NicknamePersistence = Deferred.Idle
      },
      Cmd.none
   | SaveRecipientNickname(_, Finished(Error err)) ->
      {
         state with
            NicknamePersistence = Deferred.Resolved(Error err)
      },
      Alerts.toastCommand err
   | SaveMerchantNickname(merchant, Started) ->
      let updateMerchant = async {
         let! res = OrgService.updateMerchant merchant
         return SaveMerchantNickname(merchant, Finished res)
      }

      {
         state with
            NicknamePersistence = Deferred.InProgress
      },
      Cmd.fromAsync updateMerchant
   | SaveMerchantNickname(merchant, Finished(Ok _)) ->
      // Update parent context of merchant aliases via context reducer.
      merchantDispatch (MerchantProvider.Action.UpdateMerchantAlias merchant)

      {
         state with
            EditingNickname = false
            NicknamePersistence = Deferred.Idle
      },
      Cmd.none
   | SaveMerchantNickname(_, Finished(Error err)) ->
      {
         state with
            NicknamePersistence = Deferred.Resolved(Error err)
      },
      Alerts.toastCommand err
   | EditTransferRecipient recipientId ->
      let browserQuery = Routes.IndexUrl.accountBrowserQuery ()

      let path =
         {
            browserQuery with
               Transaction = None
               Action =
                  Some(AccountActionView.EditTransferRecipient recipientId)
         }
         |> Routes.TransactionsUrl.queryPath

      state, Cmd.navigate path

let private nicknameCancelButton dispatch =
   Html.a [
      attr.href ""
      attr.text "Cancel"
      attr.style [ style.padding 10 ]
      attr.classes [ "secondary" ]
      attr.onClick (fun e ->
         e.preventDefault ()
         dispatch Msg.ToggleNicknameEdit)
   ]

let private nicknameSaveButton onClick =
   Html.a [
      attr.href ""
      attr.text "Save"
      attr.style [ style.padding 10 ]
      attr.onClick (fun e ->
         e.preventDefault ()
         onClick ())
   ]

[<ReactComponent>]
let RecipientNicknameEditComponent
   (session: UserSession)
   (org: Org)
   dispatch
   (recipientId: AccountId)
   (recipientEnv: RecipientAccountEnvironment)
   =
   let name, nickname =
      org.DomesticTransferRecipients
      |> Map.tryFind recipientId
      |> Option.map (fun r -> r.Name, r.Nickname)
      |> Option.defaultValue ("", None)

   let pendingNickname, setNickname = React.useState nickname

   let nicknameInputRef = React.useInputRef ()

   React.useEffectOnce (fun () ->
      match nicknameInputRef.current with
      | None -> ()
      | Some input -> input.focus ())

   classyNode Html.div [ "nickname" ] [
      Html.input [
         attr.ref nicknameInputRef
         attr.ariaLabel "Transfer Recipient Nickname"
         attr.type' "text"
         attr.placeholder "Edit recipient nickname"

         attr.value (pendingNickname |> Option.defaultValue "")

         attr.onChange (fun input ->
            if String.IsNullOrWhiteSpace input then None else Some input
            |> setNickname)
      ]

      if pendingNickname = (Some name) then
         Html.small $"No change from original name {name}."
      elif pendingNickname <> nickname then
         match pendingNickname with
         | None ->
            Html.small
               $"Transactions will display as {name} for past and future transactions."
         | Some alias ->
            Html.small
               $"Transactions for {name} will display as {alias} for past and future transactions."

      classyNode Html.div [ "nickname-controls" ] [
         if pendingNickname = (Some name) then
            nicknameCancelButton dispatch
         elif pendingNickname <> nickname then
            nicknameCancelButton dispatch

            nicknameSaveButton (fun () ->
               let msg =
                  Msg.SaveRecipientNickname(
                     {
                        CommandInput = {
                           RecipientId = recipientId
                           RecipientAccountEnvironment = recipientEnv
                           Nickname = pendingNickname
                        }
                        Org = org
                        InitiatedBy = (InitiatedById session.EmployeeId)

                     },
                     Started
                  )

               dispatch msg)
      ]
   ]

[<ReactComponent>]
let MerchantNicknameEditComponent
   (orgId: OrgId)
   (debitMerchant: string)
   (merchants: Map<string, Merchant>)
   dispatch
   =
   let merchantAlias = getMerchantAlias debitMerchant merchants
   let pendingNickname, setNickname = React.useState merchantAlias

   let nicknameInputRef = React.useInputRef ()

   React.useEffectOnce (fun () ->
      match nicknameInputRef.current with
      | None -> ()
      | Some input -> input.focus ())

   classyNode Html.div [ "nickname" ] [
      Html.input [
         attr.ref nicknameInputRef
         attr.ariaLabel "Merchant Recipient Nickname"
         attr.type' "text"
         attr.placeholder "Edit merchant nickname"

         attr.value (pendingNickname |> Option.defaultValue "")

         attr.onChange (fun input ->
            if String.IsNullOrWhiteSpace input then None else Some input
            |> setNickname)
      ]

      if pendingNickname = (Some debitMerchant) then
         Html.small $"No change from original name {debitMerchant}."
      elif pendingNickname <> merchantAlias then
         match pendingNickname with
         | None ->
            Html.small
               $"Transactions will display with the original name {debitMerchant} for past and future transactions."
         | Some name ->
            Html.small
               $"Transactions for {debitMerchant} will display as {name} for past and future transactions."

      classyNode Html.div [ "nickname-controls" ] [
         if pendingNickname = (Some debitMerchant) then
            nicknameCancelButton dispatch
         elif pendingNickname <> merchantAlias then
            nicknameCancelButton dispatch

            nicknameSaveButton (fun () ->
               let msg =
                  Msg.SaveMerchantNickname(
                     {
                        OrgId = orgId
                        Name = debitMerchant.ToLower()
                        Alias = pendingNickname
                     },
                     Started
                  )

               dispatch msg)
      ]
   ]

/// May edit transfer recipient if domestic and status is not Closed.
let private canEditTransferRecipient
   (org: Org)
   (txn: Transaction.T)
   : DomesticTransferRecipient option
   =
   let recipientIdOpt =
      txn.Events
      |> List.tryPick (function
         | AccountEvent.DomesticTransferPending e ->
            Some e.Data.BaseInfo.Recipient.RecipientAccountId
         | _ -> None)

   recipientIdOpt
   |> Option.bind (fun recipientId ->
      Map.tryFind recipientId org.DomesticTransferRecipients)
   |> Option.filter (fun r -> r.Status <> RecipientRegistrationStatus.Closed)

let private canAddCategoryAndNotes =
   function
   | TransactionType.InternalTransferBetweenOrgs
   | TransactionType.DomesticTransfer
   | TransactionType.Payment
   | TransactionType.Purchase -> true
   | _ -> false

let renderTransactionInfo
   (org: OrgWithAccountProfiles)
   (txnInfo: TransactionWithAncillaryInfo)
   (isEditingNickname: bool)
   (merchants: Map<string, Merchant>)
   (session: UserSession)
   dispatch
   =
   let txn = transactionUIFriendly org txnInfo.Transaction

   let RecipientNicknameEditComponent =
      RecipientNicknameEditComponent session org.Org dispatch

   React.fragment [
      Html.h6 txn.Name

      Html.section [
         match txn.Amount with
         | Some amount -> Html.h3 amount
         | None -> ()

         Html.small [
            attr.text txn.Date
            attr.style [ style.color "var(--primary)" ]
         ]
      ]

      Html.section [
         Html.div [
            Html.small "Status:"
            Html.p [
               attr.style [ style.display.inlineBlock; style.marginLeft 10 ]
               attr.text txnInfo.Transaction.Status.Display
            ]
         ]

         match txn.Source with
         | Some source ->
            Html.div [
               Html.small "From:"
               Html.p [
                  attr.style [ style.display.inlineBlock; style.marginLeft 10 ]
                  attr.text source
               ]
            ]
         | None -> ()

         match txnInfo.Transaction.Type with
         | TransactionType.DomesticTransfer when isEditingNickname ->
            match canEditTransferRecipient org.Org txnInfo.Transaction with
            | Some recipient ->
               RecipientNicknameEditComponent
                  recipient.RecipientAccountId
                  RecipientAccountEnvironment.Domestic
            | None -> ()
         | TransactionType.Purchase when isEditingNickname ->
            let merchant =
               txnInfo.Transaction.Events
               |> List.tryLast
               |> Option.bind (function
                  | AccountEvent.DebitedAccount e ->
                     getMerchantAlias e.Data.Merchant merchants
                     |> Option.defaultValue e.Data.Merchant
                     |> Some
                  | _ -> None)

            match merchant with
            | Some merchant ->
               MerchantNicknameEditComponent
                  txnInfo.Transaction.OrgId
                  merchant
                  merchants
                  dispatch
            | None -> ()
         | _ ->
            match txn.Destination with
            | Some destination ->
               Html.div [
                  Html.small "To:"
                  Html.p [
                     attr.style [
                        style.display.inlineBlock
                        style.marginLeft 10
                     ]
                     attr.text destination
                  ]
               ]
            | None -> ()

         classyNode Html.section [ "transaction-detail-history" ] [
            Html.small "History:"
            Html.ul [
               for t in txnInfo.Transaction.Events do
                  Html.li [
                     match t with
                     | AccountEvent.DomesticTransferPending e ->
                        Html.p
                           $"Funds deducted from {e.Data.BaseInfo.Sender.Name}"
                     | AccountEvent.DomesticTransferProgress e ->
                        Html.p $"Progress Update: {e.Data.InProgressInfo}"
                     | AccountEvent.DomesticTransferFailed e ->
                        Html.p $"Failed: {e.Data.Reason.Display}"

                        Html.p
                           $"Refunded account: {e.Data.BaseInfo.Sender.Name}"
                     | AccountEvent.DomesticTransferCompleted _ ->
                        Html.p "ACH transfer processor completed transfer"
                     | AccountEvent.DomesticTransferScheduled e ->
                        let date =
                           DateTime.dateUIFriendly e.Data.BaseInfo.ScheduledDate

                        Html.p $"Scheduled for {date}"
                     | AccountEvent.PlatformPaymentRequested e ->
                        Html.p (
                           if e.OrgId = org.Org.OrgId then
                              $"Payment request sent to {txn.Source}"
                           else
                              $"Received payment request from {txn.Destination}"
                        )
                     | AccountEvent.PlatformPaymentPaid _ ->
                        Html.p $"Payment fulfilled by {txn.Source}"
                     | AccountEvent.PlatformPaymentDeposited _ ->
                        Html.p $"Deposit received by {txn.Destination}"
                     | AccountEvent.DepositedCash e ->
                        Html.p $"Deposited money via {e.Data.Origin}"
                     | AccountEvent.DebitedAccount e ->
                        let em = e.Data.EmployeePurchaseReference

                        Html.p
                           $"Purchase by {em.EmployeeName}'s card **{em.EmployeeCardNumberLast4}"
                     | AccountEvent.InternalTransferWithinOrgPending e ->
                        Html.p
                           $"Funds deducted from {e.Data.BaseInfo.Sender.Name}"
                     | AccountEvent.InternalTransferWithinOrgCompleted _ ->
                        Html.p "Deduction of funds finalized"
                     | AccountEvent.InternalTransferWithinOrgDeposited e ->
                        Html.p
                           $"Funds deposited to {e.Data.BaseInfo.Recipient.Name}"
                     | AccountEvent.InternalTransferBetweenOrgsScheduled e ->
                        let date =
                           DateTime.dateUIFriendly e.Data.BaseInfo.ScheduledDate

                        Html.p $"Scheduled for {date}"
                     | AccountEvent.InternalTransferBetweenOrgsPending _ ->
                        Html.p $"Funds deducted from {txn.Source}"
                     | AccountEvent.InternalTransferBetweenOrgsCompleted _ ->
                        Html.p "Deduction of funds finalized"
                     | AccountEvent.InternalTransferBetweenOrgsDeposited e ->
                        Html.p
                           $"Funds deposited to {e.Data.BaseInfo.Recipient.Name}"
                     | AccountEvent.InternalTransferBetweenOrgsFailed e ->
                        Html.p $"Failed: {e.Data.Reason}"
                     | AccountEvent.InternalAutomatedTransferPending _ ->
                        Html.p $"Funds deducted from {txn.Source}"
                     | AccountEvent.InternalAutomatedTransferCompleted _ ->
                        Html.p "Deduction of funds finalized"
                     | AccountEvent.InternalAutomatedTransferDeposited e ->
                        Html.p
                           $"Funds deposited to {e.Data.BaseInfo.Recipient.Name}"
                     | AccountEvent.InternalAutomatedTransferFailed e ->
                        Html.p $"Failed: {e.Data.Reason}"
                     | _ -> Html.p "Unknown"

                     AccountEnvelope.unwrap t
                     |> snd
                     |> _.Timestamp
                     |> DateTime.dateUIFriendlyWithSeconds
                     |> Html.small
                  ]
            ]
         ]
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
   (org: Org)
   (txnInfo: TransactionMaybe)
   (isEditingNickname: bool)
   dispatch
   =
   let dropdownItems =
      match txnInfo with
      | Deferred.Resolved(Ok(Some txn)) ->
         match txn.Transaction.Type with
         | TransactionType.DomesticTransfer ->
            Some(
               [
                  {
                     Text = "Nickname recipient"
                     OnClick = fun _ -> dispatch ToggleNicknameEdit
                     IsSelected = isEditingNickname
                  }

               ]
               @ match canEditTransferRecipient org txn.Transaction with
                 | None -> []
                 | Some recipient -> [
                    {
                       Text = "Edit recipient"
                       OnClick =
                          fun _ ->
                             recipient.RecipientAccountId
                             |> Msg.EditTransferRecipient
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
   =
   let merchants = React.useContext MerchantProvider.stateContext
   let merchantDispatchCtx = React.useContext MerchantProvider.dispatchContext

   let state, dispatch =
      React.useElmish (init txnId, update merchantDispatchCtx, [| box txnId |])

   let categories = React.useContext TransactionCategoryProvider.context

   classyNode Html.article [ "transaction-detail" ] [
      CloseButton.render (fun _ ->
         {
            Routes.IndexUrl.accountBrowserQuery () with
               Transaction = None
         }
         |> Routes.TransactionsUrl.queryPath
         |> Router.navigate)

      match state.Transaction with
      | Deferred.Resolved(Ok(Some txn)) ->
         renderTransactionInfo
            org
            txn
            state.EditingNickname
            merchants
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
      | _ -> ()

      Html.section [
         attr.style [ style.position.relative ]

         attr.children [
            renderFooterMenuControls
               org.Org
               state.Transaction
               state.EditingNickname
               dispatch
         ]
      ]
   ]
