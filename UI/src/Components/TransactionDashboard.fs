module TransactionDashboard

open Feliz
open Feliz.Router
open Feliz.UseElmish
open Elmish
open Fable.Core.JsInterop
open Fable.FontAwesome

open UIDomain.Account
open Bank.Account.Forms
open Bank.Org.Forms
open Bank.Employee.Forms
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes
open EmployeeSearch
open TransactionDetail

let navigation (view: AccountActionView option) =
   {
      Routes.IndexUrl.accountBrowserQuery () with
         Action = view
   }
   |> Routes.TransactionsUrl.queryPath

type State = { PendingAction: Envelope option }

type Msg =
   | Cancel
   | NetworkAckCommand of Envelope
   | AccountEventReceived of CorrelationId
   | CheckForEventConfirmation of Envelope * attemptNumber: int
   | SubmitCommandForApproval of action: string
   | Noop

let init () = { PendingAction = None }, Cmd.none

// HTTP request returned 200. Command accepted by network.  Wait
// for account actor cluster to successfully process the command into
// an event and send out a confirmation via SignalR.
let networkAck state (envelope: Envelope) =
   let state = {
      state with
         PendingAction = Some envelope
   }

   let delayedMsg = Msg.CheckForEventConfirmation(envelope, 1)

   state, Cmd.fromTimeout 3000 delayedMsg

let update
   (handlePollingConfirmation: AccountEventPersistedConfirmation list -> unit)
   msg
   (state: State)
   =
   match msg with
   | Cancel -> state, Cmd.navigate (navigation None)
   | NetworkAckCommand envelope -> networkAck state envelope
   | AccountEventReceived _ when state.PendingAction.IsNone -> state, Cmd.none
   | AccountEventReceived correlationId ->
      match state.PendingAction with
      | Some envelope when envelope.CorrelationId = correlationId ->
         { state with PendingAction = None }, Cmd.navigate (navigation None)
      | _ -> state, Cmd.none
   // Verify the PendingAction was persisted.
   // If a SignalR event doesn't dispatch a Msg.AccountEventReceived within
   // a few seconds of the initial network request to process the command then
   // assume the SignalR message or connection was dropped. Revert to
   // polling for the latest account read model state.
   | CheckForEventConfirmation(commandResponse, attemptNumber) ->
      let checkAgainMsg =
         Msg.CheckForEventConfirmation(commandResponse, attemptNumber + 1)

      if attemptNumber > 10 then
         Log.error
            "Could not confirm event was processed after several attempts."

         state, Cmd.none
      else
         match state.PendingAction with
         | Some action when action.CorrelationId = commandResponse.CorrelationId ->
            let getReadModel = async {
               let! confirmationMaybe =
                  TransactionService.getCorrelatedTransactionConfirmations
                     commandResponse.CorrelationId

               match confirmationMaybe with
               | Error e ->
                  Log.error $"Error checking for txn confirmation. {e}"
                  return Msg.Noop
               | Ok None ->
                  do! Async.Sleep 2500
                  return checkAgainMsg
               | Ok(Some conf) ->
                  handlePollingConfirmation conf
                  return Msg.Noop
            }

            state, Cmd.fromAsync getReadModel
         | _ -> state, Cmd.none
   | SubmitCommandForApproval action ->
      state,
      Cmd.batch [
         Alerts.toastSuccessCommand $"Submitted {action} for approval."
         Cmd.navigate (navigation None)
      ]
   | Noop -> state, Cmd.none

let private renderMenuButton (view: AccountActionView) =
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

let private renderActionMenu () =
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
         renderMenuButton AccountActionView.RegisterTransferRecipient
      ]
   ]

let private renderAccountActions
   dispatch
   orgDispatch
   session
   org
   (view: AccountActionView)
   =
   classyNode Html.article [ "form-wrapper" ] [
      Html.h6 view.Display

      CloseButton.render (fun _ -> dispatch Cancel)

      match view with
      | AccountActionView.Deposit ->
         DepositForm.DepositFormComponent
            session
            org
            (_.Envelope >> Msg.NetworkAckCommand >> dispatch)
      | AccountActionView.RegisterTransferRecipient ->
         RegisterTransferRecipientForm.RegisterTransferRecipientFormComponent
            session
            org.Org
            None
            (fun conf ->
               match conf.PendingEvent with
               | OrgEvent.RegisteredDomesticTransferRecipient e ->
                  conf.PendingCommand
                  |> OrgProvider.Msg.OrgCommand
                  |> orgDispatch

                  let redirectTo =
                     (RecipientAccountEnvironment.Domestic,
                      e.Data.Recipient.RecipientAccountId)
                     |> Some
                     |> AccountActionView.Transfer
                     |> Some

                  Router.navigate (navigation redirectTo)
               | evt ->
                  Log.error
                     $"Unknown evt {evt} in RegisterTransferRecipient submit handler")
      | AccountActionView.EditTransferRecipient accountId ->
         RegisterTransferRecipientForm.RegisterTransferRecipientFormComponent
            session
            org.Org
            (Some accountId)
            (fun conf ->
               match conf.PendingEvent with
               | OrgEvent.EditedDomesticTransferRecipient _ ->
                  conf.PendingCommand
                  |> OrgProvider.Msg.OrgCommand
                  |> orgDispatch

                  Router.navigate (navigation None)
               | evt ->
                  Log.error
                     $"Unknown evt {evt} in EditTransferRecipient submit handler")
      | AccountActionView.Transfer selectedRecipient ->
         TransferForm.TransferFormComponent
            session
            org
            selectedRecipient
            (_.Envelope >> Msg.NetworkAckCommand >> dispatch)
            (fun approvableTransfer ->
               approvableTransfer
               |> OrgCommand.RequestCommandApproval
               |> OrgProvider.Msg.OrgCommand
               |> orgDispatch

               dispatch (Msg.SubmitCommandForApproval "transfer"))
      | AccountActionView.Purchase ->
         EmployeeCardSelectSearchComponent {|
            OrgId = org.Org.OrgId
            MakeChildrenOnSelect =
               Some
               <| fun card employee -> [
                  PurchaseForm.PurchaseFormComponent
                     org
                     card.CardId
                     employee
                     (_.Envelope >> Msg.NetworkAckCommand >> dispatch)
               ]
            OnSelect = None
         |}
   ]

[<ReactComponent>]
let TransactionDashboardComponent
   (url: Routes.TransactionsUrl)
   (session: UserSession)
   =
   let orgCtx = React.useContext OrgProvider.context
   let orgDispatch = React.useContext OrgProvider.dispatchContext

   let signalRDispatch =
      React.useContext SignalRAccountEventProvider.dispatchContext

   // Did not receive a SignalR event in time so reverted to polling.
   // If the polling confirmation succeeds then update the SignalR context
   // to mimick a SignalR AccountEventPersisted event being received.
   let handlePollingConfirmation
      (confs: AccountEventPersistedConfirmation list)
      =
      for conf in confs do
         signalRDispatch
         <| SignalRAccountEventProvider.Msg.AccountEventPersisted conf

   let _, dispatch =
      React.useElmish (
         init,
         update handlePollingConfirmation,
         [| box session.OrgId |]
      )

   SignalRAccountEventProvider.useAccountEventSubscription {
      ComponentName = "AccountAction"
      OrgId = Some session.OrgId
      OnReceive =
         fun conf ->
            // Update account context so AccountSummary & AccountSelection
            // components are up to date with the latest balance
            // & other metrics info. Ensure we are using current account info
            // when attempting to initiate transactions against an account.
            orgDispatch (OrgProvider.Msg.AccountUpdated conf)

            // Handle closing the ScreenOverlayPortal containing the
            // submitted form or redirecting to another form.
            conf.EventPersisted
            |> AccountEnvelope.unwrap
            |> snd
            |> _.CorrelationId
            |> Msg.AccountEventReceived
            |> dispatch
   }

   classyNode Html.div [ "transaction-dashboard" ] [
      ServiceHealth.ServiceHealthComponent()

      classyNode Html.main [ "container-fluid" ] [
         classyNode Html.div [ "grid" ] [
            Html.section [
               Html.h4 "Transactions"

               match orgCtx with
               | Deferred.Resolved(Ok(Some org)) ->
                  TransactionTable.TransactionTableComponent org session
               | _ -> Html.progress []
            ]

            match orgCtx, Routes.IndexUrl.accountBrowserQuery().Action with
            | Deferred.Resolved(Ok(Some org)), Some action ->
               renderAccountActions dispatch orgDispatch session org action
               |> ScreenOverlay.Portal
            | _ -> Html.aside [ renderActionMenu () ]

            match Routes.TransactionsUrl.transactionIdMaybe url with
            | Some txnId ->
               match orgCtx with
               | Deferred.Resolved(Ok(Some org)) ->
                  TransactionDetailComponent session org txnId
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
