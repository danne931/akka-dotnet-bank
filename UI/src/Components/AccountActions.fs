module AccountActions

open Feliz
open Feliz.UseElmish
open Feliz.Router
open Elmish

open Bank.Account.Domain
open UIDomain.Account
open Bank.Account.Forms
open Bank.Org.Forms
open Bank.Employee.Domain
open Bank.Employee.Forms
open Bank.Transfer.Domain
open Bank.Org.Domain
open Lib.SharedTypes
open EmployeeSearch

let navigation (accountId: AccountId) (view: AccountActionView option) =
   let queryString =
      {
         Routes.IndexUrl.accountBrowserQuery () with
            Action = view
      }
      |> AccountBrowserQuery.toQueryParams
      |> Router.encodeQueryString

   [| Routes.TransactionUrl.BasePath; string accountId; queryString |]

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
   (accountId: AccountId)
   msg
   (state: State)
   =
   let navigation = navigation accountId

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

[<ReactComponent>]
let AccountActionsComponent
   (session: UserSession)
   (account: Account)
   (org: OrgWithAccountProfiles)
   (view: AccountActionView)
   =
   let signalRDispatch =
      React.useContext SignalRAccountEventProvider.dispatchContext

   let orgDispatch = React.useContext OrgProvider.dispatchContext

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
         update handlePollingConfirmation account.AccountId,
         [| box account.AccountId |]
      )

   SignalRAccountEventProvider.useAccountEventSubscription {
      ComponentName = "AccountAction"
      AccountId = Some account.AccountId
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

   classyNode Html.article [ "form-wrapper" ] [
      Html.h6 (
         match view with
         | AccountActionView.RegisterTransferRecipient ->
            "Add a Transfer Recipient"
         | AccountActionView.EditTransferRecipient _ ->
            "Edit Transfer Recipient"
         | AccountActionView.Transfer _ -> "Transfer Money"
         | AccountActionView.Purchase -> "Purchase"
         | AccountActionView.Deposit -> "Deposit Cash"
      )

      CloseButton.render (fun _ -> dispatch Cancel)

      match view with
      | AccountActionView.Deposit ->
         DepositForm.DepositFormComponent
            session
            account
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
                      e.Data.Recipient.AccountId)
                     |> Some
                     |> AccountActionView.Transfer
                     |> Some

                  Router.navigate (navigation account.AccountId redirectTo)
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

                  Router.navigate (navigation account.AccountId None)
               | evt ->
                  Log.error
                     $"Unknown evt {evt} in EditTransferRecipient submit handler")
      | AccountActionView.Transfer selectedRecipient ->
         TransferForm.TransferFormComponent
            session
            account
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
            OrgId = account.OrgId
            MakeChildrenOnSelect =
               Some
               <| fun card employee -> [
                  PurchaseForm.PurchaseFormComponent
                     (_.Envelope >> Msg.NetworkAckCommand >> dispatch)
                     account
                     card.CardId
                     employee
               ]
            OnSelect = None
         |}
   ]
