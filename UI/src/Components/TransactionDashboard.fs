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
open Bank.Employee.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes
open EmployeeSearch
open TransactionDetail
open SignalRBroadcast

let navigation (view: AccountActionView option) =
   {
      Routes.IndexUrl.accountBrowserQuery () with
         Action = view
   }
   |> Routes.TransactionsUrl.queryPath

type State = { Noop: bool }

type Msg =
   | CommandProcessed of
      commandName: string *
      redirect: AccountActionView option
   | CommandProcessedPendingApproval of
      commandName: string *
      redirect: AccountActionView option
   | Cancel

let init () = { Noop = true }, Cmd.none

let update msg state =
   match msg with
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
   session
   org
   (view: AccountActionView)
   dispatch
   =
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
            org.Org
            None
            (fun conf ->
               match conf.PendingEvent with
               | OrgEvent.RegisteredDomesticTransferRecipient e ->
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
            org.Org
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

[<ReactComponent>]
let TransactionDashboardComponent
   (url: Routes.TransactionsUrl)
   (session: UserSession)
   =
   let orgCtx = React.useContext OrgProvider.context
   let orgDispatch = React.useContext OrgProvider.dispatchContext

   let _, dispatch = React.useElmish (init, update, [||])

   SignalREventProvider.useEventSubscription {
      ComponentName = "TransactionDashboard"
      OrgId = Some session.OrgId
      EventTypes = [
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
                  TransactionTable.TransactionTableComponent org session
               | _ -> Html.progress []
            ]

            match orgCtx, Routes.IndexUrl.accountBrowserQuery().Action with
            | Deferred.Resolved(Ok(Some org)), Some action ->
               renderAccountActions session org action dispatch
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
