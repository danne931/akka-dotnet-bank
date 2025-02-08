module TransactionDashboard

open Feliz
open Fable.Core.JsInterop

open Bank.Employee.Domain
open UIDomain.Account
open TransactionDetail

[<ReactComponent>]
let TransactionDashboardComponent
   (url: Routes.TransactionsUrl)
   (session: UserSession)
   =
   let orgCtx = React.useContext OrgProvider.context

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
               AccountActions.AccountActionsComponent session org action
               |> ScreenOverlay.Portal
            | _ -> Html.aside [ AccountActionMenu.render () ]

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
