module App

open Fable.Core.JsInterop
importSideEffects "./Styles/site.sass"

open Feliz
open Feliz.Router
open Browser.Dom

open SignalRConnectionProvider
open SignalRAccountEventProvider
open TransactionCategoryProvider
open UserSessionProvider
open MerchantProvider
open OrgProvider

[<ReactComponent>]
let App () =
   let currentUrl, setUrl =
      React.useState (Routes.IndexUrl.parse <| Router.currentUrl ())

   React.useEffect (
      fun () -> ScreenOverlay.manageVisibility currentUrl
      , [| box (string currentUrl) |]
   )

   let activePage =
      match currentUrl with
      | Routes.IndexUrl.Analytics url ->
         UserSessionSuspense(fun session ->
            React.suspense (
               [
                  React.lazy' (
                     fun () -> importDynamic "./Components/AnalyticsDashboard"
                     , {| Session = session; Url = url |}
                  )
               ],
               Html.progress []
            ))
         |> OrgProvider
      | Routes.IndexUrl.Account url ->
         AccountDashboard.AccountDashboardComponent url
         |> UserSessionSuspense
         |> OrgProvider
      | Routes.IndexUrl.Approvals url ->
         ApprovalDashboard.ApprovalDashboardComponent url
         |> UserSessionSuspense
         |> OrgProvider
      | Routes.IndexUrl.Employees url ->
         EmployeeDashboard.EmployeeDashboardComponent url
         |> UserSessionSuspense
         |> OrgProvider
      | Routes.IndexUrl.EmployeeHistory url ->
         EmployeeHistoryDashboard.EmployeeHistoryDashboardComponent url
         |> UserSessionSuspense
         |> OrgProvider
      | Routes.IndexUrl.Cards url ->
         CardDashboard.CardDashboardComponent url
         |> UserSessionSuspense
         |> OrgProvider
      | Routes.IndexUrl.Transaction url ->
         TransactionDashboard.TransactionDashboardComponent url
         |> UserSessionSuspense
         |> (OrgProvider
             << SignalRConnectionProvider
             << SignalRAccountEventProvider
             << MerchantProvider
             << TransactionCategoryProvider)
      | Routes.IndexUrl.Payments url ->
         PaymentDashboard.PaymentDashboardComponent url
         |> UserSessionSuspense
         |> OrgProvider
      | Routes.IndexUrl.NotFound -> Html.h1 "Not Found"

   [
      Navigation.NavigationComponent()

      Html.div [
         React.router [
            router.onUrlChanged (Routes.IndexUrl.parse >> setUrl)
            router.children [
               classyNode Html.div [ "container-fluid"; "app-shell" ] [
                  classyNode Html.div [ "grid" ] [
                     classyNode Html.aside [ "menu" ] [
                        SidebarMenu.SidebarMenuComponent currentUrl
                        |> UserSessionSuspense
                        |> OrgProvider
                     ]

                     Html.section [ activePage ]
                  ]
               ]
            ]
         ]
      ]
   ]
   |> React.strictMode

let root = ReactDOM.createRoot <| document.getElementById "bank-react-root"

App() |> UserSessionProvider |> root.render
