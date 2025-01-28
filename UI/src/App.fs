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
open Bank.Employee.Domain

[<ReactComponent>]
let App () =
   let currentUrl, setUrl =
      React.useState (Routes.IndexUrl.parse <| Router.currentUrl ())

   React.useEffect (
      fun () -> ScreenOverlay.manageVisibility currentUrl
      , [| box (string currentUrl) |]
   )

   let appShell (activePage: UserSession -> Fable.React.ReactElement) =
      (fun (session: UserSession) ->
         React.strictMode [
            Navigation.NavigationComponent()

            Html.div [
               React.router [
                  router.onUrlChanged (Routes.IndexUrl.parse >> setUrl)
                  router.children [
                     classyNode Html.div [ "container-fluid"; "app-shell" ] [
                        classyNode Html.div [ "grid" ] [
                           classyNode Html.aside [ "menu" ] [
                              SidebarMenu.SidebarMenuComponent
                                 currentUrl
                                 session
                           ]

                           Html.section [ activePage session ]
                        ]
                     ]
                  ]
               ]
            ]
         ])
      |> UserSessionSuspense
      |> OrgProvider
      |> UserSessionProvider

   match currentUrl with
   | Routes.IndexUrl.Analytics url ->
      appShell (fun session ->
         React.suspense (
            [
               React.lazy' (
                  fun () -> importDynamic "./Components/AnalyticsDashboard"
                  , {| Session = session; Url = url |}
               )
            ],
            Html.progress []
         ))
   | Routes.IndexUrl.Account url ->
      appShell (AccountDashboard.AccountDashboardComponent url)
   | Routes.IndexUrl.Approvals url ->
      appShell (ApprovalDashboard.ApprovalDashboardComponent url)
   | Routes.IndexUrl.Employees url ->
      appShell (EmployeeDashboard.EmployeeDashboardComponent url)
   | Routes.IndexUrl.EmployeeHistory url ->
      appShell (EmployeeHistoryDashboard.EmployeeHistoryDashboardComponent url)
   | Routes.IndexUrl.Cards url ->
      appShell (CardDashboard.CardDashboardComponent url)
   | Routes.IndexUrl.Transaction url ->
      TransactionDashboard.TransactionDashboardComponent url
      |> appShell
      |> (SignalRConnectionProvider
          << SignalRAccountEventProvider
          << MerchantProvider
          << TransactionCategoryProvider)
   | Routes.IndexUrl.Payments url ->
      appShell (PaymentDashboard.PaymentDashboardComponent url)
   | Routes.IndexUrl.NotFound -> Html.h1 "Not Found"

let root = ReactDOM.createRoot <| document.getElementById "bank-react-root"

App() |> root.render
