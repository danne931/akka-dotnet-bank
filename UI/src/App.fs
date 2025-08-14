module App

open Fable.Core.JsInterop
importSideEffects "./Styles/site.sass"

open Feliz
open Feliz.Router
open Browser.Dom

open SignalRConnectionProvider
open SignalREventProvider
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

   let appShell
      (contextProvider: (ReactElement -> ReactElement) option)
      (activePage: UserSession -> Fable.React.ReactElement)
      =
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

                           classyNode Html.section [ "active-page" ] [
                              activePage session
                           ]
                        ]
                     ]
                  ]
               ]
            ]
         ])
      |> UserSessionSuspense
      |> (contextProvider |> Option.defaultValue id)
      |> OrgProvider
      |> UserSessionProvider

   match currentUrl with
   | Routes.IndexUrl.Analytics url ->
      appShell None (fun session ->
         React.suspense (
            [
               React.lazy' (
                  fun () -> importDynamic "./Analytics/DashboardComponent"
                  , {| Session = session; Url = url |}
               )
            ],
            Html.progress []
         ))
   | Routes.IndexUrl.Account url ->
      let contextProviders = SignalRConnectionProvider << SignalREventProvider

      appShell
         (Some contextProviders)
         (AccountDashboard.AccountDashboardComponent url)
   | Routes.IndexUrl.Approvals url ->
      let contextProviders = SignalRConnectionProvider << SignalREventProvider

      appShell
         (Some contextProviders)
         (ApprovalDashboard.ApprovalDashboardComponent url)
   | Routes.IndexUrl.Employees url ->
      let contextProviders = SignalRConnectionProvider << SignalREventProvider

      appShell
         (Some contextProviders)
         (EmployeeDashboard.EmployeeDashboardComponent url)
   | Routes.IndexUrl.History url ->
      let contextProviders = SignalRConnectionProvider << SignalREventProvider

      appShell
         (Some contextProviders)
         (HistoryDashboard.HistoryDashboardComponent url)
   | Routes.IndexUrl.Cards url ->
      let contextProviders = SignalRConnectionProvider << SignalREventProvider

      appShell
         (Some contextProviders)
         (CardDashboard.CardDashboardComponent url)
   | Routes.IndexUrl.Transactions url ->
      let contextProviders =
         SignalRConnectionProvider
         << SignalREventProvider
         << MerchantProvider
         << TransactionCategoryProvider

      appShell
         (Some contextProviders)
         (TransactionDashboard.TransactionDashboardComponent url)
   | Routes.IndexUrl.Payments url ->
      let contextProviders = SignalRConnectionProvider << SignalREventProvider

      appShell
         (Some contextProviders)
         (PaymentDashboard.PaymentDashboardComponent url)
   | Routes.IndexUrl.NotFound -> Html.h1 "Not Found"

let root = ReactDOM.createRoot <| document.getElementById "bank-react-root"

App() |> root.render
