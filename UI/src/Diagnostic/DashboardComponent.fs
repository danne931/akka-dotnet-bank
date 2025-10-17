module DiagnosticDashboard

open Feliz

open Bank.Employee.Domain

[<ReactComponent>]
let DiagnosticDashboardComponent
   (url: Routes.DiagnosticUrl)
   (session: UserSession)
   =
   Html.div [
      ServiceHealth.ServiceHealthComponent()

      classyNode Html.main [ "container-fluid" ] [
         SagaHistory.SagaHistoryComponent url session
      ]
   ]
