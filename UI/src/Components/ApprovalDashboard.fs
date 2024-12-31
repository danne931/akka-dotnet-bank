module ApprovalDashboard

open Feliz
open Feliz.Router

open Bank.Employee.Domain

let ApprovalDashboardComponent
   (url: Routes.ApprovalsUrl)
   (session: UserSession)
   =
   classyNode Html.div [ "approval-dashboard" ] [
      classyNode Html.main [ "container-fluid" ] [
         classyNode Html.nav [ "link-menu" ] [
            Html.a [
               attr.text "Pending Approvals"
               attr.href ""
               if url <> Routes.ApprovalsUrl.Approvals then
                  attr.className "secondary"

               attr.onClick (fun e ->
                  e.preventDefault ()
                  Router.navigate Routes.ApprovalsUrl.BasePath)
            ]

            Html.a [
               attr.text "Approval Rule Management"
               attr.href ""
               if url <> Routes.ApprovalsUrl.ApprovalRuleManagement then
                  attr.className "secondary"

               attr.onClick (fun e ->
                  e.preventDefault ()

                  Router.navigate
                     Routes.ApprovalsUrl.ApprovalRuleManagementPath)
            ]
         ]

         Html.hr []
         Html.br []

         match url with
         | Routes.ApprovalsUrl.Approvals ->
            ApprovalProgress.ApprovalProgressComponent session
         | Routes.ApprovalsUrl.ApprovalRuleManagement ->
            ApprovalRule.ApprovalRuleComponent session
         | Routes.ApprovalsUrl.NotFound -> Html.p "Uh oh! Unknown URL."
      ]
   ]
