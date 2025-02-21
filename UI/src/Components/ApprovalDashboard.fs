module ApprovalDashboard

open Feliz
open Feliz.Router

open Bank.Employee.Domain

[<ReactComponent>]
let ApprovalDashboardComponent
   (url: Routes.ApprovalsUrl)
   (session: UserSession)
   =
   let orgCtx = React.useContext OrgProvider.context

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
            match orgCtx with
            | Deferred.Resolved(Ok(Some org)) ->
               ApprovalProgress.ApprovalProgressComponent session org.Org
            | _ -> Html.progress []
         | Routes.ApprovalsUrl.ApprovalRuleManagement ->
            match orgCtx with
            | Deferred.Resolved(Ok(Some org)) ->
               ApprovalRuleManagement.ApprovalRuleManagementDashboardComponent
                  session
                  org.Org
            | _ -> Html.progress []
         | Routes.ApprovalsUrl.NotFound -> Html.p "Uh oh! Unknown URL."
      ]
   ]
