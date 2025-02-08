module SidebarMenu

open Feliz

open Bank.Org.Domain
open Bank.Employee.Domain
open Lib.SharedTypes

type private MenuUrl =
   | Analytics
   | Account
   | Approvals
   | Transactions
   | EmployeeHistory
   | Employee
   | Card
   | Payment

type private MenuItem = {
   Url: MenuUrl
   SelectedUrl: Routes.IndexUrl
   Name: string
   Href: string
   CallToActionIndicator: string option
}

let private renderListItem (item: MenuItem) =
   Html.li [
      match item.Url, item.SelectedUrl with
      | Analytics, Routes.IndexUrl.Analytics _ -> attr.classes [ "selected" ]
      | Account, Routes.IndexUrl.Account _ -> attr.classes [ "selected" ]
      | Approvals, Routes.IndexUrl.Approvals _ -> attr.classes [ "selected" ]
      | Transactions, Routes.IndexUrl.Transactions _ ->
         attr.classes [ "selected" ]
      | EmployeeHistory, Routes.IndexUrl.EmployeeHistory _ ->
         attr.classes [ "selected" ]
      | Employee, Routes.IndexUrl.Employees _ -> attr.classes [ "selected" ]
      | Card, Routes.IndexUrl.Cards _ -> attr.classes [ "selected" ]
      | Payment, Routes.IndexUrl.Payments _ -> attr.classes [ "selected" ]
      | _ -> ()

      attr.children [
         Html.a [
            attr.href ("#" + item.Href)
            attr.children [
               Html.p item.Name

               match item.CallToActionIndicator with
               | Some indicator ->
                  Html.p [
                     attr.text indicator
                     attr.classes [ "call-to-action" ]
                  ]
               | None -> ()
            ]
         ]
      ]
   ]

[<ReactComponent>]
let SidebarMenuComponent (currentUrl: Routes.IndexUrl) (session: UserSession) =
   let orgCtx = React.useContext OrgProvider.context

   React.fragment [
      Html.ul [
         attr.classes [ "sidebar-menu" ]
         attr.role "listbox"
         attr.children [
            renderListItem {
               Url = Analytics
               SelectedUrl = currentUrl
               Name = "Analytics"
               Href = Routes.AnalyticsUrl.BasePath
               CallToActionIndicator = None
            }

            renderListItem {
               Url = Account
               SelectedUrl = currentUrl
               Name = "Accounts"
               Href = Routes.AccountUrl.BasePath
               CallToActionIndicator = None
            }

            match session.Role with
            | Role.Admin ->
               let name = "Approvals"

               renderListItem {
                  Url = Approvals
                  SelectedUrl = currentUrl
                  Name = name
                  Href = Routes.ApprovalsUrl.BasePath
                  CallToActionIndicator =
                     match orgCtx with
                     | Deferred.Resolved(Ok(Some org)) ->
                        let activeApprovalsInProgressCnt =
                           CommandApprovalProgress.numberOfApprovalsUserCanManage
                              org.Org.CommandApprovalRules
                              org.Org.CommandApprovalProgress
                              session.EmployeeId

                        if activeApprovalsInProgressCnt > 0 then
                           Some $"({activeApprovalsInProgressCnt})"
                        else
                           None
                     | _ -> None
               }
            | _ -> ()

            renderListItem {
               Url = Transactions
               SelectedUrl = currentUrl
               Name = "Transactions"
               Href = Routes.TransactionsUrl.BasePath
               CallToActionIndicator = None
            }

            renderListItem {
               Url = Payment
               SelectedUrl = currentUrl
               Name = "Payments"
               Href = Routes.PaymentUrl.BasePath
               CallToActionIndicator = None
            }

            renderListItem {
               Url = EmployeeHistory
               SelectedUrl = currentUrl
               Name = "Employee History"
               Href = Routes.EmployeeHistoryUrl.BasePath
               CallToActionIndicator = None
            }

            renderListItem {
               Url = Employee
               SelectedUrl = currentUrl
               Name = "Employees"
               Href = Routes.EmployeeUrl.BasePath
               CallToActionIndicator = None
            }

            renderListItem {
               Url = Card
               SelectedUrl = currentUrl
               Name = "Cards"
               Href = Routes.CardUrl.BasePath
               CallToActionIndicator = None
            }
         ]
      ]
   ]
