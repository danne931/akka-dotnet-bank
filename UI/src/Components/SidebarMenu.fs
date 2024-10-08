module SidebarMenu

open Feliz
open Bank.Employee.Domain

type private MenuUrl =
   | Analytics
   | Account
   | Transaction
   | EmployeeHistory
   | Employee
   | Card
   | Payment

type private MenuItem = {
   Url: MenuUrl
   SelectedUrl: Routes.IndexUrl
   Name: string
   Href: string
}

let private renderListItem (item: MenuItem) =
   Html.li [
      match item.Url, item.SelectedUrl with
      | Analytics, Routes.IndexUrl.Analytics _ -> attr.classes [ "selected" ]
      | Account, Routes.IndexUrl.Account _ -> attr.classes [ "selected" ]
      | Transaction, Routes.IndexUrl.Transaction _ ->
         attr.classes [ "selected" ]
      | EmployeeHistory, Routes.IndexUrl.EmployeeHistory _ ->
         attr.classes [ "selected" ]
      | Employee, Routes.IndexUrl.Employees _ -> attr.classes [ "selected" ]
      | Card, Routes.IndexUrl.Cards _ -> attr.classes [ "selected" ]
      | Payment, Routes.IndexUrl.Payments _ -> attr.classes [ "selected" ]
      | _ -> ()

      attr.children [
         Html.a [ attr.text item.Name; attr.href ("#" + item.Href) ]
      ]
   ]

let render (currentUrl: Routes.IndexUrl) (session: UserSession) =
   React.fragment [
      Html.ul [
         attr.role "listbox"
         attr.children [
            renderListItem {
               Url = Analytics
               SelectedUrl = currentUrl
               Name = "Analytics"
               Href = Routes.AnalyticsUrl.BasePath
            }

            renderListItem {
               Url = Account
               SelectedUrl = currentUrl
               Name = "Accounts"
               Href = Routes.AccountUrl.BasePath
            }

            renderListItem {
               Url = Transaction
               SelectedUrl = currentUrl
               Name = "Transactions"
               Href = Routes.TransactionUrl.BasePath
            }

            renderListItem {
               Url = Payment
               SelectedUrl = currentUrl
               Name = "Payments"
               Href = Routes.PaymentUrl.BasePath
            }

            renderListItem {
               Url = EmployeeHistory
               SelectedUrl = currentUrl
               Name = "Employee History"
               Href = Routes.EmployeeHistoryUrl.BasePath
            }

            renderListItem {
               Url = Employee
               SelectedUrl = currentUrl
               Name = "Employees"
               Href = Routes.EmployeeUrl.BasePath
            }

            renderListItem {
               Url = Card
               SelectedUrl = currentUrl
               Name = "Cards"
               Href = Routes.CardUrl.BasePath
            }
         ]
      ]
   ]
