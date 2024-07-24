module SidebarMenu

open Feliz
open Lib.SharedTypes

type private MenuUrl =
   | Transaction
   | EmployeeHistory
   | Employee
   | Card

type private MenuItem = {
   Url: MenuUrl
   SelectedUrl: Routes.IndexUrl
   Name: string
   Href: string
}

let private renderListItem (item: MenuItem) =
   Html.li [
      match item.Url, item.SelectedUrl with
      | Transaction, Routes.IndexUrl.Account _ -> attr.classes [ "selected" ]
      | EmployeeHistory, Routes.IndexUrl.EmployeeHistory _ ->
         attr.classes [ "selected" ]
      | Employee, Routes.IndexUrl.Employees _ -> attr.classes [ "selected" ]
      | Card, Routes.IndexUrl.Cards _ -> attr.classes [ "selected" ]
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
               Url = Transaction
               SelectedUrl = currentUrl
               Name = "Transactions"
               Href = Routes.AccountUrl.BasePath
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
