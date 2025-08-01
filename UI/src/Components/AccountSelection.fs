module AccountSelection

open Feliz

open Bank.Account.Domain
open Lib.SharedTypes

let renderAccountListItem
   (onSelect: Account -> unit)
   (selectedAccountId: AccountId option)
   (account: Account)
   =
   Html.li [
      Html.a [
         attr.text $"{account.Name} ({Money.format account.AvailableBalance})"

         attr.href ""
         attr.value (AccountId.get account.AccountId)
         attr.onClick (fun e ->
            e.preventDefault ()
            onSelect account)

         match selectedAccountId with
         | Some selectedId when selectedId = account.AccountId ->
            attr.classes [ "selected" ]
         | _ -> ()
      ]
   ]

[<ReactComponent>]
let AccountSelectionComponent
   (onSelect: Account -> unit)
   (selectedAccountId: AccountId option)
   (accounts: Map<AccountId, Account>)
   =
   let isAccountSelectionOpen, setAccountSelectionOpen = React.useState false

   Html.ul [
      attr.custom (
         "data-tooltip",
         "Select an account to process transactions on."
      )
      attr.custom ("data-placement", "left")
      attr.children [
         Html.li [
            Html.details [
               attr.role "list"
               attr.custom ("dir", "rtl")
               attr.isOpen isAccountSelectionOpen
               attr.onClick (fun e ->
                  e.preventDefault ()
                  setAccountSelectionOpen (not isAccountSelectionOpen))

               attr.children [
                  Html.summary [
                     attr.custom ("aria-haspopup", "listbox")
                     attr.role "link"
                     attr.classes [ "contrast" ]
                     attr.text "Account"
                  ]

                  Html.ul [
                     attr.role "listbox"
                     attr.children [
                        for account in accounts.Values ->
                           renderAccountListItem
                              onSelect
                              selectedAccountId
                              account
                     ]
                  ]
               ]
            ]
         ]
      ]
   ]
