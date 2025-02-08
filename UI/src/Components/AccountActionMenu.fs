module AccountActionMenu

open Feliz
open Feliz.Router
open Fable.FontAwesome

open UIDomain.Account

let private renderMenuButton (view: AccountActionView) =
   Html.button [
      attr.classes [ "outline"; "grid" ]

      attr.onClick (fun _ ->
         AccountActions.navigation (Some view) |> Router.navigate)

      attr.children [
         match view with
         | AccountActionView.Deposit ->
            Html.span [ Fa.i [ Fa.Solid.PiggyBank ] [] ]
            Html.span "Deposit"
         | AccountActionView.Purchase ->
            Html.span [ Fa.i [ Fa.Solid.CreditCard ] [] ]
            Html.span "Debit Card Purchase"
         | AccountActionView.Transfer _ ->
            Html.span [ Fa.i [ Fa.Solid.ArrowsAltH ] [] ]
            Html.span "Transfer"
         | AccountActionView.RegisterTransferRecipient ->
            Html.span [ Fa.i [ Fa.Solid.UserPlus ] [] ]
            Html.span "Add a Transfer Recipient"
         | _ -> ()
      ]
   ]

let render () =
   classyNode Html.div [ "action-menu" ] [
      classyNode Html.div [ "grid" ] [
         renderMenuButton AccountActionView.Purchase
      ]

      classyNode Html.div [ "grid" ] [
         renderMenuButton AccountActionView.Deposit
      ]

      classyNode Html.div [ "grid" ] [
         renderMenuButton (AccountActionView.Transfer None)
      ]

      classyNode Html.div [ "grid" ] [
         renderMenuButton AccountActionView.RegisterTransferRecipient
      ]
   ]
