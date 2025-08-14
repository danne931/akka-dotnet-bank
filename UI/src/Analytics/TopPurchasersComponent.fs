[<RequireQualifiedAccess>]
module TopPurchasers

open Feliz
open Feliz.Router
open Fable.FontAwesome

open Bank.Org.Domain

let private renderAmount (amount: decimal) =
   Html.p [ attr.classes [ "debit" ]; attr.text (Money.format amount) ]

let render (topN: EmployeePurchaserTopN list) =
   Html.div [
      classyNode Html.div [ "top-n-list-container" ] [
         for purchaser in topN do
            Html.div [
               Html.b purchaser.EmployeeName
               renderAmount purchaser.Amount
            ]
      ]

      Html.button [
         attr.classes [ "outline" ]
         attr.children [ Fa.i [ Fa.Solid.History ] []; Html.span "View all" ]
         attr.onClick (fun _ -> Router.navigate Routes.TransactionsUrl.BasePath)
      ]
   ]
