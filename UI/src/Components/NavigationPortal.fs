[<RequireQualifiedAccess>]
module Navigation

open Feliz
open Browser.Dom

let private portalId = "navigation-portal"

let Portal (content: ReactElement) =
   let root = document.getElementById portalId
   ReactDOM.createPortal (content, root)

let element =
   classyNode Html.nav [ "container-fluid" ] [
      Html.ul [
         Html.li [
            Html.a [
               attr.href ""
               attr.onClick (fun e -> e.preventDefault ())
               attr.children [ Html.strong "Bank" ]
            ]
         ]
      ]

      Html.div [ attr.id portalId ]
   ]
