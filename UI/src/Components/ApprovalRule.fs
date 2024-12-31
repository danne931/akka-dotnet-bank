module ApprovalRule

open Feliz
open Feliz.Router
open Fable.FontAwesome
open Elmish
open Feliz.UseElmish
open Elmish.SweetAlert

open Bank.Employee.Domain
open UIDomain.Employee
open Lib.SharedTypes
open Lib.Time

[<ReactComponent>]
let ApprovalRuleComponent (session: UserSession) =
   //let orgCtx = React.useContext OrgProvider.context
   //let orgDispatch = React.useContext OrgProvider.dispatchContext

   //let state, dispatch = React.useElmish (init orgCtx, update, [| box orgCtx |])

   Html.p "TODO: Implement approval rule management dashboard"
