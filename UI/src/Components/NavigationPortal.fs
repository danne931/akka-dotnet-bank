[<RequireQualifiedAccess>]
module Navigation

open Feliz
open Browser.Dom

open Lib.SharedTypes
open Bank.Employee.Domain

let private portalId = "navigation-portal"

type UserSessionMsg = UserSessionProvider.Msg

let Portal (content: ReactElement) =
   let root = document.getElementById portalId
   ReactDOM.createPortal (content, root)

[<ReactComponent>]
let UserSessionSelectionComponent
   (selectedEmployeeId: EmployeeId)
   (userSessions: UserSession list)
   =
   let userSessionDispatch =
      React.useContext UserSessionProvider.dispatchContext

   let isUserSessionSelectionOpen, setUserSessionSelectionOpen =
      React.useState false

   Html.ul [
      attr.custom (
         "data-tooltip",
         "Select a user session to initiate transactions with."
      )
      attr.custom ("data-placement", "left")
      attr.children [
         Html.li [
            Html.details [
               attr.role "list"
               attr.custom ("dir", "rtl")
               attr.isOpen isUserSessionSelectionOpen
               attr.onClick (fun e ->
                  e.preventDefault ()
                  setUserSessionSelectionOpen (not isUserSessionSelectionOpen))

               attr.children [
                  Html.summary [
                     attr.custom ("aria-haspopup", "listbox")
                     attr.role "link"
                     attr.classes [ "contrast" ]
                     attr.text "User"
                  ]

                  Html.ul [
                     attr.role "listbox"
                     attr.children [
                        for session in userSessions ->
                           Html.li [
                              Html.a [
                                 attr.text
                                    $"{session.Name} ({session.Role.Display})"

                                 attr.href ""
                                 attr.value (EmployeeId.get session.EmployeeId)

                                 attr.onClick (fun e ->
                                    e.preventDefault ()

                                    if
                                       selectedEmployeeId <> session.EmployeeId
                                    then
                                       userSessionDispatch (
                                          UserSessionMsg.SetUserSession(
                                             session,
                                             Started
                                          )
                                       ))

                                 if selectedEmployeeId = session.EmployeeId then
                                    attr.classes [ "selected" ]
                              ]
                           ]
                     ]
                  ]
               ]
            ]
         ]
      ]
   ]

[<ReactComponent>]
let NavigationComponent () =
   let currentSessionCtx = React.useContext UserSessionProvider.context

   let selectableSessionsCtx =
      React.useContext UserSessionProvider.demoSessionsContext

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

      classyNode Html.div [ "grid"; "nav-selection-container" ] [
         Html.div [ attr.id portalId ]

         match currentSessionCtx, selectableSessionsCtx with
         | Deferred.Resolved(Ok session), Deferred.Resolved(Ok sessions) ->
            UserSessionSelectionComponent session.EmployeeId sessions
         | _ -> ()
      ]
   ]
