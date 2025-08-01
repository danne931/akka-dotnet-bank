module AccountDashboard

open Feliz
open Feliz.Router
open Fable.FontAwesome
open Fable.Core.JsInterop

open Bank.Account.Domain
open Bank.Employee.Domain
open UIDomain.Account
open Bank.Account.Forms.AccountCreateForm

[<ReactComponent>]
let AccountNumberComponent (account: Account) =
   let display, toggleDisplay = React.useState false

   React.fragment [
      if display then
         Html.b (string account.AccountNumber)
      else
         Html.b $"*****{account.AccountNumber.Last4}"

      Html.a [
         attr.href ""

         attr.onClick (fun e ->
            e.preventDefault ()
            toggleDisplay (not display))

         attr.children [
            if display then
               Fa.i [ Fa.Solid.EyeSlash ] []
            else
               Fa.i [ Fa.Solid.Eye ] []
         ]
      ]
   ]

let private onClose _ =
   Router.navigate Routes.AccountUrl.BasePath

let renderAccounts (orgCtx: OrgProvider.State) =
   React.fragment [
      classyNode Html.div [ "title-with-button-container" ] [
         Html.h4 "Accounts"
         Html.button [
            attr.children [
               Fa.i [ Fa.Solid.Plus ] []
               Html.span "Create Account"
            ]

            attr.onClick (fun _ ->
               Router.navigate Routes.AccountUrl.CreateAccountPath)
         ]
      ]

      Html.progress [
         match orgCtx with
         | Deferred.Resolved _ -> attr.value 100
         | _ -> ()
      ]

      match orgCtx with
      | Deferred.Resolved(Ok(Some org)) ->
         classyNode Html.div [ "org-summary"; "grid" ] [
            Html.div [
               Html.small "Available balance across all accounts: "

               Html.h3 [
                  attr.classes [ "balance" ]
                  attr.text (Money.format org.AvailableBalance)
               ]
            ]

            Html.div [
               Html.small "Pending Deductions: "

               Html.div [
                  Html.h3 [
                     attr.classes [ "pending-deductions"; "debit" ]
                     attr.text (Money.format org.PendingDeductions.Money)
                  ]
                  Html.small $"({org.PendingDeductions.Count} transactions)"
               ]
            ]
         ]

         classyNode Html.div [ "grid"; "accounts" ] [
            for account in org.Accounts.Values do
               Html.article [
                  Html.div [
                     Html.p account.Name
                     Html.h5 [
                        attr.style [ style.margin 0 ]
                        attr.text (Money.format account.AvailableBalance)
                     ]
                  ]

                  Html.div [
                     Html.small "Account Number:"
                     AccountNumberComponent account
                  ]

                  Html.div [
                     Html.small "Routing Number:"
                     Html.b (string account.RoutingNumber)
                  ]

                  Html.button [
                     attr.classes [ "outline" ]
                     attr.children [
                        Fa.i [ Fa.Solid.History ] []
                        Html.span "View Transactions"
                     ]

                     attr.onClick (fun _ ->
                        {
                           TransactionBrowserQuery.empty with
                              Accounts =
                                 Some [
                                    {
                                       AccountId = account.AccountId
                                       Display = account.FullName
                                    }
                                 ]
                        }
                        |> Routes.TransactionsUrl.queryPath
                        |> Router.navigate)
                  ]
               ]
         ]
      | Deferred.Resolved(Ok None) -> Html.p "No accounts found."
      | _ -> ()
   ]

[<ReactComponent>]
let AccountDashboardComponent (url: Routes.AccountUrl) (session: UserSession) =
   let orgCtx = React.useContext OrgProvider.context
   let orgDispatch = React.useContext OrgProvider.dispatchContext

   classyNode Html.div [ "account-dashboard" ] [
      match url with
      | Routes.AccountUrl.CreateAccount ->
         match orgCtx with
         | Deferred.Resolved(Ok(Some org)) ->
            classyNode Html.article [ "form-wrapper" ] [
               Html.h6 "Create Account"
               CloseButton.render onClose

               AccountCreateFormComponent
                  session
                  org.Org
                  (_.PendingState
                   >> OrgProvider.Msg.AccountCreated
                   >> orgDispatch
                   >> onClose)
            ]
            |> ScreenOverlay.Portal
         | _ -> ()
      | _ -> ()

      classyNode Html.main [ "container-fluid" ] [
         classyNode Html.nav [ "link-menu" ] [
            Html.a [
               attr.text "Accounts"
               attr.href ""
               if url <> Routes.AccountUrl.Account then
                  attr.className "secondary"

               attr.onClick (fun e ->
                  e.preventDefault ()
                  Router.navigate Routes.AccountUrl.BasePath)
            ]

            Html.a [
               attr.text "Automatic Balance Management"
               attr.href ""
               if url <> Routes.AccountUrl.AutoBalanceManagement then
                  attr.className "secondary"

               attr.onClick (fun e ->
                  e.preventDefault ()
                  Router.navigate Routes.AccountUrl.AutoBalanceManagementPath)
            ]
         ]

         Html.hr []
         Html.br []

         match url with
         | Routes.AccountUrl.Account
         | Routes.AccountUrl.CreateAccount -> renderAccounts orgCtx
         | Routes.AccountUrl.AutoBalanceManagement
         | Routes.AccountUrl.CreateRule _
         | Routes.AccountUrl.EditRule _ ->
            match orgCtx with
            | Deferred.Resolved(Ok(Some org)) ->
               // Lazy import to avoid fetching LeaderLine library when not
               // needed.  LeaderLine, the SVG line drawing library, is used
               // only by AutomaticBalanceDashboardComponent.
               React.suspense (
                  [
                     React.lazy' (
                        fun () ->
                           importDynamic "./AutomaticBalanceManagementDashboard"
                        , {|
                           Session = session
                           Accounts = org.Accounts
                           Url = url
                        |}
                     )
                  ],
                  Html.progress []
               )
            | _ -> ()
         | Routes.AccountUrl.NotFound -> Html.p "Uh oh! Unknown URL."
      ]
   ]
