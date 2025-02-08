[<RequireQualifiedAccess>]
module Routes

open System
open Feliz.Router

open UIDomain.Account
open UIDomain.Employee
open UIDomain.Card
open Lib.SharedTypes
open Bank.Transfer.Domain

[<RequireQualifiedAccess>]
type AnalyticsUrl =
   | Analytics
   | NotFound

module AnalyticsUrl =
   [<Literal>]
   let BasePath = "analytics"

   let parse =
      function
      | [] -> AnalyticsUrl.Analytics
      | _ -> AnalyticsUrl.NotFound

[<RequireQualifiedAccess>]
type CreateAutoTransferRuleUrl =
   | ZeroBalance
   | TargetBalance
   | PercentDistribution

[<RequireQualifiedAccess>]
type AccountUrl =
   | Account
   | CreateAccount
   | AutoBalanceManagement
   | CreateRule of CreateAutoTransferRuleUrl
   | EditRule of ruleId: Guid
   | NotFound

module AccountUrl =
   [<Literal>]
   let BasePath = "accounts"

   let CreateAccountPath = [| BasePath; "create" |]

   let AutoBalanceManagementPath = [| BasePath; "auto-balance-management" |]

   let CreateZeroBalanceRulePath = [|
      yield! AutoBalanceManagementPath
      "zero-balance"
   |]

   let CreateTargetBalanceRulePath = [|
      yield! AutoBalanceManagementPath
      "target-balance"
   |]

   let CreatePercentDistributionRulePath = [|
      yield! AutoBalanceManagementPath
      "percent-distribution"
   |]

   let editRulePath (ruleId: Guid) = [|
      yield! AutoBalanceManagementPath
      (string ruleId)
   |]

   let parse =
      function
      // Matches /
      | [] -> AccountUrl.Account
      | [ "create" ] -> AccountUrl.CreateAccount
      | [ "auto-balance-management" ] -> AccountUrl.AutoBalanceManagement
      | [ "auto-balance-management"; "zero-balance" ] ->
         AccountUrl.CreateRule CreateAutoTransferRuleUrl.ZeroBalance
      | [ "auto-balance-management"; "target-balance" ] ->
         AccountUrl.CreateRule CreateAutoTransferRuleUrl.TargetBalance
      | [ "auto-balance-management"; "percent-distribution" ] ->
         AccountUrl.CreateRule CreateAutoTransferRuleUrl.PercentDistribution
      | [ "auto-balance-management"; Route.Guid ruleId ] ->
         AccountUrl.EditRule(ruleId)
      | _ -> AccountUrl.NotFound

[<RequireQualifiedAccess>]
type ApprovalsUrl =
   | Approvals
   | ApprovalRuleManagement
   | NotFound

module ApprovalsUrl =
   [<Literal>]
   let BasePath = "approvals"

   let ApprovalRuleManagementPath = [| BasePath; "approval-rule-management" |]

   let parse =
      function
      | [] -> ApprovalsUrl.Approvals
      | [ "approval-rule-management" ] -> ApprovalsUrl.ApprovalRuleManagement
      | _ -> ApprovalsUrl.NotFound

[<RequireQualifiedAccess>]
type TransactionsUrl =
   | Transactions
   | TransactionsWithQuery of AccountBrowserQuery
   | NotFound

module TransactionsUrl =
   [<Literal>]
   let BasePath = "transactions"

   let queryPath (query: AccountBrowserQuery) = [|
      BasePath
      query |> AccountBrowserQuery.toQueryParams |> Router.encodeQueryString
   |]

   let parse =
      function
      // Matches /
      | [] -> TransactionsUrl.Transactions
      // ?action=deposit&isCategorized=false&date=Last30Days
      | [ Route.Query queryParams ] ->
         let query = AccountBrowserQuery.fromQueryParams queryParams
         TransactionsUrl.TransactionsWithQuery query
      | _ -> TransactionsUrl.NotFound

   let transactionIdMaybe =
      function
      | TransactionsUrl.TransactionsWithQuery query -> query.Transaction
      | _ -> None

[<RequireQualifiedAccess>]
type EmployeeUrl =
   | Employees
   | EmployeesWithSearchQuery of EmployeeBrowserQuery
   | NotFound

module EmployeeUrl =
   [<Literal>]
   let BasePath = "employees"

   let parse =
      function
      | [] -> EmployeeUrl.Employees
      | [ Route.Query queryParams ] ->
         let query = EmployeeBrowserQuery.fromQueryParams queryParams
         EmployeeUrl.EmployeesWithSearchQuery query
      | _ -> EmployeeUrl.NotFound

   let employeeIdMaybe =
      function
      | EmployeeUrl.EmployeesWithSearchQuery query ->
         match query.Action with
         | Some(EmployeeActionView.ViewEmployee id) -> Some id
         | _ -> None
      | _ -> None

[<RequireQualifiedAccess>]
type EmployeeHistoryUrl =
   | EmployeeHistory
   | EmployeeHistoryWithSearchQuery of EmployeeHistoryBrowserQuery
   | NotFound

module EmployeeHistoryUrl =
   [<Literal>]
   let BasePath = "employee-history"

   let parse =
      function
      | [] -> EmployeeHistoryUrl.EmployeeHistory
      | [ Route.Query queryParams ] ->
         let query = EmployeeHistoryBrowserQuery.fromQueryParams queryParams
         EmployeeHistoryUrl.EmployeeHistoryWithSearchQuery query
      | _ -> EmployeeHistoryUrl.NotFound

[<RequireQualifiedAccess>]
type CardUrl =
   | Cards
   | CardsWithSearchQuery of CardBrowserQuery
   | NotFound

module CardUrl =
   [<Literal>]
   let BasePath = "cards"

   let parse =
      function
      | [] -> CardUrl.Cards
      | [ Route.Query queryParams ] ->
         let query = CardBrowserQuery.fromQueryParams queryParams
         CardUrl.CardsWithSearchQuery query
      | _ -> CardUrl.NotFound

[<RequireQualifiedAccess>]
type PaymentUrl =
   | Payments
   | RequestPayment
   | ViewPayment of PaymentId
   | NotFound

module PaymentUrl =
   [<Literal>]
   let BasePath = "payments"

   let RequestPaymentPath = [| BasePath; "request" |]

   let selectedPath (paymentId: PaymentId) = [| BasePath; string paymentId |]

   let parse =
      function
      | [] -> PaymentUrl.Payments
      | [ "request" ] -> PaymentUrl.RequestPayment
      | [ Route.Guid paymentId ] -> PaymentUrl.ViewPayment(PaymentId paymentId)
      | _ -> PaymentUrl.NotFound

[<RequireQualifiedAccess>]
type IndexUrl =
   | Analytics of AnalyticsUrl
   | Account of AccountUrl
   | Approvals of ApprovalsUrl
   | Transactions of TransactionsUrl
   | EmployeeHistory of EmployeeHistoryUrl
   | Employees of EmployeeUrl
   | Cards of CardUrl
   | Payments of PaymentUrl
   | NotFound

module IndexUrl =
   let parse (segments: string list) =
      let segments =
         if segments.IsEmpty then
            [ AnalyticsUrl.BasePath ]
         else
            segments

      match segments with
      | AnalyticsUrl.BasePath :: segments ->
         IndexUrl.Analytics(AnalyticsUrl.parse segments)
      | AccountUrl.BasePath :: segments ->
         IndexUrl.Account(AccountUrl.parse segments)
      | ApprovalsUrl.BasePath :: segments ->
         IndexUrl.Approvals(ApprovalsUrl.parse segments)
      // Matches /transactions/{TransactionsUrl}
      | TransactionsUrl.BasePath :: segments ->
         IndexUrl.Transactions(TransactionsUrl.parse segments)
      // Matches /employee-history/{EmployeeHistoryUrl}
      | EmployeeHistoryUrl.BasePath :: segments ->
         IndexUrl.EmployeeHistory(EmployeeHistoryUrl.parse segments)
      // Matches /employees/{EmployeeUrl}
      | EmployeeUrl.BasePath :: segments ->
         IndexUrl.Employees(EmployeeUrl.parse segments)
      // Matches /cards/{CardUrl}
      | CardUrl.BasePath :: segments -> IndexUrl.Cards(CardUrl.parse segments)
      | PaymentUrl.BasePath :: segments ->
         IndexUrl.Payments(PaymentUrl.parse segments)
      | _ -> IndexUrl.NotFound

   let current () = Router.currentUrl () |> parse

   let accountBrowserQuery () =
      match current () with
      | IndexUrl.Transactions url ->
         match url with
         | TransactionsUrl.TransactionsWithQuery query -> query
         | _ -> AccountBrowserQuery.empty
      | _ -> AccountBrowserQuery.empty

   let employeeBrowserQuery () =
      match current () with
      | IndexUrl.Employees url ->
         match url with
         | EmployeeUrl.EmployeesWithSearchQuery query -> query
         | _ -> EmployeeBrowserQuery.empty
      | _ -> EmployeeBrowserQuery.empty

   let employeeHistoryBrowserQuery () =
      match current () with
      | IndexUrl.EmployeeHistory url ->
         match url with
         | EmployeeHistoryUrl.EmployeeHistoryWithSearchQuery query -> query
         | _ -> EmployeeHistoryBrowserQuery.empty
      | _ -> EmployeeHistoryBrowserQuery.empty

   let cardBrowserQuery () =
      match current () with
      | IndexUrl.Cards url ->
         match url with
         | CardUrl.CardsWithSearchQuery query -> query
         | _ -> CardBrowserQuery.empty
      | _ -> CardBrowserQuery.empty
