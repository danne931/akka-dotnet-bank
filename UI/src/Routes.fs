[<RequireQualifiedAccess>]
module Routes

open System
open Feliz.Router

open UIDomain.History
open UIDomain.Account
open UIDomain.Employee
open UIDomain.Card
open Lib.SharedTypes

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
   | TransactionsWithQuery of TransactionBrowserQuery
   | NotFound

module TransactionsUrl =
   [<Literal>]
   let BasePath = "transactions"

   let queryPath (query: TransactionBrowserQuery) = [|
      BasePath
      query |> TransactionBrowserQuery.toQueryParams |> Router.encodeQueryString
   |]

   let parse =
      function
      // Matches /
      | [] -> TransactionsUrl.Transactions
      // ?action=deposit&isCategorized=false&date=Last30Days
      | [ Route.Query queryParams ] ->
         let query = TransactionBrowserQuery.fromQueryParams queryParams
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
type HistoryUrl =
   | History
   | HistoryWithSearchQuery of HistoryBrowserQuery
   | NotFound

module HistoryUrl =
   [<Literal>]
   let BasePath = "history"

   let parse =
      function
      | [] -> HistoryUrl.History
      | [ Route.Query queryParams ] ->
         let query = HistoryBrowserQuery.fromQueryParams queryParams
         HistoryUrl.HistoryWithSearchQuery query
      | _ -> HistoryUrl.NotFound

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
   | ViewPayment of PaymentRequestId
   | NotFound

module PaymentUrl =
   [<Literal>]
   let BasePath = "payments"

   let RequestPaymentPath = [| BasePath; "request" |]

   let selectedPath (paymentId: PaymentRequestId) = [|
      BasePath
      string paymentId
   |]

   let parse =
      function
      | [] -> PaymentUrl.Payments
      | [ "request" ] -> PaymentUrl.RequestPayment
      | [ Route.Guid paymentId ] ->
         PaymentUrl.ViewPayment(PaymentRequestId paymentId)
      | _ -> PaymentUrl.NotFound

[<RequireQualifiedAccess>]
type IndexUrl =
   | Analytics of AnalyticsUrl
   | Account of AccountUrl
   | Approvals of ApprovalsUrl
   | Transactions of TransactionsUrl
   | History of HistoryUrl
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
      // Matches /history/{HistoryUrl}
      | HistoryUrl.BasePath :: segments ->
         IndexUrl.History(HistoryUrl.parse segments)
      // Matches /employees/{EmployeeUrl}
      | EmployeeUrl.BasePath :: segments ->
         IndexUrl.Employees(EmployeeUrl.parse segments)
      // Matches /cards/{CardUrl}
      | CardUrl.BasePath :: segments -> IndexUrl.Cards(CardUrl.parse segments)
      | PaymentUrl.BasePath :: segments ->
         IndexUrl.Payments(PaymentUrl.parse segments)
      | _ -> IndexUrl.NotFound

   let current () = Router.currentUrl () |> parse

   let transactionBrowserQuery () =
      match current () with
      | IndexUrl.Transactions url ->
         match url with
         | TransactionsUrl.TransactionsWithQuery query -> query
         | _ -> TransactionBrowserQuery.empty
      | _ -> TransactionBrowserQuery.empty

   let employeeBrowserQuery () =
      match current () with
      | IndexUrl.Employees url ->
         match url with
         | EmployeeUrl.EmployeesWithSearchQuery query -> query
         | _ -> EmployeeBrowserQuery.empty
      | _ -> EmployeeBrowserQuery.empty

   let historyBrowserQuery () =
      match current () with
      | IndexUrl.History url ->
         match url with
         | HistoryUrl.HistoryWithSearchQuery query -> query
         | _ -> HistoryBrowserQuery.empty
      | _ -> HistoryBrowserQuery.empty

   let cardBrowserQuery () =
      match current () with
      | IndexUrl.Cards url ->
         match url with
         | CardUrl.CardsWithSearchQuery query -> query
         | _ -> CardBrowserQuery.empty
      | _ -> CardBrowserQuery.empty
