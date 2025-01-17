[<RequireQualifiedAccess>]
module EmployeeService

open Fable.SimpleHttp
open FsToolkit.ErrorHandling
open Feliz.Router

open Bank.Employee.Domain
open UIDomain
open UIDomain.Employee
open UIDomain.Card
open Lib.SharedTypes
open Lib.NetworkQuery
open RoutePaths

let networkQueryFromBrowserQuery (query: EmployeeBrowserQuery) : EmployeeQuery = {
   EmployeeIds = query.SelectedEmployees |> Option.map (List.map _.Id)
   Roles = query.Roles
}

let networkQueryFromHistoryBrowserQuery
   (query: EmployeeHistoryBrowserQuery)
   : EmployeeHistoryQuery
   =
   {
      Page = 1
      DateRange = query.Date |> Option.map DateFilter.toDateRange
      EventType = query.EventType
      EmployeeIds = query.SelectedEmployees |> Option.map (List.map _.Id)
      InitiatedByIds =
         query.SelectedInitiatedBy
         |> Option.map (List.map (_.Id >> InitiatedById))
   }

let networkQueryFromCardBrowserQuery (query: CardBrowserQuery) : CardQuery = {
   AccountIds = query.SelectedAccounts |> Option.map (List.map _.Id)
   EmployeeIds = query.SelectedEmployees |> Option.map (List.map _.Id)
   CreatedAtDateRange = query.CreatedAt |> Option.map DateFilter.toDateRange
   Amount = query.Amount
}

let serviceName = "EmployeeService"

let private notImplemented (cmd: EmployeeCommand) =
   let msg = $"{serviceName}: Not implemented command: {cmd}"
   Log.error msg
   failwith msg

let private postJson (command: EmployeeCommand) =
   let serialized, url =
      match command with
      | EmployeeCommand.CreateEmployee cmd ->
         Serialization.serialize cmd, EmployeePath.Base
      | EmployeeCommand.DebitRequest cmd ->
         Serialization.serialize cmd, CardPath.Purchase
      | EmployeeCommand.LimitDailyDebits cmd ->
         Serialization.serialize cmd, CardPath.DailyPurchaseLimit
      | EmployeeCommand.LimitMonthlyDebits cmd ->
         Serialization.serialize cmd, CardPath.MonthlyPurchaseLimit
      | EmployeeCommand.CreateCard cmd ->
         Serialization.serialize cmd, CardPath.Base
      | EmployeeCommand.LockCard cmd ->
         Serialization.serialize cmd, CardPath.LockCard
      | EmployeeCommand.UnlockCard cmd ->
         Serialization.serialize cmd, CardPath.UnlockCard
      | EmployeeCommand.EditCardNickname cmd ->
         Serialization.serialize cmd, CardPath.UpdateNickname
      | EmployeeCommand.UpdateRole cmd ->
         Serialization.serialize cmd, EmployeePath.UpdateRole
      | EmployeeCommand.CancelInvitation cmd ->
         Serialization.serialize cmd, EmployeePath.CancelEmployeeInvitation
      | EmployeeCommand.RestoreAccess cmd ->
         Serialization.serialize cmd, EmployeePath.RestoreAccess
      | other -> notImplemented other

   Http.postJson url serialized

let getEmployee (orgId: OrgId) (employeeId: EmployeeId) : Async<EmployeeMaybe> = async {
   let! (code, responseText) =
      Http.get (RoutePaths.EmployeePath.getEmployee orgId employeeId)

   if code = 404 then
      return Ok None
   elif code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return
         responseText |> Serialization.deserialize<Employee> |> Result.map Some
}

let private getEmployeesWithPath (path: string) : Async<EmployeesMaybe> = async {
   let! (code, responseText) = Http.get path

   if code = 404 then
      return Ok None
   elif code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return
         responseText
         |> Serialization.deserialize<Employee list>
         |> Result.map Some
}

let getEmployees (orgId: OrgId) (query: EmployeeQuery) : Async<EmployeesMaybe> =
   let qParams =
      [
         match query.Roles with
         | None -> ()
         | Some roles -> "roles", listToQueryString roles

         match query.EmployeeIds with
         | None -> ()
         | Some ids -> "employeeIds", listToQueryString ids
      ]
      |> Router.encodeQueryString

   let path = EmployeePath.get orgId + qParams
   getEmployeesWithPath path

let searchEmployees (orgId: OrgId) (query: string) : Async<EmployeesMaybe> =
   getEmployeesWithPath (EmployeePath.search orgId query)

let submitCommand
   (employee: Employee)
   (command: EmployeeCommand)
   : Async<Result<EmployeeCommandReceipt, Err>>
   =
   asyncResult {
      // Pre-network request validation checks the command itself
      // (Err.ValidationError) and the Result of applying the command
      // to the current state (Err.EmployeeStateTransitionError).
      // This same validation occurs on the server when an actor is
      // processing a command.
      let! evt, newState =
         Employee.stateTransition { Info = employee; Events = [] } command

      let! res = postJson command
      let code = res.statusCode

      if code <> 200 then
         return! Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         let! envelope = Serialization.deserialize<Envelope> res.responseText

         return {
            Envelope = envelope
            PendingState = newState.Info
            PendingEvent = evt
            PendingCommand = command
         }
   }

let resendEmployeeInvitation (employee: Employee) : Async<Result<unit, Err>> = async {
   let! res =
      Http.postJson
         RoutePaths.EmployeePath.ResendInviteNotification
         (Serialization.serialize employee)

   let code = res.statusCode

   if code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return Ok()
}

let getEmployeeHistory
   (orgId: OrgId)
   (query: EmployeeHistoryQuery)
   : Async<EmployeeHistoryMaybe>
   =
   async {
      let queryParams =
         [
            "page", string query.Page

            match query.EmployeeIds with
            | None -> ()
            | Some ids -> "employeeIds", listToQueryString ids

            match query.InitiatedByIds with
            | None -> ()
            | Some ids -> "initiatedByIds", listToQueryString ids

            match query.EventType with
            | None -> ()
            | Some filters -> "events", listToQueryString filters

            match query.DateRange with
            | None -> ()
            | Some(startDate, endDate) ->
               "date", DateTime.rangeAsQueryString startDate endDate
         ]
         |> Router.encodeQueryString

      let path = EmployeePath.history orgId + queryParams
      let! (code, responseText) = Http.get path

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<EmployeeHistory list>
            |> Result.map Some
   }

let getCards (orgId: OrgId) (query: CardQuery) : Async<CardsMaybe> = async {
   let queryParams =
      [
         match query.Amount with
         | None -> ()
         | Some amount -> yield! AmountFilter.toQuery amount

         match query.EmployeeIds with
         | None -> ()
         | Some ids -> "employeeIds", listToQueryString ids

         match query.AccountIds with
         | None -> ()
         | Some ids -> "accountIds", listToQueryString ids

         match query.CreatedAtDateRange with
         | None -> ()
         | Some(startDate, endDate) ->
            "createdAt", DateTime.rangeAsQueryString startDate endDate
      ]
      |> Router.encodeQueryString

   let path = CardPath.get orgId + queryParams
   let! (code, responseText) = Http.get path

   if code = 404 then
      return Ok None
   elif code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return
         responseText
         |> Serialization.deserialize<CardWithMetrics list>
         |> Result.map Some
}
