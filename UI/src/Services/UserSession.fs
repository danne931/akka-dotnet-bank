[<RequireQualifiedAccess>]
module UserSessionService

open Fable.SimpleHttp
open FsToolkit.ErrorHandling
open Feliz.Router

open Bank.Employee.Domain
open UIDomain.Employee
open Lib.SharedTypes
open Lib.NetworkQuery
open RoutePaths

let serviceName = "UserSessionService"

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

// Mock login to get mock user session properties
let loginAndGetCurrentUserSession () = async {
   let! code, _ = Http.post UserSessionPath.Login ""

   if code <> 200 then
      return Error(Err.UnexpectedError "Issue with mock login")
   else
      let! code, session = Http.get UserSessionPath.GetSession

      if code <> 200 then
         return Error(Err.UnexpectedError "Issue getting session")
      else
         return Serialization.deserialize<UserSession> session
}

/// Get selectable user sessions for demonstration purposes.
let getDemoUserSessions (orgId: OrgId) = async {
   let! code, responseText =
      Http.get (UserSessionPath.getDemoUserSessions orgId)

   if code <> 200 then
      return Error(Err.UnexpectedError "Issue getting session")
   else
      return Serialization.deserialize<UserSession list> responseText
}

// User session is selectable in UI for demonstration purposes.
let overrideDemoUserSession (employeeId: EmployeeId) = async {
   let! code, _ =
      Http.put (UserSessionPath.overrideDemoUserSession employeeId) ""

   if code <> 200 then
      return Error(Err.UnexpectedError "Issue with demo user session override")
   else
      return Ok()
}
