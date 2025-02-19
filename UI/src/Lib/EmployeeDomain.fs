module UIDomain.Employee

open Bank.Employee.Domain
open Lib.SharedTypes
open Lib.NetworkQuery

type EmployeeMaybe = Result<Employee option, Err>

type EmployeesMaybe = Result<Employee list option, Err>

type EmployeeCommandReceipt = {
   PendingCommand: EmployeeCommand
   PendingEvent: EmployeeEvent
   PendingState: Employee
   Envelope: Envelope
}

type SelectedEmployee = {
   Id: EmployeeId
   Name: string
   Email: string
}

[<RequireQualifiedAccess>]
type EmployeeActionView =
   | Create
   | ViewEmployee of EmployeeId

type EmployeeBrowserQuery = {
   Action: EmployeeActionView option
   SelectedEmployees: (SelectedEmployee list) option
   Roles: (Role list) option
} with

   member x.ChangeDetection =
      Serialization.serialize {|
         Roles = x.Roles
         SelectedEmployees = x.SelectedEmployees
      |}

let parseEmployees =
   Serialization.deserialize<SelectedEmployee list> >> Result.toOption

module EmployeeBrowserQuery =
   let toQueryParams (query: EmployeeBrowserQuery) : (string * string) list =
      let agg = []

      let agg =
         match query.Action with
         | Some view -> ("action", string view) :: agg
         | None -> agg

      let agg =
         match query.SelectedEmployees with
         | None -> agg
         | Some employees ->
            ("employees", Serialization.serialize employees) :: agg

      let agg =
         match query.Roles with
         | Some roles -> ("roles", listToQueryString roles) :: agg
         | None -> agg

      agg

   let private (|ViewEmployee|_|) (input: string) =
      let needle = "ViewEmployee "

      if input.StartsWith(needle) then
         input.Substring(needle.Length)
         |> Guid.parseOptional
         |> Option.map EmployeeId
      else
         None

   let fromQueryParams
      (queryParams: (string * string) list)
      : EmployeeBrowserQuery
      =
      let queryParams = Map.ofList queryParams

      {
         Action =
            Map.tryFind "action" queryParams
            |> Option.bind (function
               | "Create" -> Some EmployeeActionView.Create
               | ViewEmployee id -> Some(EmployeeActionView.ViewEmployee id)
               | view ->
                  Log.error $"Employee action view not implemented: {view}"
                  None)
         Roles =
            Map.tryFind "roles" queryParams
            |> Option.bind EmployeeQuery.rolesFromQueryString
         SelectedEmployees =
            Map.tryFind "employees" queryParams |> Option.bind parseEmployees
      }

   let empty: EmployeeBrowserQuery = {
      Action = None
      Roles = None
      SelectedEmployees = None
   }
