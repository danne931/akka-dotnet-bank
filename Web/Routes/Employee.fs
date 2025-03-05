module Bank.Employee.Routes

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Akka.Actor
open Akkling
open FsToolkit.ErrorHandling

open Bank.Employee.Domain
open Bank.Employee.Api
open Bank.Org.Domain
open CommandApproval
open RoutePaths
open Lib.SharedTypes
open Bank.UserSession.Middleware
open Email

let startEmployeeRoutes (app: WebApplication) =
   app
      .MapGet(
         EmployeePath.GetEmployee,
         Func<Guid, Task<IResult>>(fun employeeId ->
            getEmployee (EmployeeId employeeId)
            |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.GetEmployees)
   |> ignore

   app
      .MapGet(
         EmployeePath.Get,
         Func<Guid, string, string, string, Task<IResult>>
            (fun
                 orgId
                 ([<FromQuery>] employeeIds)
                 ([<FromQuery>] roles)
                 ([<FromQuery>] status) ->
               let query = {
                  EmployeeIds =
                     EmployeeQuery.employeeIdsFromQueryString employeeIds
                  Roles = EmployeeQuery.rolesFromQueryString roles
                  Status =
                     // Only supporting search by Active status for now.
                     // Need to create an EmployeeStatusType type separate from
                     // EmployeeStatus type to handle cases such as
                     // EmployeeStatus.PendingInviteConfirmation of InviteToken
                     match status with
                     | "Active" -> Some EmployeeStatus.Active
                     | _ -> None
               }

               getEmployees (OrgId orgId) query
               |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.GetEmployees)
   |> ignore

   app
      .MapGet(
         EmployeePath.Search,
         Func<Guid, string, Task<IResult>>(fun orgId searchQuery ->
            searchEmployees (OrgId orgId) searchQuery
            |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.GetEmployees)
   |> ignore

   app
      .MapPost(
         EmployeePath.Base,
         Func<ActorSystem, CreateEmployeeCommand, Task<IResult>>(fun sys cmd ->
            processCommand sys (EmployeeCommand.CreateEmployee cmd)
            |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.CreateEmployee)
   |> ignore

   app
      .MapPost(
         EmployeePath.UpdateRole,
         Func<ActorSystem, UpdateRoleCommand, Task<IResult>>(fun sys cmd ->
            taskResult {
               let validation =
                  cmd
                  |> UpdateRoleCommand.toEvent
                  |> Result.map EmployeeEnvelope.get

               let! res = validation |> Result.mapError Err.ValidationError

               let msg =
                  cmd
                  |> UpdateEmployeeRole
                  |> ApprovableCommand.PerCommand
                  |> OrgMessage.ApprovableRequest

               (OrgActor.get sys cmd.OrgId) <! msg
               return res
            }
            |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.UpdateRole)
   |> ignore

   app
      .MapPost(
         EmployeePath.CancelEmployeeInvitation,
         Func<ActorSystem, CancelInvitationCommand, Task<IResult>>
            (fun sys cmd ->
               processCommand sys (EmployeeCommand.CancelInvitation cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageEmployeeAccess)
   |> ignore

   app
      .MapPost(
         EmployeePath.RestoreAccess,
         Func<ActorSystem, RestoreAccessCommand, Task<IResult>>(fun sys cmd ->
            processCommand sys (EmployeeCommand.RestoreAccess cmd)
            |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageEmployeeAccess)
   |> ignore

   app
      .MapPost(
         EmployeePath.ResendInviteNotification,
         Func<ActorSystem, Employee, HttpContext, Task<IResult>>
            (fun sys employee context -> task {
               match employee.Status with
               | EmployeeStatus.PendingInviteConfirmation token ->
                  if token.IsExpired() then
                     let initiator = {
                        Id =
                           context.Session.GetString("EmployeeId")
                           |> Guid.Parse
                           |> EmployeeId
                           |> InitiatedById
                        Name = context.Session.GetString("Name")
                     }

                     let cmd =
                        RefreshInvitationTokenCommand.create
                           employee.CompositeId
                           initiator
                           { Reason = None }
                        |> EmployeeCommand.RefreshInvitationToken

                     match! (processCommand sys cmd) with
                     | Ok _ -> return Results.Ok()
                     | Error e -> return RouteUtil.badRequest e
                  else
                     let invite: EmployeeInviteEmailInfo = {
                        OrgId = employee.OrgId
                        Name = employee.Name
                        Email = employee.Email
                        Token = token
                     }

                     EmailProducerActor.getProxy sys
                     <! EmailMessage.EmployeeInvite invite

                     return Results.Ok()
               | _ ->
                  let msg =
                     $"Employee status {employee.Status} not in a state to invite."

                  ActorUtil.SystemLog.error sys (exn msg) msg
                  //return Results.Forbid()
                  return Results.NotFound()
            })
      )
      .RBAC(Permissions.ResendInviteNotification)
   |> ignore
