module Bank.Routes.UserSession

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open FsToolkit.ErrorHandling
open Akka.Actor

open Bank.Employee.Api
open Bank.Employee.Domain
open RoutePaths
open Lib.SharedTypes
open Bank.UserSession.Middleware
open BankActorRegistry
open Email

let private getValidInviteFromQuery (token: string) = taskResultOption {
   let lift = Ok >> Task.FromResult
   let! token = token |> Guid.parseOptional |> lift

   return! getEmployeeByInviteToken token
}

let private authorizeInvite
   (registry: BankActorRegistry)
   (invite: EmployeePendingInviteConfirmation)
   (authProviderUserId: Guid)
   =
   let cmd =
      ConfirmInvitationCommand.create
         {
            Name = invite.Name
            Id = InitiatedById invite.EmployeeId
         }
         invite.OrgId
         invite.InviteConfirmation.CorrelationId
         {
            Email = invite.Email
            AuthProviderUserId = authProviderUserId
            Reference = None
         }
      |> EmployeeCommand.ConfirmInvitation

   processCommand registry cmd

let private setUserSessionContext
   (context: HttpContext)
   (employeeId: EmployeeId)
   =
   task {
      let! employeeRes = getEmployee employeeId

      return
         match employeeRes with
         | Error err -> Results.BadRequest err
         | Ok None -> Results.NotFound()
         | Ok(Some employee) ->
            context.Session.SetString("Name", employee.Name)
            context.Session.SetString("Email", string employee.Email)
            context.Session.SetString("EmployeeId", string employee.EmployeeId)
            context.Session.SetString("OrgId", string employee.OrgId)
            context.Session.SetString("Role", string employee.Role)

            Results.Ok()
   }

let start (app: WebApplication) =
   app.MapGet(
      UserSessionPath.GetDemoUserSessions,
      Func<Guid, Task<IResult>>(fun orgId ->
         getDemoUserSessions (OrgId orgId) |> RouteUtil.unwrapTaskResultOption)
   )
   |> ignore

   app.MapPut(
      UserSessionPath.OverrideDemoUserSession,
      Func<Guid, HttpContext, Task<IResult>>(fun employeeId context ->
         setUserSessionContext context (EmployeeId employeeId))
   )
   |> ignore

   // TODO: Integrate with auth provider
   app.MapPost(
      UserSessionPath.Login,
      Func<HttpContext, Task<IResult>>(fun context ->
         setUserSessionContext
            context
            Constants.LOGGED_IN_EMPLOYEE_ID_REMOVE_SOON)
   )
   |> ignore

   app.MapGet(
      UserSessionPath.GetSession,
      Func<HttpContext, Task<IResult>>(fun context -> task {
         match getSession context with
         | Some session ->
            return Results.Content <| Serialization.serialize session
         | _ -> return Results.Unauthorized()
      })
   )
   |> ignore

   app.MapGet(
      UserSessionPath.AuthorizeInvite,
      Func<ActorSystem, BankActorRegistry, string, Task<IResult>>
         (fun system registry ([<FromQuery>] token) -> task {
            let! opt = getValidInviteFromQuery token

            match opt with
            | Ok(Some invite) ->
               // TODO: Redirect to auth provider sign in URL which upon
               // successful login will return to the AuthorizationCallback route.
               // Move this authorizeInvite call to the AuthorizationCallback
               // route handler.
               let authProviderUserId = Guid.NewGuid()

               match! authorizeInvite registry invite authProviderUserId with
               | Ok _ -> return Results.Ok()
               | Error err ->
                  let msg = $"Error authorizing invite: {err}"
                  ActorUtil.SystemLog.error system (exn msg) msg
                  //return Results.Forbid()
                  return Results.NotFound()
            | _ ->
               let msg =
                  $"Could not find valid employee invite from token: {token}"

               ActorUtil.SystemLog.error system (exn msg) msg
               //return Results.Forbid()
               return Results.NotFound()
         })
   )
   |> ignore

   app.MapGet(
      UserSessionPath.AuthorizationCallback,
      Func<BankActorRegistry, Task<IResult>>(fun registry -> task {
         // TODO: Expect auth provider to set session info on login.
         //       Get user email from session
         // let email = session.Email
         let authProviderEmail = Email.deserialize ""
         let authProviderUserId = Guid.NewGuid()

         match! getEmployeeInviteByEmail authProviderEmail with
         | Ok(Some invite) ->
            match! authorizeInvite registry invite authProviderUserId with
            | Ok _ -> return Results.Ok()
            | _ ->
               // TODO: replace notfound with forbid once auth configured
               //return Results.Forbid()
               return Results.NotFound()
         | _ ->
            //return Results.Forbid()
            return Results.NotFound()
      })
   )
   |> ignore
