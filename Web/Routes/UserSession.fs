module Bank.UserSession.Routes

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

let private getValidInviteFromQuery (token: string) = taskResultOption {
   let lift = Ok >> Task.FromResult
   let! token = token |> Guid.parseOptional |> lift

   return! getEmployeeByInviteToken token
}

let private authorizeInvite
   (system: ActorSystem)
   (employee: Employee)
   (authProviderEmail: Email)
   (authProviderUserId: Guid)
   =
   let cmd =
      ConfirmInvitationCommand.create employee.CompositeId {
         Email = authProviderEmail
         AuthProviderUserId = authProviderUserId
         Reference = None
      }
      |> EmployeeCommand.ConfirmInvitation

   processCommand system cmd

let startUserSessionRoutes (app: WebApplication) =
   // TODO: Integrate with auth provider
   app.MapPost(
      UserSessionPath.Login,
      Func<HttpContext, Task<IResult>>(fun context -> task {
         let! employeeRes =
            getEmployee Constants.LOGGED_IN_EMPLOYEE_ID_REMOVE_SOON

         match employeeRes with
         | Error err -> return Results.BadRequest err
         | Ok None -> return Results.NotFound()
         | Ok(Some employee) ->
            context.Session.SetString("FirstName", employee.FirstName)
            context.Session.SetString("LastName", employee.LastName)
            context.Session.SetString("Email", string employee.Email)

            context.Session.SetString("EmployeeId", string employee.EmployeeId)

            context.Session.SetString("OrgId", string employee.OrgId)

            context.Session.SetString("Role", string employee.Role)
            return Results.Ok()
      })
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
      Func<ActorSystem, string, Task<IResult>>
         (fun system ([<FromQuery>] token) -> task {
            let! opt = getValidInviteFromQuery token

            match opt with
            | Ok(Some(employee, token)) ->
               // TODO: Redirect to auth provider sign in URL which upon
               // successful login will return to the AuthorizationCallback route.
               // Remove this authorizeInvite call to the AuthorizationCallback
               // route handler.
               let authProviderUserId = Guid.NewGuid()

               match!
                  authorizeInvite
                     system
                     employee
                     employee.Email
                     authProviderUserId
               with
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
      Func<ActorSystem, Task<IResult>>(fun system -> task {
         // TODO: Expect auth provider to set session info on login.
         //       Get user email from session
         // let email = session.Email
         let authProviderEmail = Email.deserialize ""
         let authProviderUserId = Guid.NewGuid()

         match! getEmployeeInviteByEmail authProviderEmail with
         | Ok(Some(employee, _)) ->
            match!
               authorizeInvite
                  system
                  employee
                  authProviderEmail
                  authProviderUserId
            with
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
