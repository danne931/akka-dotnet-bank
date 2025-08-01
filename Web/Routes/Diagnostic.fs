module Bank.Routes.Diagnostic

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open type Microsoft.AspNetCore.Http.Results
open Akka.Actor
open Akkling
open FSharp.Control

open Lib.SharedTypes
open Bank.Account.Domain
open AccountLoadTestTypes
open RoutePaths
open Bank.UserSession.Middleware
open Lib.CircuitBreaker

let start (app: WebApplication) =
   app
      .MapGet(
         DiagnosticPath.Account,
         Func<ActorSystem, Guid, Task<IResult>>(fun sys id ->
            let ref = AccountActor.get sys (ParentAccountId id)

            let askTask: ParentAccountSnapshot option Task =
               ref.Ask(
                  AccountMessage.GetAccount,
                  Some(TimeSpan.FromSeconds 3.)
               )
               |> Async.toTask

            RouteUtil.unwrapTaskOption askTask)
      )
      .RBAC(Permissions.Diagnostic)
   |> ignore

   app
      .MapGet(
         DiagnosticPath.CircuitBreaker,
         Func<ActorSystem, Task<IResult>>(fun sys ->
            let ref = CircuitBreakerActor.get sys

            let lookup: CircuitBreakerState Task =
               ref <? CircuitBreakerMessage.Lookup |> Async.toTask

            lookup |> RouteUtil.unwrapTask)
      )
      .RBAC(Permissions.Diagnostic)
   |> ignore

   app
      .MapGet(
         DiagnosticPath.LoadTest,
         Func<ActorSystem, Task<IResult>>(fun sys -> task {
            if Env.allowLiveLoadTest then
               let ref = AccountLoadTestActor.get sys
               ref <! AccountLoadTestMessage.StartLoadTest
               return Ok()
            else
               return Unauthorized()
         })
      )
      .RBAC(Permissions.Diagnostic)
   |> ignore

   app
      .MapGet(
         DiagnosticPath.LoadTestProgress,
         Func<ActorSystem, Task<IResult>>(fun sys -> task {
            if Env.allowLiveLoadTest then
               let ref = AccountLoadTestActor.get sys

               let! (progress: AccountLoadTestActor.AccountLoadTestStateMessage) =
                  ref <? AccountLoadTestMessage.Lookup |> Async.toTask

               return Ok progress
            else
               return Unauthorized()
         })
      )
      .RBAC(Permissions.Diagnostic)
   |> ignore

   app
      .MapGet(
         DiagnosticPath.LoadTestTeardown,
         Func<ActorSystem, Task<IResult>>(fun sys -> task {
            if Env.allowLiveLoadTest then
               let ref = AccountLoadTestActor.get sys
               ref <! AccountLoadTestMessage.Teardown
               return Ok()
            else
               return Unauthorized()
         })
      )
      .RBAC(Permissions.Diagnostic)
   |> ignore
