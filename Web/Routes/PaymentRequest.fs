module Bank.Routes.PaymentRequest

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.FSharp.Core
open System
open System.Threading.Tasks

open Lib.SharedTypes
open Bank.Payment.Api
open Bank.Payment.Domain
open Bank.Account.Domain
open RoutePaths
open Bank.UserSession.Middleware
open BankActorRegistry

let processAccountCommand = Bank.Account.Api.processCommand

let start (app: WebApplication) =
   app
      .MapGet(
         PaymentPath.Payments,
         Func<Guid, Task<IResult>>(fun orgId ->
            OrgId orgId |> getPayments |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.ViewPayments)
   |> ignore

   app
      .MapPost(
         PaymentPath.RequestPayment,
         Func<BankActorRegistry, RequestPaymentCommand, Task<IResult>>
            (fun sys cmd ->
               processAccountCommand sys (AccountCommand.RequestPayment cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManagePayment)
   |> ignore

   app
      .MapPost(
         PaymentPath.CancelPayment,
         Func<BankActorRegistry, CancelPaymentRequestCommand, Task<IResult>>
            (fun registry cmd ->
               processAccountCommand
                  registry
                  (AccountCommand.CancelPaymentRequest cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManagePayment)
   |> ignore

   app
      .MapPost(
         PaymentPath.DeclinePayment,
         Func<BankActorRegistry, DeclinePaymentRequestCommand, Task<IResult>>
            (fun registry cmd ->
               processAccountCommand
                  registry
                  (AccountCommand.DeclinePaymentRequest cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManagePayment)
   |> ignore
