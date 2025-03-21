module Bank.Transfer.Routes

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.FSharp.Core
open System
open System.Threading.Tasks
open Akka.Actor
open Akkling
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Bank.Transfer.Api
open Bank.Transfer.Domain
open Bank.Org.Domain
open Bank.Account.Domain
open CommandApproval
open RoutePaths
open Bank.UserSession.Middleware

let processAccountCommand = Bank.Account.Api.processCommand
let processOrgCommand = Bank.Org.Api.processCommand

let startTransferRoutes (app: WebApplication) =
   app
      .MapPost(
         TransferPath.DomesticTransferRecipient,
         Func<
            ActorSystem,
            RegisterDomesticTransferRecipientCommand,
            Task<IResult>
          >
            (fun sys cmd ->
               processOrgCommand
                  sys
                  (OrgCommand.RegisterDomesticTransferRecipient cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageTransferRecipient)
   |> ignore

   app
      .MapPost(
         TransferPath.DomesticTransferRecipientEdit,
         Func<ActorSystem, EditDomesticTransferRecipientCommand, Task<IResult>>
            (fun sys cmd ->
               processOrgCommand
                  sys
                  (OrgCommand.EditDomesticTransferRecipient cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageTransferRecipient)
   |> ignore

   app
      .MapGet(
         TransferPath.RetryableDomesticTransfersUponRecipientCorrection,
         Func<Guid, Task<IResult>>(fun recipientAccountId ->
            AccountId recipientAccountId
            |> getDomesticTransfersRetryableUponRecipientCorrection
            |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.ManageTransferRecipient)
   |> ignore

   app
      .MapPost(
         TransferPath.InternalWithinOrg,
         Func<ActorSystem, InternalTransferWithinOrgCommand, Task<IResult>>
            (fun sys cmd ->
               processAccountCommand sys (AccountCommand.InternalTransfer cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.SubmitTransfer)
   |> ignore


   app
      .MapPost(
         TransferPath.InternalBetweenOrgs,
         Func<ActorSystem, InternalTransferBetweenOrgsCommand, Task<IResult>>
            (fun sys cmd ->
               taskResult {
                  let validation =
                     cmd
                     |> InternalTransferBetweenOrgsCommand.toEvent
                     |> Result.map AccountEnvelope.get

                  let! res = validation |> Result.mapError Err.ValidationError

                  let msg =
                     cmd
                     |> InternalTransferBetweenOrgs
                     |> ApprovableCommand.AmountBased
                     |> OrgMessage.ApprovableRequest

                  (OrgActor.get sys cmd.OrgId) <! msg
                  return res
               }
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.SubmitTransfer)
   |> ignore

   app
      .MapPost(
         TransferPath.ScheduleInternalBetweenOrgs,
         Func<
            ActorSystem,
            ScheduleInternalTransferBetweenOrgsCommand,
            Task<IResult>
          >
            (fun sys cmd ->
               processAccountCommand
                  sys
                  (AccountCommand.ScheduleInternalTransferBetweenOrgs cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.SubmitTransfer)
   |> ignore

   app
      .MapPost(
         TransferPath.Domestic,
         Func<ActorSystem, DomesticTransferCommand, Task<IResult>>
            (fun sys cmd ->
               taskResult {
                  let validation =
                     cmd
                     |> DomesticTransferCommand.toEvent
                     |> Result.map AccountEnvelope.get

                  let! res = validation |> Result.mapError Err.ValidationError

                  let msg =
                     cmd
                     |> DomesticTransfer
                     |> ApprovableCommand.AmountBased
                     |> OrgMessage.ApprovableRequest

                  (OrgActor.get sys cmd.OrgId) <! msg
                  return res
               }
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.SubmitTransfer)
   |> ignore

   app
      .MapPost(
         TransferPath.ScheduleDomestic,
         Func<ActorSystem, ScheduleDomesticTransferCommand, Task<IResult>>
            (fun sys cmd ->
               processAccountCommand
                  sys
                  (AccountCommand.ScheduleDomesticTransfer cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.SubmitTransfer)
   |> ignore

   app
      .MapPost(
         TransferPath.NicknameRecipient,
         Func<
            ActorSystem,
            NicknameDomesticTransferRecipientCommand,
            Task<IResult>
          >
            (fun sys cmd ->
               processOrgCommand
                  sys
                  (OrgCommand.NicknameDomesticTransferRecipient cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageTransferRecipient)
   |> ignore

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
         Func<ActorSystem, RequestPlatformPaymentCommand, Task<IResult>>
            (fun sys cmd ->
               processAccountCommand
                  sys
                  (AccountCommand.RequestPlatformPayment cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManagePayment)
   |> ignore

   app
      .MapPost(
         PaymentPath.CancelPayment,
         Func<ActorSystem, CancelPlatformPaymentCommand, Task<IResult>>
            (fun sys cmd ->
               processAccountCommand
                  sys
                  (AccountCommand.CancelPlatformPayment cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManagePayment)
   |> ignore

   app
      .MapPost(
         PaymentPath.DeclinePayment,
         Func<ActorSystem, DeclinePlatformPaymentCommand, Task<IResult>>
            (fun sys cmd ->
               processAccountCommand
                  sys
                  (AccountCommand.DeclinePlatformPayment cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManagePayment)
   |> ignore

   app
      .MapPost(
         PaymentPath.FulfillPayment,
         Func<ActorSystem, FulfillPlatformPaymentCommand, Task<IResult>>
            (fun sys cmd ->
               taskResult {
                  let validation =
                     cmd
                     |> FulfillPlatformPaymentCommand.toEvent
                     |> Result.map AccountEnvelope.get

                  let! res = validation |> Result.mapError Err.ValidationError

                  let msg =
                     cmd
                     |> FulfillPlatformPayment
                     |> ApprovableCommand.AmountBased
                     |> OrgMessage.ApprovableRequest

                  (OrgActor.get sys cmd.OrgId) <! msg
                  return res
               }
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManagePayment)
   |> ignore

   app
      .MapPost(
         TransferPath.ConfigureAutoTransferRule,
         Func<ActorSystem, ConfigureAutoTransferRuleCommand, Task<IResult>>
            (fun sys cmd ->
               processAccountCommand
                  sys
                  (AccountCommand.ConfigureAutoTransferRule cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageAutoTransferRules)
   |> ignore

   app
      .MapPost(
         TransferPath.DeleteAutoTransferRule,
         Func<ActorSystem, DeleteAutoTransferRuleCommand, Task<IResult>>
            (fun sys cmd ->
               processAccountCommand
                  sys
                  (AccountCommand.DeleteAutoTransferRule cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageAutoTransferRules)
   |> ignore
