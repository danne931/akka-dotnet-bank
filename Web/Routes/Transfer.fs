module Bank.Routes.Transfer

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

let start (app: WebApplication) =
   app
      .MapPost(
         TransferPath.DomesticTransferRecipient,
         Func<
            ActorSystem,
            RegisterDomesticTransferRecipientCommand,
            Task<IResult>
          >
            (fun sys cmd ->
               cmd
               |> ParentAccountCommand.RegisterDomesticTransferRecipient
               |> AccountCommand.ParentAccount
               |> processAccountCommand sys
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageTransferRecipient)
   |> ignore

   app
      .MapPost(
         TransferPath.DomesticTransferRecipientEdit,
         Func<ActorSystem, EditDomesticTransferRecipientCommand, Task<IResult>>
            (fun sys cmd ->
               cmd
               |> ParentAccountCommand.EditDomesticTransferRecipient
               |> AccountCommand.ParentAccount
               |> processAccountCommand sys
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

                  OrgActor.get sys cmd.OrgId <! msg
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
               cmd
               |> ParentAccountCommand.NicknameDomesticTransferRecipient
               |> AccountCommand.ParentAccount
               |> processAccountCommand sys
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageTransferRecipient)
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
