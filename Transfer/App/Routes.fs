module Bank.Routes.Transfer

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.FSharp.Core
open System
open System.Threading.Tasks
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
open BankActorRegistry
open PartnerBank.Service.Domain

let start
   (app: WebApplication)
   (processAccountCommand: _ -> AccountCommand -> TaskResult<Envelope, Err>)
   (createCounterPartyInPartnerBank:
      PartnerBankCounterpartyRequest
         -> Task<Result<PartnerBankCreateCounterpartyResponse, Err>>)
   =
   app
      .MapPost(
         TransferPath.RegisterCounterparty,
         Func<BankActorRegistry, RegisterCounterpartyCommand, Task<IResult>>
            (fun registry cmd ->
               taskResult {
                  let! evt =
                     RegisterCounterpartyCommand.toEvent cmd
                     |> Result.mapError Err.ValidationError

                  let cp = evt.Data.Counterparty

                  let! partnerBankRes =
                     createCounterPartyInPartnerBank {
                        Name = cp.FirstName + " " + cp.LastName
                        AccountNumber = cp.AccountNumber
                        RoutingNumber = cp.RoutingNumber
                        Depository = cp.Depository
                        Address = cp.Address
                     }

                  let cmd = {
                     cmd with
                        Data.PartnerBankCounterpartyId =
                           partnerBankRes.PartnerBankCounterpartyId
                  }

                  return!
                     cmd
                     |> ParentAccountCommand.RegisterCounterparty
                     |> AccountCommand.ParentAccount
                     |> processAccountCommand registry
               }
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageTransferRecipient)
   |> ignore

   app
      .MapPost(
         TransferPath.EditCounterparty,
         Func<BankActorRegistry, EditCounterpartyCommand, Task<IResult>>
            (fun registry cmd ->
               cmd
               |> ParentAccountCommand.EditCounterparty
               |> AccountCommand.ParentAccount
               |> processAccountCommand registry
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageTransferRecipient)
   |> ignore

   app
      .MapGet(
         TransferPath.RetryableDomesticTransfersUponCounterpartyCorrection,
         Func<Guid, Task<IResult>>(fun counterpartyId ->
            CounterpartyId counterpartyId
            |> getDomesticTransfersRetryableUponCounterpartyCorrection
            |> RouteUtil.unwrapTaskResultOption)
      )
      .RBAC(Permissions.ManageTransferRecipient)
   |> ignore

   app
      .MapPost(
         TransferPath.InternalWithinOrg,
         Func<BankActorRegistry, InternalTransferWithinOrgCommand, Task<IResult>>
            (fun registry cmd ->
               processAccountCommand
                  registry
                  (AccountCommand.InternalTransfer cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.SubmitTransfer)
   |> ignore

   app
      .MapPost(
         TransferPath.InternalBetweenOrgs,
         Func<
            BankActorRegistry,
            InternalTransferBetweenOrgsCommand,
            Task<IResult>
          >
            (fun registry cmd ->
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
                     |> GuaranteedDelivery.message cmd.OrgId.Value

                  let registry: IOrgGuaranteedDeliveryActor = registry
                  registry.OrgGuaranteedDeliveryActor() <! msg

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
            BankActorRegistry,
            ScheduleInternalTransferBetweenOrgsCommand,
            Task<IResult>
          >
            (fun registry cmd ->
               processAccountCommand
                  registry
                  (AccountCommand.ScheduleInternalTransferBetweenOrgs cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.SubmitTransfer)
   |> ignore

   app
      .MapPost(
         TransferPath.Domestic,
         Func<BankActorRegistry, DomesticTransferCommand, Task<IResult>>
            (fun registry cmd ->
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
                     |> GuaranteedDelivery.message cmd.OrgId.Value

                  let registry: IOrgGuaranteedDeliveryActor = registry
                  registry.OrgGuaranteedDeliveryActor() <! msg

                  return res
               }
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.SubmitTransfer)
   |> ignore

   app
      .MapPost(
         TransferPath.ScheduleDomestic,
         Func<BankActorRegistry, ScheduleDomesticTransferCommand, Task<IResult>>
            (fun registry cmd ->
               processAccountCommand
                  registry
                  (AccountCommand.ScheduleDomesticTransfer cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.SubmitTransfer)
   |> ignore

   app
      .MapPost(
         TransferPath.NicknameCounterparty,
         Func<BankActorRegistry, NicknameCounterpartyCommand, Task<IResult>>
            (fun registry cmd ->
               cmd
               |> ParentAccountCommand.NicknameCounterparty
               |> AccountCommand.ParentAccount
               |> processAccountCommand registry
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageTransferRecipient)
   |> ignore

   app
      .MapPost(
         TransferPath.ConfigureAutoTransferRule,
         Func<BankActorRegistry, ConfigureAutoTransferRuleCommand, Task<IResult>>
            (fun registry cmd ->
               processAccountCommand
                  registry
                  (AccountCommand.ConfigureAutoTransferRule cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageAutoTransferRules)
   |> ignore

   app
      .MapPost(
         TransferPath.DeleteAutoTransferRule,
         Func<BankActorRegistry, DeleteAutoTransferRuleCommand, Task<IResult>>
            (fun registry cmd ->
               processAccountCommand
                  registry
                  (AccountCommand.DeleteAutoTransferRule cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManageAutoTransferRules)
   |> ignore
