module Bank.Routes.PaymentRequest

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.FSharp.Core
open System
open System.Threading.Tasks
open Akka.Actor
open Akkling
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Bank.Payment.Api
open Bank.Payment.InvoiceApi
open Bank.Payment.Domain
open Bank.Account.Domain
open RoutePaths
open Bank.UserSession.Middleware
open BankActorRegistry

let start (app: WebApplication) processAccountCommand =
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
            (fun registry cmd ->
               processAccountCommand
                  registry
                  (AccountCommand.RequestPayment cmd)
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

   app
      .MapPost(
         PaymentPath.UploadInvoice,
         Func<ActorSystem, HttpRequest, Guid, Task<IResult>>
            (fun sys request orgId ->
               let getFile () =
                  match request.Form.Files.Count with
                  | 0 -> Error "No file uploaded"
                  | _ -> Ok request.Form.Files[0]

               taskResult {
                  let! file = getFile ()

                  let! containerClient = BlobStorage.getContainerClient ()

                  let orgId = OrgId orgId

                  let! blobUrl =
                     FileStorage.uploadInvoice containerClient orgId file

                  let draftId = Guid.NewGuid() |> InvoiceDraftId

                  InvoiceParserActor.get sys
                  <! InvoiceParserActor.Message.ParseInvoice {
                     DraftId = draftId
                     OrgId = orgId
                     BlobUrl = blobUrl
                  }

                  return {| DraftId = draftId |}
               }
               |> TaskResult.mapError Err.UnexpectedError
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.ManagePayment)
   |> ignore
