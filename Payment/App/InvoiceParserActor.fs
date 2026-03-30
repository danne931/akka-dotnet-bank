[<RequireQualifiedAccess>]
module InvoiceParserActor

open System
open System.Threading.Tasks
open Akka.Actor
open Akkling

open Lib.SharedTypes
open Bank.Payment.Domain
open BankActorRegistry

[<RequireQualifiedAccess>]
type Message =
   | ParseInvoice of InvoiceUpload
   | InvoiceParsed of InvoiceUploadId * OrgId * Result<ParsedInvoice, string>

let actorProps
   (broadcaster: SignalRBroadcast.SignalRBroadcast)
   (parse: Uri -> Task<Result<ParsedInvoice, string>>)
   (persistence: InvoiceDraftPersistence)
   =
   let handler (ctx: Actor<Message>) =
      let logDebug = logDebug ctx
      let logError = logError ctx

      let rec loop () = actor {
         let! msg = ctx.Receive()

         match msg with
         | Message.ParseInvoice info ->
            logDebug $"Processing invoice {info.UploadId} for org {info.OrgId}"

            let draft = InvoiceDraft.create info.UploadId info.OrgId
            let! saveResult = persistence.create draft

            match saveResult with
            | Error err -> logError $"Failed to create invoice: {err}"
            | Ok() ->
               let parseInBackground = async {
                  let! result = parse info.BlobUrl |> Async.AwaitTask

                  return
                     Message.InvoiceParsed(info.UploadId, info.OrgId, result)
               }

               ctx.Self <!| parseInBackground
         | Message.InvoiceParsed(uploadId, orgId, parseResult) ->
            match parseResult with
            | Ok parsed ->
               match! persistence.parseCompleted uploadId parsed with
               | Ok() ->
                  logDebug $"Successfully parsed invoice {uploadId}"
                  broadcaster.invoiceParsed orgId uploadId parsed
               | Error errMsg -> logError $"Failed to parse invoice: {errMsg}"
            | Error err ->
               logError $"Failed to parse invoice: {err}"

               match! persistence.parseFailed uploadId err with
               | Error errMsg -> logError $"Failed to mark as failed: {errMsg}"
               | Ok() -> ()

         return! loop ()
      }

      loop ()

   props handler

let get (sys: ActorSystem) : IActorRef<Message> =
   let registry = Akka.Hosting.ActorRegistry.For sys
   registry.Get<ActorMarker.InvoiceParser>() |> typed
