[<RequireQualifiedAccess>]
module PaymentService

open Fable.SimpleHttp
open FsToolkit.ErrorHandling
open Fable.Core
open Browser.Types

open Lib.SharedTypes
open Bank.Transfer.Domain
open Bank.Payment.Domain
open RoutePaths

let private serviceName = "PaymentService"

let getPayments
   (orgId: OrgId)
   : Async<Result<PaymentRequestSummary option, Err>>
   =
   async {
      let path = PaymentPath.payments orgId

      let! code, responseText = Http.get path

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<PaymentRequestSummary>
            |> Result.map Some
   }

let getDomesticTransfersRetryableUponCounterpartyEdit
   (counterpartyId: CounterpartyId)
   : Async<Result<DomesticTransfer list option, Err>>
   =
   async {
      let path =
         TransferPath.retryableDomesticTransfersUponCounterpartyCorrection
            counterpartyId

      let! code, responseText = Http.get path

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<DomesticTransfer list>
            |> Result.map Some
   }

[<Emit("new FormData()")>]
let private createFormData () : Browser.Types.FormData = jsNative

/// Upload invoice document with optional Azure Document Intelligence parsing
let uploadInvoice
   (orgId: OrgId)
   (file: File)
   : Async<Result<InvoiceUploadId, Err>>
   =
   async {
      let path = PaymentPath.uploadInvoice orgId

      let form = createFormData ()
      form.append ("file", file)

      let! res =
         Http.request path
         |> Http.method POST
         |> Http.content (BodyContent.Form form)
         |> Http.send

      if res.statusCode <> 200 then
         return Error(Err.InvalidStatusCodeError(serviceName, res.statusCode))
      else
         return Serialization.deserialize<InvoiceUploadId> res.responseText
   }

let getInvoiceAttachment
   (uploadId: InvoiceUploadId)
   : Async<Result<File option, Err>>
   =
   async {
      let path =
         PaymentPath.InvoiceAttachment.Replace("{uploadId}", string uploadId)

      let! res =
         Http.request path
         |> Http.method HttpMethod.GET
         |> Http.overrideResponseType ResponseTypes.Blob
         |> Http.send

      match res.content with
      | ResponseContent.Blob blob ->
         // TODO: File name is currently empty string.  Change it
         let file = File.fromBlob blob ""
         return Ok(Some file)
      | _ ->
         if res.statusCode = 404 then
            return Ok None
         else
            return Error(Err.UnexpectedError res.responseText)
   }

[<Literal>]
let private InvoiceUploadSignalRName = "InvoiceParsed"

/// Register a callback for handling parsed invoices received over SignalR
let listenForInvoiceParsed
   (onInvoiceParsed: SignalRBroadcast.InvoiceParsed -> unit)
   (conn: SignalR.Connection)
   =
   conn.on (
      InvoiceUploadSignalRName,
      fun (msg: string) ->
         let deserialized =
            Serialization.deserialize<SignalRBroadcast.InvoiceParsed> msg

         match deserialized with
         | Error err ->
            Log.error $"Error deserializing invoice parsed msg {err}"
         | Ok evt -> onInvoiceParsed evt
   )

/// Remove event listener for invoice parsing via SignalR
let removeInvoiceParseListener (conn: SignalR.Connection) =
   conn.off InvoiceUploadSignalRName
