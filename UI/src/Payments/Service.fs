[<RequireQualifiedAccess>]
module PaymentService

open Fable.SimpleHttp
open FsToolkit.ErrorHandling
open Fable.Core

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

/// Upload invoice document for Azure Document Intelligence parsing
let uploadInvoice
   (orgId: OrgId)
   (file: Browser.Types.File)
   : Async<Result<InvoiceDraftId, Err>>
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
         let result =
            Serialization.deserialize<{| UploadId: InvoiceDraftId |}>
               res.responseText

         return result |> Result.map _.UploadId
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
