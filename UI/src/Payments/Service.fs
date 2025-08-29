[<RequireQualifiedAccess>]
module PaymentService

open Fable.SimpleHttp
open FsToolkit.ErrorHandling

open Bank.Transfer.Domain
open Bank.Payment.Domain
open Lib.SharedTypes
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
