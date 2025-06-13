module DiagnosticsService

open Fable.SimpleHttp
open Lib.SharedTypes
open Lib.CircuitBreaker
open RoutePaths

[<RequireQualifiedAccess>]
type ServiceHealth =
   | Good
   | Bad

let health circuitBreakerState =
   match circuitBreakerState with
   | CircuitBreakerStatus.Closed -> ServiceHealth.Good
   | _ -> ServiceHealth.Bad

[<RequireQualifiedAccess>]
type ServiceDiagnostics = {
   DomesticTransfer: ServiceHealth
   Email: ServiceHealth
   KnowYourCustomer: ServiceHealth
   PartnerBank: ServiceHealth
}

let getServiceHealth () : Async<Result<ServiceDiagnostics, Err>> = async {
   let! (code, responseText) = Http.get DiagnosticPath.CircuitBreaker

   if code <> 200 then
      return Error(Err.InvalidStatusCodeError("Diagnostic Service", code))
   else
      return
         responseText
         |> Serialization.deserialize<CircuitBreakerState>
         |> Result.map (fun circuitBreaker -> {
            DomesticTransfer = health circuitBreaker.DomesticTransfer
            Email = health circuitBreaker.Email
            KnowYourCustomer = health circuitBreaker.KnowYourCustomer
            PartnerBank = health circuitBreaker.PartnerBank
         })
}

let listenForCircuitBreakerEvent
   (onCircuitBreakerEvent: CircuitBreakerEvent -> unit)
   (conn: SignalR.Connection)
   =
   conn.on (
      "CircuitBreakerMessage",
      fun (msg: string) ->
         let deserialized = Serialization.deserialize<CircuitBreakerEvent> msg

         match deserialized with
         | Error err ->
            Log.error $"Error deserializing circuit breaker msg {err}"
         | Ok msg -> onCircuitBreakerEvent msg
   )
