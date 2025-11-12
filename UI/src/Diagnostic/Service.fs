module DiagnosticsService

open Fable.SimpleHttp
open Feliz.Router

open Lib.SharedTypes
open Lib.CircuitBreaker
open Lib.NetworkQuery
open RoutePaths
open SagaDTO

[<RequireQualifiedAccess>]
type ServiceHealth =
   | Good
   | Bad

let health circuitBreakerState =
   match circuitBreakerState with
   | CircuitBreakerStatus.Closed -> ServiceHealth.Good
   | _ -> ServiceHealth.Bad

type ServiceDiagnostics = {
   Email: ServiceHealth
   KnowYourCustomer: ServiceHealth
   PartnerBank: ServiceHealth
   CardIssuer: ServiceHealth
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
            Email = health circuitBreaker.Email
            KnowYourCustomer = health circuitBreaker.KnowYourCustomer
            PartnerBank = health circuitBreaker.PartnerBank
            CardIssuer = health circuitBreaker.CardIssuer
         })
}

[<Literal>]
let private CircuitBreakerSignalRName = "CircuitBreakerMessage"

/// Register a callback for handling circuit breaker updates received over SignalR
let listenForCircuitBreakerEvent
   (onCircuitBreakerEvent: CircuitBreakerEvent -> unit)
   (conn: SignalR.Connection)
   =
   conn.on (
      CircuitBreakerSignalRName,
      fun (msg: string) ->
         let deserialized = Serialization.deserialize<CircuitBreakerEvent> msg

         match deserialized with
         | Error err ->
            Log.error $"Error deserializing circuit breaker msg {err}"
         | Ok msg -> onCircuitBreakerEvent msg
   )

/// Remove event listener for circuit breaker updates via SignalR
let removeCircuitBreakerListener (conn: SignalR.Connection) =
   conn.off CircuitBreakerSignalRName

[<Literal>]
let private SagaSignalRName = "SagaUpdated"

/// Register a callback for handling saga updates received over SignalR
let listenForSagaUpdate
   (onSagaUpdated: SignalRBroadcast.SagaUpdated -> unit)
   (conn: SignalR.Connection)
   =
   conn.on (
      SagaSignalRName,
      fun (msg: string) ->
         let deserialized =
            Serialization.deserialize<SignalRBroadcast.SagaUpdated> msg

         match deserialized with
         | Error err -> Log.error $"Error deserializing saga update msg {err}"
         | Ok msg -> onSagaUpdated msg
   )

/// Remove event listener for saga updates via SignalR
let removeSagaUpdateListener (conn: SignalR.Connection) =
   conn.off SagaSignalRName

let getSagaHistory
   (orgId: OrgId)
   (query: SagaQuery)
   : Async<Result<SagaDTO list option, Err>>
   =
   async {
      let queryParams =
         [
            "pageLimit", string query.PageLimit

            match query.Cursor with
            | Some cursor ->
               "cursorSagaId", string cursor.SagaId
               "cursorCreatedAt", DateTime.toISOString cursor.CreatedAt
            | None -> ()

            match query.Status with
            | None -> ()
            | Some filters -> "status", listToQueryString filters

            match query.SagaKind with
            | None -> ()
            | Some filters -> "kind", listToQueryString filters

            match query.DateRange with
            | None -> ()
            | Some(startDate, endDate) ->
               "date", DateTime.rangeAsQueryString startDate endDate
         ]
         |> Router.encodeQueryString

      let path = DiagnosticPath.sagas orgId + queryParams
      let! (code, responseText) = Http.get path

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error(Err.InvalidStatusCodeError("Diagnostic Service", code))
      else
         return
            Serialization.deserialize<SagaDTO list> responseText
            |> Result.map Some
   }

let retrySagaActivity
   (orgId: OrgId)
   (sagaId: CorrelationId)
   (activity: ActivityRecoverableByHumanInTheLoop)
   : Async<Result<unit, Err>>
   =
   async {
      let path = DiagnosticPath.retrySagaActivity orgId sagaId (string activity)
      let! code, _ = Http.post path null

      if code <> 200 then
         return Error(Err.InvalidStatusCodeError("Diagnostic Service", code))
      else
         return Ok()
   }
