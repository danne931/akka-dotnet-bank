[<RequireQualifiedAccess>]
module SignalRBroadcaster

open Akkling
open Akka.Actor
open System

open Lib.CircuitBreaker
open SignalRBroadcast
open ActorUtil
open BankActorRegistry

let init
   (system: ActorSystem)
   (registry: #ICircuitBreakerActor)
   : SignalRBroadcast
   =
   let pubSub = PubSub.get system
   let signalRPath = ActorMetadata.signalR.Path.ToStringWithoutAddress()
   let sendToSignalR = PubSub.sendPointToPoint pubSub signalRPath

   let sendError = SignalRActor.Msg.Error >> sendToSignalR

   {
      parentAccountEventPersisted =
         fun event ->
            let msg =
               SignalRActor.Msg.ParentAccountEventPersisted {
                  Date = DateTime.UtcNow
                  EventPersisted = event
               }

            sendToSignalR msg
      accountEventPersisted =
         fun event account ->
            let msg =
               SignalRActor.Msg.AccountEventPersisted {
                  Date = DateTime.UtcNow
                  EventPersisted = event
                  Account = account
               }

            sendToSignalR msg
      employeeEventPersisted =
         fun event employee ->
            let msg =
               SignalRActor.Msg.EmployeeEventPersisted {
                  Date = DateTime.UtcNow
                  EventPersisted = event
                  Employee = employee
               }

            sendToSignalR msg

      orgEventPersisted =
         fun event org ->
            let msg =
               SignalRActor.Msg.OrgEventPersisted {
                  Date = DateTime.UtcNow
                  EventPersisted = event
                  Org = org
               }

            sendToSignalR msg

      accountEventError =
         fun orgId accountId correlationId err ->
            EventProcessingError.Account(
               orgId,
               accountId,
               correlationId,
               err,
               DateTime.UtcNow
            )
            |> sendError

      employeeEventError =
         fun orgId employeeId correlationId err ->
            EventProcessingError.Employee(
               orgId,
               employeeId,
               correlationId,
               err,
               DateTime.UtcNow
            )
            |> sendError

      orgEventError =
         fun orgId correlationId err ->
            EventProcessingError.Org(orgId, correlationId, err, DateTime.UtcNow)
            |> sendError

      circuitBreaker =
         fun msg ->
            registry.CircuitBreakerActor()
            <! CircuitBreakerMessage.CircuitBreaker msg

            sendToSignalR (SignalRActor.Msg.CircuitBreaker msg)

      sagaUpdated =
         fun orgId saga ->
            let msg =
               SignalRActor.Msg.SagaUpdated {
                  Date = DateTime.UtcNow
                  Saga = saga
                  OrgId = orgId
               }

            sendToSignalR msg

      invoiceParsed =
         fun orgId draftId parsedData ->
            let msg =
               SignalRActor.Msg.InvoiceParsed {
                  Date = DateTime.UtcNow
                  DraftId = draftId
                  ParsedData = parsedData
                  OrgId = orgId
               }

            sendToSignalR msg
   }
