[<RequireQualifiedAccess>]
module SignalRActor

open Akka.Actor
open Akkling
open Microsoft.AspNetCore.SignalR

open ActorUtil
open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Org.Domain
open Bank.Hubs
open Lib.SharedTypes

[<RequireQualifiedAccess>]
type Msg =
   | AccountEventPersisted of AccountEventPersistedConfirmation
   | EmployeeEventPersisted of EmployeeEventPersistedConfirmation
   | OrgEventPersisted of OrgEventPersistedConfirmation
   | Error of EventProcessingError
   | CircuitBreaker of CircuitBreakerEvent

let actorProps (hub: IHubContext<BankHub, IBankClient>) =
   let handler (ctx: Actor<_>) =
      PubSub.subscribePointToPoint (PubSub.get ctx.System) ctx.Self

      actor {
         let! msg = ctx.Receive()

         match msg with
         | Msg.AccountEventPersisted msg ->
            hub.Clients
               .Group(string msg.Account.OrgId)
               .AccountEventPersistenceConfirmation(Serialization.serialize msg)
            |> ignore
         | Msg.EmployeeEventPersisted msg ->
            hub.Clients
               .Group(string msg.Employee.OrgId)
               .EmployeeEventPersistenceConfirmation(
                  Serialization.serialize msg
               )
            |> ignore
         | Msg.OrgEventPersisted msg ->
            hub.Clients
               .Group(string msg.Org.OrgId)
               .OrgEventPersistenceConfirmation(Serialization.serialize msg)
            |> ignore
         | Msg.Error err ->
            hub.Clients
               .Group(string err.OrgId)
               .EventProcessingError(Serialization.serialize err)
            |> ignore
         | Msg.CircuitBreaker msg ->
            hub.Clients.All.CircuitBreakerMessage(Serialization.serialize msg)
            |> ignore
      }

   props handler

let start (system: ActorSystem) (hub: IHubContext<BankHub, IBankClient>) =
   spawn system ActorMetadata.signalR.Name <| actorProps hub
