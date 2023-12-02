[<RequireQualifiedAccess>]
module SignalRProxy

open Akka.Actor
open Akka.Cluster.Tools.PublishSubscribe

open Bank.Account.Domain
open ActorUtil

let init (system: ActorSystem) : SignalRBroadcast =
   let mediator = DistributedPubSub.Get(system).Mediator
   let path = $"/user/{ActorMetadata.signalR.Name}"

   {
      accountEventPersisted =
         fun event account ->
            mediator.Tell(
               Send(path, SignalRMessage.AccountEventPersisted(event, account))
            )
      accountEventValidationFail =
         fun accountId errMsg ->
            mediator.Tell(
               Send(
                  path,
                  SignalRMessage.AccountEventValidationFail(accountId, errMsg)
               )
            )
      accountEventPersistenceFail =
         fun accountId errMsg ->
            mediator.Tell(
               Send(
                  path,
                  SignalRMessage.AccountEventPersistenceFail(accountId, errMsg)
               )
            )
      circuitBreaker =
         fun msg -> mediator.Tell(Send(path, SignalRMessage.CircuitBreaker msg))
      endBillingCycle =
         fun () -> mediator.Tell(Send(path, SignalRMessage.EndBillingCycle))
   }