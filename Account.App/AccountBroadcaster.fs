[<RequireQualifiedAccess>]
module AccountBroadcaster

open Akkling
open Akka.Actor
open System

open Lib.SharedTypes
open Bank.Account.Domain
open ActorUtil
open AccountLoadTestTypes

let init (system: ActorSystem) : AccountBroadcast =
   let pubSub = PubSub.get system
   let signalRPath = ActorMetadata.signalR.Path.ToStringWithoutAddress()
   let sendToSignalR = PubSub.sendPointToPoint pubSub signalRPath

   let loadTestPath =
      ActorMetadata.accountLoadTest.SingletonPath.ToStringWithoutAddress()

   {
      accountEventPersisted =
         fun event account ->
            let msg =
               SignalRActor.Msg.AccountEventPersisted(
                  {
                     Date = DateTime.UtcNow
                     EventPersisted = event
                     Account = account
                  }
               )

            sendToSignalR msg

            if Env.allowLiveLoadTest then
               let msg =
                  AccountLoadTestMessage.AccountEventPersisted {
                     AccountId = account.AccountId
                     Event = event
                     AccountBalance = account.Balance
                  }

               PubSub.sendPointToPoint pubSub loadTestPath msg

      accountEventValidationFail =
         fun orgId accountId err ->
            let msg =
               SignalRActor.Msg.AccountEventValidationFail {
                  OrgId = orgId
                  AccountId = accountId
                  Error = err
                  Date = DateTime.UtcNow
               }

            sendToSignalR msg
      accountEventPersistenceFail =
         fun orgId accountId err ->
            let msg =
               SignalRActor.Msg.AccountEventPersistenceFail {
                  OrgId = orgId
                  AccountId = accountId
                  Error = err
                  Date = DateTime.UtcNow
               }

            sendToSignalR msg
      circuitBreaker =
         fun msg ->
            let circuitBreakerAref = CircuitBreakerActor.get system
            circuitBreakerAref <! CircuitBreakerMessage.CircuitBreaker msg

            sendToSignalR (SignalRActor.Msg.CircuitBreaker msg)
   }
