[<RequireQualifiedAccess>]
module SignalRBroadcaster

open Akkling
open Akka.Actor
open System

open Lib.CircuitBreaker
open Bank.Account.Domain
open SignalRBroadcast
open ActorUtil
open AccountLoadTestTypes

let init (system: ActorSystem) : SignalRBroadcast =
   let pubSub = PubSub.get system
   let signalRPath = ActorMetadata.signalR.Path.ToStringWithoutAddress()
   let sendToSignalR = PubSub.sendPointToPoint pubSub signalRPath

   let loadTestPath =
      ActorMetadata.accountLoadTest.SingletonPath.ToStringWithoutAddress()

   let sendError = SignalRActor.Msg.Error >> sendToSignalR

   {
      parentAccountEventPersisted =
         fun event ->
            let msg =
               SignalRActor.Msg.ParentAccountEventPersisted(
                  {
                     Date = DateTime.UtcNow
                     EventPersisted = event
                  }
               )

            sendToSignalR msg
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
                     OrgId = account.OrgId
                     Event = event
                     AccountBalance = account.Balance
                  }

               PubSub.sendPointToPoint pubSub loadTestPath msg

      employeeEventPersisted =
         fun event employee ->
            let msg =
               SignalRActor.Msg.EmployeeEventPersisted(
                  {
                     Date = DateTime.UtcNow
                     EventPersisted = event
                     Employee = employee
                  }
               )

            sendToSignalR msg

      orgEventPersisted =
         fun event org ->
            let msg =
               SignalRActor.Msg.OrgEventPersisted(
                  {
                     Date = DateTime.UtcNow
                     EventPersisted = event
                     Org = org
                  }
               )

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
            let circuitBreakerAref = CircuitBreakerActor.get system
            circuitBreakerAref <! CircuitBreakerMessage.CircuitBreaker msg

            sendToSignalR (SignalRActor.Msg.CircuitBreaker msg)
   }
