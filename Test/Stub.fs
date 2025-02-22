[<RequireQualifiedAccess>]
module Stub

open System
open Bank.Org.Domain

let billingStatement =
   BillingStatement.billingStatement
      AccountStub.accountStateAfterCreateWithEvents
      AccountStub.billingPeriod
      Int64.MaxValue

let signalRBroadcast: SignalRBroadcast = {
   circuitBreaker = fun msg -> ()
   accountEventPersisted = fun evt accountState -> ()
   employeeEventPersisted = fun evt employeeState -> ()
   orgEventPersisted = fun evt orgState -> ()
   accountEventError = fun orgId accountId err -> ()
   employeeEventError = fun orgId employeeId err -> ()
   orgEventError = fun orgId err -> ()
}

let akkaStreamsRestartSettings () =
   Akka.Streams.RestartSettings.Create(
      (TimeSpan.FromSeconds 3),
      (TimeSpan.FromSeconds 30),
      0.2
   )
