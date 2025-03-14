module DomesticTransferActorTests

open System
open System.Threading.Tasks
open Akka.Actor
open Akkling
open Akkling.Cluster.Sharding
open Expecto

open Util
open ActorUtil
open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Transfer.Domain
open DomesticTransferRecipientActor
open Email

module Stub = AccountStub

let initMockAccountActor (tck: TestKit.Tck) =
   let handler (ctx: Actor<_>) (msg: obj) =
      match msg with
      | :? ShardEnvelope as envelope ->
         match envelope.Message with
         | :? AccountMessage as msg ->
            match msg with
            | AccountMessage.StateChange cmd ->
               match cmd with
               | AccountCommand.UpdateDomesticTransferProgress cmd ->
                  tck.TestActor.Tell cmd
                  ignored ()
               | AccountCommand.FailDomesticTransfer cmd ->
                  tck.TestActor.Tell cmd
                  ignored ()
               | AccountCommand.CompleteDomesticTransfer cmd ->
                  tck.TestActor.Tell cmd
                  ignored ()
               | msg -> unhandled msg
            | msg -> unhandled msg
         | msg -> unhandled msg
      | msg -> unhandled msg

   spawnAnonymous tck <| props (actorOf2 handler)

let mockSender = {
   Name = "Big Fish"
   AccountNumber = "544112"
   RoutingNumber = "923921"
}

let mockRecipient = {
   Name = "Small Fish"
   AccountNumber = "23423421"
   RoutingNumber = "1092351"
   Depository = "checking"
}

// Will fail with an intermittent "Connection" error
// for the 2nd and 3rd request received.
let mockTransferRequestFactory () : DomesticTransferRequest =
   let mutable requestCount = 0

   fun (action: DomesticTransferServiceAction) (txn: DomesticTransfer) ->
      requestCount <- requestCount + 1

      if requestCount > 2 && requestCount <= 4 then
         "Connection terminated"
         |> exn
         |> Err.NetworkError
         |> Error
         |> Task.FromResult
      else
         let status =
            match action with
            | DomesticTransferServiceAction.TransferAck -> "Processing"
            | DomesticTransferServiceAction.ProgressCheck -> "Complete"

         let response: DomesticTransferServiceResponse = {
            Sender = mockSender
            Recipient = mockRecipient
            Ok = true
            Reason = ""
            Status = status
            TransactionId = string <| Guid.NewGuid()
         }

         response |> Ok |> Task.FromResult

let mockTransferRequestInvalid
   (action: DomesticTransferServiceAction)
   (txn: DomesticTransfer)
   =
   let response: DomesticTransferServiceResponse = {
      Sender = mockSender
      Recipient = mockRecipient
      Ok = false
      Reason = "InvalidAction"
      Status = ""
      TransactionId = string <| Guid.NewGuid()
   }

   response |> Ok |> Task.FromResult

let mockTransferRequestInactiveAccount
   (action: DomesticTransferServiceAction)
   (txn: DomesticTransfer)
   =
   let response: DomesticTransferServiceResponse = {
      Sender = mockSender
      Recipient = mockRecipient
      Ok = false
      Reason = "InactiveAccount"
      Status = ""
      TransactionId = string <| Guid.NewGuid()
   }

   response |> Ok |> Task.FromResult

let mockTransferRequestSerializationIssue
   (action: DomesticTransferServiceAction)
   (txn: DomesticTransfer)
   =
   "Serialization" |> Err.SerializationError |> Result.Error |> Task.FromResult

let initCircuitBreaker (tck: TestKit.Tck) =
   let breaker =
      Akka.Pattern.CircuitBreaker(
         tck.Sys.Scheduler,
         maxFailures = 2,
         callTimeout = TimeSpan.FromSeconds 1,
         resetTimeout = TimeSpan.FromMilliseconds 100
      )

   // TODO:
   // OnHalfOpen/OnClose is not being triggered in test environment
   // despite a resetTimeout of 100ms.
   // Not sure how to test reprocessing of stashed transfers
   // if can't hook into HalfOpen or Closed status or initiate
   // them manually.
   (*
   breaker.OnHalfOpen(fun () -> 
      tck.TestActor.Tell CircuitBreakerStatus.HalfOpen
   )
   |> ignore
   *)

   breaker.OnOpen(fun () -> tck.TestActor.Tell CircuitBreakerStatus.Open)

let initDomesticTransferActor
   (tck: TestKit.Tck)
   (breaker: Akka.Pattern.CircuitBreaker)
   (getAccountActor: AccountId -> IEntityRef<AccountMessage>)
   (emailActor: IActorRef<EmailMessage>)
   (transferRequest: DomesticTransferRequest)
   =
   let prop =
      DomesticTransferRecipientActor.actorProps
         breaker
         getAccountActor
         (fun _ -> emailActor)
         transferRequest

   spawn tck ActorMetadata.domesticTransfer.Name prop

let init (tck: TestKit.Tck) (mockTransferRequest: DomesticTransferRequest) =
   let breaker = initCircuitBreaker tck
   let mockAccountRef = initMockAccountActor tck

   let emailProbe = TestKit.probe tck

   let domesticTransferRef =
      initDomesticTransferActor tck breaker
      <| getAccountEntityRef mockAccountRef
      <| (typed emailProbe :> IActorRef<EmailMessage>)
      <| mockTransferRequest

   domesticTransferRef, breaker, emailProbe

[<Tests>]
let tests =
   testList "Domestic Transfer Actor" [
      akkaTest
         "Issuing a transfer for a valid 3rd party bank recipient should approve the transfer"
      <| None
      <| fun tck ->
         let domesticTransferRef, _, _ =
            init tck <| mockTransferRequestFactory ()

         let evt = Stub.event.domesticTransferPending
         let txn = TransferEventToDomesticTransfer.fromPending evt

         domesticTransferRef
         <! DomesticTransferMessage.TransferRequest(
            DomesticTransferServiceAction.TransferAck,
            txn
         )

         let cmd = tck.ExpectMsg<UpdateDomesticTransferProgressCommand>()

         Expect.equal
            (AccountId.fromEntityId cmd.EntityId)
            txn.Sender.AccountId
            $"SenderAccountId from Transfer Transaction should be
              EntityId of resulting UpdateTransferProgressCommand"

         let _ =
            Expect.wantOk
               (UpdateDomesticTransferProgressCommand.toEvent cmd)
               "TransferProgress cmd -> event validation ok"

         domesticTransferRef
         <! DomesticTransferMessage.TransferRequest(
            DomesticTransferServiceAction.ProgressCheck,
            txn
         )

         let cmd = tck.ExpectMsg<CompleteDomesticTransferCommand>()

         Expect.equal
            (AccountId.fromEntityId cmd.EntityId)
            txn.Sender.AccountId
            $"SenderAccountId from Transfer Transaction should be
              EntityId of resulting ApproveTransferCommand"

      akkaTest
         "Issuing a transfer for an inactive 3rd party bank recipient should
          reject the transfer"
      <| None
      <| fun tck ->
         let domesticTransferRef, _, _ =
            init tck mockTransferRequestInactiveAccount

         let evt = Stub.event.domesticTransferPending
         let txn = TransferEventToDomesticTransfer.fromPending evt

         domesticTransferRef
         <! DomesticTransferMessage.TransferRequest(
            DomesticTransferServiceAction.TransferAck,
            txn
         )

         let msg = tck.ExpectMsg<FailDomesticTransferCommand>()

         Expect.equal
            (AccountId.fromEntityId msg.EntityId)
            txn.Sender.AccountId
            $"SenderAccountId from Transaction should be
              EntityId of resulting RejectTransferCommand"

      akkaTest
         "Issuing a transfer request with an API mismatch should email support."
      <| None
      <| fun tck ->
         let domesticTransferRef, _, emailProbe =
            init tck mockTransferRequestInvalid

         let subscriber = tck.CreateTestProbe()

         tck.Sys.EventStream.Subscribe(
            subscriber.Ref,
            typeof<Akka.Event.UnhandledMessage>
         )
         |> ignore

         let evt = Stub.event.domesticTransferPending
         let txn = TransferEventToDomesticTransfer.fromPending evt

         domesticTransferRef
         <! DomesticTransferMessage.TransferRequest(
            DomesticTransferServiceAction.TransferAck,
            txn
         )

         let msg = emailProbe.ExpectMsg<EmailMessage>()

         match msg with
         | EmailMessage.ApplicationErrorRequiresSupport _ ->
            Expect.isTrue true ""
         | msg ->
            Expect.isTrue
               false
               $"Expected EmailMessage.ApplicationErrorRequiresSupport.
                 Received {msg}"

         tck.ExpectNoMsg()
         subscriber.ExpectMsg<Akka.Event.UnhandledMessage>() |> ignore

      akkaTest
         "Experiencing serialization issues for transfer request should email support."
      <| None
      <| fun tck ->
         let domesticTransferRef, _, emailProbe =
            init tck mockTransferRequestSerializationIssue

         let subscriber = tck.CreateTestProbe()

         tck.Sys.EventStream.Subscribe(
            subscriber.Ref,
            typeof<Akka.Event.UnhandledMessage>
         )
         |> ignore

         let evt = Stub.event.domesticTransferPending
         let txn = TransferEventToDomesticTransfer.fromPending evt

         domesticTransferRef
         <! DomesticTransferMessage.TransferRequest(
            DomesticTransferServiceAction.TransferAck,
            txn
         )

         let msg = emailProbe.ExpectMsg<EmailMessage>()

         match msg with
         | EmailMessage.ApplicationErrorRequiresSupport _ ->
            Expect.isTrue true ""
         | msg ->
            Expect.isTrue
               false
               $"Expected EmailMessage.ApplicationErrorRequiresSupport.
                 Received {msg}"

         tck.ExpectNoMsg()
         subscriber.ExpectMsg<Akka.Event.UnhandledMessage>() |> ignore

      akkaTest
         "Issuing a transfer for a valid 3rd party bank recipient
          should open the circuit breaker for intermittent failures."
      <| None
      <| fun tck ->
         let domesticTransferRef, breaker, _ =
            init tck <| mockTransferRequestFactory ()

         let evt = Stub.event.domesticTransferPending
         let txn = TransferEventToDomesticTransfer.fromPending evt

         let transferMsg =
            DomesticTransferMessage.TransferRequest(
               DomesticTransferServiceAction.TransferAck,
               txn
            )

         domesticTransferRef <! transferMsg

         let cmd = tck.ExpectMsg<UpdateDomesticTransferProgressCommand>()

         let _ =
            Expect.wantOk
               (UpdateDomesticTransferProgressCommand.toEvent cmd)
               "TransferProgress cmd -> event validation ok"

         domesticTransferRef
         <! DomesticTransferMessage.TransferRequest(
            DomesticTransferServiceAction.ProgressCheck,
            txn
         )

         // 1st message succeeds
         tck.ExpectMsg<CompleteDomesticTransferCommand>() |> ignore

         // Message 2 & 3 fail, opening the circuit breaker &
         // stashing corresponding messages.  Subsequent messages
         // are stashed for later processing without attempting to make
         // a request.
         domesticTransferRef <! transferMsg
         domesticTransferRef <! transferMsg

         tck.AwaitAssert(fun () ->
            Expect.equal
               breaker.CurrentFailureCount
               2
               "Expected circuit breaker fail count = 2")

         TestKit.expectMsg tck CircuitBreakerStatus.Open |> ignore

         domesticTransferRef <! transferMsg

         tck.AwaitAssert(fun () ->
            Expect.equal
               breaker.CurrentFailureCount
               2
               "Circuit breaker fail count still = 2 since subsequent messages 
                received after the breaker opens are stashed for later processing")
   ]
