[<RequireQualifiedAccess>]
module DomesticTransferConsumerActor

open System
open System.Text
open System.Text.Json
open Akkling
open Akkling.Cluster.Sharding
open Akka.Actor
open Akka.Streams.Amqp.RabbitMq
open Akka.Streams.Amqp.RabbitMq.Dsl
open FsToolkit.ErrorHandling
open Akkling.Streams

open Lib.ActivePatterns
open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Transfer.Domain
open SignalRBroadcast
open Email
open Lib.Types

let mutable cnt = 0

module Command = DomesticTransferToCommand
type private FailReason = DomesticTransferFailReason

type private CircuitBreakerMessage =
   | BreakerHalfOpen
   | BreakerClosed

module private Msg =
   let progress =
      AccountMessage.StateChange
      << AccountCommand.UpdateDomesticTransferProgress

   let complete =
      AccountMessage.StateChange << AccountCommand.CompleteDomesticTransfer

   let fail = AccountMessage.StateChange << AccountCommand.FailDomesticTransfer

let private progressFromResponse (response: DomesticTransferServiceResponse) =
   match response.Status with
   | "Complete" -> DomesticTransferProgress.Completed
   | "ReceivedRequest" ->
      DomesticTransferProgress.InProgress
         DomesticTransferInProgress.InitialHandshakeAck
   | status ->
      DomesticTransferProgress.InProgress(
         DomesticTransferInProgress.Other status
      )

let private failReasonFromError (err: string) : FailReason =
   match err with
   | Contains "CorruptData" -> FailReason.CorruptData
   | Contains "InvalidAction" -> FailReason.InvalidAction
   | Contains "InvalidAmount" -> FailReason.InvalidAmount
   | Contains "InvalidAccountInfo" -> FailReason.InvalidAccountInfo
   | Contains "InvalidPaymentNetwork" -> FailReason.InvalidPaymentNetwork
   | Contains "InvalidDepository" -> FailReason.InvalidDepository
   | Contains "InactiveAccount" -> FailReason.AccountClosed
   | e -> FailReason.Unknown e

let private networkSender
   (sender: DomesticTransferSender)
   : DomesticTransferServiceSender
   =
   {
      Name = sender.Name
      AccountNumber = string sender.AccountNumber
      RoutingNumber = string sender.RoutingNumber
   }

let private networkRecipient
   (recipient: DomesticTransferRecipient)
   : DomesticTransferServiceRecipient
   =
   {
      Name = recipient.Name
      AccountNumber = string recipient.AccountNumber
      RoutingNumber = string recipient.RoutingNumber
      Depository =
         match recipient.Depository with
         | DomesticRecipientAccountDepository.Checking -> "checking"
         | DomesticRecipientAccountDepository.Savings -> "savings"
   }

let deserializeFromRabbit
   (system: ActorSystem)
   (msg: CommittableIncomingMessage)
   =
   try
      let serializer =
         system.Serialization.FindSerializerForType(
            typeof<DomesticTransferMessage>
         )

      msg.Message.Bytes.ToArray()
      |> serializer.FromBinary<DomesticTransferMessage>
      |> Ok
   with ex ->
      Error ex

let handleTransfer
   (mailbox: Actor<obj>)
   (requestTransfer: DomesticTransferRequest)
   (action: DomesticTransferServiceAction)
   (txn: DomesticTransfer)
   =
   task {
      printfn "TRY TRANSFER"
      let! result = requestTransfer action txn
      printfn "result from transfer %A" result

      return
         match result with
         | Ok res -> DomesticTransferMessage.TransferResponse(res, action, txn)
         | Error err ->
            match err with
            | Err.SerializationError errMsg ->
               let errMsg = $"Corrupt data: {errMsg}"
               logError mailbox errMsg

               let res = {
                  Sender = networkSender txn.Sender
                  Recipient = networkRecipient txn.Recipient
                  Ok = false
                  Status = ""
                  Reason = "CorruptData"
                  TransactionId = string txn.TransferId
               }

               DomesticTransferMessage.TransferResponse(res, action, txn)
            | Err.NetworkError err -> failwith err.Message
            | err -> failwith (string err)
   }

let initTransferSink
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: RabbitQueueSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (requestTransfer: DomesticTransferRequest)
   (killSwitch: Akka.Streams.SharedKillSwitch)
   (mailbox: Actor<obj>)
   parallelism
   =
   let logError, logWarning = logError mailbox, logWarning mailbox

   let handleTransferWithCircuitBreaker
      (committable: CommittableIncomingMessage)
      (msg: DomesticTransferMessage)
      =
      breaker.WithCircuitBreaker(fun () -> task {
         try
            match msg with
            | DomesticTransferMessage.TransferRequest(action, txn) ->
               let! response =
                  handleTransfer mailbox requestTransfer action txn

               do! committable.Ack()
               return response
            | _ ->
               do! committable.Ack()
               return msg
         with e ->
            do! committable.Nack()
            return failwith e.Message
      })
      |> Async.AwaitTask

   let source =
      let settings =
         Lib.Rabbit.createSourceSettings queueSettings.Name queueConnection

      AmqpSource.CommittableSource(settings, bufferSize = parallelism)

   let sink =
      Sink.onComplete (function
         | None ->
            logWarning "Domestic transfer Rabbit consumer stream completed."
         | Some err ->
            logError
               $"Domestic transfer rabbit consumer stream completed with error: {err}")

   source
   |> Source.viaMat (killSwitch.Flow()) Keep.none
   |> Source.asyncMapUnordered parallelism (fun msg -> async {
      let nack = msg.Nack >> Async.AwaitTask

      if breaker.IsOpen then
         logWarning "Circuit breaker open - will try again later"
         do! nack ()
      else
         match deserializeFromRabbit mailbox.System msg with
         | Error e ->
            logError $"DomesticTransfer deserialization broken {e.Message}"
            do! nack ()
         | Ok transferMsg ->
            try
               let! response = handleTransferWithCircuitBreaker msg transferMsg

               match response with
               | DomesticTransferMessage.TransferResponse(res, action, txn) ->
                  let accountRef = getAccountRef txn.Sender.AccountId

                  if res.Ok then
                     let progress = progressFromResponse res

                     match progress with
                     | DomesticTransferProgress.Completed ->
                        let cmd = Command.complete txn
                        accountRef <! Msg.complete cmd
                     | DomesticTransferProgress.InProgress progress ->
                        let msg = Msg.progress <| Command.progress txn progress

                        match action with
                        | DomesticTransferServiceAction.TransferAck ->
                           accountRef <! msg
                        | DomesticTransferServiceAction.ProgressCheck ->
                           let previousProgress = txn.Status

                           if
                              (DomesticTransferProgress.InProgress progress)
                              <> previousProgress
                           then
                              accountRef <! msg
                     | _ -> ()
                  else
                     let err = failReasonFromError res.Reason

                     match err with
                     | FailReason.CorruptData
                     | FailReason.InvalidPaymentNetwork
                     | FailReason.InvalidDepository
                     | FailReason.InvalidAction ->
                        logError $"Transfer API requires code update: {err}"

                        getEmailRef mailbox.System
                        <! EmailMessage.ApplicationErrorRequiresSupport(
                           string err,
                           txn.Sender.OrgId
                        )
                     | FailReason.InvalidAmount
                     | FailReason.InvalidAccountInfo
                     | FailReason.AccountClosed
                     | _ ->
                        let msg = Msg.fail <| Command.fail txn err
                        accountRef <! msg
               | _ -> ()

               printfn "rezz %A" response
            with e ->
               logError $"Error processing transfer message: {e.Message}"

      return 100
   })
   |> Source.toMat sink Keep.none
   |> Graph.run (mailbox.System.Materializer())

let actorProps
   (broadcaster: SignalRBroadcast)
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (breaker: Akka.Pattern.CircuitBreaker)
   (requestTransfer: DomesticTransferRequest)
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: RabbitQueueSettings)
   : Props<obj>
   =
   let createKillSwitch () =
      KillSwitch.shared "CircuitBreakerOpen.DomesticTransfer"

   let mutable killSwitch = createKillSwitch ()

   let initStream =
      initTransferSink
         queueConnection
         queueSettings
         breaker
         getAccountRef
         getEmailRef
         requestTransfer

   let rec init (ctx: Actor<obj>) = actor {
      let! msg = ctx.Receive()

      breaker.OnHalfOpen(fun () ->
         cnt <- cnt + 1

         ctx.Self <! BreakerHalfOpen

         broadcaster.circuitBreaker {
            Service = CircuitBreakerService.DomesticTransfer
            Status = CircuitBreakerStatus.HalfOpen
            Timestamp = DateTime.UtcNow
         })
      |> ignore

      breaker.OnClose(fun () ->
         ctx.Self <! BreakerClosed

         broadcaster.circuitBreaker {
            Service = CircuitBreakerService.DomesticTransfer
            Status = CircuitBreakerStatus.Closed
            Timestamp = DateTime.UtcNow
         })
      |> ignore

      breaker.OnOpen(fun () ->
         logWarning ctx "Breaker open - pause processing"
         // Shutdown processing of messages off RabbitMq until
         // BreakerHalfOpen.  When BreakerHalfOpen the stream will
         // be reinitialized and attempt to process just 1 message off
         // the queue.  If successful then the stream will return to
         // processing messages normally.
         killSwitch.Shutdown()

         // Need to refresh the kill switch for stream reinitialization.
         // Otherwise, it appears the stream will never start back up again
         // due to being configured with a killswitch that has already been
         // shut down.
         killSwitch <- createKillSwitch ()

         broadcaster.circuitBreaker {
            Service = CircuitBreakerService.DomesticTransfer
            Status = CircuitBreakerStatus.Open
            Timestamp = DateTime.UtcNow
         })
      |> ignore

      match msg with
      | LifecycleEvent e ->
         match e with
         | PreStart ->
            logInfo ctx "Prestart - Initialize EmailActor Queue Source"

            return! processing ctx queueSettings.MaxParallelism
         | _ -> return ignored ()
      | msg ->
         logError ctx $"Unknown msg {msg}"
         return unhandled ()
   }

   and processing (ctx: Actor<obj>) (maxParallel: int) =
      initStream killSwitch ctx maxParallel

      actor {
         let! msg = ctx.Receive()

         match msg with
         | :? CircuitBreakerMessage as msg ->
            match msg with
            | BreakerHalfOpen ->
               logWarning ctx "Breaker half open - try processing one"
               return! processing ctx 1
            | BreakerClosed ->
               logWarning ctx "Breaker closed - resume processing"
               return! processing ctx queueSettings.MaxParallelism
         | LifecycleEvent e ->
            printfn "lifecycle event down below %A" e

            match e with
            | PostStop ->
               printfn "POSTSTOP transfer consumer"
               //queue.Complete()
               return! init ctx
            | _ -> return ignored ()
         | msg ->
            logError ctx $"Unknown msg {msg}"
            return unhandled ()
      }

   props init

let private transferRequest
   (action: DomesticTransferServiceAction)
   (txn: DomesticTransfer)
   =
   taskResult {
      let msg = {|
         Action =
            match action with
            | DomesticTransferServiceAction.TransferAck -> "TransferRequest"
            | DomesticTransferServiceAction.ProgressCheck -> "ProgressCheck"
         Sender = networkSender txn.Sender
         Recipient = networkRecipient txn.Recipient
         Amount = txn.Amount
         Date = txn.ScheduledDate
         TransactionId = string txn.TransferId
         PaymentNetwork =
            match txn.Recipient.PaymentNetwork with
            | PaymentNetwork.ACH -> "ach"
      |}

      let! response =
         TCP.request
            EnvTransfer.config.MockDomesticTransferProcessor.Host
            EnvTransfer.config.MockDomesticTransferProcessor.Port
            Encoding.UTF8
            (JsonSerializer.SerializeToUtf8Bytes msg)

      return!
         Serialization.deserialize<DomesticTransferServiceResponse> response
   }

let initProps
   (broadcaster: SignalRBroadcast)
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (breaker: Akka.Pattern.CircuitBreaker)
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: RabbitQueueSettings)
   =
   actorProps
      broadcaster
      getAccountRef
      getEmailRef
      breaker
      transferRequest
      queueConnection
      queueSettings

let getProducer (system: ActorSystem) : IActorRef<DomesticTransferMessage> =
   Akka.Hosting.ActorRegistry
      .For(system)
      .Get<ActorUtil.ActorMetadata.DomesticTransferProducerMarker>()
   |> typed
