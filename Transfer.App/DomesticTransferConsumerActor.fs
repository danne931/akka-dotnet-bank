[<RequireQualifiedAccess>]
module DomesticTransferConsumerActor

open System.Text
open System.Text.Json
open System.Threading.Tasks
open Akkling
open Akkling.Cluster.Sharding
open Akkling.Streams
open Akka.Actor
open Akka.Streams.Amqp.RabbitMq
open FsToolkit.ErrorHandling

open Lib.ActivePatterns
open Lib.SharedTypes
open Bank.Transfer.Domain
open SignalRBroadcast
open Lib.Types
open DomesticTransferSaga

type private FailReason = DomesticTransferFailReason

let private progressFromResponse (response: DomesticTransferServiceResponse) =
   match response.Status with
   | "Complete" -> DomesticTransferServiceProgress.Settled
   | "ReceivedRequest" -> DomesticTransferServiceProgress.InitialHandshakeAck
   | status -> DomesticTransferServiceProgress.InProgress status

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

// Notify account actor of DomesticTransfer Progress, Completed, or Failed.
let onSuccessfulServiceResponse
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   (txn: DomesticTransfer)
   (res: DomesticTransferServiceResponse)
   =
   let orgId = txn.Sender.OrgId
   let corrId = txn.TransferId |> TransferId.get |> CorrelationId
   let txnSagaRef = getSagaRef corrId

   let progress =
      if res.Ok then
         progressFromResponse res
      else
         res.Reason
         |> failReasonFromError
         |> DomesticTransferServiceProgress.Failed

   let msg =
      DomesticTransferSagaEvent.TransferProcessorProgressUpdate progress
      |> AppSaga.Message.domesticTransfer orgId corrId

   txnSagaRef <! msg

let protectedAction
   (networkRequest: DomesticTransferRequest)
   (mailbox: Actor<_>)
   (msg: DomesticTransferMessage)
   : TaskResult<DomesticTransferServiceResponse, Err>
   =
   task {
      match msg with
      | DomesticTransferMessage.TransferRequest(action, txn) ->
         let! result = networkRequest action txn

         return
            match result with
            | Ok res -> Ok res
            | Error err ->
               match err with
               | Err.SerializationError errMsg ->
                  let errMsg = $"Corrupt data: {errMsg}"
                  logError mailbox errMsg

                  Ok {
                     Sender = networkSender txn.Sender
                     Recipient = networkRecipient txn.Recipient
                     Ok = false
                     Status = ""
                     Reason = "CorruptData"
                     TransactionId = string txn.TransferId
                  }
               | err -> Error err
   }

let actorProps
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueSettings)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (networkRequest: DomesticTransferRequest)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   : Props<obj>
   =
   let consumerQueueOpts
      : Lib.Queue.QueueConsumerOptions<
           DomesticTransferMessage,
           DomesticTransferMessage,
           DomesticTransferServiceResponse
         > = {
      Service = CircuitBreakerService.DomesticTransfer
      onCircuitBreakerEvent = broadcaster.circuitBreaker
      protectedAction =
         fun mailbox msg -> protectedAction networkRequest mailbox msg
      queueMessageToActionRequest = fun _ msg -> Task.FromResult(Some msg)
      onSuccessFlow =
         Flow.map
            (fun (mailbox, queueMessage, transferResponse) ->
               match queueMessage with
               | DomesticTransferMessage.TransferRequest(action, txn) ->
                  onSuccessfulServiceResponse getSagaRef txn transferResponse

                  transferResponse)
            Flow.id
         |> Some
   }

   Lib.Queue.consumerActorProps
      queueConnection
      queueSettings
      streamRestartSettings
      breaker
      consumerQueueOpts

let private networkRequestToTransferProcessor
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
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueSettings)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   : Props<obj>
   =
   actorProps
      queueConnection
      queueSettings
      streamRestartSettings
      breaker
      broadcaster
      networkRequestToTransferProcessor
      getSagaRef

let getProducer (system: ActorSystem) : IActorRef<DomesticTransferMessage> =
   Akka.Hosting.ActorRegistry
      .For(system)
      .Get<ActorUtil.ActorMetadata.DomesticTransferProducerMarker>()
   |> typed
