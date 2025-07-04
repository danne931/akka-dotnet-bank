[<RequireQualifiedAccess>]
module DomesticTransferServiceActor

open System.Text
open System.Text.Json
open System.Threading.Tasks
open Akkling
open Akkling.Streams
open Akka.Actor
open Akka.Streams.Amqp.RabbitMq
open Akkling.Cluster.Sharding
open FsToolkit.ErrorHandling

open Lib.ActivePatterns
open Lib.SharedTypes
open Lib.CircuitBreaker
open Bank.Transfer.Domain
open SignalRBroadcast
open Lib.Types
open DomesticTransferSaga
open DomesticTransfer.Service.Domain
open TransferMessages

type private Message = DomesticTransferServiceMessage

type private NetworkRequest =
   DomesticTransferServiceAction
      -> DomesticTransfer
      -> Task<Result<DomesticTransferServiceResponse, Err>>

type private InfraFailReason = DomesticTransferInfraFailReason
type private FailReason = DomesticTransferThirdPartyFailReason

let private failReasonFromError (err: string) : FailReason =
   match err with
   | Contains "CorruptData" -> FailReason.Infra InfraFailReason.CorruptData
   | Contains "InvalidAction" -> FailReason.Infra InfraFailReason.InvalidAction
   | Contains "InvalidAmount" -> FailReason.InvalidAmount
   | Contains "InvalidAccountInfo" -> FailReason.RecipientAccountInvalidInfo
   | Contains "InvalidPaymentNetwork" ->
      FailReason.Infra InfraFailReason.InvalidPaymentNetwork
   | Contains "InvalidDepository" ->
      FailReason.Infra InfraFailReason.RecipientAccountInvalidDepository
   | Contains "InactiveAccount" -> FailReason.RecipientAccountInvalidInfo
   | Contains "NoTransferProcessing" -> FailReason.NoTransferFound
   | e -> FailReason.Infra(InfraFailReason.Unknown e)

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
   (logDebug: string -> unit)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   (txn: DomesticTransfer)
   (res: DomesticTransferServiceResponse)
   =
   let orgId = txn.Sender.OrgId
   let corrId = txn.TransferId |> TransferId.get |> CorrelationId

   let latestProgressUpdate =
      if res.Ok then
         match res.Status with
         | "Complete" -> DomesticTransferThirdPartyUpdate.Settled
         | "ReceivedRequest" ->
            DomesticTransferThirdPartyUpdate.ServiceAckReceived
         | status ->
            let expectedSettlementDate =
               res.ExpectedSettlementDate
               |> Option.defaultValue txn.ExpectedSettlementDate

            DomesticTransferThirdPartyUpdate.ProgressDetail {
               Detail = status
               ExpectedSettlementDate = expectedSettlementDate
            }
      else
         res.Reason
         |> failReasonFromError
         |> DomesticTransferThirdPartyUpdate.Failed

   let newProgressToSave =
      match txn.Status with
      | DomesticTransferProgress.WaitingForTransferServiceAck ->
         Some latestProgressUpdate
      // Don't save a new progress update if there has been no change.
      | DomesticTransferProgress.ThirdParty existingProgressUpdate when
         existingProgressUpdate <> latestProgressUpdate
         ->
         Some latestProgressUpdate
      | _ -> None

   match newProgressToSave with
   | Some progressUpdate ->
      let msg =
         progressUpdate
         |> DomesticTransferSagaEvent.TransferProcessorProgressUpdate
         |> AppSaga.Message.domesticTransfer orgId corrId

      getSagaRef corrId <! msg
   | _ ->
      logDebug (
         "No domestic transfer progress update will be saved."
         + $" - Transfer ID ({txn.TransferId})"
         + $" - Existing status ({txn.Status})"
         + $" - Service status ({latestProgressUpdate})"
      )

let protectedAction
   (networkRequest: NetworkRequest)
   (mailbox: Actor<_>)
   (msg: Message)
   : TaskResult<DomesticTransferServiceResponse, Err>
   =
   task {
      match msg with
      | Message.TransferRequest(action, txn) ->
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
                     ExpectedSettlementDate = None
                  }
               | err -> Error err
   }

let actorProps
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (networkRequest: NetworkRequest)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   : Props<obj>
   =
   let consumerQueueOpts
      : Lib.Queue.QueueConsumerOptions<
           DomesticTransferServiceMessage,
           DomesticTransferServiceMessage,
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
               | Message.TransferRequest(_, txn) ->
                  onSuccessfulServiceResponse
                     (logDebug mailbox)
                     getSagaRef
                     txn
                     transferResponse

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
   (queueSettings: QueueEnvConfig)
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

let getProducer (system: ActorSystem) : IActorRef<Message> =
   Akka.Hosting.ActorRegistry
      .For(system)
      .Get<ActorUtil.ActorMetadata.DomesticTransferProducerMarker>()
   |> typed
