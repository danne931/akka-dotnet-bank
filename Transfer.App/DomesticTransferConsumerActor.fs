[<RequireQualifiedAccess>]
module DomesticTransferConsumerActor

open System.Text
open System.Text.Json
open System.Threading.Tasks
open Akkling
open Akkling.Cluster.Sharding
open Akka.Actor
open Akka.Streams.Amqp.RabbitMq
open FsToolkit.ErrorHandling

open Lib.ActivePatterns
open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Transfer.Domain
open SignalRBroadcast
open Email
open Lib.Types

module Command = DomesticTransferToCommand
type private FailReason = DomesticTransferFailReason

let private progressFromResponse (response: DomesticTransferServiceResponse) =
   match response.Status with
   | "Complete" -> DomesticTransferProgress.Completed
   | "ReceivedRequest" ->
      DomesticTransferInProgress.InitialHandshakeAck
      |> DomesticTransferProgress.InProgress
   | status ->
      DomesticTransferInProgress.Other status
      |> DomesticTransferProgress.InProgress

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

let protectedAction
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (networkRequest: DomesticTransferRequest)
   (mailbox: Actor<_>)
   (msg: DomesticTransferMessage)
   : Task<unit>
   =
   match msg with
   | DomesticTransferMessage.TransferRequest(action, txn) -> task {
      let! result = networkRequest action txn

      let onOk res =
         let accountRef = getAccountRef txn.Sender.AccountId

         if res.Ok then
            let progress = progressFromResponse res

            match progress with
            | DomesticTransferProgress.Completed ->
               let msg =
                  Command.complete txn
                  |> AccountCommand.CompleteDomesticTransfer
                  |> AccountMessage.StateChange

               accountRef <! msg
            | DomesticTransferProgress.InProgress progress ->
               let msg =
                  Command.progress txn progress
                  |> AccountCommand.UpdateDomesticTransferProgress
                  |> AccountMessage.StateChange

               match action with
               | DomesticTransferServiceAction.TransferAck -> accountRef <! msg
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
               logError mailbox $"Transfer API requires code update: {err}"

               getEmailRef mailbox.System
               <! EmailMessage.ApplicationErrorRequiresSupport(
                  string err,
                  txn.Sender.OrgId
               )
            | FailReason.InvalidAmount
            | FailReason.InvalidAccountInfo
            | FailReason.AccountClosed
            | _ ->
               let msg =
                  Command.fail txn err
                  |> AccountCommand.FailDomesticTransfer
                  |> AccountMessage.StateChange

               accountRef <! msg

      match result with
      | Ok res -> onOk res
      | Error err ->
         match err with
         | Err.SerializationError errMsg ->
            let errMsg = $"Corrupt data: {errMsg}"
            logError mailbox errMsg

            onOk {
               Sender = networkSender txn.Sender
               Recipient = networkRecipient txn.Recipient
               Ok = false
               Status = ""
               Reason = "CorruptData"
               TransactionId = string txn.TransferId
            }
         | Err.NetworkError err -> failwith err.Message
         | err -> failwith (string err)
     }

let actorProps
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueSettings)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (networkRequest: DomesticTransferRequest)
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   : Props<obj>
   =
   let consumerQueueOpts
      : Lib.Queue.QueueConsumerOptions<
           DomesticTransferMessage,
           DomesticTransferMessage
         > = {
      Service = CircuitBreakerService.DomesticTransfer
      onCircuitBreakerEvent = broadcaster.circuitBreaker
      protectedAction = protectedAction getAccountRef getEmailRef networkRequest
      queueMessageToActionRequests = fun _ msg -> Task.FromResult(Some [ msg ])
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
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   : Props<obj>
   =
   actorProps
      queueConnection
      queueSettings
      streamRestartSettings
      breaker
      broadcaster
      networkRequestToTransferProcessor
      getAccountRef
      getEmailRef

let getProducer (system: ActorSystem) : IActorRef<DomesticTransferMessage> =
   Akka.Hosting.ActorRegistry
      .For(system)
      .Get<ActorUtil.ActorMetadata.DomesticTransferProducerMarker>()
   |> typed
