[<RequireQualifiedAccess>]
module PartnerBankServiceActor

open System
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Akkling
open Akkling.Streams
open Akka.Streams.Amqp.RabbitMq
open FsToolkit.ErrorHandling

open Lib.ActivePatterns
open Lib.SharedTypes
open Lib.CircuitBreaker
open Bank.Transfer.Domain
open SignalRBroadcast
open Lib.Types
open OrgOnboardingSaga
open PurchaseSaga
open PlatformTransferSaga
open DomesticTransferSaga
open PartnerBank.Service.Domain
open BankActorRegistry

type private NetworkRequest =
   PartnerBankServiceMessage -> Task<Result<PartnerBankResponse, Err>>

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

let protectedAction
   (networkRequest: NetworkRequest)
   (mailbox: Actor<_>)
   (msg: PartnerBankServiceMessage)
   : TaskResult<PartnerBankResponse, Err>
   =
   task {
      let! result = networkRequest msg

      return
         match msg, result with
         | PartnerBankServiceMessage.TransferDomestic info,
           Error(Err.SerializationError errMsg) ->
            let errMsg = $"Corrupt data: {errMsg}"
            logError mailbox errMsg

            {
               Sender = networkSender info.Transfer.Sender
               Recipient = networkRecipient info.Transfer.Recipient
               Ok = false
               Status = ""
               Reason = "CorruptData"
               TransactionId = string info.Transfer.TransferId
               ExpectedSettlementDate = None
            }
            |> PartnerBankResponse.TransferDomestic
            |> Ok
         | _ -> result
   }

// Notify account actor of DomesticTransfer Progress, Completed, or Failed.
let onOkDomesticTransferResponse
   (logDebug: string -> unit)
   (registry: #ISagaActor)
   (txn: DomesticTransfer)
   (res: PartnerBankDomesticTransferResponse)
   =
   let orgId = txn.Sender.OrgId
   let corrId = txn.TransferId.AsCorrelationId

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

      registry.SagaActor corrId <! msg
   | _ ->
      logDebug (
         "No domestic transfer progress update will be saved."
         + $" - Transfer ID ({txn.TransferId})"
         + $" - Existing status ({txn.Status})"
         + $" - Service status ({latestProgressUpdate})"
      )

let actorProps
   (registry: #ISagaActor)
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (networkRequest: NetworkRequest)
   : Props<obj>
   =
   let consumerQueueOpts
      : Lib.Queue.QueueConsumerOptions<
           PartnerBankServiceMessage,
           PartnerBankServiceMessage,
           PartnerBankResponse
         > = {
      Service = CircuitBreakerService.PartnerBank
      onCircuitBreakerEvent = broadcaster.circuitBreaker
      protectedAction =
         fun mailbox msg -> protectedAction networkRequest mailbox msg
      queueMessageToActionRequest = fun _ msg -> Task.FromResult(Some msg)
      onSuccessFlow =
         Flow.map
            (fun (mailbox, queueMessage: PartnerBankServiceMessage, response) ->
               let metadata = queueMessage.Metadata
               let corrId = metadata.CorrelationId
               let orgId = metadata.OrgId

               match queueMessage, response with
               | PartnerBankServiceMessage.LinkAccount req,
                 PartnerBankResponse.LinkAccount res ->
                  let msg =
                     Ok res.Link
                     |> OrgOnboardingSagaEvent.LinkAccountToPartnerBankResponse
                     |> AppSaga.Message.orgOnboard orgId corrId

                  registry.SagaActor corrId <! msg
               | PartnerBankServiceMessage.Purchase req,
                 PartnerBankResponse.Purchase res ->
                  let msg =
                     Ok(SettlementId res.ConfirmationId)
                     |> PurchaseSagaEvent.PartnerBankSyncResponse
                     |> AppSaga.Message.purchase orgId corrId

                  registry.SagaActor corrId <! msg
               | PartnerBankServiceMessage.TransferBetweenOrganizations req,
                 PartnerBankResponse.TransferBetweenOrganizations res ->
                  let msg =
                     Ok(SettlementId res.ConfirmationId)
                     |> PlatformTransferSagaEvent.PartnerBankSyncResponse
                     |> AppSaga.Message.platformTransfer orgId corrId

                  registry.SagaActor corrId <! msg
               | PartnerBankServiceMessage.TransferDomestic info,
                 PartnerBankResponse.TransferDomestic res ->
                  onOkDomesticTransferResponse
                     (logDebug mailbox)
                     registry
                     info.Transfer
                     res
               | _ -> logError mailbox "Partner Bank Sync: Mixed req/res."

               response)
            Flow.id
         |> Some
   }

   Lib.Queue.consumerActorProps
      queueConnection
      queueSettings
      streamRestartSettings
      breaker
      consumerQueueOpts

let private networkRequest
   (msg: PartnerBankServiceMessage)
   : TaskResult<PartnerBankResponse, Err>
   =
   taskResult {
      match msg with
      | PartnerBankServiceMessage.LinkAccount req ->
         do! Task.Delay 5000

         return
            PartnerBankResponse.LinkAccount {
               Accepted = true
               Link = {
                  AccountNumber =
                     ParentAccountNumber <| AccountNumber.generate ()
                  RoutingNumber = ParentRoutingNumber RoutingNumber.Empty
               }
            }
      | PartnerBankServiceMessage.TransferDomestic info ->
         let txn = info.Transfer

         let request = {|
            Action =
               match info.Action with
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
               EnvTransfer.config.MockPartnerBank.Host
               EnvTransfer.config.MockPartnerBank.Port
               Encoding.UTF8
               (JsonSerializer.SerializeToUtf8Bytes request)

         let! res =
            Serialization.deserialize<PartnerBankDomesticTransferResponse>
               response

         return PartnerBankResponse.TransferDomestic res
      | PartnerBankServiceMessage.Purchase info ->
         return PartnerBankResponse.Purchase { ConfirmationId = Guid.NewGuid() }
      | PartnerBankServiceMessage.TransferBetweenOrganizations info ->
         do! Task.Delay(5000)

         return
            PartnerBankResponse.TransferBetweenOrganizations {
               ConfirmationId = Guid.NewGuid()
            }
   }

let initProps
   registry
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   : Props<obj>
   =
   actorProps
      registry
      queueConnection
      queueSettings
      streamRestartSettings
      breaker
      broadcaster
      networkRequest
