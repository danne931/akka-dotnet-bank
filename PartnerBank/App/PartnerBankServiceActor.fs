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

open Lib.SharedTypes
open Lib.CircuitBreaker
open SignalRBroadcast
open Lib.Types
open OrgOnboardingSaga
open PurchaseSaga
open PlatformTransferSaga
open DomesticTransferSaga
open PartnerBank.Service.Domain
open BankActorRegistry
open Bank.Account.Domain

type private NetworkRequest =
   PartnerBankServiceMessage -> Task<Result<PartnerBankResponse, Err>>

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
               let metadata = queueMessage.SagaMetadata
               let corrId = metadata.CorrelationId
               let orgId = metadata.OrgId

               match queueMessage, response with
               | PartnerBankServiceMessage.CreateLegalEntity req,
                 PartnerBankResponse.CreateLegalEntity res ->
                  let msg =
                     Ok res
                     |> OrgOnboardingSagaEvent.CreateLegalEntityWithPartnerBankResponse
                     |> AppSaga.Message.orgOnboard orgId corrId

                  registry.SagaActor corrId <! msg
               | PartnerBankServiceMessage.CreateInternalAccount req,
                 PartnerBankResponse.CreateInternalAccount res ->
                  let msg =
                     Ok {
                        PartnerBankLegalEntityId = req.LegalEntityId
                        PartnerBankAccountId = res.PartnerBankAccountId
                        AccountNumber = res.AccountNumber
                        RoutingNumber = res.RoutingNumber
                     }
                     |> OrgOnboardingSagaEvent.CreateInternalAccountWithPartnerBankResponse
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
                  let txn = info.Transfer
                  let updatedProgress = res.NewProgressToSave txn

                  match updatedProgress with
                  | Some progressUpdate ->
                     let msg =
                        progressUpdate
                        |> DomesticTransferSagaEvent.TransferProcessorProgressUpdate
                        |> AppSaga.Message.domesticTransfer orgId corrId

                     registry.SagaActor corrId <! msg
                  | _ ->
                     logDebug
                        mailbox
                        ("No domestic transfer progress update will be saved."
                         + $" - Transfer ID ({txn.TransferId})"
                         + $" - Existing status ({txn.Status})"
                         + $" - Service status ({res.Progress txn.ExpectedSettlementDate})")
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

let private tcp =
   TCP.request
      EnvPartnerBank.config.MockPartnerBank.Host
      EnvPartnerBank.config.MockPartnerBank.Port
      Encoding.UTF8

let private networkRequest
   (msg: PartnerBankServiceMessage)
   : TaskResult<PartnerBankResponse, Err>
   =
   taskResult {
      match msg with
      | PartnerBankServiceMessage.CreateLegalEntity req ->
         let request = {|
            Action = "CreateLegalBusinessEntity"
            TransactionId = req.SagaMetadata.CorrelationId.ToString()
            Data = req.AsDTO
         |}

         let serialized = JsonSerializer.SerializeToUtf8Bytes request

         let! res = tcp serialized

         let! res =
            Serialization.deserialize<LegalBusinessEntityCreateResponseDTO> res

         let! entity = LegalBusinessEntityDTO.toEntity res
         return PartnerBankResponse.CreateLegalEntity entity
      | PartnerBankServiceMessage.CreateInternalAccount req ->
         let request = {|
            Action = "CreateInternalAccount"
            TransactionId = string req.SagaMetadata.CorrelationId
            Data = req.AsDTO
         |}

         let serialized = JsonSerializer.SerializeToUtf8Bytes request

         let! res = tcp serialized

         let! res =
            Serialization.deserialize<InternalAccountCreateResponseDTO> res

         let! entity = res.AsEntity

         return PartnerBankResponse.CreateInternalAccount entity
      | PartnerBankServiceMessage.TransferDomestic info ->
         let txn = info.Transfer

         let request: PartnerBankDomesticTransferRequest = {
            Action = info.Action
            Sender = txn.Sender
            Recipient = txn.Recipient
            Amount = txn.Amount
            Date = txn.ScheduledDate
            TransactionId = txn.TransferId
            PaymentNetwork = txn.Recipient.PaymentNetwork
         }

         let serializedReq = JsonSerializer.SerializeToUtf8Bytes request.AsDTO

         let! res = tcp serializedReq

         let! res =
            Serialization.deserialize<PartnerBankDomesticTransferResponse> res

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
