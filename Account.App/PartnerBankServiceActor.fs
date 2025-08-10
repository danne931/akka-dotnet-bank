[<RequireQualifiedAccess>]
module PartnerBankServiceActor

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
open PartnerBank.Service.Domain
open BankActorRegistry

let protectedAction
   (networkRequest:
      PartnerBankServiceMessage -> Task<Result<PartnerBankResponse, Err>>)
   (mailbox: Actor<_>)
   (msg: PartnerBankServiceMessage)
   : TaskResult<PartnerBankResponse, Err>
   =
   task {
      let! result = networkRequest msg
      return result
   }

let actorProps
   (registry: #ISagaActor)
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (networkRequest:
      PartnerBankServiceMessage -> Task<Result<PartnerBankResponse, Err>>)
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

let private networkRequestToPartnerBankService
   (msg: PartnerBankServiceMessage)
   =
   taskResult {
      match msg with
      | PartnerBankServiceMessage.LinkAccount req ->
         // TODO: HTTP to partner bank to create the parent account link
         do! Task.Delay(5000)

         return
            PartnerBankResponse.LinkAccount {
               Accepted = true
               Link = {
                  AccountNumber =
                     ParentAccountNumber <| AccountNumber.generate ()
                  RoutingNumber = ParentRoutingNumber RoutingNumber.Empty
               }
            }
      | PartnerBankServiceMessage.TransferBetweenOrganizations req ->
         // TODO: HTTP to partner bank to sync a transfer between
         //       orgs parent accounts within the partner bank
         do! Task.Delay(2500)

         return
            PartnerBankResponse.TransferBetweenOrganizations {
               ConfirmationId = System.Guid.NewGuid()
            }
      | PartnerBankServiceMessage.Purchase req ->
         // TODO: HTTP to partner bank to sync a purchase

         return
            PartnerBankResponse.Purchase {
               ConfirmationId = System.Guid.NewGuid()
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
      networkRequestToPartnerBankService
