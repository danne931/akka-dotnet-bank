module CardIssuerServiceActor

open System
open System.Threading.Tasks
open Akkling
open Akka.Actor
open Akkling.Streams
open Akka.Streams.Amqp.RabbitMq
open Akkling.Cluster.Sharding
open FsToolkit.ErrorHandling

open Bank.Employee.Domain
open Lib.SharedTypes
open Lib.CircuitBreaker
open SignalRBroadcast
open Lib.Types
open Lib.Queue
open CardSetupSaga
open EmployeeOnboardingSaga
open CardIssuer.Service.Domain

let private networkRequestToCardIssuerService
   (msg: CardIssuerMessage)
   : Task<Result<CardIssuerResponse, Err>>
   =
   taskResult {
      match msg with
      | CardIssuerMessage.CreateCard req ->
         // TODO: HTTP request to the card issuer API
         do! Task.Delay(TimeSpan.FromSeconds 1.3)

         return
            CardIssuerResponse.CreateCard {
               ProviderCardId = Guid.NewGuid() |> ThirdPartyProviderCardId
            }
      | CardIssuerMessage.CloseCard req ->
         // TODO: HTTP request to the card issuer API
         do! Task.Delay(TimeSpan.FromSeconds 3.3)

         return
            CardIssuerResponse.CloseCard {
               Customer = null
               ProviderCardId = req.ProviderCardId
            }
   }

let protectedAction
   (networkRequest: CardIssuerMessage -> Task<Result<CardIssuerResponse, Err>>)
   (mailbox: Actor<_>)
   (msg: CardIssuerMessage)
   : TaskResult<CardIssuerResponse, Err>
   =
   task {
      let! result = networkRequest msg
      return result
   }

let actorProps
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (networkRequest: CardIssuerMessage -> Task<Result<CardIssuerResponse, Err>>)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   : Props<obj>
   =
   let consumerQueueOpts = {
      Service = CircuitBreakerService.CardIssuer
      onCircuitBreakerEvent = broadcaster.circuitBreaker
      protectedAction =
         fun mailbox msg -> protectedAction networkRequest mailbox msg
      queueMessageToActionRequest = fun _ msg -> Task.FromResult(Some msg)
      onSuccessFlow =
         Flow.map
            (fun (mailbox, queueMessage: CardIssuerMessage, response) ->
               let metadata = queueMessage.Metadata
               let corrId = metadata.CorrelationId
               let orgId = metadata.OrgId

               match queueMessage, response with
               | CardIssuerMessage.CreateCard req,
                 CardIssuerResponse.CreateCard res ->
                  match req.ReplyTo with
                  | SagaReplyTo.CardSetup ->
                     let msg =
                        Ok res.ProviderCardId
                        |> CardSetupSagaEvent.CardCreateResponse
                        |> AppSaga.Message.cardSetup orgId corrId

                     getSagaRef corrId <! msg
                  | SagaReplyTo.EmployeeOnboard ->
                     let onboardingMsg =
                        Ok res.ProviderCardId
                        |> EmployeeOnboardingSagaEvent.CardCreateResponse
                        |> AppSaga.Message.employeeOnboard orgId corrId

                     getSagaRef corrId <! onboardingMsg
               | CardIssuerMessage.CloseCard _,
                 CardIssuerResponse.CloseCard res ->
                  logDebug mailbox $"Card detached {res.ProviderCardId}"
               | req, res ->
                  logError
                     mailbox
                     $"Invalid request response in CardIssuerService {req} {res}"

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
      networkRequestToCardIssuerService
      getSagaRef

let getProducer (system: ActorSystem) : IActorRef<CardIssuerMessage> =
   Akka.Hosting.ActorRegistry
      .For(system)
      .Get<ActorUtil.ActorMetadata.CardIssuerServiceProducerMarker>()
   |> typed
