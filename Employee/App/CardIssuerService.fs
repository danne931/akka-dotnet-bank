module CardIssuerServiceActor

open System
open System.Threading.Tasks
open Akkling
open Akkling.Streams
open Akka.Streams.Amqp.RabbitMq
open FsToolkit.ErrorHandling

open Bank.Employee.Domain
open Lib.SharedTypes
open Lib.CircuitBreaker
open SignalRBroadcast
open Lib.Types
open Lib.Queue
open CardSetupSaga
open CardIssuer.Service.Domain
open BankActorRegistry
open Flurl.Http

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
   (registry: #ISagaActor)
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (networkRequest: CardIssuerMessage -> Task<Result<CardIssuerResponse, Err>>)
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
                  let msg =
                     Ok res
                     |> CardSetupSagaEvent.CardCreateResponse
                     |> AppSaga.Message.cardSetup orgId corrId

                  registry.SagaActor corrId <! msg
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

let private networkRequestToCardIssuerService
   (cardIssuerApiKey: string)
   (msg: CardIssuerMessage)
   : Task<Result<CardIssuerResponse, Err>>
   =
   task {
      match msg with
      | CardIssuerMessage.CreateCard req ->
         try
            let url = EnvEmployee.config.CardIssuerURI + "/cards"

            let! response =
               url
                  .WithHeader("Authorization", cardIssuerApiKey)
                  .PostJsonAsync(req.AsDTO)

            if not response.ResponseMessage.IsSuccessStatusCode then
               let errMsg =
                  $"Error creating card with card issuer: {response.StatusCode}"

               return Error(Err.UnexpectedError errMsg)
            else
               let! res = response.GetJsonAsync<CardCreateResponseDTO>()

               return Ok(CardIssuerResponse.CreateCard res.AsEntity)
         with e ->
            return Error(Err.UnexpectedError e.Message)
      | CardIssuerMessage.CloseCard req ->
         // TODO: HTTP request to the card issuer API
         do! Task.Delay(TimeSpan.FromSeconds 3.3)

         return
            CardIssuerResponse.CloseCard {
               Customer = null
               ProviderCardId = req.ProviderCardId
            }
            |> Ok
   }

let private mockNetworkRequestToCardIssuerService
   (msg: CardIssuerMessage)
   : Task<Result<CardIssuerResponse, Err>>
   =
   taskResult {
      match msg with
      | CardIssuerMessage.CreateCard _ ->
         do! Task.Delay(TimeSpan.FromSeconds 1.3)

         return
            CardIssuerResponse.CreateCard {
               ProviderCardId = Guid.NewGuid() |> ThirdPartyProviderCardId
               CardNumberLast4 =
                  List.init 4 (fun _ -> Random().Next(1, 9) |> string)
                  |> String.concat ""
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

// Will not consume messages off of RabbitMq if no CardIssuerApiKey
// configured for interaction with 3rd party card issuer service.
let private idleActor () =
   let handler ctx msg =
      match msg with
      | LifecycleEvent e ->
         match e with
         | PreStart ->
            logWarning
               ctx
               $"CardIssuerApiKey not set. Will not process messages."

            ignored ()
         | _ -> ignored ()
      | msg ->
         logError ctx $"Unknown Message: {msg}"
         unhandled ()

   props (actorOf2 handler)

let initProps
   registry
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   : Props<obj>
   =
   let actorProps =
      actorProps
         registry
         queueConnection
         queueSettings
         streamRestartSettings
         breaker
         broadcaster

   let conf = EnvEmployee.config

   match conf.CardIssuerMockRequests, conf.CardIssuerApiKey with
   | true, _ -> actorProps mockNetworkRequestToCardIssuerService
   | false, Some apiKey -> actorProps (networkRequestToCardIssuerService apiKey)
   | false, None -> idleActor ()
