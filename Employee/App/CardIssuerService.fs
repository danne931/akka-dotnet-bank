module CardIssuerService

open System
open System.Threading.Tasks
open Akkling
open Akkling.Streams
open Akka.Streams.Amqp.RabbitMq

open Lib.SharedTypes
open Lib.CircuitBreaker
open SignalRBroadcast
open Lib.Types
open Lib.Queue
open CardSetupSaga
open CardIssuer.Service.Domain
open Bank.Employee.Domain
open BankActorRegistry
open Flurl.Http

let actorProps
   (registry: #ISagaActor)
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (networkRequest: CardIssuerMessage -> Task<CardIssuerResponse>)
   : Props<obj>
   =
   let consumerQueueOpts = {
      Service = CircuitBreakerService.CardIssuer
      onCircuitBreakerEvent = broadcaster.circuitBreaker
      protectedAction =
         fun _ msg -> task {
            try
               let! result = networkRequest msg
               return Ok result
            with
            | :? FlurlHttpException as e ->
               let! errMsg = e.GetResponseStringAsync()
               return Error(Err.UnexpectedError errMsg)
            | e -> return Error(Err.UnexpectedError e.Message)
         }
      queueMessageToActionRequest = fun _ msg -> Task.FromResult(Some msg)
      onSuccessFlow =
         Flow.map
            (fun (mailbox, queueMessage: CardIssuerMessage, response) ->
               let metadata = queueMessage.Metadata
               let corrId = metadata.CorrelationId
               let orgId = metadata.OrgId

               match queueMessage, response with
               | CardIssuerMessage.CreateCard _,
                 CardIssuerResponse.CreateCard res ->
                  let msg =
                     Ok res
                     |> CardSetupSagaEvent.CardCreateResponse
                     |> AppSaga.Message.cardSetup orgId corrId

                  registry.SagaActor corrId <! msg
               | CardIssuerMessage.CloseCard _,
                 CardIssuerResponse.CloseCard res ->
                  logDebug mailbox $"Card detached {res.CardIssuerCardId}"
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
   : Task<CardIssuerResponse>
   =
   task {
      match msg with
      | CardIssuerMessage.CreateCard req ->
         let url = EnvEmployee.config.CardIssuerURI + "/cards"

         let! response =
            url
               .WithHeader("Authorization", cardIssuerApiKey)
               .PostJsonAsync(LithicCardCreateRequestDTO.fromEntity req)
               .ReceiveJson<LithicCardCreateResponseDTO>()

         return CardIssuerResponse.CreateCard response.AsEntity
      | CardIssuerMessage.CloseCard req ->
         // TODO: HTTP request to the card issuer API
         do! Task.Delay(TimeSpan.FromSeconds 3.3)

         return
            CardIssuerResponse.CloseCard {
               Customer = null
               CardIssuerCardId = req.CardIssuerCardId
            }
   }

let private mockNetworkRequestToCardIssuerService
   (msg: CardIssuerMessage)
   : Task<CardIssuerResponse>
   =
   task {
      match msg with
      | CardIssuerMessage.CreateCard _ ->
         do! Task.Delay(TimeSpan.FromSeconds 1.3)

         return
            CardIssuerResponse.CreateCard {
               CardIssuerCardId = Guid.NewGuid() |> CardIssuerCardId
               CardNumberLast4 =
                  List.init 4 (fun _ -> Random().Next(1, 9) |> string)
                  |> String.concat ""
               CardIssuerName = CardIssuerName.Lithic
            }
      | CardIssuerMessage.CloseCard req ->
         // TODO: HTTP request to the card issuer API
         do! Task.Delay(TimeSpan.FromSeconds 3.3)

         return
            CardIssuerResponse.CloseCard {
               Customer = null
               CardIssuerCardId = req.CardIssuerCardId
            }
   }

(*
 * Sending this request to Lithic will simulate Lithic card issuer receiving
 * purchase requests from the card networks.  Lithic will validate the
 * request against my configured card program rules.  Since I am also
 * using Lithic's Auth Stream Access (ASA) feature, Lithic will forward
 * the transaction (if it passes the rules I configure for my Lithic card
 * program) to my server so I can provide additional custom logic to determine
 * whether to approve the transaction.
 *)
let simulatePurchaseAuthorization
   (cardIssuerApiKey: string)
   (req: SimulatePurchaseRequest)
   : Task<Result<SimulatePurchaseResponse, Err>>
   =
   task {
      try
         let url = EnvEmployee.config.CardIssuerURI + "/simulate/authorize"

         let! response =
            url
               .WithHeader("Authorization", cardIssuerApiKey)
               .PostJsonAsync(LithicSimulatePurchaseRequestDTO.fromEntity req)
               .ReceiveJson<LithicSimulatePurchaseResponseDTO>()

         return Ok response.AsEntity
      with
      | :? FlurlHttpException as e ->
         let! errMsg = e.GetResponseStringAsync()
         return Error(Err.UnexpectedError errMsg)
      | e -> return Error(Err.UnexpectedError e.Message)
   }

let getCard
   (cardIssuerApiKey: string)
   (cardId: CardIssuerCardId)
   : Task<Result<CardGetResponse, Err>>
   =
   task {
      try
         let url = EnvEmployee.config.CardIssuerURI + $"/cards/{cardId}"

         let! response =
            url
               .WithHeader("Authorization", cardIssuerApiKey)
               .GetJsonAsync<LithicCardGetResponseDTO>()

         return Ok response.AsEntity
      with
      | :? FlurlHttpException as e ->
         let! errMsg = e.GetResponseStringAsync()
         return Error(Err.UnexpectedError errMsg)
      | e -> return Error(Err.UnexpectedError e.Message)
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
