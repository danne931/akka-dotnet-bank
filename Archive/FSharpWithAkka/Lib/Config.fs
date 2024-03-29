[<RequireQualifiedAccess>]
module Config

open System
open Akka.Event
open Akka.Pattern
open Akkling
open EventStore.Client
open Microsoft.AspNetCore.SignalR
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection

open BankTypes
open Bank.Account.Api
open Bank.Hubs
open ActorUtil

let enableDefaultHttpJsonSerialization (builder: WebApplicationBuilder) =
   builder.Services.ConfigureHttpJsonOptions(fun opts ->
      Serialization.withInjectedOptions opts.SerializerOptions
      ())
   |> ignore

let startActorModel () =
   let system = System.create "bank" (Configuration.load ())

   let deadLetterHandler (msg: AllDeadLetters) =
      printfn "Dead letters: %A" msg
      Ignore

   let deadLetterRef =
      spawn
         system
         ActorMetadata.deadLettersMonitor.Name
         (props (actorOf deadLetterHandler))

   EventStreaming.subscribe deadLetterRef system.EventStream |> ignore

   system

let startSignalR (builder: WebApplicationBuilder) =
   builder.Services
      .AddSignalR()
      .AddJsonProtocol(fun opts ->
         Serialization.withInjectedOptions opts.PayloadSerializerOptions)
   |> ignore

let startEventStore (builder: WebApplicationBuilder) =
   let connString =
      builder.Configuration.GetSection("ConnectionStrings").Item "EventStore"

   if isNull connString then
      failwith "EventStore connection string not found in appsettings"

   EventStoreManager.connect connString

let injectDependencies
   (builder: WebApplicationBuilder)
   (esClient: EventStoreClient)
   (actorSystem: Akka.Actor.ActorSystem)
   =
   let persistence: AccountPersistence = {
      loadAccountEvents = (getAccountEvents esClient)
      loadAccount = getAccount (getAccountEvents esClient)
      save = save esClient
   }

   let initBroadcast (provider: IServiceProvider) : AccountBroadcast = {
      broadcast =
         (fun (event, accountState) ->
            provider
               .GetRequiredService<IHubContext<AccountHub, IAccountClient>>()
               .Clients.Group(string accountState.EntityId)
               .ReceiveMessage(
                  {|
                     event = event
                     newState = accountState
                  |}
               ))
      broadcastError =
         (fun errMsg ->
            provider
               .GetRequiredService<IHubContext<AccountHub, IAccountClient>>()
               .Clients.All.ReceiveError(errMsg))
      broadcastCircuitBreaker =
         (fun circuitBreakerMessage ->
            provider
               .GetRequiredService<IHubContext<AccountHub, IAccountClient>>()
               .Clients.All.ReceiveCircuitBreakerMessage(circuitBreakerMessage))
   }

   builder.Services.AddSingleton<AccountBroadcast>(initBroadcast) |> ignore

   builder.Services.AddSingleton<IActorRef<AccountCoordinatorMessage>>
      (fun provider ->
         let broadcast = provider.GetRequiredService<AccountBroadcast>()
         AccountCoordinatorActor.start actorSystem persistence broadcast)
   |> ignore

   builder.Services.AddSingleton<IActorRef<DomesticTransferRecipientActor.Message>>
      (fun provider ->
         DomesticTransferRecipientActor.start
            actorSystem
            (CircuitBreaker(
               actorSystem.Scheduler,
               maxFailures = 2,
               callTimeout = TimeSpan.FromSeconds 7,
               resetTimeout = TimeSpan.FromMinutes 1
            ))
            (provider.GetRequiredService<AccountBroadcast>()))
   |> ignore

   builder.Services.AddSingleton<AccountPersistence>(persistence) |> ignore

   builder.Services.AddSingleton<EventStoreClient>(esClient) |> ignore
   ()
