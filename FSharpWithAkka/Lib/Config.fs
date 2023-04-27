[<RequireQualifiedAccess>]
module Config

open Echo
open EventStore.Client
open Microsoft.AspNetCore.SignalR
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection

open Bank.Account.Api
open Bank.Hubs
open type AccountActor.AccountRegistry

let startActorModel () =
   ProcessConfig.initialise () |> ignore

   Process
      .DeadLetters()
      .Observe<DeadLetter>()
      .Subscribe(fun i -> printfn "Dead Process: %A" i)
   |> ignore

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
   =
   builder.Services.AddSingleton<AccountActor.AccountRegistry>(fun provider -> {
      loadAccount = getAccount (getAccountEvents esClient)
      saveAndPublish = saveAndPublish esClient
      startChildActors =
         (fun id -> [
         (*
            MaintenanceFeeActor.Start
                AccountAPI.GetAccountEvents esClient
                //lookBackDate: () => DateTime.UtcNow.AddDays(-30),
                //scheduledAt: () => TimeSpan.FromDays(30),
                //lookBackDate: () => DateTime.UtcNow.AddMinutes(-2),
                //scheduledAt: () => TimeSpan.FromMinutes(2),
                id
            )
            *)
         ])
      broadcast =
         (fun (event, accountState) ->
            provider
               .GetRequiredService<IHubContext<AccountHub, IAccountClient>>()
               .Clients.Group(accountState.EntityId.ToString())
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
   })
   |> ignore

   builder.Services.AddSingleton<EventStoreClient>(esClient) |> ignore
   ()
