[<RequireQualifiedAccess>]
module Config

open Echo
open EventStore.Client
open Microsoft.AspNetCore.SignalR
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection

open Lib.Types
open Bank.Transfer.Domain
open Bank.Account.Domain
open Bank.Account.Api
open Bank.Hubs
open type AccountActor.AccountRegistry

let startActorModel () =
   //ProcessConfig.initialise() |> ignore
   //Process.DeadLetters
   None

let startEventStore (builder: WebApplicationBuilder) =
   EventStoreManager.connect
      "esdb://127.0.0.1:2113?tls=false&keepAliveTimeout=10000&keepAliveInterval=10000" // builder.Configuration.GetConnectionString "EventStore"

let injectDependencies
   (builder: WebApplicationBuilder)
   (esClient: EventStoreClient)
   =
   //builder.Services.AddSingleton<Validator<TransferCmd>>(TransferValidation())

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
               .ReceiveMessage((event, accountState)))
      broadcastError =
         (fun errMsg ->
            provider
               .GetRequiredService<IHubContext<AccountHub, IAccountClient>>()
               .Clients.All.ReceiveError(errMsg))
   })
   |> ignore

   builder.Services.AddSingleton<EventStoreClient>(esClient)
