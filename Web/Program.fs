open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.SignalR
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Akka.Hosting
open Akka.Cluster.Hosting
open Akka.HealthCheck.Hosting
open Akka.HealthCheck.Hosting.Web
open Akka.Actor
open Akkling
open Serilog
open System.Text.Json.Serialization
open System

open Bank.Infrastructure
open Bank.UserSession.Middleware
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Hubs
open ActorUtil
open Lib.CircuitBreaker
open BankActorRegistry

let builder = Env.builder

LogInfra.start builder |> ignore

builder.Services.AddSingleton<BankActorRegistry>(fun provider ->
   let system = provider.GetRequiredService<ActorSystem>()
   BankActorRegistry system)
|> ignore

// Endpoint serialization
builder.Services.ConfigureHttpJsonOptions(fun opts ->
   JsonFSharpOptions
      .ThothLike()
      .WithUnwrapOption()
      .AddToJsonSerializerOptions(
         Serialization.mergeDefaultJsonOptions opts.SerializerOptions
      )

   ())
|> ignore

SignalRInfra.start builder

builder.Services.AddAkka(
   Env.config.AkkaSystemName,
   (fun builder provider ->
      let signalRHub =
         provider.GetRequiredService<IHubContext<BankHub, IBankClient>>()

      let initConfig =
         AkkaInfra.withClustering [|
            ClusterMetadata.roles.web
            ClusterMetadata.roles.signalR
         |]
         << AkkaInfra.withPetabridgeCmd
         << AkkaInfra.withHealthCheck
         << AkkaInfra.withLogging

      (initConfig builder)
         .WithCustomSerializer(
            BankSerializer.Name,
            [
               typedefof<OrgMessage>
               typedefof<AccountMessage>
               typedefof<Account>
               typedefof<EmployeeMessage>
               typedefof<CircuitBreakerMessage>
               typedefof<CircuitBreakerState>
               typedefof<EmailMessage.EmailMessage>
               // NOTE: Akka ShardRegionProxy defined in Akka.Hosting below
               //       does not recognize Akkling ShardEnvelope as Akka
               //       ShardingEnvelope so need to explicitly add it for
               //       message extraction.
               //
               //       This would be unnecessary if I was using Akka.Hosting
               //       IRequiredActor<> to dependency inject the account shard
               //       region proxy into routes. However, doing so would lose
               //       Akkling's typed actor message benefits when forwarding
               //       a message from Akka ShardRegionProxy.
               typedefof<Akkling.Cluster.Sharding.ShardEnvelope>
            ],
            fun system -> BankSerializer system
         )
         .WithDistributedPubSub(ClusterMetadata.roles.signalR)
         .WithShardRegionProxy<ActorMarker.Org>(
            ClusterMetadata.orgShardRegion.name,
            ClusterMetadata.roles.org,
            ClusterMetadata.orgShardRegion.messageExtractor
         )
         .WithShardRegionProxy<ActorMarker.Account>(
            ClusterMetadata.accountShardRegion.name,
            ClusterMetadata.roles.account,
            ClusterMetadata.accountShardRegion.messageExtractor
         )
         .WithShardRegionProxy<ActorMarker.Employee>(
            ClusterMetadata.employeeShardRegion.name,
            ClusterMetadata.roles.employee,
            ClusterMetadata.employeeShardRegion.messageExtractor
         )
         .WithSingletonProxy<ActorMarker.CircuitBreaker>(
            ActorMetadata.circuitBreaker.Name,
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingletonProxy<ActorMarker.EmailProxy>(
            ActorMetadata.emailProxy.Name,
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMarker.AccountLoadTest>(
            ActorMetadata.accountLoadTest.Name,
            (fun _ _ _ ->
               let registry = provider.GetRequiredService<BankActorRegistry>()
               let typedProps = AccountLoadTestActor.actorProps registry
               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.web)
         )
         .WithActors(fun system registry ->
            registry.Register<ActorMarker.OrgGuaranteedDeliveryProducer>(
               GuaranteedDelivery.producer<OrgMessage> {
                  System = system
                  ShardRegion = registry.Get<ActorMarker.Org>()
                  ProducerName = "web-to-org-actor-proxy"
               }
               |> untyped
            )

            registry.Register<ActorMarker.AccountGuaranteedDeliveryProducer>(
               GuaranteedDelivery.producer<AccountMessage> {
                  System = system
                  ShardRegion = registry.Get<ActorMarker.Account>()
                  ProducerName = "web-to-account-actor-proxy"
               }
               |> untyped
            )

            registry.Register<ActorMarker.EmployeeGuaranteedDeliveryProducer>(
               GuaranteedDelivery.producer<EmployeeMessage> {
                  System = system
                  ShardRegion = registry.Get<ActorMarker.Employee>()
                  ProducerName = "web-to-employee-actor-proxy"
               }
               |> untyped
            )

            SignalRActor.start system signalRHub |> ignore)
      |> ignore

      ())
)
|> ignore

builder.Services.AddEndpointsApiExplorer().AddSwaggerGen() |> ignore
builder.Services.WithAkkaHealthCheck HealthCheckType.All |> ignore

builder.Services
   .AddDistributedMemoryCache()
   .AddSession(fun opts ->
      opts.Cookie.IsEssential <- true
      opts.Cookie.HttpOnly <- true
      opts.IdleTimeout <- TimeSpan.FromMinutes 30.)
|> ignore

let app = builder.Build()

app
   .UseSession()
   .UseDefaultFiles()
   .UseStaticFiles()
   .UseSerilogRequestLogging()
   .UseMiddleware<RoleMiddleware>()
|> ignore

// Available at endpoint: /swagger/index.html
if app.Environment.IsDevelopment() then
   app.UseSwagger().UseSwaggerUI() |> ignore

app.MapHub<BankHub> Constants.SIGNAL_R_HUB |> ignore

// Available at endpoint: /healthz/akka
// Changing the default ResponseWriter to JsonResponseWriter
// provides a more verbose response than the default string
// response of "healthy"/"unhealthy".
app.MapAkkaHealthCheckRoutes(
   optionConfigure =
      fun _ opt ->
         opt.ResponseWriter <-
            fun httpCtx healthReport ->
               Helper.JsonResponseWriter(httpCtx, healthReport)
)
|> ignore

Bank.Routes.Org.start app Bank.Transfer.Api.getDomesticTransferRecipients
Bank.Routes.UserSession.start app
Bank.Routes.Transfer.start app Bank.Account.Api.processCommand
Bank.Routes.PaymentRequest.start app Bank.Account.Api.processCommand
Bank.Routes.Account.start app
Bank.Routes.Diagnostic.start app
Bank.Routes.Transaction.start app
Bank.Routes.Employee.start app
Bank.Routes.Card.start app
Bank.Routes.Analytics.start app

app.Run()
