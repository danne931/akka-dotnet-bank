open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.SignalR
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Akka.Hosting
open Akka.Cluster.Hosting
open Akka.HealthCheck.Hosting
open Akka.HealthCheck.Hosting.Web
open Serilog
open System.Text.Json.Serialization
open System

open Bank.Infrastructure
open Bank.UserSession.Middleware
open Bank.UserSession.Routes
open Bank.Org.Routes
open Bank.Account.Routes
open Bank.Account.Domain
open Bank.Transfer.Routes
open Bank.Diagnostic.Routes
open Bank.Transaction.Routes
open Bank.Employee.Routes
open Bank.Card.Routes
open Bank.Analytics.Routes
open Bank.Hubs
open ActorUtil
open Lib.SharedTypes

let builder = Env.builder

LogInfra.start builder

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
               typedefof<AccountMessage>
               typedefof<Account>
               typedefof<CircuitBreakerMessage>
               typedefof<CircuitBreakerActorState>
               typedefof<Email.EmailMessage>
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
            fun system -> BankSerializer(system)
         )
         .WithDistributedPubSub(ClusterMetadata.roles.signalR)
         .WithShardRegionProxy<ActorMetadata.OrgMarker>(
            ClusterMetadata.orgShardRegion.name,
            ClusterMetadata.roles.org,
            ClusterMetadata.orgShardRegion.messageExtractor
         )
         .WithShardRegionProxy<ActorMetadata.AccountMarker>(
            ClusterMetadata.accountShardRegion.name,
            ClusterMetadata.roles.account,
            ClusterMetadata.accountShardRegion.messageExtractor
         )
         .WithShardRegionProxy<ActorMetadata.EmployeeMarker>(
            ClusterMetadata.employeeShardRegion.name,
            ClusterMetadata.roles.employee,
            ClusterMetadata.employeeShardRegion.messageExtractor
         )
         .WithSingletonProxy<ActorMetadata.CircuitBreakerMarker>(
            ActorMetadata.circuitBreaker.Name,
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingletonProxy<ActorMetadata.EmailProxyMarker>(
            ActorMetadata.emailProxy.Name,
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMetadata.AccountLoadTestMarker>(
            ActorMetadata.accountLoadTest.Name,
            (fun system _ _ ->
               let typedProps =
                  AccountLoadTestActor.actorProps <| AccountActor.get system

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.web)
         )
         .WithActors(fun system _ ->
            SignalRActor.start system signalRHub |> ignore)
      |> ignore

      ())
)
|> ignore

builder.Services.AddEndpointsApiExplorer().AddSwaggerGen() |> ignore
builder.Services.WithAkkaHealthCheck(HealthCheckType.All) |> ignore

builder.Services
   .AddDistributedMemoryCache()
   .AddSession(fun opts ->
      opts.Cookie.IsEssential <- true
      opts.Cookie.HttpOnly <- true
      opts.IdleTimeout <- TimeSpan.FromMinutes 30)
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

app.MapHub<BankHub>(Constants.SIGNAL_R_HUB) |> ignore

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

startOrgRoutes app
startUserSessionRoutes app
startTransferRoutes app
startAccountRoutes app
startDiagnosticRoutes app
startTransactionRoutes app
startEmployeeRoutes app
startCardRoutes app
startAnalyticsRoutes app

app.Run()
