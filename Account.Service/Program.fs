open Microsoft.Extensions.DependencyInjection
open Akka.Actor
open Akka.Event
open Akka.Hosting
open Akka.Cluster.Hosting
open Akka.Persistence
open Akka.Persistence.Hosting
open Akka.Persistence.Sql.Hosting
open Akka.Routing
open Akka.Streams.Amqp.RabbitMq
open Akkling

open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Infrastructure
open ActorUtil
open SignalRBroadcast

let builder = Env.builder

builder.Services.AddSingleton<SignalRBroadcast>(fun provider ->
   let system = provider.GetRequiredService<ActorSystem>()
   SignalRBroadcaster.init system)
|> ignore

builder.Services.AddSingleton<AmqpConnectionDetails>(fun _ ->
   Lib.Rabbit.createConnection Env.config.RabbitConnection)
|> ignore

let journalOpts = AkkaInfra.getJournalOpts ()

journalOpts.Adapters
   .AddEventAdapter<OrganizationEventPersistenceAdapter>(
      "org-v1",
      [ typedefof<OrgMessage> ]
   )
   .AddEventAdapter<AccountEventPersistenceAdapter>(
      "account-v1",
      [ typedefof<AccountMessage> ]
   )
   .AddEventAdapter<EmployeeEventPersistenceAdapter>(
      "employee-v1",
      [ typedefof<EmployeeMessage> ]
   )
|> ignore

let snapshotOpts = AkkaInfra.getSnapshotOpts ()

builder.Services.AddAkka(
   Env.config.AkkaSystemName,
   (fun builder provider ->
      let initConfig =
         AkkaInfra.withClustering [|
            ClusterMetadata.roles.org
            ClusterMetadata.roles.account
            ClusterMetadata.roles.signalR
            ClusterMetadata.roles.employee
         |]
         << AkkaInfra.withPetabridgeCmd
         << AkkaInfra.withHealthCheck
         << AkkaInfra.withLogging

      (initConfig builder)
         .WithSqlPersistence(
            Env.config.ConnectionStrings.PostgresAdoFormat,
            Env.config.AkkaPersistence.DbProvider,
            PersistenceMode.Both
         )
         .WithJournalAndSnapshot(journalOpts, snapshotOpts)
         .WithCustomSerializer(
            BankSerializer.Name,
            [ typedefof<obj> ],
            (fun system -> BankSerializer(system))
         )
         .WithDistributedPubSub(ClusterMetadata.roles.signalR)
         .WithSingletonProxy<ActorMetadata.SchedulingMarker>(
            ActorMetadata.scheduling.Name,
            ClusterSingletonOptions(Role = ClusterMetadata.roles.scheduling)
         )
         .WithShardRegion<ActorMetadata.OrgMarker>(
            ClusterMetadata.orgShardRegion.name,
            (fun persistenceId ->
               let system = provider.GetRequiredService<ActorSystem>()

               let props =
                  OrgActor.initProps
                     (provider.GetRequiredService<SignalRBroadcast>())
                     // TODO: Create org-specific Environment file & replace
                     //       account env var here.
                     Env.config.AccountActorSupervisor
                     persistenceId
                     (AccountActor.get system)
                     (EmployeeActor.get system)
                     Bank.Transfer.Api.getDomesticTransfersRetryableUponRecipientCorrection

               props),
            ClusterMetadata.orgShardRegion.messageExtractor,
            ShardOptions(
               RememberEntities = true,
               Role = ClusterMetadata.roles.org
            )
         )
         .WithShardRegion<ActorMetadata.AccountMarker>(
            ClusterMetadata.accountShardRegion.name,
            (fun persistenceId ->
               let system = provider.GetRequiredService<ActorSystem>()

               let props =
                  AccountActor.initProps
                     (provider.GetRequiredService<SignalRBroadcast>())
                     system
                     Env.config.AccountActorSupervisor
                     persistenceId
                     (OrgActor.get system)
                     (EmployeeActor.get system)
                     DomesticTransferProducerActor.get
                     EmailProducerActor.get
                     AccountClosureActor.get
                     BillingStatementActor.get
                     SchedulingActor.get

               props),
            ClusterMetadata.accountShardRegion.messageExtractor,
            ShardOptions(
               RememberEntities = true,
               Role = ClusterMetadata.roles.account
            )
         )
         .WithShardRegion<ActorMetadata.EmployeeMarker>(
            ClusterMetadata.employeeShardRegion.name,
            (fun persistenceId ->
               let system = provider.GetRequiredService<ActorSystem>()

               let props =
                  EmployeeActor.initProps
                     // TODO: Create employee-specific Environment file & replace
                     //       account env var here.
                     Env.config.AccountActorSupervisor
                     persistenceId
                     (provider.GetRequiredService<SignalRBroadcast>())
                     (AccountActor.get system)
                     (OrgActor.get system)
                     (EmailProducerActor.get)

               props),
            ClusterMetadata.employeeShardRegion.messageExtractor,
            ShardOptions(Role = ClusterMetadata.roles.employee)
         )
         .WithSingleton<ActorMetadata.BillingCycleMarker>(
            ActorMetadata.billingCycle.Name,
            (fun system _ resolver ->
               let typedProps =
                  BillingCycleActor.actorProps
                  <| Env.config.BillingCycleFanoutThrottle
                  <| AccountActor.get system

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMetadata.TransferProgressTrackingMarker>(
            ActorMetadata.transferProgressTracking.Name,
            (fun system _ _ ->
               let typedProps =
                  TransferProgressTrackingActor.initProps
                     system
                     DomesticTransferProducerActor.get

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMetadata.AutoTransferSchedulingMarker>(
            ActorMetadata.autoTransferScheduling.Name,
            (fun system _ _ ->
               let typedProps =
                  AutomaticTransferSchedulingActor.initProps
                     system
                     (AccountActor.get system)

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMetadata.AccountClosureMarker>(
            ActorMetadata.accountClosure.Name,
            (fun system _ _ ->
               let typedProps =
                  AccountClosureActor.initProps
                     (AccountActor.get system)
                     EmailProducerActor.get
                     SchedulingActor.get
                     Env.config.AccountDeleteThrottle

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMetadata.OrgReadModelSyncMarker>(
            ActorMetadata.orgReadModelSync.Name,
            (fun system _ _ ->
               let typedProps =
                  OrgReadModelSyncActor.initProps
                     (OrgActor.get system)
                     // TODO: Create org-specific Environment file & replace
                     //       account env var here.
                     Env.config.AccountEventProjectionChunking
                     Env.config.AccountEventReadModelPersistenceBackoffRestart
                     Env.config.AccountEventReadModelRetryPersistenceAfter

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.org)
         )
         .WithSingleton<ActorMetadata.AccountReadModelSyncMarker>(
            ActorMetadata.accountReadModelSync.Name,
            (fun system _ _ ->
               let typedProps =
                  AccountReadModelSyncActor.initProps
                     (AccountActor.get system)
                     Env.config.AccountEventProjectionChunking
                     Env.config.AccountEventReadModelPersistenceBackoffRestart
                     Env.config.AccountEventReadModelRetryPersistenceAfter

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMetadata.EmployeeReadModelSyncMarker>(
            ActorMetadata.employeeReadModelSync.Name,
            (fun system _ _ ->
               let typedProps =
                  EmployeeReadModelSyncActor.initProps
                     (EmployeeActor.get system)
                     // TODO: Create employee-specific Environment file & replace
                     //       account env var here.
                     Env.config.AccountEventProjectionChunking
                     Env.config.AccountEventReadModelPersistenceBackoffRestart
                     Env.config.AccountEventReadModelRetryPersistenceAfter

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.employee)
         )
         // TODO: Do more than just printing dead letter messages.
         .WithSingleton<ActorMetadata.AuditorMarker>(
            ActorMetadata.auditor.Name,
            (fun system _ _ ->
               let handler (ctx: Actor<_>) =
                  EventStreaming.subscribe ctx.Self system.EventStream
                  |> ignore

                  logInfo ctx "Auditor subscribed to system event stream"

                  let rec loop () = actor {
                     let! (msg: AllDeadLetters) = ctx.Receive()

                     let logMsg = $"Dead letter: {msg}"

                     match msg.Message with
                     | :? SaveSnapshotSuccess -> logWarning ctx logMsg
                     | _ -> logError ctx logMsg

                     return! loop ()
                  }

                  loop ()

               (props handler).ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMetadata.AccountSeederMarker>(
            ActorMetadata.accountSeeder.Name,
            (fun system _ _ ->
               AccountSeederActor.actorProps
                  (OrgActor.get system)
                  (AccountActor.get system)
                  (EmployeeActor.get system)
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMetadata.CircuitBreakerMarker>(
            ActorMetadata.circuitBreaker.Name,
            (fun _ _ _ ->
               CircuitBreakerActor.initProps
                  Env.config.CircuitBreakerActorSupervisor),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         // Consume EmailMessages off of RabbitMq
         .WithSingleton<ActorMetadata.EmailConsumerMarker>(
            ActorMetadata.emailConsumer.Name,
            (fun system _ _ ->
               EmailConsumerActor.initProps
                  (EnvNotifications.config.circuitBreaker system)
                  (provider.GetRequiredService<SignalRBroadcast>())
                  (provider.GetRequiredService<AmqpConnectionDetails>())
                  EnvNotifications.config.RabbitQueue
                  EnvNotifications.config.EmailBearerToken
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         // Forward Email messages from web node to EmailProducer actors.
         .WithSingleton<ActorMetadata.EmailProxyMarker>(
            ActorMetadata.emailProxy.Name,
            (fun system _ _ ->
               (fun (msg: Email.EmailMessage) ->
                  EmailProducerActor.get system <<! msg
                  ignored ())
               |> actorOf
               |> props
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         // Consume DomesticTransferMessages off of RabbitMq
         .WithSingleton<ActorMetadata.DomesticTransferConsumerMarker>(
            ActorMetadata.domesticTransferConsumer.Name,
            (fun system _ _ ->
               DomesticTransferConsumerActor.initProps
                  (provider.GetRequiredService<SignalRBroadcast>())
                  (AccountActor.get system)
                  EmailProducerActor.get
                  (EnvTransfer.config.domesticTransferCircuitBreaker system)
                  (provider.GetRequiredService<AmqpConnectionDetails>())
                  EnvTransfer.config.RabbitQueue
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithActors(fun system registry ->
            registry.Register<ActorMetadata.BillingStatementMarker>(
               BillingStatementActor.start
                  system
                  Env.config.BillingStatementPersistenceChunking
                  Env.config.BillingStatementPersistenceBackoffRestart
                  Env.config.BillingStatementRetryPersistenceAfter
               |> untyped
            )

            // Other actors in the system send EmailMessages to this actor
            // which will enqueue the message into RabbitMq for the
            // EmailConsumer Singleton Actor to process.
            registry.Register<ActorMetadata.EmailProducerMarker>(
               EmailProducerActor.start
                  system
                  (provider.GetRequiredService<AmqpConnectionDetails>())
                  EnvNotifications.config.RabbitQueue
               |> untyped
            )

            // Other actors in the system send DomesticTransferMessages to this actor
            // which will enqueue the message into RabbitMq for the
            // DomesticTransferConsumer Singleton Actor to process.
            registry.Register<ActorMetadata.DomesticTransferProducerMarker>(
               DomesticTransferProducerActor.start
                  system
                  (provider.GetRequiredService<AmqpConnectionDetails>())
                  EnvTransfer.config.RabbitQueue
               |> untyped
            )

            ())
         .AddStartup(
            StartupTask(fun _ registry -> task {
               if not Env.isProd then
                  AccountSeederActor.get registry
                  <! AccountSeederMessage.SeedAccounts
            })
         )
      |> ignore

      ())
)
|> ignore

let host = builder.Build()
host.Run()
