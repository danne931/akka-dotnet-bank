open Microsoft.Extensions.DependencyInjection
open Akka.Actor
open Akka.Event
open Akka.Hosting
open Akka.Cluster.Hosting
open Akka.Persistence
open Akka.Persistence.Hosting
open Akka.Persistence.Sql.Hosting
open Akkling

open CachedOrgSettings
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open PartnerBank.Service.Domain
open CardIssuer.Service.Domain
open Bank.Infrastructure
open ActorUtil
open SignalRBroadcast
open BankActorRegistry
open EmailMessage

let builder = Env.builder

LogInfra.start builder |> ignore

builder.Services.AddSingleton<BankActorRegistry>(fun provider ->
   let system = provider.GetRequiredService<ActorSystem>()
   BankActorRegistry system)
|> ignore

builder.Services.AddSingleton<SignalRBroadcast>(fun provider ->
   let system = provider.GetRequiredService<ActorSystem>()
   let registry = provider.GetRequiredService<BankActorRegistry>()
   SignalRBroadcaster.init system registry)
|> ignore

builder.Services.AddSingleton<OrgSettingsCache>(fun provider ->
   let system = provider.GetRequiredService<ActorSystem>()

   {
      Get = OrgSettingsCache.get system
      Update = OrgSettingsCache.update system
   })
|> ignore

builder.Services.AddSingleton<Lib.Queue.QueueConnectionDetails>(fun _ ->
   Lib.Queue.createConnection Env.config.QueueConnection)
|> ignore

let getQueueConnection (provider: System.IServiceProvider) =
   provider.GetRequiredService<Lib.Queue.QueueConnectionDetails>()

let getBroadcaster (provider: System.IServiceProvider) =
   provider.GetRequiredService<SignalRBroadcast>()

let getActorRegistry (provider: System.IServiceProvider) =
   provider.GetRequiredService<BankActorRegistry>()

builder.Services.AddAkka(
   Env.config.AkkaSystemName,
   (fun builder provider ->
      let builder =
         builder
            .AddHocon(
               "akka.reliable-delivery.sharding.consumer-controller.allow-bypass: true",
               HoconAddMode.Prepend
            )
            // TODO: Remove once guaranteed delivery consumer controller logging
            // on passivation fixed:
            // https://github.com/akkadotnet/akka.net/issues/7664#issuecomment-2991799919
            .ConfigureLoggers(fun conf ->
               conf.WithLogFilter(fun logBuilder ->
                  logBuilder.ExcludeMessageContaining("DeathWatch").Build()
                  |> ignore)
               |> ignore)

      let initConfig =
         AkkaInfra.withClustering [|
            ClusterMetadata.roles.org
            ClusterMetadata.roles.account
            ClusterMetadata.roles.signalR
            ClusterMetadata.roles.employee
            ClusterMetadata.roles.saga
            ClusterMetadata.roles.crdt
         |]
         << AkkaInfra.withPetabridgeCmd
         << AkkaInfra.withHealthCheck
         << AkkaInfra.withLogging
         << AkkaInfra.withConflictFreeReplicatedDataTypes

      (initConfig builder)
         .WithSqlPersistence(
            Env.config.ConnectionStrings.PostgresAdoFormat,
            Env.config.AkkaPersistence.DbProvider,
            PersistenceMode.Both
         )
         .WithSnapshot(AkkaInfra.getSnapshotOpts ())
         .WithJournal(
            AkkaInfra.getJournalOpts (),
            fun journalBuilder ->
               journalBuilder
                  .AddEventAdapter<OrganizationEventPersistenceAdapter>(
                     "org-v1",
                     [ typeof<OrgEvent> ]
                  )
                  .AddEventAdapter<AccountEventPersistenceAdapter>(
                     "account-v1",
                     [ typeof<AccountEvent> ]
                  )
                  .AddEventAdapter<EmployeeEventPersistenceAdapter>(
                     "employee-v1",
                     [ typeof<EmployeeEvent> ]
                  )
                  .AddEventAdapter<SagaEventPersistenceAdapter>(
                     "saga-v1",
                     [ typeof<Lib.Saga.IPersistableSagaEvent> ]
                  )
               |> ignore
         )
         .WithCustomSerializer(
            BankSerializer.Name,
            [ typedefof<obj> ],
            fun system -> BankSerializer system
         )
         .WithDistributedPubSub(ClusterMetadata.roles.signalR)
         .WithSingletonProxy<ActorMarker.Scheduling>(
            ActorMetadata.scheduling.Name,
            ClusterSingletonOptions(Role = ClusterMetadata.roles.scheduling)
         )
         .WithShardRegion<ActorMarker.Org>(
            ClusterMetadata.orgShardRegion.name,
            (fun persistenceId ->
               let system = provider.GetRequiredService<ActorSystem>()

               GuaranteedDelivery.consumer<OrgMessage>
                  system
                  (fun controllerRef ->
                     OrgActor.initProps
                        (getActorRegistry provider)
                        (getBroadcaster provider)
                        // TODO: Create org-specific Environment file & replace
                        //       account env var here.
                        Env.config.AccountActorSupervisor
                        persistenceId
                        (typed controllerRef))),
            ClusterMetadata.orgShardRegion.messageExtractor,
            ShardOptions(
               RememberEntities = true,
               Role = ClusterMetadata.roles.org
            )
         )
         .WithShardRegion<ActorMarker.Account>(
            ClusterMetadata.accountShardRegion.name,
            (fun persistenceId ->
               let system = provider.GetRequiredService<ActorSystem>()

               GuaranteedDelivery.consumer<AccountMessage>
                  system
                  (fun controllerRef ->
                     AccountActor.initProps
                        (getActorRegistry provider)
                        (getBroadcaster provider)
                        Env.config.AccountActorSupervisor
                        persistenceId
                        Bank.Transfer.Api.getDomesticTransfersRetryableUponCounterpartyCorrection
                        (typed controllerRef))),
            ClusterMetadata.accountShardRegion.messageExtractor,
            ShardOptions(
               RememberEntities = true,
               Role = ClusterMetadata.roles.account
            )
         )
         .WithShardRegion<ActorMarker.Employee>(
            ClusterMetadata.employeeShardRegion.name,
            (fun persistenceId ->
               let system = provider.GetRequiredService<ActorSystem>()

               GuaranteedDelivery.consumer<EmployeeMessage>
                  system
                  (fun controllerRef ->
                     EmployeeActor.initProps
                        (getActorRegistry provider)
                        (getBroadcaster provider)
                        // TODO: Create employee-specific Environment file & replace
                        //       account env var here.
                        Env.config.AccountActorSupervisor
                        persistenceId
                        (typed controllerRef))),
            ClusterMetadata.employeeShardRegion.messageExtractor,
            ShardOptions(
               RememberEntities = true,
               Role = ClusterMetadata.roles.employee
            )
         )
         .WithShardRegion<ActorMarker.Saga>(
            ClusterMetadata.sagaShardRegion.name,
            (fun persistenceId ->
               let system = provider.GetRequiredService<ActorSystem>()
               let registry = getActorRegistry provider

               let orgSettingsCache =
                  provider.GetRequiredService<OrgSettingsCache>()

               let broadcaster =
                  provider.GetRequiredService<SignalRBroadcast>()

               GuaranteedDelivery.consumer<AppSaga.AppSagaMessage>
                  system
                  (fun controllerRef ->
                     AppSaga.initProps
                        registry
                        orgSettingsCache
                        broadcaster
                        Env.config.SagaPassivateIdleEntityAfter
                        persistenceId
                        (Some(typed controllerRef))
                     |> _.ToProps())),
            ClusterMetadata.sagaShardRegion.messageExtractor,
            ShardOptions(
               RememberEntities = false,
               PassivateIdleEntityAfter =
                  Env.config.SagaPassivateIdleEntityAfter,
               Role = ClusterMetadata.roles.saga
            )
         )
         // The KnowYourCustomerService will consume know-your-customer
         // messages from RabbitMq & interact with a third party business
         // verification API such as middesk.com.  It will notify the
         // OrgOnboardingSaga if the customer is in legal good standing.
         .WithSingleton<ActorMarker.KYCService>(
            ActorMetadata.knowYourCustomer.Name,
            (fun system _ _ ->
               KnowYourCustomerServiceActor.initProps
                  (getActorRegistry provider)
                  (getQueueConnection provider)
                  EnvOrg.config.KnowYourCustomerServiceQueue
                  Env.config.QueueConsumerStreamBackoffRestart
                  (EnvOrg.config.KnowYourCustomerServiceCircuitBreaker system)
                  (getBroadcaster provider)
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.org)
         )
         // The PartnerBankService will consume messages from
         // from RabbitMq & interact with a third party partner
         // bank API to sync transactions occuring on the platform.
         // It will notify relevant saga actors of syncing progress.
         .WithSingleton<ActorMarker.PartnerBankService>(
            ActorMetadata.partnerBankService.Name,
            (fun system _ _ ->
               PartnerBankServiceActor.initProps
                  (getActorRegistry provider)
                  (getQueueConnection provider)
                  EnvPartnerBank.config.Queue
                  Env.config.QueueConsumerStreamBackoffRestart
                  (EnvPartnerBank.config.CircuitBreaker system)
                  (getBroadcaster provider)
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMarker.CardIssuerService>(
            ActorMetadata.cardIssuerService.Name,
            (fun system _ _ ->
               CardIssuerService.initProps
                  (getActorRegistry provider)
                  (getQueueConnection provider)
                  EnvEmployee.config.CardIssuerServiceQueue
                  Env.config.QueueConsumerStreamBackoffRestart
                  (EnvEmployee.config.cardIssuerServiceCircuitBreaker system)
                  (getBroadcaster provider)
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.employee)
         )
         .WithSingleton<ActorMarker.SagaAlarmClock>(
            ActorMetadata.sagaAlarmClock.Name,
            (fun system _ _ ->
               SagaAlarmClockActor.initProps
                  system
                  (AppSaga.getEntityRef system)
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.saga)
         )
         .WithSingleton<ActorMarker.BillingCycle>(
            ActorMetadata.billingCycle.Name,
            (fun system _ _ ->
               let typedProps =
                  BillingCycleActor.actorProps
                     (getActorRegistry provider)
                     Env.config.BillingCycleFanoutThrottle

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMarker.AutoTransferScheduling>(
            ActorMetadata.autoTransferScheduling.Name,
            (fun system _ _ ->
               let typedProps =
                  AutomaticTransferSchedulingActor.initProps
                     (getActorRegistry provider)
                     system
                     EnvTransfer.config.AutoTransferComputeThrottle

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMarker.ScheduledTransfersLowBalanceWarning>(
            ActorMetadata.scheduledTransfersLowBalanceWarning.Name,
            (fun system _ _ ->
               let typedProps =
                  ScheduledTransfersLowBalanceWarningActor.initProps
                     (getActorRegistry provider)
                     system

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMarker.AccountClosure>(
            ActorMetadata.accountClosure.Name,
            (fun system _ _ ->
               let typedProps =
                  AccountClosureActor.initProps
                     (getActorRegistry provider)
                     Env.config.AccountDeleteThrottle

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMarker.OrgReadModelSync>(
            ActorMetadata.orgReadModelSync.Name,
            (fun _ _ _ ->
               let typedProps =
                  OrgReadModelSyncActor.initProps
                     // TODO: Create org-specific Environment file & replace
                     //       account env var here.
                     Env.config.AccountEventProjectionChunking
                     Env.config.AccountEventReadModelPersistenceBackoffRestart
                     Env.config.AccountEventReadModelRetryPersistenceAfter

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.org)
         )
         .WithSingleton<ActorMarker.AccountReadModelSync>(
            ActorMetadata.accountReadModelSync.Name,
            (fun _ _ _ ->
               let typedProps =
                  AccountReadModelSyncActor.initProps
                     Env.config.AccountEventProjectionChunking
                     Env.config.AccountEventReadModelPersistenceBackoffRestart
                     Env.config.AccountEventReadModelRetryPersistenceAfter

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMarker.EmployeeReadModelSync>(
            ActorMetadata.employeeReadModelSync.Name,
            (fun _ _ _ ->
               let typedProps =
                  EmployeeReadModelSyncActor.initProps
                     // TODO: Create employee-specific Environment file & replace
                     //       account env var here.
                     Env.config.AccountEventProjectionChunking
                     Env.config.AccountEventReadModelPersistenceBackoffRestart
                     Env.config.AccountEventReadModelRetryPersistenceAfter

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.employee)
         )
         .WithSingleton<ActorMarker.SagaReadModelSync>(
            ActorMetadata.sagaReadModelSync.Name,
            (fun system _ _ ->
               let typedProps =
                  AppSagaReadModelSyncActor.initProps
                     (AppSaga.getEntityRef system)
                     // TODO: Create saga-specific Environment file & replace
                     //       account env var here.
                     Env.config.AccountEventProjectionChunking
                     Env.config.AccountEventReadModelPersistenceBackoffRestart
                     Env.config.AccountEventReadModelRetryPersistenceAfter

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.saga)
         )
         // TODO: Do more than just printing dead letter messages.
         .WithSingleton<ActorMarker.Auditor>(
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
         // TODO:
         // We may not always receive Lithic purchase progress updates via webhook.
         // We should reconcile InProgress purchases that have not received updates in some time.
         // We will query our card issuer (ex: Lithic) for a purchase progress report
         // and compare with InProgress purchase saga states.
         .WithSingleton<ActorMarker.PurchaseReconciliation>(
            ActorMetadata.purchaseReconciliation.Name,
            (fun system _ _ ->
               let handler (ctx: Actor<_>) =
                  let rec loop () = actor {
                     let! msg = ctx.Receive()
                     return ignored ()
                  }

                  loop ()

               (props handler).ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         // TODO:
         // We may not always receive transfer progress updates from partner bank.
         // We should reconcile InProgress transfers that have not received updates in some time.
         // We will query our partner banks (ex: Column) for a progress report and compare with
         // InProgress transfer saga states.
         .WithSingleton<ActorMarker.TransferReconciliation>(
            ActorMetadata.transferReconciliation.Name,
            (fun system _ _ ->
               let handler (ctx: Actor<_>) =
                  let rec loop () = actor {
                     let! msg = ctx.Receive()
                     return ignored ()
                  }

                  loop ()

               (props handler).ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMarker.AccountSeeder>(
            ActorMetadata.accountSeeder.Name,
            (fun _ _ _ ->
               AccountSeederActor.actorProps
                  (getActorRegistry provider)
                  PartnerBankServiceActor.createCounterParty
                  Bank.Org.Api.submitOnboardingApplication
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMarker.CircuitBreaker>(
            ActorMetadata.circuitBreaker.Name,
            (fun _ _ _ ->
               CircuitBreakerActor.initProps
                  Env.config.CircuitBreakerActorSupervisor),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         // Consume EmailMessages off of RabbitMq
         .WithSingleton<ActorMarker.Email>(
            ActorMetadata.email.Name,
            (fun system _ _ ->
               EmailServiceActor.initProps
                  (provider.GetRequiredService<OrgSettingsCache>())
                  (EnvNotifications.config.circuitBreaker system)
                  (getBroadcaster provider)
                  (getQueueConnection provider)
                  EnvNotifications.config.Queue
                  Env.config.QueueConsumerStreamBackoffRestart
                  EnvNotifications.config.EmailBearerToken
                  (getActorRegistry provider)
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         // Forward Email messages from web node to EmailProducer actors.
         .WithSingleton<ActorMarker.EmailProxy>(
            ActorMetadata.emailProxy.Name,
            (fun _ _ _ ->
               let registry: IEmailActor = getActorRegistry provider

               (fun (msg: EmailMessage) ->
                  registry.EmailActor() <<! msg
                  ignored ())
               |> actorOf
               |> props
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithActors(fun system registry ->
            registry.Register<ActorMarker.BillingStatement>(
               BillingStatementActor.start
                  system
                  Env.config.BillingStatementPersistenceChunking
                  Env.config.BillingStatementPersistenceBackoffRestart
                  Env.config.BillingStatementRetryPersistenceAfter
                  (getActorRegistry provider)
               |> untyped
            )

            // OrgOnboarding saga will send a know-your-customer verification
            // message to this actor which will enqueue the message into RabbitMq
            // for the KnowYourCustomerService singleton actor to process.
            registry.Register<ActorMarker.KYCServiceProducer>(
               Lib.Queue.startProducer<KYCMessage>
                  system
                  (getQueueConnection provider)
                  EnvOrg.config.KnowYourCustomerServiceQueue
                  Env.config.QueueConsumerStreamBackoffRestart
               |> untyped
            )

            // Enqueues messages into RabbitMq for the PartnerBankService
            // singleton actor to process.
            registry.Register<ActorMarker.PartnerBankServiceProducer>(
               Lib.Queue.startProducer<PartnerBankServiceMessage>
                  system
                  (getQueueConnection provider)
                  EnvPartnerBank.config.Queue
                  Env.config.QueueConsumerStreamBackoffRestart
               |> untyped
            )

            // Enqueues messages into RabbitMq for the CardIssuerService
            // singleton actor to process.
            registry.Register<ActorMarker.CardIssuerServiceProducer>(
               Lib.Queue.startProducer<CardIssuerMessage>
                  system
                  (getQueueConnection provider)
                  EnvEmployee.config.CardIssuerServiceQueue
                  Env.config.QueueConsumerStreamBackoffRestart
               |> untyped
            )

            // Other actors in the system send EmailMessages to this actor
            // which will enqueue the message into RabbitMq for the
            // EmailService Singleton Actor to process.
            registry.Register<ActorMarker.EmailProducer>(
               Lib.Queue.startProducer<EmailMessage>
                  system
                  (getQueueConnection provider)
                  EnvNotifications.config.Queue
                  Env.config.QueueConsumerStreamBackoffRestart
               |> untyped
            )

            registry.Register<ActorMarker.SagaGuaranteedDeliveryProducer>(
               GuaranteedDelivery.producer<AppSaga.AppSagaMessage> {
                  System = system
                  ShardRegion = registry.Get<ActorMarker.Saga>()
                  ProducerName = "bank-to-saga-actor"
               }
               |> untyped
            )

            registry.Register<ActorMarker.OrgGuaranteedDeliveryProducer>(
               GuaranteedDelivery.producer<OrgMessage> {
                  System = system
                  ShardRegion = registry.Get<ActorMarker.Org>()
                  ProducerName = "bank-to-org-actor"
               }
               |> untyped
            )

            registry.Register<ActorMarker.AccountGuaranteedDeliveryProducer>(
               GuaranteedDelivery.producer<AccountMessage> {
                  System = system
                  ShardRegion = registry.Get<ActorMarker.Account>()
                  ProducerName = "bank-to-account-actor"
               }
               |> untyped
            )

            registry.Register<ActorMarker.EmployeeGuaranteedDeliveryProducer>(
               GuaranteedDelivery.producer<EmployeeMessage> {
                  System = system
                  ShardRegion = registry.Get<ActorMarker.Employee>()
                  ProducerName = "bank-to-employee-actor"
               }
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
