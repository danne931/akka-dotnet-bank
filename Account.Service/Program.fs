open Microsoft.Extensions.DependencyInjection
open Akka.Actor
open Akka.Event
open Akka.Hosting
open Akka.Cluster.Hosting
open Akka.Persistence
open Akka.Persistence.Hosting
open Akka.Persistence.Sql.Hosting
open Akkling

open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open PartnerBank.Service.Domain
open CardIssuer.Service.Domain
open Bank.Infrastructure
open ActorUtil
open SignalRBroadcast
open TransferMessages

let builder = Env.builder

LogInfra.start builder |> ignore

builder.Services.AddSingleton<SignalRBroadcast>(fun provider ->
   let system = provider.GetRequiredService<ActorSystem>()
   SignalRBroadcaster.init system)
|> ignore

builder.Services.AddSingleton<Lib.Queue.QueueConnectionDetails>(fun _ ->
   Lib.Queue.createConnection Env.config.QueueConnection)
|> ignore

let private getQueueConnection (provider: System.IServiceProvider) =
   provider.GetRequiredService<Lib.Queue.QueueConnectionDetails>()

let journalOpts = AkkaInfra.getJournalOpts ()

journalOpts.Adapters
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

let snapshotOpts = AkkaInfra.getSnapshotOpts ()

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
            fun system -> BankSerializer system
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

               GuaranteedDelivery.consumer<OrgMessage>
                  system
                  (fun controllerRef ->
                     OrgActor.initProps
                        (provider.GetRequiredService<SignalRBroadcast>())
                        // TODO: Create org-specific Environment file & replace
                        //       account env var here.
                        Env.config.AccountActorSupervisor
                        persistenceId
                        (AppSaga.getEntityRef system)
                        (fun () ->
                           AppSaga.getGuaranteedDeliveryProducerRef system)
                        (fun () ->
                           AccountActor.getGuaranteedDeliveryProducerRef
                              system)
                        (fun () ->
                           EmployeeActor.getGuaranteedDeliveryProducerRef
                              system)
                        (typed controllerRef))),
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

               GuaranteedDelivery.consumer<AccountMessage>
                  system
                  (fun controllerRef ->
                     AccountActor.initProps
                        (provider.GetRequiredService<SignalRBroadcast>())
                        Env.config.AccountActorSupervisor
                        persistenceId
                        BillingStatementActor.get
                        Bank.Transfer.Api.getDomesticTransfersRetryableUponRecipientCorrection
                        (AppSaga.getEntityRef system)
                        (fun () ->
                           AppSaga.getGuaranteedDeliveryProducerRef system)
                        EmailServiceActor.getProducer
                        AccountClosureActor.get
                        (typed controllerRef))),
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

               GuaranteedDelivery.consumer<EmployeeMessage>
                  system
                  (fun controllerRef ->
                     EmployeeActor.initProps
                        // TODO: Create employee-specific Environment file & replace
                        //       account env var here.
                        Env.config.AccountActorSupervisor
                        persistenceId
                        (provider.GetRequiredService<SignalRBroadcast>())
                        (AppSaga.getEntityRef system)
                        (fun () ->
                           AppSaga.getGuaranteedDeliveryProducerRef system)
                        (typed controllerRef))),
            ClusterMetadata.employeeShardRegion.messageExtractor,
            ShardOptions(Role = ClusterMetadata.roles.employee)
         )
         .WithShardRegion<ActorMetadata.SagaMarker>(
            ClusterMetadata.sagaShardRegion.name,
            (fun persistenceId ->
               let system = provider.GetRequiredService<ActorSystem>()

               GuaranteedDelivery.consumer<AppSaga.AppSagaMessage>
                  system
                  (fun controllerRef ->
                     AppSaga.initProps
                        (OrgActor.get system)
                        (EmployeeActor.get system)
                        (AccountActor.get system)
                        EmailServiceActor.getProducer
                        DomesticTransferServiceActor.getProducer
                        SchedulingActor.get
                        KnowYourCustomerServiceActor.getProducer
                        PartnerBankServiceActor.getProducer
                        CardIssuerServiceActor.getProducer
                        Env.config.AccountActorSupervisor
                        Env.config.SagaPassivateIdleEntityAfter
                        persistenceId
                        (Some(typed controllerRef)))),
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
         .WithSingleton<ActorMetadata.KYCServiceMarker>(
            ActorMetadata.knowYourCustomer.Name,
            (fun system _ _ ->
               KnowYourCustomerServiceActor.initProps
                  (getQueueConnection provider)
                  EnvOrg.config.KnowYourCustomerServiceQueue
                  Env.config.QueueConsumerStreamBackoffRestart
                  (EnvOrg.config.KnowYourCustomerServiceCircuitBreaker system)
                  (provider.GetRequiredService<SignalRBroadcast>())
                  (AppSaga.getEntityRef system)
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.org)
         )
         // The PartnerBankService will consume messages from
         // from RabbitMq & interact with a third party partner
         // bank API to sync transactions occuring on the platform.
         // It will notify relevant saga actors of syncing progress.
         .WithSingleton<ActorMetadata.PartnerBankServiceMarker>(
            ActorMetadata.partnerBankService.Name,
            (fun system _ _ ->
               PartnerBankServiceActor.initProps
                  (getQueueConnection provider)
                  Env.config.PartnerBankServiceQueue
                  Env.config.QueueConsumerStreamBackoffRestart
                  (Env.config.PartnerBankServiceCircuitBreaker system)
                  (provider.GetRequiredService<SignalRBroadcast>())
                  (AppSaga.getEntityRef system)
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMetadata.CardIssuerServiceMarker>(
            ActorMetadata.cardIssuerService.Name,
            (fun system _ _ ->
               CardIssuerServiceActor.initProps
                  (getQueueConnection provider)
                  EnvEmployee.config.CardIssuerServiceQueue
                  Env.config.QueueConsumerStreamBackoffRestart
                  (EnvEmployee.config.cardIssuerServiceCircuitBreaker system)
                  (provider.GetRequiredService<SignalRBroadcast>())
                  (AppSaga.getEntityRef system)
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.employee)
         )
         .WithSingleton<ActorMetadata.SagaAlarmClockMarker>(
            ActorMetadata.sagaAlarmClock.Name,
            (fun system _ _ ->
               SagaAlarmClockActor.initProps
                  system
                  (AppSaga.getEntityRef system)
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.saga)
         )
         .WithSingleton<ActorMetadata.BillingCycleMarker>(
            ActorMetadata.billingCycle.Name,
            (fun system _ resolver ->
               let typedProps =
                  BillingCycleActor.actorProps
                     Env.config.BillingCycleFanoutThrottle
                     (fun () ->
                        AppSaga.getGuaranteedDeliveryProducerRef system)

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
                     EnvTransfer.config.AutoTransferComputeThrottle

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMetadata.ScheduledTransfersLowBalanceWarningMarker>(
            ActorMetadata.scheduledTransfersLowBalanceWarning.Name,
            (fun system _ _ ->
               let typedProps =
                  ScheduledTransfersLowBalanceWarningActor.initProps
                     system
                     (fun () -> EmailServiceActor.getProducer system)

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         .WithSingleton<ActorMetadata.AccountClosureMarker>(
            ActorMetadata.accountClosure.Name,
            (fun system _ _ ->
               let typedProps =
                  AccountClosureActor.initProps
                     (AccountActor.get system)
                     EmailServiceActor.getProducer
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
                     // TODO: Create employee-specific Environment file & replace
                     //       account env var here.
                     Env.config.AccountEventProjectionChunking
                     Env.config.AccountEventReadModelPersistenceBackoffRestart
                     Env.config.AccountEventReadModelRetryPersistenceAfter

               typedProps.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.employee)
         )
         .WithSingleton<ActorMetadata.SagaReadModelSyncMarker>(
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
                  (fun () -> OrgActor.getGuaranteedDeliveryProducerRef system)
                  (fun () ->
                     AccountActor.getGuaranteedDeliveryProducerRef system)
                  (fun () ->
                     EmployeeActor.getGuaranteedDeliveryProducerRef system)
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
         .WithSingleton<ActorMetadata.EmailMarker>(
            ActorMetadata.email.Name,
            (fun system _ _ ->
               EmailServiceActor.initProps
                  (EnvNotifications.config.circuitBreaker system)
                  (provider.GetRequiredService<SignalRBroadcast>())
                  (getQueueConnection provider)
                  EnvNotifications.config.Queue
                  Env.config.QueueConsumerStreamBackoffRestart
                  EnvNotifications.config.EmailBearerToken
                  (AppSaga.getEntityRef system)
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         // Forward Email messages from web node to EmailProducer actors.
         .WithSingleton<ActorMetadata.EmailProxyMarker>(
            ActorMetadata.emailProxy.Name,
            (fun system _ _ ->
               (fun (msg: Email.EmailMessage) ->
                  EmailServiceActor.getProducer system <<! msg
                  ignored ())
               |> actorOf
               |> props
               |> _.ToProps()),
            ClusterSingletonOptions(Role = ClusterMetadata.roles.account)
         )
         // Consume DomesticTransferMessages off of RabbitMq
         .WithSingleton<ActorMetadata.DomesticTransferMarker>(
            ActorMetadata.domesticTransfer.Name,
            (fun system _ _ ->
               DomesticTransferServiceActor.initProps
                  (getQueueConnection provider)
                  EnvTransfer.config.Queue
                  Env.config.QueueConsumerStreamBackoffRestart
                  (EnvTransfer.config.domesticTransferCircuitBreaker system)
                  (provider.GetRequiredService<SignalRBroadcast>())
                  (AppSaga.getEntityRef system)
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
                  (fun () -> AppSaga.getGuaranteedDeliveryProducerRef system)
               |> untyped
            )

            // OrgOnboarding saga will send a know-your-customer verification
            // message to this actor which will enqueue the message into RabbitMq
            // for the KnowYourCustomerService singleton actor to process.
            registry.Register<ActorMetadata.KYCServiceProducerMarker>(
               Lib.Queue.startProducer<KYCMessage>
                  system
                  (getQueueConnection provider)
                  EnvOrg.config.KnowYourCustomerServiceQueue
                  Env.config.QueueConsumerStreamBackoffRestart
               |> untyped
            )

            // Enqueues messages into RabbitMq for the PartnerBankService
            // singleton actor to process.
            registry.Register<ActorMetadata.PartnerBankServiceProducerMarker>(
               Lib.Queue.startProducer<PartnerBankServiceMessage>
                  system
                  (getQueueConnection provider)
                  Env.config.PartnerBankServiceQueue
                  Env.config.QueueConsumerStreamBackoffRestart
               |> untyped
            )

            // Enqueues messages into RabbitMq for the CardIssuerService
            // singleton actor to process.
            registry.Register<ActorMetadata.CardIssuerServiceProducerMarker>(
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
            registry.Register<ActorMetadata.EmailProducerMarker>(
               Lib.Queue.startProducer<Email.EmailMessage>
                  system
                  (getQueueConnection provider)
                  EnvNotifications.config.Queue
                  Env.config.QueueConsumerStreamBackoffRestart
               |> untyped
            )

            // Other actors in the system send DomesticTransferMessages to this actor
            // which will enqueue the message into RabbitMq for the
            // DomesticTransferService Singleton Actor to process.
            registry.Register<ActorMetadata.DomesticTransferProducerMarker>(
               Lib.Queue.startProducer<DomesticTransferServiceMessage>
                  system
                  (getQueueConnection provider)
                  EnvTransfer.config.Queue
                  Env.config.QueueConsumerStreamBackoffRestart
               |> untyped
            )

            registry.Register<
               ActorMetadata.SagaGuaranteedDeliveryProducerMarker
             >(
               GuaranteedDelivery.producer<AppSaga.AppSagaMessage> {
                  System = system
                  ShardRegion = registry.Get<ActorMetadata.SagaMarker>()
                  ProducerName = "bank-to-saga-actor"
               }
               |> untyped
            )

            registry.Register<
               ActorMetadata.OrgGuaranteedDeliveryProducerMarker
             >(
               GuaranteedDelivery.producer<OrgMessage> {
                  System = system
                  ShardRegion = registry.Get<ActorMetadata.OrgMarker>()
                  ProducerName = "bank-to-org-actor"
               }
               |> untyped
            )

            registry.Register<
               ActorMetadata.AccountGuaranteedDeliveryProducerMarker
             >(
               GuaranteedDelivery.producer<AccountMessage> {
                  System = system
                  ShardRegion = registry.Get<ActorMetadata.AccountMarker>()
                  ProducerName = "bank-to-account-actor"
               }
               |> untyped
            )

            registry.Register<
               ActorMetadata.EmployeeGuaranteedDeliveryProducerMarker
             >(
               GuaranteedDelivery.producer<EmployeeMessage> {
                  System = system
                  ShardRegion = registry.Get<ActorMetadata.EmployeeMarker>()
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
