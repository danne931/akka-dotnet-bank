module ActorUtil

open System
open System.Threading.Tasks
open Akkling
open Akkling.Cluster.Sharding
open Akkling.Persistence
open Akka.Streams
open Akkling.Streams
open Akka.Actor
open Akka.Cluster.Sharding
open Akka.Persistence
open Akka.Persistence.Query
open Akka.Persistence.Extras
open Akka.Persistence.Sql.Query
open Akka.Cluster.Tools.PublishSubscribe

open Lib.Types

module SystemLog =
   let info (sys: ActorSystem) (msg: string) =
      sys.Log.Log(Akka.Event.LogLevel.InfoLevel, null, msg)

   let warning (sys: ActorSystem) (msg: string) =
      sys.Log.Log(Akka.Event.LogLevel.WarningLevel, null, msg)

   let error (sys: ActorSystem) (err: exn) =
      sys.Log.Log(Akka.Event.LogLevel.ErrorLevel, err, err.Message)

module PubSub =
   let get (system: ActorSystem) : DistributedPubSub =
      DistributedPubSub.Get(system)

   /// Point-to-point mode where one actor ref is subscribed
   /// to receive messages sent over the mediator.
   let subscribePointToPoint (pubSub: DistributedPubSub) (aref: IActorRef<_>) =
      pubSub.Mediator.Tell(Put(untyped aref))

   /// Point-to-point mode where each message is delivered to
   /// one destination actor ref by actor path.
   let sendPointToPoint
      (pubSub: DistributedPubSub)
      (destinationPath: string)
      (msg: obj)
      =
      pubSub.Mediator.Tell(Send(destinationPath, msg))

   // Subscribe an actor ref to a topic to receive all messages sent
   // for a particular topic.
   let subscribeToTopic
      (pubSub: DistributedPubSub)
      (mailbox: Actor<_>)
      (topic: string)
      =
      pubSub.Mediator.Tell(Subscribe(topic, untyped mailbox.Self))

   /// Publish a message to all subscribers of a topic.
   let publishToTopic (pubSub: DistributedPubSub) (topic: string) (msg: obj) =
      pubSub.Mediator.Tell(Publish(topic, msg))

module CRDT =
   /// Include this in the key you create for adding
   /// CRDTs via Akka.DistributedData if you want to
   /// ensure they are persisted.  Any data without
   /// this key will not exist after cluster restart.
   let PersistableKey = "durable"

let getChildActorRef<'t, 'r>
   (actorCtx: Actor<'t>)
   (name: string)
   : IActorRef<'r> option
   =
   let accountRef = actorCtx.UntypedContext.Child name

   match accountRef = ActorRefs.Nobody with
   | true -> None
   | false -> Some(typed accountRef)

// Recommended to have ~10 shards per node. If 4 nodes are deployed in
// K8s then number of shards should be ~40.
let messageExtractor (maxNumberOfShards: int) =
   HashCodeMessageExtractor.Create(
      maxNumberOfShards = maxNumberOfShards,
      entityIdExtractor =
         (fun msg ->
            match msg with
            | :? ShardEnvelope as e -> e.EntityId
            | _ -> null),
      messageExtractor =
         fun msg ->
            match msg with
            | :? ShardEnvelope as e -> e.Message
            | _ -> msg
   )

module ClusterMetadata =
   type ShardRegion = {
      name: string
      messageExtractor: IMessageExtractor
   }

   let orgShardRegion = {
      name = "org"
      messageExtractor =
         // TODO: Create separate config const for org number of shards
         messageExtractor Env.config.AccountCluster.NumberOfShards
   }

   let accountShardRegion = {
      name = "account"
      messageExtractor =
         messageExtractor Env.config.AccountCluster.NumberOfShards
   }

   let employeeShardRegion = {
      name = "employee"
      messageExtractor =
         // TODO: Create separate config const for employee number of shards
         messageExtractor Env.config.AccountCluster.NumberOfShards
   }

   let sagaShardRegion = {
      name = "saga"
      messageExtractor =
         // TODO: Create separate config const for saga number of shards
         messageExtractor Env.config.AccountCluster.NumberOfShards
   }

   let roles = {|
      web = "web-role"
      org = "org-role"
      account = "account-role"
      signalR = "signal-r-role"
      scheduling = "scheduling-role"
      employee = "employee-role"
      saga = "saga-role"
      crdt = "crdt-role"
   |}

module ActorMetadata =
   type ActorMetadata = {
      Name: string
      Route: string
   } with

      member x.Path = ActorPath.Parse $"akka://bank/user/{x.Route}"

      member x.SingletonPath =
         ActorPath.Parse $"akka://bank/user/{x.Route}/{x.Route}"

      member x.ProxyPath = ActorPath.Parse $"akka://bank/user/{x.Route}-proxy"

   type ChildEntityActorMetadata = {
      Name: string
      RouteBuilder: string -> string
   } with

      member x.Path(entityId: string) =
         ActorPath.Parse $"akka://bank/user/{x.RouteBuilder entityId}"

   let org = { Name = "org"; Route = "org" }

   let orgReadModelSync = {
      Name = "org-read-model-sync"
      Route = "org-read-model-sync"
   }

   let partnerBankService = {
      Name = "partner-bank-service-consumer"
      Route = "partner-bank-service-consumer"
   }

   let partnerBankServiceProducer = {
      Name = "partner-bank-service-producer"
      Route = "partner-bank-service-producer"
   }

   let knowYourCustomer = {
      Name = "know-your-customer-consumer"
      Route = "know-your-customer-consumer"
   }

   let knowYourCustomerProducer = {
      Name = "know-your-customer-producer"
      Route = "know-your-customer-producer"
   }

   let cardIssuerService = {
      Name = "card-issuer-service-consumer"
      Route = "card-issuer-service-consumer"
   }

   let cardIssuerServiceProducer = {
      Name = "card-issuer-service-producer"
      Route = "card-issuer-service-producer"
   }

   let account = { Name = "account"; Route = "account" }

   let accountClosure = {
      Name = "account-closure"
      Route = "account-closure"
   }

   let accountReadModelSync = {
      Name = "account-read-model-sync"
      Route = "account-read-model-sync"
   }

   let employee = {
      Name = "employee"
      Route = "employee"
   }

   let employeeReadModelSync = {
      Name = "employee-read-model-sync"
      Route = "employee-read-model-sync"
   }

   let sagaReadModelSync = {
      Name = "saga-read-model-sync"
      Route = "saga-read-model-sync"
   }

   let sagaAlarmClock = {
      Name = "saga-alarm-clock"
      Route = "saga-alarm-clock"
   }

   let autoTransferScheduling = {
      Name = "auto-transfer-scheduling"
      Route = "auto-transfer-scheduling"
   }

   let scheduledTransfersLowBalanceWarning = {
      Name = "scheduled-transfers-low-balance-warning"
      Route = "scheduled-transfers-low-balance-warning"
   }

   let billingCycle = {
      Name = "billing-cycle"
      Route = "billing-cycle"
   }

   let billingStatement = {
      Name = "billing-statement"
      Route = "billing-statement"
   }

   let email = {
      Name = "email-consumer"
      Route = "email-consumer"
   }

   let emailProducer = {
      Name = "email-producer"
      Route = "email-producer"
   }

   let emailProxy = {
      Name = "email-proxy"
      Route = "email-proxy"
   }

   let auditor = { Name = "auditor"; Route = "auditor" }

   let accountSeeder = {
      Name = "account-seeder"
      Route = "account-seeder"
   }

   let accountLoadTest = {
      Name = "account-load-test"
      Route = "account-load-test"
   }

   let signalR = {
      Name = "signal-r"
      Route = "signal-r"
   }

   let scheduling = {
      Name = "scheduling"
      Route = "scheduling"
   }

   let circuitBreaker = {
      Name = "circuit-breaker"
      Route = "circuit-breaker"
   }

let readJournal (system: ActorSystem) : SqlReadJournal =
   PersistenceQuery
      .Get(system)
      .ReadJournalFor<SqlReadJournal>(SqlReadJournal.Identifier)

let getEntityRef
   system
   (shardRegionMeta: ClusterMetadata.ShardRegion)
   (entityId: Guid)
   : IEntityRef<'t>
   =
   let fac = {
      TypeName = shardRegionMeta.name
      ShardRegion =
         ClusterSharding.Get(system).ShardRegion(shardRegionMeta.name)
   }

   let shardId = shardRegionMeta.messageExtractor.ShardId(string entityId)

   fac.RefFor shardId <| string entityId

module PersistentActorEventHandler =
   type T = {
      LifecyclePreStart: Eventsourced<obj> -> Effect<obj>
      LifecyclePostStop: Eventsourced<obj> -> Effect<obj>
      LifecyclePreRestart: Eventsourced<obj> -> exn -> obj -> Effect<obj>
      LifecyclePostRestart: Eventsourced<obj> -> exn -> Effect<obj>
      RecoveryCompleted: Eventsourced<obj> -> Effect<obj>
      ReplaySucceed: Eventsourced<obj> -> Effect<obj>
      // After failure handler invoked, actor is stopped
      ReplayFailed: Eventsourced<obj> -> exn -> obj -> Effect<obj>
      // After rejection handler invoked, no automatic actions
      PersistRejected: Eventsourced<obj> -> exn -> obj -> int64 -> Effect<obj>
      // After failure handler invoked, actor is stopped
      PersistFailed: Eventsourced<obj> -> exn -> obj -> int64 -> Effect<obj>
      DeleteMessagesSuccess: Eventsourced<obj> -> Effect<obj>
      // After failure handler invoked, no automatic actions
      DeleteMessagesFailure:
         Eventsourced<obj> -> DeleteMessagesFailure -> Effect<obj>
      SaveSnapshotSuccess:
         Eventsourced<obj> -> SaveSnapshotSuccess -> Effect<obj>
      SaveSnapshotFailure:
         Eventsourced<obj> -> SaveSnapshotFailure -> Effect<obj>
      DeleteSnapshotSuccess: Eventsourced<obj> -> Effect<obj>
      DeleteSnapshotFailure:
         Eventsourced<obj> -> DeleteSnapshotFailure -> Effect<obj>
      DeleteSnapshotsSuccess: Eventsourced<obj> -> Effect<obj>
      DeleteSnapshotsFailure:
         Eventsourced<obj> -> DeleteSnapshotsFailure -> Effect<obj>
   }

   let init = {
      LifecyclePreStart = fun _ -> ignored ()
      LifecyclePostStop = fun _ -> ignored ()
      LifecyclePreRestart = fun _ _ _ -> ignored ()
      LifecyclePostRestart = fun _ _ -> ignored ()
      RecoveryCompleted = fun _ -> ignored ()
      ReplaySucceed = fun _ -> ignored ()
      ReplayFailed = fun _ _ _ -> ignored ()
      PersistRejected = fun _ _ _ _ -> ignored ()
      PersistFailed = fun _ _ _ _ -> ignored ()
      DeleteMessagesSuccess = fun _ -> ignored ()
      DeleteMessagesFailure = fun _ _ -> ignored ()
      SaveSnapshotSuccess = fun _ _ -> ignored ()
      SaveSnapshotFailure = fun _ _ -> ignored ()
      DeleteSnapshotSuccess = fun _ -> ignored ()
      DeleteSnapshotFailure = fun _ _ -> ignored ()
      DeleteSnapshotsSuccess = fun _ -> ignored ()
      DeleteSnapshotsFailure = fun _ _ -> ignored ()
   }

   let handleEvent
      (handler: T)
      (mailbox: Eventsourced<obj>)
      (msg: obj)
      : Effect<obj>
      =
      match msg with
      | LifecycleEvent e ->
         match e with
         | PreStart ->
            logDebug mailbox "<PreStart>"
            handler.LifecyclePreStart mailbox
         | PostStop ->
            logDebug mailbox "<PostStop>"
            handler.LifecyclePostStop mailbox
         | PreRestart(err: exn, msg: obj) ->
            logDebug mailbox $"<PreRestart>: {err} - {msg}"
            handler.LifecyclePreRestart mailbox err msg
         | PostRestart(err: exn) ->
            logDebug mailbox $"<PostRestart>: {err}"
            handler.LifecyclePostRestart mailbox err
      | :? Akkling.Persistence.PersistentLifecycleEvent as evt ->
         match evt with
         | ReplaySucceed ->
            logDebug mailbox "<ReplaySucceed>"
            handler.ReplaySucceed mailbox
         | ReplayFailed(err: exn, msg: obj) ->
            logError mailbox $"<ReplayFailed>: %s{err.Message} - {msg}"
            handler.ReplayFailed mailbox err msg
         | PersistRejected(err: exn, evt: obj, sequenceNr: int64) ->
            logError
               mailbox
               $"<PersistRejected>: %s{err.Message} - Sequence #{sequenceNr} - {evt}"

            handler.PersistRejected mailbox err evt sequenceNr
         | PersistFailed(err: exn, evt: obj, sequenceNr: int64) ->
            logError
               mailbox
               $"<PersistFailed>: %s{err.Message} - Sequence #{sequenceNr} - {evt}"

            handler.PersistFailed mailbox err evt sequenceNr
      | :? Akka.Persistence.RecoveryCompleted ->
         logDebug mailbox "<RecoveryCompleted>"
         handler.RecoveryCompleted mailbox
      | :? DeleteMessagesSuccess ->
         logDebug mailbox "<DeleteMessagesSuccess>"
         handler.DeleteMessagesSuccess mailbox
      | :? DeleteMessagesFailure as err ->
         logError mailbox $"<DeleteMessagesFailure>: {err.Cause}"
         handler.DeleteMessagesFailure mailbox err
      | :? SaveSnapshotSuccess as res ->
         logDebug mailbox $"<SaveSnapshotSuccess>: {res.Metadata}"
         handler.SaveSnapshotSuccess mailbox res
      | :? SaveSnapshotFailure as err ->
         logError mailbox $"<SaveSnapshotFailure>: {err.Metadata}"
         handler.SaveSnapshotFailure mailbox err
      | :? DeleteSnapshotSuccess ->
         logDebug mailbox "<DeleteSnapshotSuccess>"
         handler.DeleteSnapshotSuccess mailbox
      | :? DeleteSnapshotFailure as err ->
         logError mailbox $"<DeleteSnapshotFailure>: {err.Cause}"
         handler.DeleteSnapshotFailure mailbox err
      | :? DeleteSnapshotsSuccess ->
         logDebug mailbox "<DeleteSnapshotsSuccess>"
         handler.DeleteSnapshotsSuccess mailbox
      | :? DeleteSnapshotsFailure as err ->
         logError mailbox $"<DeleteSnapshotsFailure>: {err.Cause}"
         handler.DeleteSnapshotsFailure mailbox err
      | msg ->
         logError mailbox $"<UnknownMessage>: %s{msg.GetType().FullName}"
         unhandled ()

/// <summary>
/// Add an item to an Akka.Streams queue.
/// </summary>
let queueOffer<'t>
   (queue: ISourceQueueWithComplete<'t>)
   (msg: 't)
   : Result<Effect<obj>, string> Task
   =
   task {
      let! result = queue.AsyncOffer msg

      return
         match result with
         | :? QueueOfferResult.Dropped -> Error "Message dropped"
         | :? QueueOfferResult.Failure as f ->
            Error $"Failed with exception: {f.Cause}"
         | :? QueueOfferResult.QueueClosed -> Error "Queue closed"
         | :? QueueOfferResult.Enqueued -> Ok <| ignored ()
         | _ -> Ok <| unhandled ()
   }

/// <summary>
/// Create props for a parent supervisor actor (which just forwards messages to
/// child) so can configure non-default SupervisorStrategy for top-level actors.
/// </summary>
let supervisorProps
   (spawnChild: Actor<_> -> IActorRef<_>)
   (strategy: Akka.Actor.SupervisorStrategy)
   =
   let init (ctx: Actor<_>) =
      let child = spawnChild ctx

      actor {
         let! msg = ctx.Receive()
         child <<! msg
         return ignored ()
      }

   {
      props init with
         SupervisionStrategy = Some strategy
   }

module PersistenceSupervisor =
   /// If using a PersistenceSupervisor between a
   /// AtLeastOnceDelivery ShardingConsumerController and a sharded
   /// entity actor we must explicitly forward the Passivate message
   /// to the parent ShardingConsumerController for
   /// passivation to work.
   type PersistenceSupervisorCompatibleWithGuaranteedDelivery
      (
         childProps: Props,
         persistenceId: string,
         config: PersistenceSupervisionConfig,
         strategy: SupervisorStrategy
      )
      =
      inherit PersistenceSupervisor(childProps, persistenceId, config, strategy)

      override _.Receive(message: obj) =
         match message with
         | :? Akka.Cluster.Sharding.Passivate as passivate ->
            PersistenceSupervisor.Context.Parent.Forward passivate
            true
         | _ -> base.Receive message

   type PersistenceSupervisorOptions = {
      EnvConfig: PersistenceSupervisorEnvConfig
      IsPersistableMessage: obj -> bool
      ChildProps: Props
      PersistenceId: string
      CompatibleWithGuaranteedDelivery: bool
   }

   /// <summary>
   /// Create PersistenceSupervisor props to ensure
   /// failed Persist calls are retried and messages received
   /// during backoff period are not lost.
   /// See https://devops.petabridge.com/articles/state-management/akkadotnet-persistence-failure-handling.html
   /// for info on why PersistenceSupervisor may be preferred over
   /// BackoffSupervisor in persistent actor scenarios.
   /// </summary>
   let create (opts: PersistenceSupervisorOptions) =
      let config =
         PersistenceSupervisionConfig(
            opts.IsPersistableMessage,
            (fun msg confirmationId ->
               ConfirmableMessageEnvelope(confirmationId, "", msg)),
            minBackoff = opts.EnvConfig.MinBackoff,
            maxBackoff = opts.EnvConfig.MaxBackoff
         )

      let strategy =
         SupervisorStrategy.StoppingStrategy.WithMaxNrOfRetries
            opts.EnvConfig.MaxNrOfRetries

      if opts.CompatibleWithGuaranteedDelivery then
         Props.Create(fun () ->
            new PersistenceSupervisorCompatibleWithGuaranteedDelivery(
               opts.ChildProps,
               opts.PersistenceId,
               config,
               strategy = strategy
            ))
      else
         Props.Create(fun () ->
            new PersistenceSupervisor(
               opts.ChildProps,
               opts.PersistenceId,
               config,
               strategy = strategy
            ))

   /// <summary>
   /// Durable persist with ack sent to PersistenceSupervisor parent actor.
   /// A command arrives to the actor and we wish to persist the
   /// subsequent event.
   /// </summary>
   let confirmPersist
      (ctx: Eventsourced<obj>)
      (confirmationId: int64)
      (evt: obj)
      =
      evt
      |> PersistentEffect.Persist
      |> Effects.andThen (fun () ->
         ctx.Parent() <! Confirmation(confirmationId, ctx.Pid))

   /// <summary>
   /// Durable PersistAll with ack sent to PersistenceSupervisor parent actor.
   /// A command arrives to the actor and produces several events for
   /// that single command.  The group of events is persisted atomically.
   /// </summary>
   let confirmPersistAll
      (ctx: Eventsourced<obj>)
      (confirmationId: int64)
      (evts: obj seq)
      =
      // NOTE:
      // Confirmation is expected for the single command that arrived.
      // Confirm once for the group of events we are persisting to
      // avoid producing a persistent effect for each event in the list.
      // Fixes warning log "Received confirmation for unknown event."
      let mutable confirmed = false

      evts
      |> PersistentEffect.PersistAll
      |> Effects.andThen (fun () ->
         if not confirmed then
            confirmed <- true
            ctx.Parent() <! Confirmation(confirmationId, ctx.Pid))
