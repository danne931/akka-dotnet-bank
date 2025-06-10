[<RequireQualifiedAccess>]
module SchedulingActor

open Akka.Hosting
open Akka.Actor
open Akka.Quartz.Actor.Commands
open Akka.Cluster.Sharding
open Akkling.Cluster.Sharding
open Akkling
open Quartz

open Bank.Account.Domain
open Bank.Transfer.Domain
open BillingStatement
open ActorUtil
open Lib.SharedTypes
open Lib.Postgres
open Lib.Saga
open Bank.Scheduler

let actorProps
   (quartzPersistentActorRef: IActorRef)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   =
   let handler (ctx: Actor<SchedulerMessage>) = actor {
      let logInfo = logInfo ctx
      let! msg = ctx.Receive()

      match msg with
      | AccountClosureCronJobSchedule ->
         logInfo $"Scheduling nightly account closure checker"

         let trigger = AccountClosureTriggers.scheduleNightlyCheck logInfo
         let path = ActorMetadata.accountClosure.ProxyPath

         let job =
            CreatePersistentJob(
               path,
               {
                  Manifest = "AccountClosureActorMessage"
                  Message = AccountClosureMessage.ScheduleDeleteAll
               },
               trigger
            )

         quartzPersistentActorRef.Tell(job, ActorRefs.Nobody)
         return ignored ()
      | BillingCycleCronJobSchedule ->
         logInfo $"Scheduling monthly billing cycle"

         let trigger = BillingCycleTriggers.scheduleMonthly logInfo
         let path = ActorMetadata.billingCycle.ProxyPath

         let job =
            CreatePersistentJob(
               path,
               {
                  Manifest = "BillingCycleActorMessage"
                  Message = BillingCycleMessage.BillingCycleFanout
               },
               trigger
            )

         quartzPersistentActorRef.Tell(job, ActorRefs.Nobody)
         return ignored ()
      | BalanceManagementCronJobSchedule ->
         logInfo $"Scheduling balance management cron jobs"

         let path = ActorMetadata.autoTransferScheduling.ProxyPath
         let trigger = BalanceManagementTriggers.schedule logInfo

         let dailyJob =
            let schedule = AutomaticTransfer.CronSchedule.Daily

            CreatePersistentJob(
               path,
               {
                  Manifest = "BalanceManagementMessage"
                  Message =
                     AutomaticTransfer.Message.StartScheduledAutoTransfers
                        schedule
               },
               (trigger schedule)
            )

         let twiceMonthlyJob =
            let schedule = AutomaticTransfer.CronSchedule.TwiceMonthly

            CreatePersistentJob(
               path,
               {
                  Manifest = "BalanceManagementMessage"
                  Message =
                     AutomaticTransfer.Message.StartScheduledAutoTransfers
                        schedule
               },
               (trigger schedule)
            )

         quartzPersistentActorRef.Tell(dailyJob, ActorRefs.Nobody)
         quartzPersistentActorRef.Tell(twiceMonthlyJob, ActorRefs.Nobody)

         return ignored ()
      | DeleteAccountsJobSchedule accountIds ->
         logInfo $"Scheduling deletion of accounts {accountIds}"

         let trigger = AccountClosureTriggers.deleteAccounts logInfo
         let path = ActorMetadata.accountClosure.ProxyPath

         let job =
            CreatePersistentJob(
               path,
               {
                  Manifest = "AccountClosureActorMessage"
                  Message = AccountClosureMessage.DeleteAll accountIds
               },
               trigger
            )

         quartzPersistentActorRef.Tell(job, ActorRefs.Nobody)
         return ignored ()
      | SagaAlarmClockCronJobSchedule ->
         logInfo $"Scheduling saga alarm clock."

         let name = "SagaAlarmClock"
         let group = "Bank"

         let builder =
            TriggerBuilder
               .Create()
               .ForJob($"{name}Job", group)
               .WithIdentity($"{name}Trigger", group)
               .WithDescription(
                  "Wake up sleeping txn sagas which have remaining activities."
               )

         let minutes = 1
         logInfo $"Scheduling txn saga alarm clock every {minutes} minutes."

         let trigger =
            builder
               .WithSimpleSchedule(fun s ->
                  s.WithIntervalInMinutes(minutes).RepeatForever() |> ignore)
               .Build()

         let path = ActorMetadata.sagaAlarmClock.ProxyPath

         let job =
            CreatePersistentJob(
               path,
               {
                  Manifest = "SagaAlarmClockActorMessage"
                  Message = SagaAlarmClockMessage.WakeUpIfUnfinishedBusiness
               },
               trigger
            )

         quartzPersistentActorRef.Tell(job, ActorRefs.Nobody)
         return ignored ()
      | BalanceHistoryCronJobSchedule ->
         logInfo "Scheduling daily balance history update."
         let trigger = BalanceHistoryTriggers.scheduleNightly logInfo
         let path = ctx.Self.Path

         let job =
            CreatePersistentJob(
               path,
               {
                  Manifest = "SchedulingActorMessage"
                  Message = SchedulerMessage.TriggerBalanceHistoryCronJob
               },
               trigger
            )

         quartzPersistentActorRef.Tell(job)
         return ignored ()
      | TriggerBalanceHistoryCronJob ->
         logInfo "Balance history daily update triggered."
         let! res = pgProcedure "update_balance_history_for_yesterday" None

         match res with
         | Ok _ ->
            logInfo $"Balance history daily update complete."
            return ignored ()
         | Error err ->
            logError ctx $"Daily balance history update error: {err}"
            return unhandled ()
      | ScheduleInternalTransferBetweenOrgs transfer ->
         let name = "InternalTransferBetweenOrgs"
         let group = "Bank"

         let transferId = transfer.TransferId
         let correlationId = TransferId.toCorrelationId transferId
         let orgId = transfer.Sender.OrgId

         let builder =
            TriggerBuilder
               .Create()
               .ForJob($"{name}Job-{transferId}", group)
               .WithIdentity($"{name}Trigger-{transferId}", group)
               .WithDescription("Schedule internal transfer")

         let scheduledAt = transfer.ScheduledDate

         logInfo
            $"Scheduling internal transfer for {transferId} at {scheduledAt}."

         let trigger = builder.StartAt(scheduledAt).Build()

         let path =
            ClusterSharding
               .Get(ctx.System)
               .ShardRegionProxy(ClusterMetadata.sagaShardRegion.name)
               .Path

         let job =
            CreatePersistentJob(
               path,
               {
                  Manifest = "SagaActorMessage"
                  Message = {|
                     CorrelationId = correlationId
                     SagaMessage =
                        PlatformTransferSaga.PlatformTransferSagaEvent.ScheduledJobExecuted
                        |> AppSaga.Event.PlatformTransfer
                        |> SagaEvent.create orgId correlationId
                        |> SagaMessage.Event
                  |}
               },
               trigger
            )

         quartzPersistentActorRef.Tell(job, ActorRefs.Nobody)

         let msg =
            PlatformTransferSaga.PlatformTransferSagaEvent.ScheduledJobCreated
            |> AppSaga.Event.PlatformTransfer
            |> SagaEvent.create orgId correlationId
            |> SagaMessage.Event

         getSagaRef correlationId <! msg

         return ignored ()
      | ScheduleDomesticTransfer transfer ->
         let name = "DomesticTransfer"
         let group = "Bank"

         let transferId = transfer.TransferId
         let correlationId = TransferId.toCorrelationId transferId
         let orgId = transfer.Sender.OrgId

         let builder =
            TriggerBuilder
               .Create()
               .ForJob($"{name}Job-{transferId}", group)
               .WithIdentity($"{name}Trigger-{transferId}", group)
               .WithDescription("Schedule domestic transfer")

         let scheduledAt = transfer.ScheduledDate

         logInfo
            $"Scheduling domestic transfer for {transferId} at {scheduledAt}."

         let trigger = builder.StartAt(scheduledAt).Build()

         let path =
            ClusterSharding
               .Get(ctx.System)
               .ShardRegionProxy(ClusterMetadata.sagaShardRegion.name)
               .Path

         let job =
            CreatePersistentJob(
               path,
               {
                  Manifest = "SagaActorMessage"
                  Message = {|
                     CorrelationId = correlationId
                     SagaMessage =
                        DomesticTransferSaga.DomesticTransferSagaEvent.ScheduledJobExecuted
                        |> AppSaga.Event.DomesticTransfer
                        |> SagaEvent.create orgId correlationId
                        |> SagaMessage.Event
                  |}
               },
               trigger
            )

         quartzPersistentActorRef.Tell(job, ActorRefs.Nobody)

         let msg =
            DomesticTransferSaga.DomesticTransferSagaEvent.ScheduledJobCreated
            |> AppSaga.Event.DomesticTransfer
            |> SagaEvent.create orgId correlationId
            |> SagaMessage.Event

         getSagaRef correlationId <! msg

         return ignored ()
   }

   props handler

let get (system: ActorSystem) : IActorRef<SchedulerMessage> =
   typed <| ActorRegistry.For(system).Get<ActorMetadata.SchedulingMarker>()
