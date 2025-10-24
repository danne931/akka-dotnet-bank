[<RequireQualifiedAccess>]
module SchedulingActor

open Akka.Actor
open Akka.Quartz.Actor.Commands
open Akkling
open Quartz

open Bank.Account.Domain
open BillingStatement
open ActorUtil
open Lib.Postgres
open Bank.Scheduler
open TransferMessages

let actorProps (quartzPersistentActorRef: IActorRef) =
   let handler (ctx: Actor<SchedulerMessage>) = actor {
      let logInfo = logInfo ctx
      let! msg = ctx.Receive()

      match msg with
      | AccountClosureCronJobSchedule ->
         logInfo $"Scheduling account closure checker"

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
         logInfo $"Scheduling billing cycle"

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
                     AutoTransferMessage.StartScheduledAutoTransfers schedule
               },
               trigger schedule
            )

         let twiceMonthlyJob =
            let schedule = AutomaticTransfer.CronSchedule.TwiceMonthly

            CreatePersistentJob(
               path,
               {
                  Manifest = "BalanceManagementMessage"
                  Message =
                     AutoTransferMessage.StartScheduledAutoTransfers schedule
               },
               trigger schedule
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
                  Message =
                     AppSaga.SagaAlarmClockMessage.WakeUpIfUnfinishedBusiness
               },
               trigger
            )

         quartzPersistentActorRef.Tell(job, ActorRefs.Nobody)
         return ignored ()
      | ScheduledTransfersLowBalanceCheck ->
         logInfo
            "Scheduling check for accounts with insufficient balance for scheduled transfers"

         let name = "ScheduledTransfersLowBalance"
         let group = "Bank"

         let builder =
            TriggerBuilder
               .Create()
               .ForJob($"{name}Job", group)
               .WithIdentity($"{name}Trigger", group)
               .WithDescription(
                  "Daily check for accounts with insufficient balance to cover scheduled transfers"
               )

         let trigger =
            if Env.isProd then
               logInfo
                  "Scheduling daily scheduled transfers low balance check at 10:00 AM"

               builder.WithCronSchedule("0 0 10 * * ?").Build()
            else
               let minutes = 10

               logInfo
                  $"Scheduling scheduled transfers low balance check every {minutes} minutes"

               builder
                  .WithSimpleSchedule(fun s ->
                     s.WithIntervalInMinutes(minutes).RepeatForever() |> ignore)
                  .Build()

         let path = ActorMetadata.scheduledTransfersLowBalanceWarning.ProxyPath

         let job =
            CreatePersistentJob(
               path,
               {
                  Manifest = "ScheduledTransfersLowBalanceMessage"
                  Message = ScheduledTransfersLowBalanceMessage.Detect
               },
               trigger
            )

         quartzPersistentActorRef.Tell(job, ActorRefs.Nobody)
         return ignored ()
      | BalanceHistoryCronJobSchedule ->
         logInfo "Scheduling balance history update."
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

         quartzPersistentActorRef.Tell job
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
   }

   props handler
