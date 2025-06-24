namespace Bank.Scheduler

open Lib.SharedTypes

// NOTE:
// Using a QuartzMessageEnvelope type for messages serialized with
// Akka.Quartz.Actor until serialization PR merged.  Akka.Quartz.Actor
// is always passing in Object as manifest unless this PR merged:
// https://github.com/akkadotnet/Akka.Quartz.Actor/pull/335
type QuartzMessageEnvelope = { Manifest: string; Message: obj }

type SchedulerMessage =
   | AccountClosureCronJobSchedule
   | BillingCycleCronJobSchedule
   | DeleteAccountsJobSchedule of AccountId list
   | BalanceHistoryCronJobSchedule
   | TriggerBalanceHistoryCronJob
   | BalanceManagementCronJobSchedule
   | SagaAlarmClockCronJobSchedule
   | ScheduledTransfersLowBalanceCheck
