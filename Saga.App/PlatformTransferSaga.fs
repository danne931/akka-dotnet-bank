module PlatformTransferSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Saga
open Bank.Account.Domain
open Bank.Transfer.Domain
open Email
open Bank.Scheduler

[<RequireQualifiedAccess>]
type PlatformTransferSagaStatus =
   | Scheduled
   | InProgress
   | Completed
   | Failed of InternalTransferFailReason

[<RequireQualifiedAccess>]
type PlatformTransferSagaStartEvent =
   | SenderAccountDeductedFunds of BankEvent<InternalTransferBetweenOrgsPending>
   | ScheduleTransferRequest of BankEvent<InternalTransferBetweenOrgsScheduled>

[<RequireQualifiedAccess>]
type PlatformTransferSagaEvent =
   | ScheduledJobCreated
   | ScheduledJobExecuted
   | SenderAccountDeductedFunds
   | SenderAccountUnableToDeductFunds of InternalTransferFailReason
   | RecipientAccountDepositedFunds
   | RecipientAccountUnableToDepositFunds of InternalTransferFailReason
   | TransferNotificationSent
   | TransferDepositNotificationSent
   | PartnerBankSyncResponse of Result<string, string>
   | SupportTeamResolvedPartnerBankSync
   | SenderAccountRefunded
   | RecipientAccountDepositUndo
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

type private StartEvent = PlatformTransferSagaStartEvent
type private Event = PlatformTransferSagaEvent

[<RequireQualifiedAccess>]
type Activity =
   | ScheduleTransfer
   | WaitForScheduledTransferExecution
   | DeductFromSenderAccount
   | DepositToRecipientAccount
   | SyncToPartnerBank
   | SendTransferNotification
   | SendTransferDepositNotification
   | RefundSenderAccount
   | UndoRecipientDeposit
   | WaitForSupportTeamToResolvePartnerBankSync

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForScheduledTransferExecution
         | WaitForSupportTeamToResolvePartnerBankSync -> 0
         | SyncToPartnerBank -> 4
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | SendTransferNotification
         | SendTransferDepositNotification
         | SyncToPartnerBank -> Some(TimeSpan.FromMinutes 4)
         | ScheduleTransfer
         | DeductFromSenderAccount
         | DepositToRecipientAccount
         | RefundSenderAccount
         | UndoRecipientDeposit -> Some(TimeSpan.FromSeconds 5)
         | WaitForScheduledTransferExecution
         | WaitForSupportTeamToResolvePartnerBankSync -> None

type PlatformTransferSaga = {
   StartEvent: PlatformTransferSagaStartEvent
   Events: PlatformTransferSagaEvent list
   Status: PlatformTransferSagaStatus
   TransferInfo: BaseInternalTransferInfo
   LifeCycle: SagaLifeCycle<Activity>
} with

   member x.SyncedToPartnerBank =
      x.LifeCycle.Completed
      |> List.exists (fun w -> w.Activity = Activity.SyncToPartnerBank)

   member x.TransferNotificationSent =
      x.LifeCycle.Completed
      |> List.exists (fun w -> w.Activity = Activity.SendTransferNotification)

   member x.TransferDepositNotificationSent =
      x.LifeCycle.Completed
      |> List.exists (fun w ->
         w.Activity = Activity.SendTransferDepositNotification)

   member x.RequiresSenderAccountRefund =
      x.LifeCycle.InProgress
      |> List.exists (fun w -> w.Activity = Activity.RefundSenderAccount)

let applyStartEvent
   (evt: PlatformTransferSagaStartEvent)
   (timestamp: DateTime)
   =
   match evt with
   | StartEvent.SenderAccountDeductedFunds e -> {
      Status = PlatformTransferSagaStatus.InProgress
      StartEvent = evt
      Events = []
      TransferInfo = e.Data.BaseInfo
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init
                  timestamp
                  Activity.DepositToRecipientAccount
            ]
            Completed = [
               {
                  Start = timestamp
                  End = Some timestamp
                  Activity = Activity.DeductFromSenderAccount
                  MaxAttempts =
                     (Activity.DeductFromSenderAccount :> IActivity).MaxAttempts
                  Attempts = 1
               }
            ]
      }
     }
   | StartEvent.ScheduleTransferRequest e -> {
      Status = PlatformTransferSagaStatus.Scheduled
      StartEvent = evt
      Events = []
      TransferInfo = e.Data.BaseInfo
      LifeCycle =
         SagaLifeCycle.empty
         |> SagaLifeCycle.addActivity timestamp Activity.ScheduleTransfer
     }

let applyEvent (saga: PlatformTransferSaga) (evt: Event) (timestamp: DateTime) =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp
   let failActivity = SagaLifeCycle.failActivity timestamp
   let retryActivity = SagaLifeCycle.retryActivity timestamp

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | Event.ScheduledJobCreated -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.ScheduleTransfer
            |> addActivity Activity.WaitForScheduledTransferExecution
     }
   | Event.ScheduledJobExecuted -> {
      saga with
         Status = PlatformTransferSagaStatus.InProgress
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.WaitForScheduledTransferExecution
            |> addActivity Activity.DeductFromSenderAccount
     }
   | Event.SenderAccountDeductedFunds -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.DeductFromSenderAccount
            |> addActivity Activity.DepositToRecipientAccount
     }
   | Event.SenderAccountUnableToDeductFunds reason -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> failActivity Activity.DeductFromSenderAccount
         Status = PlatformTransferSagaStatus.Failed reason
     }
   | Event.RecipientAccountDepositedFunds -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.DepositToRecipientAccount
            |> addActivity Activity.SyncToPartnerBank
            |> addActivity Activity.SendTransferNotification
            |> addActivity Activity.SendTransferDepositNotification
     }
   | Event.RecipientAccountUnableToDepositFunds _ -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> failActivity Activity.DepositToRecipientAccount
            |> addActivity Activity.RefundSenderAccount
     }
   | Event.PartnerBankSyncResponse res ->
      match res with
      | Error err ->
         let activity = Activity.SyncToPartnerBank

         if saga.LifeCycle.ActivityHasRemainingAttempts activity then
            {
               saga with
                  LifeCycle = retryActivity activity saga.LifeCycle
            }
         else
            {
               saga with
                  Status =
                     InternalTransferFailReason.PartnerBankSync err
                     |> PlatformTransferSagaStatus.Failed
                  LifeCycle =
                     saga.LifeCycle
                     |> failActivity activity
                     |> addActivity
                           Activity.WaitForSupportTeamToResolvePartnerBankSync
            }
      | Ok _ -> {
         saga with
            Status =
               if
                  saga.TransferNotificationSent
                  && saga.TransferDepositNotificationSent
               then
                  PlatformTransferSagaStatus.Completed
               else
                  saga.Status
            LifeCycle =
               saga.LifeCycle |> finishActivity Activity.SyncToPartnerBank
        }
   | Event.SupportTeamResolvedPartnerBankSync -> {
      saga with
         Status = PlatformTransferSagaStatus.InProgress
         LifeCycle =
            saga.LifeCycle
            |> finishActivity
                  Activity.WaitForSupportTeamToResolvePartnerBankSync
     }
   | Event.TransferNotificationSent -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.SendTransferNotification
         Status =
            if
               saga.TransferDepositNotificationSent && saga.SyncedToPartnerBank
            then
               PlatformTransferSagaStatus.Completed
            else
               saga.Status
     }
   | Event.TransferDepositNotificationSent -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.SendTransferDepositNotification
         Status =
            if saga.TransferNotificationSent && saga.SyncedToPartnerBank then
               PlatformTransferSagaStatus.Completed
            else
               saga.Status
     }
   | Event.SenderAccountRefunded -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.RefundSenderAccount
     }
   | Event.RecipientAccountDepositUndo -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.UndoRecipientDeposit
     }
   | Event.EvaluateRemainingWork -> {
      saga with
         LifeCycle =
            SagaLifeCycle.retryActivitiesAfterInactivity
               timestamp
               saga.LifeCycle
     }
   | Event.ResetInProgressActivityAttempts -> {
      saga with
         LifeCycle = SagaLifeCycle.resetInProgressActivities saga.LifeCycle
     }

let stateTransitionStart
   (evt: PlatformTransferSagaStartEvent)
   (timestamp: DateTime)
   : Result<PlatformTransferSaga, SagaStateTransitionError>
   =
   Ok(applyStartEvent evt timestamp)

let stateTransition
   (saga: PlatformTransferSaga)
   (evt: PlatformTransferSagaEvent)
   (timestamp: DateTime)
   : Result<PlatformTransferSaga, SagaStateTransitionError>
   =
   let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

   let invalidStepProgression =
      match evt with
      | PlatformTransferSagaEvent.EvaluateRemainingWork
      | PlatformTransferSagaEvent.ResetInProgressActivityAttempts -> false
      | PlatformTransferSagaEvent.ScheduledJobCreated ->
         activityIsDone Activity.ScheduleTransfer
      | PlatformTransferSagaEvent.ScheduledJobExecuted ->
         activityIsDone Activity.WaitForScheduledTransferExecution
      | PlatformTransferSagaEvent.SenderAccountDeductedFunds
      | PlatformTransferSagaEvent.SenderAccountUnableToDeductFunds _ ->
         activityIsDone Activity.DeductFromSenderAccount
      | PlatformTransferSagaEvent.RecipientAccountDepositedFunds
      | PlatformTransferSagaEvent.RecipientAccountUnableToDepositFunds _ ->
         activityIsDone Activity.DepositToRecipientAccount
      | PlatformTransferSagaEvent.RecipientAccountDepositUndo ->
         activityIsDone Activity.UndoRecipientDeposit
      | PlatformTransferSagaEvent.PartnerBankSyncResponse _ ->
         activityIsDone Activity.SyncToPartnerBank
      | PlatformTransferSagaEvent.SupportTeamResolvedPartnerBankSync ->
         activityIsDone Activity.WaitForSupportTeamToResolvePartnerBankSync
      | PlatformTransferSagaEvent.TransferNotificationSent ->
         activityIsDone Activity.SendTransferNotification
      | PlatformTransferSagaEvent.TransferDepositNotificationSent ->
         activityIsDone Activity.SendTransferDepositNotification
      | PlatformTransferSagaEvent.SenderAccountRefunded ->
         activityIsDone Activity.RefundSenderAccount

   if saga.Status = PlatformTransferSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

type private TransferStartEvent = PlatformTransferSagaStartEvent
type private TransferEvent = PlatformTransferSagaEvent

let private scheduleTransferMessage =
   SchedulerMessage.ScheduleInternalTransferBetweenOrgs

type PersistenceHandlerDependencies = {
   getSchedulingRef: unit -> IActorRef<SchedulerMessage>
   getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   syncToPartnerBank: BaseInternalTransferInfo -> Async<TransferEvent>
   sendMessageToSelf: BaseInternalTransferInfo -> Async<TransferEvent> -> unit
}

let private depositTransfer
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (transfer: BaseInternalTransferInfo)
   =
   let cmd =
      DepositInternalTransferBetweenOrgsCommand.create
         (TransferId.toCorrelationId transfer.TransferId)
         transfer.InitiatedBy
         { BaseInfo = transfer }

   let msg =
      cmd
      |> AccountCommand.DepositTransferBetweenOrgs
      |> AccountMessage.StateChange

   getAccountRef transfer.Recipient.ParentAccountId <! msg

let onStartEventPersisted
   (dep: PersistenceHandlerDependencies)
   (evt: TransferStartEvent)
   =
   match evt with
   | TransferStartEvent.SenderAccountDeductedFunds e ->
      depositTransfer dep.getAccountRef e.Data.BaseInfo
   | TransferStartEvent.ScheduleTransferRequest e ->
      dep.getSchedulingRef () <! scheduleTransferMessage e.Data.BaseInfo

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
   (previousState: PlatformTransferSaga)
   (currentState: PlatformTransferSaga)
   (evt: TransferEvent)
   =
   let transfer = currentState.TransferInfo
   let correlationId = TransferId.toCorrelationId transfer.TransferId

   let deductFromSender () =
      let cmd = {
         InternalTransferBetweenOrgsCommand.create transfer.InitiatedBy {
            Amount = transfer.Amount
            Sender = transfer.Sender
            Recipient = transfer.Recipient
            Memo = transfer.Memo
            ScheduledDateSeedOverride = None
            OriginatedFromSchedule = true
         } with
            CorrelationId = correlationId
      }

      let msg =
         cmd
         |> AccountCommand.InternalTransferBetweenOrgs
         |> AccountMessage.StateChange

      dep.getAccountRef transfer.Sender.ParentAccountId <! msg

   let transferSentEmail () =
      let msg =
         EmailMessage.create
            transfer.Sender.OrgId
            correlationId
            (EmailInfo.InternalTransferBetweenOrgs {
               SenderAccountName = transfer.Sender.Name
               RecipientBusinessName = transfer.Recipient.Name
               Amount = transfer.Amount
            })

      dep.getEmailRef () <! msg

   let transferDepositEmail () =
      let msg =
         EmailMessage.create
            transfer.Recipient.OrgId
            correlationId
            (EmailInfo.InternalTransferBetweenOrgsDeposited {
               SenderBusinessName = transfer.Sender.Name
               RecipientAccountName = transfer.Recipient.Name
               Amount = transfer.Amount
            })

      dep.getEmailRef () <! msg

   let syncToPartnerBank () =
      dep.sendMessageToSelf transfer (dep.syncToPartnerBank transfer)

   match evt with
   | TransferEvent.ScheduledJobCreated -> ()
   | TransferEvent.ScheduledJobExecuted -> deductFromSender ()
   | TransferEvent.SenderAccountDeductedFunds ->
      depositTransfer dep.getAccountRef transfer
   | TransferEvent.RecipientAccountDepositedFunds ->
      transferSentEmail ()
      transferDepositEmail ()
      syncToPartnerBank ()
   | TransferEvent.PartnerBankSyncResponse res ->
      match res with
      | Ok _ -> ()
      | Error reason ->
         if
            previousState.LifeCycle.ActivityHasRemainingAttempts
               Activity.SyncToPartnerBank
         then
            syncToPartnerBank ()
         else
            let msg =
               EmailMessage.create
                  transfer.Sender.OrgId
                  correlationId
                  (EmailInfo.ApplicationErrorRequiresSupport reason)

            dep.getEmailRef () <! msg
   | TransferEvent.TransferNotificationSent -> ()
   | TransferEvent.TransferDepositNotificationSent -> ()
   | TransferEvent.SenderAccountUnableToDeductFunds _ -> ()
   | TransferEvent.RecipientAccountUnableToDepositFunds reason ->
      if currentState.RequiresSenderAccountRefund then
         let cmd =
            FailInternalTransferBetweenOrgsCommand.create
               correlationId
               transfer.InitiatedBy
               { BaseInfo = transfer; Reason = reason }

         let msg =
            cmd
            |> AccountCommand.FailInternalTransferBetweenOrgs
            |> AccountMessage.StateChange

         dep.getAccountRef transfer.Sender.ParentAccountId <! msg
   | TransferEvent.SupportTeamResolvedPartnerBankSync ->
      // Support team resolved dispute with partner bank so
      // reattempt syncing transaction to partner bank.
      syncToPartnerBank ()
   | TransferEvent.SenderAccountRefunded -> ()
   | TransferEvent.RecipientAccountDepositUndo -> ()
   | TransferEvent.ResetInProgressActivityAttempts -> ()
   | TransferEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.ScheduleTransfer ->
            dep.getSchedulingRef () <! scheduleTransferMessage transfer
         | Activity.DeductFromSenderAccount -> deductFromSender ()
         | Activity.DepositToRecipientAccount ->
            depositTransfer dep.getAccountRef transfer
         | Activity.SyncToPartnerBank -> syncToPartnerBank ()
         | Activity.SendTransferNotification -> transferSentEmail ()
         | Activity.SendTransferDepositNotification -> transferDepositEmail ()
         | Activity.RefundSenderAccount ->
            match
               currentState.RequiresSenderAccountRefund, currentState.Status
            with
            | true, PlatformTransferSagaStatus.Failed reason ->
               let cmd =
                  FailInternalTransferBetweenOrgsCommand.create
                     correlationId
                     transfer.InitiatedBy
                     { BaseInfo = transfer; Reason = reason }

               let msg =
                  cmd
                  |> AccountCommand.FailInternalTransferBetweenOrgs
                  |> AccountMessage.StateChange

               dep.getAccountRef transfer.Sender.ParentAccountId <! msg
            | _ -> ()
         | Activity.UndoRecipientDeposit
         | Activity.WaitForScheduledTransferExecution
         | Activity.WaitForSupportTeamToResolvePartnerBankSync -> ()

let syncTransferToPartnerBank (info: BaseInternalTransferInfo) = async {
   // HTTP to partner bank
   do! Async.Sleep(1000)

   return Ok "transfer response" |> TransferEvent.PartnerBankSyncResponse
}
