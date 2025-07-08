module PlatformTransferSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Saga
open Bank.Account.Domain
open Bank.Transfer.Domain
open Email
open PartnerBank.Service.Domain

[<RequireQualifiedAccess>]
type PlatformTransferSagaStatus =
   | Scheduled
   | InProgress
   | Completed
   | Failed of InternalTransferFailReason

[<RequireQualifiedAccess>]
type PlatformTransferSagaStartEvent =
   | SenderReservedFunds of
      BankEvent<InternalTransferBetweenOrgsPending> *
      PartnerBankAccountLink
   | ScheduleTransferRequest of BankEvent<InternalTransferBetweenOrgsScheduled>

[<RequireQualifiedAccess>]
type PlatformTransferSagaEvent =
   | ScheduledTransferActivated
   | SenderReservedFunds of PartnerBankAccountLink
   | SenderUnableToReserveFunds of InternalTransferFailReason
   | RecipientDepositedFunds of PartnerBankAccountLink
   | RecipientUnableToDepositFunds of InternalTransferFailReason
   | TransferNotificationSent
   | TransferDepositNotificationSent
   | PartnerBankSyncResponse of Result<SettlementId, string>
   | SupportTeamResolvedPartnerBankSync
   | SenderReleasedReservedFunds
   | RecipientDepositUndo
   | TransferSettled
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

type private StartEvent = PlatformTransferSagaStartEvent
type private Event = PlatformTransferSagaEvent

[<RequireQualifiedAccess; CustomEquality; NoComparison>]
type Activity =
   | WaitForScheduledTransferActivation of TimeSpan
   | ReserveSenderFunds
   | DepositToRecipientAccount
   | SyncToPartnerBank
   | SettleTransfer
   | SendTransferNotification
   | SendTransferDepositNotification
   | ReleaseSenderFunds
   | UndoRecipientDeposit
   | WaitForSupportTeamToResolvePartnerBankSync

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForScheduledTransferActivation _
         | WaitForSupportTeamToResolvePartnerBankSync -> 0
         | SyncToPartnerBank -> 4
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | SendTransferNotification
         | SendTransferDepositNotification
         | SyncToPartnerBank -> Some(TimeSpan.FromMinutes 4.)
         | ReserveSenderFunds
         | DepositToRecipientAccount
         | SettleTransfer
         | ReleaseSenderFunds
         | UndoRecipientDeposit -> Some(TimeSpan.FromSeconds 5.)
         | WaitForScheduledTransferActivation time -> Some time
         | WaitForSupportTeamToResolvePartnerBankSync -> None

   // Custom equality check so we can, for example, check for completeness
   // of WaitForScheduledTransferActivation without comparing the inner value.
   // Ex: activityIsDone (Activity.WaitForScheduledTransferActivation TimeSpan.Zero)
   override x.Equals compareTo =
      match compareTo with
      | :? Activity as compareTo -> x.GetHashCode() = compareTo.GetHashCode()
      | _ -> false

   override x.GetHashCode() =
      match x with
      | WaitForScheduledTransferActivation _ ->
         hash "WaitForScheduledTransferActivation"
      | _ -> hash (string x)

type PlatformTransferSaga = {
   StartEvent: PlatformTransferSagaStartEvent
   Events: PlatformTransferSagaEvent list
   Status: PlatformTransferSagaStatus
   TransferInfo: BaseInternalTransferBetweenOrgsInfo
   LifeCycle: SagaLifeCycle<Activity>
   PartnerBankSenderAccountLink: PartnerBankAccountLink option
   PartnerBankRecipientAccountLink: PartnerBankAccountLink option
} with

   member x.SyncedToPartnerBank =
      x.LifeCycle.Completed |> List.exists _.Activity.IsSyncToPartnerBank

   member x.IsSettled =
      x.LifeCycle.Completed |> List.exists _.Activity.IsSettleTransfer

   member x.SettlementId =
      x.Events
      |> List.tryPick (function
         | PlatformTransferSagaEvent.PartnerBankSyncResponse(Ok settlementId) ->
            Some settlementId
         | _ -> None)

   member x.TransferNotificationSent =
      x.LifeCycle.Completed |> List.exists _.Activity.IsSendTransferNotification

   member x.TransferDepositNotificationSent =
      x.LifeCycle.Completed
      |> List.exists _.Activity.IsSendTransferDepositNotification

   member x.RequiresReleaseSenderFunds =
      x.LifeCycle.InProgress |> List.exists _.Activity.IsReleaseSenderFunds

   member x.IsTransferSchedulingAwaitingActivation =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsWaitForScheduledTransferActivation

   member x.FromPaymentRequest =
      match x.StartEvent with
      | PlatformTransferSagaStartEvent.SenderReservedFunds(e, _) ->
         e.Data.BaseInfo.FromPaymentRequest
      | PlatformTransferSagaStartEvent.ScheduleTransferRequest e ->
         e.Data.BaseInfo.FromPaymentRequest

let applyStartEvent
   (evt: PlatformTransferSagaStartEvent)
   (timestamp: DateTime)
   =
   match evt with
   | StartEvent.SenderReservedFunds(e, partnerBankSenderAccountLink) -> {
      Status = PlatformTransferSagaStatus.InProgress
      StartEvent = evt
      Events = []
      TransferInfo = e.Data.BaseInfo
      PartnerBankSenderAccountLink = Some partnerBankSenderAccountLink
      PartnerBankRecipientAccountLink = None
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
                  Activity = Activity.ReserveSenderFunds
                  MaxAttempts =
                     (Activity.ReserveSenderFunds :> IActivity).MaxAttempts
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
      PartnerBankSenderAccountLink = None
      PartnerBankRecipientAccountLink = None
      LifeCycle =
         let timeUntil = e.Data.BaseInfo.ScheduledDate - DateTime.UtcNow

         SagaLifeCycle.empty
         |> SagaLifeCycle.addActivity
               timestamp
               (Activity.WaitForScheduledTransferActivation timeUntil)
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
   | Event.ScheduledTransferActivated -> {
      saga with
         Status = PlatformTransferSagaStatus.InProgress
         LifeCycle =
            saga.LifeCycle
            |> finishActivity (
               Activity.WaitForScheduledTransferActivation TimeSpan.Zero
            )
            |> addActivity Activity.ReserveSenderFunds
     }
   | Event.SenderReservedFunds partnerBankSenderAccountLink -> {
      saga with
         PartnerBankSenderAccountLink = Some partnerBankSenderAccountLink
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.ReserveSenderFunds
            |> addActivity Activity.DepositToRecipientAccount
     }
   | Event.SenderUnableToReserveFunds reason -> {
      saga with
         LifeCycle = saga.LifeCycle |> failActivity Activity.ReserveSenderFunds
         Status = PlatformTransferSagaStatus.Failed reason
     }
   | Event.RecipientDepositedFunds partnerBankRecipientAccountLink -> {
      saga with
         PartnerBankRecipientAccountLink = Some partnerBankRecipientAccountLink
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.DepositToRecipientAccount
            |> addActivity Activity.SyncToPartnerBank
     }
   | Event.RecipientUnableToDepositFunds _ -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> failActivity Activity.DepositToRecipientAccount
            |> addActivity Activity.ReleaseSenderFunds
     }
   | Event.PartnerBankSyncResponse res ->
      match res with
      | Error errMsg ->
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
                     InternalTransferFailReason.PartnerBankSync errMsg
                     |> PlatformTransferSagaStatus.Failed
                  LifeCycle =
                     saga.LifeCycle
                     |> failActivity activity
                     |> addActivity
                           Activity.WaitForSupportTeamToResolvePartnerBankSync
            }
      | Ok _ -> {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.SyncToPartnerBank
               |> addActivity Activity.SettleTransfer
        }
   | Event.SupportTeamResolvedPartnerBankSync -> {
      saga with
         Status = PlatformTransferSagaStatus.InProgress
         LifeCycle =
            saga.LifeCycle
            |> finishActivity
                  Activity.WaitForSupportTeamToResolvePartnerBankSync
     }
   | Event.SenderReleasedReservedFunds -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.ReleaseSenderFunds
     }
   | Event.RecipientDepositUndo -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.UndoRecipientDeposit
     }
   | Event.EvaluateRemainingWork ->
      // No need to increment activity Attempts counter for activating
      // a scheduled transfer.
      if saga.IsTransferSchedulingAwaitingActivation then
         saga
      else
         {
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
   | Event.TransferSettled -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.SettleTransfer
            |> addActivity Activity.SendTransferNotification
            |> if saga.FromPaymentRequest.IsNone then
                  addActivity Activity.SendTransferDepositNotification
               else
                  id
     }
   | Event.TransferNotificationSent -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.SendTransferNotification
         Status =
            if saga.FromPaymentRequest.IsSome then
               PlatformTransferSagaStatus.Completed
            elif saga.TransferDepositNotificationSent then
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
            if saga.TransferNotificationSent then
               PlatformTransferSagaStatus.Completed
            else
               saga.Status
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
      | PlatformTransferSagaEvent.ScheduledTransferActivated ->
         activityIsDone (
            Activity.WaitForScheduledTransferActivation TimeSpan.Zero
         )
      | PlatformTransferSagaEvent.SenderReservedFunds _
      | PlatformTransferSagaEvent.SenderUnableToReserveFunds _ ->
         activityIsDone Activity.ReserveSenderFunds
      | PlatformTransferSagaEvent.RecipientDepositedFunds _
      | PlatformTransferSagaEvent.RecipientUnableToDepositFunds _ ->
         activityIsDone Activity.DepositToRecipientAccount
      | PlatformTransferSagaEvent.RecipientDepositUndo ->
         activityIsDone Activity.UndoRecipientDeposit
      | PlatformTransferSagaEvent.PartnerBankSyncResponse _ ->
         activityIsDone Activity.SyncToPartnerBank
      | PlatformTransferSagaEvent.SupportTeamResolvedPartnerBankSync ->
         activityIsDone Activity.WaitForSupportTeamToResolvePartnerBankSync
      | PlatformTransferSagaEvent.TransferNotificationSent ->
         activityIsDone Activity.SendTransferNotification
      | PlatformTransferSagaEvent.TransferDepositNotificationSent ->
         activityIsDone Activity.SendTransferDepositNotification
      | PlatformTransferSagaEvent.SenderReleasedReservedFunds ->
         activityIsDone Activity.ReleaseSenderFunds
      | PlatformTransferSagaEvent.TransferSettled ->
         activityIsDone Activity.SettleTransfer

   if saga.Status = PlatformTransferSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

type private TransferStartEvent = PlatformTransferSagaStartEvent
type private TransferEvent = PlatformTransferSagaEvent

let private depositTransfer
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (transfer: BaseInternalTransferBetweenOrgsInfo)
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
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (evt: TransferStartEvent)
   =
   match evt with
   | TransferStartEvent.SenderReservedFunds(e, _) ->
      depositTransfer getAccountRef e.Data.BaseInfo
   | TransferStartEvent.ScheduleTransferRequest _ -> ()

type PersistenceHandlerDependencies = {
   getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   getPartnerBankServiceRef: unit -> IActorRef<PartnerBankServiceMessage>
   sendEventToSelf:
      BaseInternalTransferBetweenOrgsInfo -> PlatformTransferSagaEvent -> unit
}

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
   (previousState: PlatformTransferSaga)
   (currentState: PlatformTransferSaga)
   (evt: TransferEvent)
   =
   let transfer = currentState.TransferInfo
   let correlationId = TransferId.toCorrelationId transfer.TransferId

   let reserveSenderFunds () =
      let cmd = {
         InternalTransferBetweenOrgsCommand.create transfer.InitiatedBy {
            Amount = transfer.Amount
            Sender = transfer.Sender
            Recipient = transfer.Recipient
            Memo = transfer.Memo
            ScheduledDateSeedOverride = None
            OriginatedFromSchedule = true
            OriginatedFromPaymentRequest = currentState.FromPaymentRequest
         } with
            CorrelationId = correlationId
      }

      let msg =
         cmd
         |> AccountCommand.InternalTransferBetweenOrgs
         |> AccountMessage.StateChange

      dep.getAccountRef transfer.Sender.ParentAccountId <! msg

   let depositTransfer () =
      depositTransfer dep.getAccountRef transfer

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
      match
         currentState.PartnerBankSenderAccountLink,
         currentState.PartnerBankRecipientAccountLink
      with
      | Some sender, Some recipient ->
         dep.getPartnerBankServiceRef ()
         <! PartnerBankServiceMessage.TransferBetweenOrganizations {
            Amount = transfer.Amount
            From = sender
            To = recipient
            Metadata = {
               OrgId = transfer.Sender.OrgId
               CorrelationId = correlationId
            }
         }
      | _ -> ()

   let settleTransfer settlementId =
      let msg =
         SettleInternalTransferBetweenOrgsCommand.create
            correlationId
            transfer.InitiatedBy
            {
               BaseInfo = transfer
               SettlementId = settlementId
            }
         |> AccountCommand.SettleInternalTransferBetweenOrgs
         |> AccountMessage.StateChange

      dep.getAccountRef transfer.Sender.ParentAccountId <! msg

   match evt with
   | TransferEvent.ScheduledTransferActivated -> reserveSenderFunds ()
   | TransferEvent.SenderReservedFunds _ -> depositTransfer ()
   | TransferEvent.RecipientDepositedFunds _ -> syncToPartnerBank ()
   | TransferEvent.PartnerBankSyncResponse res ->
      match res with
      | Ok settlementId -> settleTransfer settlementId
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
   | TransferEvent.SenderUnableToReserveFunds _ -> ()
   | TransferEvent.RecipientUnableToDepositFunds reason ->
      if currentState.RequiresReleaseSenderFunds then
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
   | TransferEvent.SenderReleasedReservedFunds -> ()
   | TransferEvent.TransferSettled ->
      transferSentEmail ()

      if currentState.FromPaymentRequest.IsNone then
         transferDepositEmail ()
   | TransferEvent.RecipientDepositUndo -> ()
   | TransferEvent.ResetInProgressActivityAttempts -> ()
   | TransferEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.ReserveSenderFunds -> reserveSenderFunds ()
         | Activity.DepositToRecipientAccount -> depositTransfer ()
         | Activity.SyncToPartnerBank -> syncToPartnerBank ()
         | Activity.SendTransferNotification -> transferSentEmail ()
         | Activity.SendTransferDepositNotification -> transferDepositEmail ()
         | Activity.ReleaseSenderFunds ->
            match
               currentState.RequiresReleaseSenderFunds, currentState.Status
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
         | Activity.WaitForScheduledTransferActivation _ ->
            dep.sendEventToSelf transfer Event.ScheduledTransferActivated
         | Activity.UndoRecipientDeposit
         | Activity.WaitForSupportTeamToResolvePartnerBankSync -> ()
         | Activity.SettleTransfer ->
            currentState.SettlementId |> Option.iter settleTransfer
