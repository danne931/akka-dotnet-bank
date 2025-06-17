module DomesticTransferSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Transfer.Domain
open Email
open Lib.Saga
open Bank.Scheduler

type private ServiceMessage =
   DomesticTransfer.Service.Domain.DomesticTransferServiceMessage

type private ServiceAction =
   DomesticTransfer.Service.Domain.DomesticTransferServiceAction

[<RequireQualifiedAccess>]
type DomesticTransferSagaStartEvent =
   | SenderAccountDeductedFunds of BankEvent<DomesticTransferPending>
   | ScheduleTransferRequest of BankEvent<DomesticTransferScheduled>

[<RequireQualifiedAccess>]
type DomesticTransferSagaEvent =
   | ScheduledJobCreated
   | ScheduledJobExecuted
   | SenderAccountDeductedFunds
   | TransferProcessorProgressUpdate of DomesticTransferThirdPartyUpdate
   | TransferMarkedAsSettled
   | TransferInitiatedNotificationSent
   | RetryTransferServiceRequest of
      updatedRecipient: DomesticTransferRecipient option
   | SenderAccountUnableToDeductFunds of DomesticTransferFailReason
   | SenderAccountRefunded
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess>]
type Activity =
   | ScheduleTransfer
   | WaitForScheduledTransferExecution
   | DeductFromSenderAccount
   | TransferServiceAck
   | WaitForTransferServiceComplete
   | MarkTransferAsSettled
   | SendTransferInitiatedNotification
   | RefundSenderAccount
   | WaitForDevelopmentTeamFix

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForScheduledTransferExecution
         | WaitForDevelopmentTeamFix -> 0
         // Check every 4 hours, 6 times a day for 6 days.
         | WaitForTransferServiceComplete -> 36
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | WaitForScheduledTransferExecution
         | WaitForDevelopmentTeamFix -> None
         | WaitForTransferServiceComplete ->
            Some(
               if Env.isProd then
                  TimeSpan.FromHours 4
               else
                  TimeSpan.FromMinutes 1
            )
         | TransferServiceAck
         | SendTransferInitiatedNotification -> Some(TimeSpan.FromMinutes 4)
         | DeductFromSenderAccount
         | MarkTransferAsSettled
         | ScheduleTransfer
         | RefundSenderAccount -> Some(TimeSpan.FromSeconds 5)

type DomesticTransferSaga = {
   StartEvent: DomesticTransferSagaStartEvent
   Events: DomesticTransferSagaEvent list
   Status: DomesticTransferProgress
   TransferInfo: BaseDomesticTransferInfo
   LifeCycle: SagaLifeCycle<Activity>
   ReasonForRetryServiceAck: DomesticTransferFailReason option
} with

   member x.OriginatedFromSchedule =
      x.Events
      |> List.exists (function
         | DomesticTransferSagaEvent.ScheduledJobCreated -> true
         | _ -> false)

   member x.TransferMarkedAsSettled =
      x.LifeCycle.Completed
      |> List.exists (fun w -> w.Activity = Activity.MarkTransferAsSettled)

   member x.TransferInitiatedNotificationSent =
      x.LifeCycle.Completed
      |> List.exists (fun w ->
         w.Activity = Activity.SendTransferInitiatedNotification)

   member x.RequiresAccountRefund =
      x.LifeCycle.InProgress
      |> List.exists (fun w -> w.Activity = Activity.RefundSenderAccount)

   member x.RequiresTransferServiceDevelopmentFix =
      x.LifeCycle.InProgress
      |> List.exists (fun w -> w.Activity = Activity.WaitForDevelopmentTeamFix)

let applyStartEvent
   (start: DomesticTransferSagaStartEvent)
   (timestamp: DateTime)
   : DomesticTransferSaga
   =
   match start with
   | DomesticTransferSagaStartEvent.SenderAccountDeductedFunds evt -> {
      Status = DomesticTransferProgress.WaitingForTransferServiceAck
      StartEvent = start
      Events = []
      TransferInfo = evt.Data.BaseInfo
      ReasonForRetryServiceAck = None
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init timestamp Activity.TransferServiceAck
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
   | DomesticTransferSagaStartEvent.ScheduleTransferRequest evt -> {
      Status = DomesticTransferProgress.Scheduled
      StartEvent = start
      Events = []
      TransferInfo = evt.Data.BaseInfo
      ReasonForRetryServiceAck = None
      LifeCycle =
         SagaLifeCycle.empty
         |> SagaLifeCycle.addActivity timestamp Activity.ScheduleTransfer
     }

let applyEvent
   (saga: DomesticTransferSaga)
   (evt: DomesticTransferSagaEvent)
   (timestamp: DateTime)
   : DomesticTransferSaga
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp
   let failActivity = SagaLifeCycle.failActivity timestamp

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | DomesticTransferSagaEvent.ScheduledJobCreated -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.ScheduleTransfer
            |> addActivity Activity.WaitForScheduledTransferExecution
     }
   | DomesticTransferSagaEvent.ScheduledJobExecuted -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.WaitForScheduledTransferExecution
            |> addActivity Activity.DeductFromSenderAccount
         Status = DomesticTransferProgress.ProcessingSenderAccountDeduction
     }
   | DomesticTransferSagaEvent.SenderAccountDeductedFunds -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.DeductFromSenderAccount
            |> addActivity Activity.TransferServiceAck
         Status = DomesticTransferProgress.WaitingForTransferServiceAck
     }
   | DomesticTransferSagaEvent.TransferProcessorProgressUpdate progress ->
      match progress with
      | DomesticTransferThirdPartyUpdate.ServiceAckReceived -> {
         saga with
            Status = DomesticTransferProgress.ThirdParty progress
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.TransferServiceAck
               |> addActivity Activity.SendTransferInitiatedNotification
               |> addActivity Activity.WaitForTransferServiceComplete
        }
      | DomesticTransferThirdPartyUpdate.ProgressDetail _ -> {
         saga with
            Status = DomesticTransferProgress.ThirdParty progress
        }
      | DomesticTransferThirdPartyUpdate.Settled -> {
         saga with
            Status = DomesticTransferProgress.ThirdParty progress
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.WaitForTransferServiceComplete
               |> addActivity Activity.MarkTransferAsSettled
        }
      | DomesticTransferThirdPartyUpdate.Failed reason ->
         let saga = {
            saga with
               Status =
                  reason
                  |> DomesticTransferFailReason.ThirdParty
                  |> DomesticTransferProgress.Failed
         }

         match reason with
         | DomesticTransferThirdPartyFailReason.Infra _ -> {
            saga with
               LifeCycle =
                  saga.LifeCycle
                  |> failActivity Activity.TransferServiceAck
                  |> failActivity Activity.WaitForTransferServiceComplete
                  |> addActivity Activity.WaitForDevelopmentTeamFix
           }
         | DomesticTransferThirdPartyFailReason.InvalidAmount
         | DomesticTransferThirdPartyFailReason.RecipientAccountInvalidInfo
         | DomesticTransferThirdPartyFailReason.RecipientAccountNotActive ->
            let refundIfNotRetrying =
               if saga.ReasonForRetryServiceAck.IsSome then
                  id
               else
                  addActivity Activity.RefundSenderAccount

            {
               saga with
                  LifeCycle =
                     saga.LifeCycle
                     |> failActivity Activity.TransferServiceAck
                     |> failActivity Activity.WaitForTransferServiceComplete
                     |> refundIfNotRetrying
            }
   | DomesticTransferSagaEvent.RetryTransferServiceRequest updatedRecipient -> {
      saga with
         ReasonForRetryServiceAck =
            match saga.Status with
            | DomesticTransferProgress.Failed reason -> Some reason
            | _ -> None
         Status = DomesticTransferProgress.WaitingForTransferServiceAck
         TransferInfo.Recipient =
            updatedRecipient |> Option.defaultValue saga.TransferInfo.Recipient
         LifeCycle = saga.LifeCycle |> addActivity Activity.TransferServiceAck
     }
   | DomesticTransferSagaEvent.TransferMarkedAsSettled -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.MarkTransferAsSettled
         Status =
            if saga.TransferInitiatedNotificationSent then
               DomesticTransferProgress.Settled
            else
               saga.Status
     }
   | DomesticTransferSagaEvent.TransferInitiatedNotificationSent -> {
      saga with
         Status =
            if saga.TransferMarkedAsSettled then
               DomesticTransferProgress.Settled
            else
               saga.Status
         LifeCycle =
            finishActivity
               Activity.SendTransferInitiatedNotification
               saga.LifeCycle
     }
   | DomesticTransferSagaEvent.SenderAccountUnableToDeductFunds reason -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> failActivity Activity.DeductFromSenderAccount
         Status = DomesticTransferProgress.Failed reason
     }
   | DomesticTransferSagaEvent.SenderAccountRefunded -> {
      saga with
         LifeCycle = finishActivity Activity.RefundSenderAccount saga.LifeCycle
     }
   | DomesticTransferSagaEvent.EvaluateRemainingWork -> {
      saga with
         LifeCycle =
            SagaLifeCycle.retryActivitiesAfterInactivity
               timestamp
               saga.LifeCycle
     }
   | DomesticTransferSagaEvent.ResetInProgressActivityAttempts -> {
      saga with
         LifeCycle = SagaLifeCycle.resetInProgressActivities saga.LifeCycle
     }

let stateTransitionStart
   (evt: DomesticTransferSagaStartEvent)
   (timestamp: DateTime)
   : Result<DomesticTransferSaga, SagaStateTransitionError>
   =
   Ok(applyStartEvent evt timestamp)

let stateTransition
   (saga: DomesticTransferSaga)
   (evt: DomesticTransferSagaEvent)
   (timestamp: DateTime)
   : Result<DomesticTransferSaga, SagaStateTransitionError>
   =
   let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

   let invalidStepProgression =
      match evt with
      | DomesticTransferSagaEvent.EvaluateRemainingWork
      | DomesticTransferSagaEvent.ResetInProgressActivityAttempts -> false
      | DomesticTransferSagaEvent.ScheduledJobCreated ->
         activityIsDone Activity.ScheduleTransfer
      | DomesticTransferSagaEvent.ScheduledJobExecuted ->
         activityIsDone Activity.WaitForScheduledTransferExecution
      | DomesticTransferSagaEvent.SenderAccountDeductedFunds ->
         activityIsDone Activity.DeductFromSenderAccount
      | DomesticTransferSagaEvent.TransferProcessorProgressUpdate _ ->
         activityIsDone Activity.TransferServiceAck
         && activityIsDone Activity.WaitForTransferServiceComplete
      | DomesticTransferSagaEvent.RetryTransferServiceRequest _ ->
         saga.LifeCycle.ActivityHasFailed(Activity.TransferServiceAck) |> not
      | DomesticTransferSagaEvent.TransferMarkedAsSettled ->
         activityIsDone Activity.MarkTransferAsSettled
      | DomesticTransferSagaEvent.TransferInitiatedNotificationSent ->
         activityIsDone Activity.SendTransferInitiatedNotification
      | DomesticTransferSagaEvent.SenderAccountRefunded ->
         activityIsDone Activity.RefundSenderAccount
      | _ -> false

   if saga.Status = DomesticTransferProgress.Settled then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

type PersistenceHandlerDependencies = {
   getSchedulingRef: unit -> IActorRef<SchedulerMessage>
   getDomesticTransferRef: unit -> IActorRef<ServiceMessage>
   getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   logError: string -> unit
}

let onStartEventPersisted
   (dep: PersistenceHandlerDependencies)
   (evt: DomesticTransferSagaStartEvent)
   =
   match evt with
   | DomesticTransferSagaStartEvent.SenderAccountDeductedFunds e ->
      let transfer = TransferEventToDomesticTransfer.fromPending e

      let msg =
         ServiceMessage.TransferRequest(ServiceAction.TransferAck, transfer)

      dep.getDomesticTransferRef () <! msg
   | DomesticTransferSagaStartEvent.ScheduleTransferRequest e ->
      dep.getSchedulingRef ()
      <! SchedulerMessage.ScheduleDomesticTransfer e.Data.BaseInfo

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
   (previousState: DomesticTransferSaga)
   (currentState: DomesticTransferSaga)
   (evt: DomesticTransferSagaEvent)
   =
   let info = currentState.TransferInfo
   let correlationId = TransferId.toCorrelationId info.TransferId

   let transfer = {
      Sender = info.Sender
      TransferId = info.TransferId
      Recipient = info.Recipient
      InitiatedBy = info.InitiatedBy
      Amount = info.Amount
      ScheduledDate = info.ScheduledDate
      Memo = info.Memo
      Status = currentState.Status
   }

   let deductFromSenderAccount () =
      let cmd =
         DomesticTransferCommand.create correlationId info.InitiatedBy {
            Amount = info.Amount
            Sender = info.Sender
            Recipient = info.Recipient
            Memo = info.Memo
            ScheduledDateSeedOverride = None
            OriginatedFromSchedule = currentState.OriginatedFromSchedule
         }

      let msg =
         cmd |> AccountCommand.DomesticTransfer |> AccountMessage.StateChange

      dep.getAccountRef info.Sender.ParentAccountId <! msg

   let sendTransferToProcessorService () =
      let msg =
         ServiceMessage.TransferRequest(ServiceAction.TransferAck, transfer)

      dep.getDomesticTransferRef () <! msg

   let checkOnTransferProgress () =
      let msg =
         ServiceMessage.TransferRequest(ServiceAction.ProgressCheck, transfer)

      dep.getDomesticTransferRef () <! msg

   let updateTransferProgress progress =
      let cmd =
         UpdateDomesticTransferProgressCommand.create
            correlationId
            info.InitiatedBy
            {
               BaseInfo = info
               InProgressInfo = progress
            }

      let msg =
         cmd
         |> AccountCommand.UpdateDomesticTransferProgress
         |> AccountMessage.StateChange

      dep.getAccountRef info.Sender.ParentAccountId <! msg

   let updateTransferAsComplete () =
      let cmd =
         SettleDomesticTransferCommand.create correlationId info.InitiatedBy {
            BaseInfo = info
            FromRetry = currentState.ReasonForRetryServiceAck
         }

      let msg =
         cmd
         |> AccountCommand.SettleDomesticTransfer
         |> AccountMessage.StateChange

      dep.getAccountRef info.Sender.ParentAccountId <! msg

   let sendTransferInitiatedEmail () =
      let emailMsg =
         EmailMessage.create
            info.Sender.OrgId
            (TransferId.toCorrelationId info.TransferId)
            (EmailInfo.DomesticTransfer {
               SenderAccountName = info.Sender.Name
               RecipientName = info.Recipient.FullName
               Amount = info.Amount
            })

      dep.getEmailRef () <! emailMsg

   let refundSenderAccount reason =
      let msg =
         FailDomesticTransferCommand.create correlationId info.InitiatedBy {
            BaseInfo = info
            Reason = reason
         }
         |> AccountCommand.FailDomesticTransfer
         |> AccountMessage.StateChange

      dep.getAccountRef info.Sender.ParentAccountId <! msg

   match evt with
   | DomesticTransferSagaEvent.ScheduledJobCreated -> ()
   | DomesticTransferSagaEvent.ScheduledJobExecuted ->
      deductFromSenderAccount ()
   | DomesticTransferSagaEvent.SenderAccountDeductedFunds ->
      sendTransferToProcessorService ()
   | DomesticTransferSagaEvent.SenderAccountUnableToDeductFunds _ -> ()
   | DomesticTransferSagaEvent.TransferProcessorProgressUpdate progress ->
      match progress with
      | DomesticTransferThirdPartyUpdate.ServiceAckReceived ->
         if currentState.ReasonForRetryServiceAck.IsNone then
            updateTransferProgress progress

         sendTransferInitiatedEmail ()
      | DomesticTransferThirdPartyUpdate.ProgressDetail _ -> ()
      | DomesticTransferThirdPartyUpdate.Settled -> updateTransferAsComplete ()
      | DomesticTransferThirdPartyUpdate.Failed reason ->
         if currentState.RequiresTransferServiceDevelopmentFix then
            dep.logError $"Transfer API requires code update: {reason}"

            let msg =
               EmailMessage.create
                  info.Sender.OrgId
                  (TransferId.toCorrelationId info.TransferId)
                  (EmailInfo.ApplicationErrorRequiresSupport(string reason))

            dep.getEmailRef () <! msg
         elif currentState.RequiresAccountRefund then
            DomesticTransferFailReason.ThirdParty reason |> refundSenderAccount
   | DomesticTransferSagaEvent.SenderAccountRefunded -> ()
   | DomesticTransferSagaEvent.TransferMarkedAsSettled -> ()
   | DomesticTransferSagaEvent.ResetInProgressActivityAttempts -> ()
   | DomesticTransferSagaEvent.TransferInitiatedNotificationSent -> ()
   | DomesticTransferSagaEvent.RetryTransferServiceRequest _ ->
      // Development team provided fix for data incompatibility issue when
      // issuing a transfer request to the third party transfer processor.
      sendTransferToProcessorService ()
   | DomesticTransferSagaEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.ScheduleTransfer ->
            dep.getSchedulingRef ()
            <! SchedulerMessage.ScheduleDomesticTransfer info
         | Activity.DeductFromSenderAccount -> deductFromSenderAccount ()
         | Activity.TransferServiceAck -> sendTransferToProcessorService ()
         | Activity.SendTransferInitiatedNotification ->
            sendTransferInitiatedEmail ()
         | Activity.RefundSenderAccount ->
            match currentState.Status with
            | DomesticTransferProgress.Failed reason ->
               refundSenderAccount reason
            | _ -> ()
         | Activity.WaitForTransferServiceComplete -> checkOnTransferProgress ()
         | Activity.MarkTransferAsSettled -> updateTransferAsComplete ()
         | Activity.WaitForScheduledTransferExecution
         | Activity.WaitForDevelopmentTeamFix -> ()
