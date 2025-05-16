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

[<RequireQualifiedAccess>]
type DomesticTransferSagaStartEvent =
   | SenderAccountDeductedFunds of BankEvent<DomesticTransferPending>
   | ScheduleTransferRequest of BankEvent<DomesticTransferScheduled>

[<RequireQualifiedAccess>]
type DomesticTransferSagaEvent =
   | Start of DomesticTransferSagaStartEvent
   | ScheduledJobCreated
   | ScheduledJobExecuted
   | SenderAccountDeductedFunds
   | TransferProcessorProgressUpdate of DomesticTransferServiceProgress
   | TransferMarkedAsSettled
   | TransferInitiatedNotificationSent
   | RetryTransferServiceRequest
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
         | WaitForDevelopmentTeamFix -> 1
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
   Events: DomesticTransferSagaEvent list
   Status: DomesticTransferProgress
   TransferInfo: BaseDomesticTransferInfo
   LifeCycle: SagaLifeCycle<Activity>
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

let applyEvent
   (state: DomesticTransferSaga option)
   (e: DomesticTransferSagaEvent)
   (timestamp: DateTime)
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp
   let failActivity = SagaLifeCycle.failActivity timestamp

   match state with
   | None ->
      match e with
      | DomesticTransferSagaEvent.Start evt ->
         match evt with
         | DomesticTransferSagaStartEvent.SenderAccountDeductedFunds evt ->
            Some {
               Status = DomesticTransferProgress.WaitingForTransferServiceAck
               Events = [ e ]
               TransferInfo = evt.Data.BaseInfo
               LifeCycle = {
                  SagaLifeCycle.empty with
                     InProgress = [
                        ActivityLifeCycle.init
                           timestamp
                           Activity.TransferServiceAck
                     ]
                     Completed = [
                        {
                           Start = timestamp
                           End = Some timestamp
                           Activity = Activity.DeductFromSenderAccount
                           MaxAttempts =
                              (Activity.DeductFromSenderAccount :> IActivity)
                                 .MaxAttempts
                           Attempts = 1
                        }
                     ]
               }
            }
         | DomesticTransferSagaStartEvent.ScheduleTransferRequest evt ->
            Some {
               Status = DomesticTransferProgress.Scheduled
               Events = [ e ]
               TransferInfo = evt.Data.BaseInfo
               LifeCycle =
                  SagaLifeCycle.empty |> addActivity Activity.ScheduleTransfer
            }
      | _ -> state
   | Some state ->
      let state =
         match e with
         | DomesticTransferSagaEvent.Start _ -> state
         | DomesticTransferSagaEvent.ScheduledJobCreated -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.ScheduleTransfer
                  |> addActivity Activity.WaitForScheduledTransferExecution
           }
         | DomesticTransferSagaEvent.ScheduledJobExecuted -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.WaitForScheduledTransferExecution
                  |> addActivity Activity.DeductFromSenderAccount
               Status =
                  DomesticTransferProgress.ProcessingSenderAccountDeduction
           }
         | DomesticTransferSagaEvent.SenderAccountDeductedFunds -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.DeductFromSenderAccount
                  |> addActivity Activity.TransferServiceAck
               Status = DomesticTransferProgress.WaitingForTransferServiceAck
           }
         | DomesticTransferSagaEvent.TransferProcessorProgressUpdate progress ->
            match progress with
            | DomesticTransferServiceProgress.InitialHandshakeAck -> {
               state with
                  Status = DomesticTransferProgress.InProgress progress
                  LifeCycle =
                     state.LifeCycle
                     |> finishActivity Activity.TransferServiceAck
                     |> addActivity Activity.SendTransferInitiatedNotification
                     |> addActivity Activity.WaitForTransferServiceComplete
              }
            | DomesticTransferServiceProgress.InProgress _ -> {
               state with
                  Status = DomesticTransferProgress.InProgress progress
              }
            | DomesticTransferServiceProgress.Settled -> {
               state with
                  Status = DomesticTransferProgress.InProgress progress
                  LifeCycle =
                     state.LifeCycle
                     |> finishActivity Activity.WaitForTransferServiceComplete
                     |> addActivity Activity.MarkTransferAsSettled
              }
            | DomesticTransferServiceProgress.Failed reason ->
               let state = {
                  state with
                     Status = DomesticTransferProgress.Failed reason
               }

               match reason with
               | DomesticTransferFailReason.SenderAccountNotActive
               | DomesticTransferFailReason.SenderAccountInsufficientFunds ->
                  state
               | DomesticTransferFailReason.CorruptData
               | DomesticTransferFailReason.InvalidPaymentNetwork
               | DomesticTransferFailReason.InvalidDepository
               | DomesticTransferFailReason.InvalidAction -> {
                  state with
                     LifeCycle =
                        state.LifeCycle
                        |> failActivity Activity.TransferServiceAck
                        |> failActivity Activity.WaitForTransferServiceComplete
                        |> addActivity Activity.WaitForDevelopmentTeamFix
                 }
               | DomesticTransferFailReason.InvalidAmount
               | DomesticTransferFailReason.InvalidAccountInfo
               | DomesticTransferFailReason.AccountClosed
               | DomesticTransferFailReason.Unknown _ -> {
                  state with
                     LifeCycle =
                        state.LifeCycle
                        |> failActivity Activity.TransferServiceAck
                        |> failActivity Activity.WaitForTransferServiceComplete
                        |> addActivity Activity.RefundSenderAccount
                 }
         | DomesticTransferSagaEvent.RetryTransferServiceRequest -> {
            state with
               Status = DomesticTransferProgress.WaitingForTransferServiceAck
               LifeCycle =
                  state.LifeCycle |> addActivity Activity.TransferServiceAck
           }
         | DomesticTransferSagaEvent.TransferMarkedAsSettled -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.MarkTransferAsSettled
               Status =
                  if state.TransferInitiatedNotificationSent then
                     DomesticTransferProgress.Completed
                  else
                     state.Status
           }
         | DomesticTransferSagaEvent.TransferInitiatedNotificationSent -> {
            state with
               Status =
                  if state.TransferMarkedAsSettled then
                     DomesticTransferProgress.Completed
                  else
                     state.Status
               LifeCycle =
                  finishActivity
                     Activity.SendTransferInitiatedNotification
                     state.LifeCycle
           }
         | DomesticTransferSagaEvent.SenderAccountUnableToDeductFunds reason -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> failActivity Activity.DeductFromSenderAccount
               Status = DomesticTransferProgress.Failed reason
           }
         | DomesticTransferSagaEvent.SenderAccountRefunded -> {
            state with
               LifeCycle =
                  finishActivity Activity.RefundSenderAccount state.LifeCycle
           }
         | DomesticTransferSagaEvent.EvaluateRemainingWork -> {
            state with
               LifeCycle =
                  SagaLifeCycle.retryActivitiesAfterInactivity
                     timestamp
                     state.LifeCycle
           }
         | DomesticTransferSagaEvent.ResetInProgressActivityAttempts -> {
            state with
               LifeCycle =
                  SagaLifeCycle.resetInProgressActivities state.LifeCycle
           }

      Some {
         state with
            Events = e :: state.Events
      }

let stateTransition
   (state: DomesticTransferSaga option)
   (evt: DomesticTransferSagaEvent)
   (timestamp: DateTime)
   : Result<DomesticTransferSaga option, SagaStateTransitionError>
   =
   match state with
   | None ->
      match evt with
      | DomesticTransferSagaEvent.Start _ -> Ok(applyEvent state evt timestamp)
      | _ -> Error SagaStateTransitionError.HasNotStarted
   | Some saga ->
      let eventIsStartEvent =
         match evt with
         | DomesticTransferSagaEvent.Start _ -> true
         | _ -> false

      let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

      let invalidStepProgression =
         match evt with
         | DomesticTransferSagaEvent.Start _
         | DomesticTransferSagaEvent.EvaluateRemainingWork
         | DomesticTransferSagaEvent.ResetInProgressActivityAttempts -> false
         | DomesticTransferSagaEvent.ScheduledJobCreated ->
            activityIsDone Activity.ScheduleTransfer
         | DomesticTransferSagaEvent.ScheduledJobExecuted ->
            activityIsDone Activity.WaitForScheduledTransferExecution
         | DomesticTransferSagaEvent.SenderAccountDeductedFunds
         | DomesticTransferSagaEvent.SenderAccountUnableToDeductFunds _ ->
            activityIsDone Activity.DeductFromSenderAccount
         | DomesticTransferSagaEvent.TransferProcessorProgressUpdate _ ->
            activityIsDone Activity.TransferServiceAck
            && activityIsDone Activity.WaitForTransferServiceComplete
         | DomesticTransferSagaEvent.RetryTransferServiceRequest ->
            saga.LifeCycle.ActivityHasFailed(Activity.TransferServiceAck) |> not
         | DomesticTransferSagaEvent.TransferMarkedAsSettled ->
            activityIsDone Activity.MarkTransferAsSettled
         | DomesticTransferSagaEvent.TransferInitiatedNotificationSent ->
            activityIsDone Activity.SendTransferInitiatedNotification
         | DomesticTransferSagaEvent.SenderAccountRefunded ->
            activityIsDone Activity.RefundSenderAccount

      if saga.Status = DomesticTransferProgress.Completed then
         Error SagaStateTransitionError.HasAlreadyCompleted
      elif eventIsStartEvent then
         Error SagaStateTransitionError.HasAlreadyStarted
      elif invalidStepProgression then
         Error SagaStateTransitionError.InvalidStepProgression
      else
         Ok(applyEvent state evt timestamp)

type PersistenceHandlerDependencies = {
   getSchedulingRef: unit -> IActorRef<SchedulerMessage>
   getDomesticTransferRef: unit -> IActorRef<DomesticTransferMessage>
   getAccountRef: AccountId -> IEntityRef<AccountMessage>
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
         DomesticTransferMessage.TransferRequest(
            DomesticTransferServiceAction.TransferAck,
            transfer
         )

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
   let compositeId = info.Sender.AccountId, info.Sender.OrgId
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
         DomesticTransferCommand.create
            compositeId
            correlationId
            info.InitiatedBy
            {
               Amount = info.Amount
               Sender = info.Sender
               Recipient = info.Recipient
               Memo = info.Memo
               ScheduledDateSeedOverride = None
               OriginatedFromSchedule = currentState.OriginatedFromSchedule
            }

      let msg =
         cmd |> AccountCommand.DomesticTransfer |> AccountMessage.StateChange

      dep.getAccountRef info.Sender.AccountId <! msg

   let sendTransferToProcessorService () =
      let msg =
         DomesticTransferMessage.TransferRequest(
            DomesticTransferServiceAction.TransferAck,
            transfer
         )

      dep.getDomesticTransferRef () <! msg

   let checkOnTransferProgress () =
      let msg =
         DomesticTransferMessage.TransferRequest(
            DomesticTransferServiceAction.ProgressCheck,
            transfer
         )

      dep.getDomesticTransferRef () <! msg

   let updateTransferProgress progress =
      let cmd =
         UpdateDomesticTransferProgressCommand.create
            compositeId
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

      dep.getAccountRef info.Sender.AccountId <! msg

   let updateTransferAsComplete () =
      let cmd =
         CompleteDomesticTransferCommand.create
            compositeId
            correlationId
            info.InitiatedBy
            {
               BaseInfo = info
               // Will be overwritten during the account actor state transition
               // upon detecting a previously failed transfer by TransferId.
               FromRetry = None
            }

      let msg =
         cmd
         |> AccountCommand.CompleteDomesticTransfer
         |> AccountMessage.StateChange

      dep.getAccountRef info.Sender.AccountId <! msg

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
         FailDomesticTransferCommand.create
            compositeId
            correlationId
            info.InitiatedBy
            { BaseInfo = info; Reason = reason }
         |> AccountCommand.FailDomesticTransfer
         |> AccountMessage.StateChange

      dep.getAccountRef info.Sender.AccountId <! msg

   match evt with
   | DomesticTransferSagaEvent.Start _ -> ()
   | DomesticTransferSagaEvent.ScheduledJobCreated -> ()
   | DomesticTransferSagaEvent.ScheduledJobExecuted ->
      deductFromSenderAccount ()
   | DomesticTransferSagaEvent.SenderAccountDeductedFunds ->
      sendTransferToProcessorService ()
   | DomesticTransferSagaEvent.SenderAccountUnableToDeductFunds _ -> ()
   | DomesticTransferSagaEvent.TransferProcessorProgressUpdate progress ->
      match progress with
      | DomesticTransferServiceProgress.InitialHandshakeAck ->
         updateTransferProgress progress
         sendTransferInitiatedEmail ()
      | DomesticTransferServiceProgress.InProgress _ -> ()
      | DomesticTransferServiceProgress.Settled -> updateTransferAsComplete ()
      | DomesticTransferServiceProgress.Failed reason ->
         if currentState.RequiresTransferServiceDevelopmentFix then
            dep.logError $"Transfer API requires code update: {reason}"

            let msg =
               EmailMessage.create
                  info.Sender.OrgId
                  (TransferId.toCorrelationId info.TransferId)
                  (EmailInfo.ApplicationErrorRequiresSupport(string reason))

            dep.getEmailRef () <! msg
         elif currentState.RequiresAccountRefund then
            refundSenderAccount reason
   | DomesticTransferSagaEvent.SenderAccountRefunded ->
      (*
      let info = e.Data.BaseInfo

      let failDueToRecipient =
         match e.Data.Reason with
         | DomesticTransferFailReason.InvalidAccountInfo ->
            Some DomesticTransferRecipientFailReason.InvalidAccountInfo
         | DomesticTransferFailReason.AccountClosed ->
            Some DomesticTransferRecipientFailReason.ClosedAccount
         | _ -> None

      match failDueToRecipient with
      | Some failReason ->
         let cmd =
            FailDomesticTransferRecipientCommand.create e.OrgId e.InitiatedBy {
               RecipientId = info.Recipient.RecipientAccountId
               TransferId = info.TransferId
               Reason = failReason
            }
            |> OrgCommand.FailDomesticTransferRecipient

         getOrgRef e.OrgId <! OrgMessage.StateChange cmd
      | None -> ()
      *)
      ()
   | DomesticTransferSagaEvent.TransferMarkedAsSettled ->
      (*
      match e.Data.FromRetry with
      | Some DomesticTransferFailReason.InvalidAccountInfo ->
         let info = e.Data.BaseInfo

         let cmd =
            DomesticTransferRetryConfirmsRecipientCommand.create
               e.OrgId
               e.InitiatedBy
               {
                  RecipientId = info.Recipient.RecipientAccountId
                  TransferId = info.TransferId
               }
            |> OrgCommand.DomesticTransferRetryConfirmsRecipient

         getOrgRef e.OrgId <! OrgMessage.StateChange cmd
      | _ -> ()
      *)
      ()
   | DomesticTransferSagaEvent.ResetInProgressActivityAttempts -> ()
   | DomesticTransferSagaEvent.TransferInitiatedNotificationSent -> ()
   | DomesticTransferSagaEvent.RetryTransferServiceRequest ->
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
