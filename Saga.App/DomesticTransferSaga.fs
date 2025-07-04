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
open TransferMessages

type private ServiceMessage = DomesticTransferServiceMessage

type private ServiceAction =
   DomesticTransfer.Service.Domain.DomesticTransferServiceAction

[<RequireQualifiedAccess>]
type DomesticTransferSagaStartEvent =
   | SenderReservedFunds of BankEvent<DomesticTransferPending>
   | ScheduleTransferRequest of BankEvent<DomesticTransferScheduled>

[<RequireQualifiedAccess>]
type DomesticTransferSagaEvent =
   | ScheduledTransferActivated
   | SenderReservedFunds
   | SenderReleasedReservedFunds
   | TransferProcessorProgressUpdate of DomesticTransferThirdPartyUpdate
   | SenderDeductedFunds
   | TransferInitiatedNotificationSent
   | RetryTransferServiceRequest of
      updatedRecipient: DomesticTransferRecipient option
   | SenderUnableToReserveFunds of DomesticTransferFailReason
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess; CustomEquality; NoComparison>]
type Activity =
   | WaitForScheduledTransferActivation of TimeSpan
   | ReserveSenderFunds
   | ReleaseSenderReservedFunds
   | TransferServiceAck
   | WaitForTransferServiceComplete
   | DeductSenderFunds
   | SendTransferInitiatedNotification
   | WaitForDevelopmentTeamFix

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForScheduledTransferActivation _
         | WaitForDevelopmentTeamFix -> 0
         // Check every 4 hours, 6 times a day for 6 days.
         | WaitForTransferServiceComplete -> 36
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | WaitForScheduledTransferActivation time -> Some time
         | WaitForDevelopmentTeamFix -> None
         | WaitForTransferServiceComplete ->
            Some(
               if Env.isProd then
                  TimeSpan.FromHours 4.
               else
                  TimeSpan.FromMinutes 1.
            )
         | TransferServiceAck
         | SendTransferInitiatedNotification -> Some(TimeSpan.FromMinutes 4.)
         | ReserveSenderFunds
         | ReleaseSenderReservedFunds
         | DeductSenderFunds -> Some(TimeSpan.FromSeconds 5.)

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

type DomesticTransferSaga = {
   StartEvent: DomesticTransferSagaStartEvent
   Events: DomesticTransferSagaEvent list
   Status: DomesticTransferProgress
   TransferInfo: BaseDomesticTransferInfo
   ExpectedSettlementDate: DateTime
   LifeCycle: SagaLifeCycle<Activity>
   ReasonForRetryServiceAck: DomesticTransferFailReason option
} with

   member x.OriginatedFromSchedule = x.StartEvent.IsScheduleTransferRequest

   member x.SenderDeductedFunds =
      x.LifeCycle.Completed |> List.exists _.Activity.IsDeductSenderFunds

   member x.TransferInitiatedNotificationSent =
      x.LifeCycle.Completed
      |> List.exists _.Activity.IsSendTransferInitiatedNotification

   member x.RequiresAccountRefund =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsReleaseSenderReservedFunds

   member x.RequiresTransferServiceDevelopmentFix =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsWaitForDevelopmentTeamFix

   member x.IsTransferSchedulingAwaitingActivation =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsWaitForScheduledTransferActivation

let applyStartEvent
   (start: DomesticTransferSagaStartEvent)
   (timestamp: DateTime)
   : DomesticTransferSaga
   =
   match start with
   | DomesticTransferSagaStartEvent.SenderReservedFunds evt -> {
      Status = DomesticTransferProgress.WaitingForTransferServiceAck
      StartEvent = start
      Events = []
      TransferInfo = evt.Data.BaseInfo
      ExpectedSettlementDate = evt.Data.ExpectedSettlementDate
      ReasonForRetryServiceAck = None
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init timestamp Activity.TransferServiceAck
               ActivityLifeCycle.init
                  timestamp
                  Activity.WaitForTransferServiceComplete
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
   | DomesticTransferSagaStartEvent.ScheduleTransferRequest evt -> {
      Status = DomesticTransferProgress.Scheduled
      StartEvent = start
      Events = []
      TransferInfo = evt.Data.BaseInfo
      ExpectedSettlementDate = evt.Data.ExpectedSettlementDate
      ReasonForRetryServiceAck = None
      LifeCycle =
         let timeUntil = evt.Data.BaseInfo.ScheduledDate - DateTime.UtcNow

         SagaLifeCycle.empty
         |> SagaLifeCycle.addActivity
               timestamp
               (Activity.WaitForScheduledTransferActivation timeUntil)
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
   | DomesticTransferSagaEvent.ScheduledTransferActivated -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity (
               Activity.WaitForScheduledTransferActivation TimeSpan.Zero
            )
            |> addActivity Activity.ReserveSenderFunds
         Status = DomesticTransferProgress.ProcessingSenderAccountDeduction
     }
   | DomesticTransferSagaEvent.SenderReservedFunds -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.ReserveSenderFunds
            |> addActivity Activity.TransferServiceAck
            |> addActivity Activity.WaitForTransferServiceComplete
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
        }
      | DomesticTransferThirdPartyUpdate.ProgressDetail detail -> {
         saga with
            Status = DomesticTransferProgress.ThirdParty progress
            ExpectedSettlementDate =
               if
                  detail.ExpectedSettlementDate.Date
                  <> saga.ExpectedSettlementDate.Date
               then
                  detail.ExpectedSettlementDate
               else
                  saga.ExpectedSettlementDate
        }
      | DomesticTransferThirdPartyUpdate.Settled -> {
         saga with
            Status = DomesticTransferProgress.ThirdParty progress
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.WaitForTransferServiceComplete
               |> addActivity Activity.DeductSenderFunds
        }
      | DomesticTransferThirdPartyUpdate.Failed reason ->
         let saga = {
            saga with
               LifeCycle =
                  saga.LifeCycle
                  |> failActivity Activity.TransferServiceAck
                  |> failActivity Activity.WaitForTransferServiceComplete
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
                  |> addActivity Activity.WaitForDevelopmentTeamFix
           }
         | DomesticTransferThirdPartyFailReason.NoTransferFound
         | DomesticTransferThirdPartyFailReason.InvalidAmount
         | DomesticTransferThirdPartyFailReason.RecipientAccountInvalidInfo
         | DomesticTransferThirdPartyFailReason.RecipientAccountNotActive ->
            let refundIfNotRetrying =
               if saga.ReasonForRetryServiceAck.IsSome then
                  id
               else
                  addActivity Activity.ReleaseSenderReservedFunds

            {
               saga with
                  LifeCycle = refundIfNotRetrying saga.LifeCycle
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
         LifeCycle =
            saga.LifeCycle
            |> addActivity Activity.TransferServiceAck
            |> addActivity Activity.WaitForTransferServiceComplete
     }
   | DomesticTransferSagaEvent.SenderDeductedFunds -> {
      saga with
         LifeCycle = saga.LifeCycle |> finishActivity Activity.DeductSenderFunds
         Status =
            if saga.TransferInitiatedNotificationSent then
               DomesticTransferProgress.Settled
            else
               saga.Status
     }
   | DomesticTransferSagaEvent.TransferInitiatedNotificationSent -> {
      saga with
         Status =
            if saga.SenderDeductedFunds then
               DomesticTransferProgress.Settled
            else
               saga.Status
         LifeCycle =
            finishActivity
               Activity.SendTransferInitiatedNotification
               saga.LifeCycle
     }
   | DomesticTransferSagaEvent.SenderUnableToReserveFunds reason -> {
      saga with
         LifeCycle = saga.LifeCycle |> failActivity Activity.ReserveSenderFunds
         Status = DomesticTransferProgress.Failed reason
     }
   | DomesticTransferSagaEvent.SenderReleasedReservedFunds -> {
      saga with
         LifeCycle =
            finishActivity Activity.ReleaseSenderReservedFunds saga.LifeCycle
     }
   | DomesticTransferSagaEvent.EvaluateRemainingWork ->
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
      | DomesticTransferSagaEvent.ScheduledTransferActivated ->
         activityIsDone (
            Activity.WaitForScheduledTransferActivation TimeSpan.Zero
         )
      | DomesticTransferSagaEvent.SenderReservedFunds ->
         activityIsDone Activity.ReserveSenderFunds
      | DomesticTransferSagaEvent.TransferProcessorProgressUpdate _ ->
         activityIsDone Activity.WaitForTransferServiceComplete
      | DomesticTransferSagaEvent.RetryTransferServiceRequest _ ->
         saga.LifeCycle.ActivityHasFailed Activity.TransferServiceAck |> not
      | DomesticTransferSagaEvent.SenderDeductedFunds ->
         activityIsDone Activity.DeductSenderFunds
      | DomesticTransferSagaEvent.TransferInitiatedNotificationSent ->
         activityIsDone Activity.SendTransferInitiatedNotification
      | DomesticTransferSagaEvent.SenderReleasedReservedFunds ->
         activityIsDone Activity.ReleaseSenderReservedFunds
      | _ -> false

   if saga.Status = DomesticTransferProgress.Settled then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

let onStartEventPersisted
   (getDomesticTransferRef: unit -> IActorRef<ServiceMessage>)
   (evt: DomesticTransferSagaStartEvent)
   =
   match evt with
   | DomesticTransferSagaStartEvent.SenderReservedFunds e ->
      let transfer = TransferEventToDomesticTransfer.fromPending e

      let msg =
         ServiceMessage.TransferRequest(ServiceAction.TransferAck, transfer)

      getDomesticTransferRef () <! msg
   | DomesticTransferSagaStartEvent.ScheduleTransferRequest _ -> ()

type PersistenceHandlerDependencies = {
   getSchedulingRef: unit -> IActorRef<SchedulerMessage>
   getDomesticTransferRef: unit -> IActorRef<ServiceMessage>
   getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   sendEventToSelf:
      BaseDomesticTransferInfo -> DomesticTransferSagaEvent -> unit
   logError: string -> unit
}

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
      ExpectedSettlementDate = currentState.ExpectedSettlementDate
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

   let updateTransferProgress (progress: DomesticTransferThirdPartyUpdate) =
      let cmd =
         UpdateDomesticTransferProgressCommand.create
            correlationId
            info.InitiatedBy
            {
               BaseInfo = info
               InProgressInfo = progress
               NewExpectedSettlementDate =
                  match progress with
                  | DomesticTransferThirdPartyUpdate.ProgressDetail p ->
                     Some p.ExpectedSettlementDate
                  | _ -> None
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

   let releaseSenderAccountReservedFunds reason =
      let msg =
         FailDomesticTransferCommand.create correlationId info.InitiatedBy {
            BaseInfo = info
            Reason = reason
         }
         |> AccountCommand.FailDomesticTransfer
         |> AccountMessage.StateChange

      dep.getAccountRef info.Sender.ParentAccountId <! msg

   match evt with
   | DomesticTransferSagaEvent.ScheduledTransferActivated ->
      deductFromSenderAccount ()
   | DomesticTransferSagaEvent.SenderReservedFunds ->
      sendTransferToProcessorService ()
   | DomesticTransferSagaEvent.SenderUnableToReserveFunds _ -> ()
   | DomesticTransferSagaEvent.TransferProcessorProgressUpdate progress ->
      match progress with
      | DomesticTransferThirdPartyUpdate.ServiceAckReceived ->
         if currentState.ReasonForRetryServiceAck.IsNone then
            updateTransferProgress progress

         sendTransferInitiatedEmail ()
      | DomesticTransferThirdPartyUpdate.ProgressDetail detail ->
         if
            detail.ExpectedSettlementDate.Date
            <> previousState.ExpectedSettlementDate.Date
         then
            updateTransferProgress progress
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
            DomesticTransferFailReason.ThirdParty reason
            |> releaseSenderAccountReservedFunds
   | DomesticTransferSagaEvent.SenderReleasedReservedFunds -> ()
   | DomesticTransferSagaEvent.SenderDeductedFunds -> ()
   | DomesticTransferSagaEvent.ResetInProgressActivityAttempts -> ()
   | DomesticTransferSagaEvent.TransferInitiatedNotificationSent -> ()
   | DomesticTransferSagaEvent.RetryTransferServiceRequest _ ->
      // Development team provided fix for data incompatibility issue when
      // issuing a transfer request to the third party transfer processor.
      sendTransferToProcessorService ()
   | DomesticTransferSagaEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.ReserveSenderFunds -> deductFromSenderAccount ()
         | Activity.TransferServiceAck -> sendTransferToProcessorService ()
         | Activity.SendTransferInitiatedNotification ->
            sendTransferInitiatedEmail ()
         | Activity.ReleaseSenderReservedFunds ->
            match currentState.Status with
            | DomesticTransferProgress.Failed reason ->
               releaseSenderAccountReservedFunds reason
            | _ -> ()
         | Activity.WaitForTransferServiceComplete -> checkOnTransferProgress ()
         | Activity.DeductSenderFunds -> updateTransferAsComplete ()
         | Activity.WaitForScheduledTransferActivation _ ->
            dep.sendEventToSelf
               currentState.TransferInfo
               DomesticTransferSagaEvent.ScheduledTransferActivated
         | Activity.WaitForDevelopmentTeamFix -> ()
