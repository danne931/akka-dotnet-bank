module DomesticTransferSaga

open System
open Akkling

open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Transfer.Domain
open EmailMessage
open Lib.Saga
open DomesticTransferSaga
open BankActorRegistry
open PartnerBank.Service.Domain

let applyStartEvent
   (start: DomesticTransferSagaStartEvent)
   (timestamp: DateTime)
   : DomesticTransferSaga
   =
   match start with
   | DomesticTransferSagaStartEvent.SenderReservedFunds(evt, link) -> {
      Status = DomesticTransferProgress.WaitingForTransferServiceAck
      StartEvent = start
      StartedAt = timestamp
      Events = []
      TransferInfo = evt.Data.BaseInfo
      PartnerBankAccountLink = link
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
   | DomesticTransferSagaStartEvent.ScheduleTransferRequest(evt, link) -> {
      Status = DomesticTransferProgress.Scheduled
      StartEvent = start
      StartedAt = timestamp
      Events = []
      TransferInfo = evt.Data.BaseInfo
      PartnerBankAccountLink = link
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
   let retryActivity = SagaLifeCycle.retryActivity timestamp

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
         Status = DomesticTransferProgress.ProcessingAccountDeduction
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
   | DomesticTransferSagaEvent.PartnerBankProgressUpdate progress ->
      match progress with
      | DomesticTransferPartnerBankUpdate.ServiceAckReceived -> {
         saga with
            Status = DomesticTransferProgress.PartnerBank progress
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.TransferServiceAck
               |> addActivity Activity.SendTransferInitiatedNotification
        }
      | DomesticTransferPartnerBankUpdate.ProgressDetail detail -> {
         saga with
            Status = DomesticTransferProgress.PartnerBank progress
            ExpectedSettlementDate =
               if
                  detail.ExpectedSettlementDate.Date
                  <> saga.ExpectedSettlementDate.Date
               then
                  detail.ExpectedSettlementDate
               else
                  saga.ExpectedSettlementDate
        }
      | DomesticTransferPartnerBankUpdate.Settled -> {
         saga with
            Status = DomesticTransferProgress.PartnerBank progress
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.WaitForTransferServiceComplete
               |> addActivity Activity.DeductSenderFunds
        }
      | DomesticTransferPartnerBankUpdate.Failed reason ->
         let saga = {
            saga with
               LifeCycle =
                  saga.LifeCycle
                  |> failActivity Activity.TransferServiceAck
                  |> failActivity Activity.WaitForTransferServiceComplete
               Status =
                  reason
                  |> DomesticTransferFailReason.PartnerBank
                  |> DomesticTransferProgress.Failed
         }

         match reason with
         | DomesticTransferPartnerBankFailReason.Infra _ ->
            let activity = Activity.WaitForDevelopmentTeamFix

            let life =
               if saga.LifeCycle.ActivityIsInProgress activity then
                  retryActivity activity saga.LifeCycle
               else
                  addActivity activity saga.LifeCycle

            { saga with LifeCycle = life }
         | DomesticTransferPartnerBankFailReason.NoTransferFound
         | DomesticTransferPartnerBankFailReason.InvalidAmount
         | DomesticTransferPartnerBankFailReason.SenderAccountNotActive
         | DomesticTransferPartnerBankFailReason.CounterpartyAccountInvalidInfo
         | DomesticTransferPartnerBankFailReason.CounterpartyAccountNotActive ->
            let life =
               saga.LifeCycle
               |> addActivity Activity.ReleaseSenderReservedFunds
               |> finishActivity Activity.WaitForDevelopmentTeamFix

            { saga with LifeCycle = life }
   | DomesticTransferSagaEvent.RetryTransferServiceRequest updated -> {
      saga with
         ReasonForRetryServiceAck =
            match saga.Status with
            | DomesticTransferProgress.Failed reason -> Some reason
            | _ -> None
         Status = DomesticTransferProgress.WaitingForTransferServiceAck
         TransferInfo.Counterparty =
            updated |> Option.defaultValue saga.TransferInfo.Counterparty
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
      | DomesticTransferSagaEvent.PartnerBankProgressUpdate _ ->
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
   (registry: #IPartnerBankServiceActor)
   (evt: DomesticTransferSagaStartEvent)
   =
   match evt with
   | DomesticTransferSagaStartEvent.SenderReservedFunds(e, link) ->
      let info = e.Data.BaseInfo

      let msg =
         PartnerBankServiceMessage.TransferDomestic {
            Action = DomesticTransferServiceAction.TransferAck
            OriginatingAccountId = link.PartnerBankAccountId
            CounterpartyId = info.Counterparty.PartnerBankCounterpartyId
            Direction = info.MoneyFlow
            Amount = info.Amount
            PaymentNetwork = info.Counterparty.PaymentNetwork
            Date = info.ScheduledDate
            Status = DomesticTransferProgress.WaitingForTransferServiceAck
            TransferId = info.TransferId
            SagaMetadata = {
               OrgId = info.Originator.OrgId
               CorrelationId = e.CorrelationId
            }
         }

      registry.PartnerBankServiceActor() <! msg
   | DomesticTransferSagaStartEvent.ScheduleTransferRequest _ -> ()

type OperationEnv = {
   sendEventToSelf:
      BaseDomesticTransferInfo -> DomesticTransferSagaEvent -> unit
   logError: string -> unit
}

let onEventPersisted
   (broadcaster: SignalRBroadcast.SignalRBroadcast)
   (registry:
      #IPartnerBankServiceActor & #IAccountActor & #IEmailActor & #ISchedulerActor)
   (operationEnv: OperationEnv)
   (previousState: DomesticTransferSaga)
   (currentState: DomesticTransferSaga)
   (evt: DomesticTransferSagaEvent)
   =
   broadcaster.sagaUpdated (AppSaga.Saga.DomesticTransfer currentState).AsDTO

   let info = currentState.TransferInfo
   let correlationId = info.TransferId.AsCorrelationId

   let deductFromSenderAccount () =
      let cmd =
         DomesticTransferCommand.create correlationId info.InitiatedBy {
            Amount = info.Amount
            Originator = info.Originator
            Counterparty = info.Counterparty
            MoneyFlow = info.MoneyFlow
            Memo = info.Memo
            ScheduledDateSeedOverride = None
            OriginatedFromSchedule = currentState.OriginatedFromSchedule
         }

      let msg =
         cmd |> AccountCommand.DomesticTransfer |> AccountMessage.StateChange

      registry.AccountActor info.Originator.ParentAccountId <! msg

   let sendTransferToProcessorService action =
      let msg =
         PartnerBankServiceMessage.TransferDomestic {
            Action = action
            OriginatingAccountId =
               currentState.PartnerBankAccountLink.PartnerBankAccountId
            CounterpartyId = info.Counterparty.PartnerBankCounterpartyId
            Direction = info.MoneyFlow
            Amount = info.Amount
            PaymentNetwork = info.Counterparty.PaymentNetwork
            Date = info.ScheduledDate
            Status = currentState.Status
            TransferId = info.TransferId
            SagaMetadata = {
               OrgId = info.Originator.OrgId
               CorrelationId = correlationId
            }
         }

      registry.PartnerBankServiceActor() <! msg

   let checkOnTransferProgress () =
      sendTransferToProcessorService DomesticTransferServiceAction.ProgressCheck

   let updateTransferProgress (progress: DomesticTransferPartnerBankUpdate) =
      let cmd =
         UpdateDomesticTransferProgressCommand.create
            correlationId
            info.InitiatedBy
            {
               BaseInfo = info
               InProgressInfo = progress
               NewExpectedSettlementDate =
                  match progress with
                  | DomesticTransferPartnerBankUpdate.ProgressDetail p ->
                     Some p.ExpectedSettlementDate
                  | _ -> None
            }

      let msg =
         cmd
         |> AccountCommand.UpdateDomesticTransferProgress
         |> AccountMessage.StateChange

      registry.AccountActor info.Originator.ParentAccountId <! msg

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

      registry.AccountActor info.Originator.ParentAccountId <! msg

   let sendTransferInitiatedEmail () =
      let emailMsg =
         EmailMessage.create
            info.Originator.OrgId
            info.TransferId.AsCorrelationId
            (EmailInfo.DomesticTransfer {
               SenderAccountName = info.Originator.Name
               RecipientName = info.Counterparty.FullName
               Amount = info.Amount
            })

      registry.EmailActor() <! emailMsg

   let releaseSenderAccountReservedFunds reason =
      let msg =
         FailDomesticTransferCommand.create correlationId info.InitiatedBy {
            BaseInfo = info
            Reason = reason
         }
         |> AccountCommand.FailDomesticTransfer
         |> AccountMessage.StateChange

      registry.AccountActor info.Originator.ParentAccountId <! msg

   match evt with
   | DomesticTransferSagaEvent.ScheduledTransferActivated ->
      deductFromSenderAccount ()
   | DomesticTransferSagaEvent.SenderReservedFunds ->
      sendTransferToProcessorService DomesticTransferServiceAction.TransferAck
   | DomesticTransferSagaEvent.SenderUnableToReserveFunds _ -> ()
   | DomesticTransferSagaEvent.PartnerBankProgressUpdate progress ->
      match progress with
      | DomesticTransferPartnerBankUpdate.ServiceAckReceived ->
         if currentState.ReasonForRetryServiceAck.IsNone then
            updateTransferProgress progress

         sendTransferInitiatedEmail ()
      | DomesticTransferPartnerBankUpdate.ProgressDetail detail ->
         if
            detail.ExpectedSettlementDate.Date
            <> previousState.ExpectedSettlementDate.Date
         then
            updateTransferProgress progress
      | DomesticTransferPartnerBankUpdate.Settled -> updateTransferAsComplete ()
      | DomesticTransferPartnerBankUpdate.Failed reason ->
         if currentState.RequiresTransferServiceDevelopmentFix then
            operationEnv.logError $"Transfer API requires code update: {reason}"

            let msg =
               EmailMessage.create
                  info.Originator.OrgId
                  info.TransferId.AsCorrelationId
                  (EmailInfo.ApplicationErrorRequiresSupport(string reason))

            registry.EmailActor() <! msg
         elif currentState.RequiresAccountRefund then
            DomesticTransferFailReason.PartnerBank reason
            |> releaseSenderAccountReservedFunds
   | DomesticTransferSagaEvent.SenderReleasedReservedFunds -> ()
   | DomesticTransferSagaEvent.SenderDeductedFunds -> ()
   | DomesticTransferSagaEvent.ResetInProgressActivityAttempts -> ()
   | DomesticTransferSagaEvent.TransferInitiatedNotificationSent -> ()
   | DomesticTransferSagaEvent.RetryTransferServiceRequest _ ->
      // Development team provided fix for data incompatibility issue when
      // issuing a transfer request to the third party transfer processor.
      sendTransferToProcessorService DomesticTransferServiceAction.TransferAck
   | DomesticTransferSagaEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.ReserveSenderFunds -> deductFromSenderAccount ()
         | Activity.TransferServiceAck ->
            sendTransferToProcessorService
               DomesticTransferServiceAction.TransferAck
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
            operationEnv.sendEventToSelf
               currentState.TransferInfo
               DomesticTransferSagaEvent.ScheduledTransferActivated
         | Activity.WaitForDevelopmentTeamFix -> ()
