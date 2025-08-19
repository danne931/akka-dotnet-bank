module BillingSaga

open System
open Akkling

open Bank.Account.Domain
open EmailMessage
open Lib.Saga
open BillingSaga
open BankActorRegistry

let applyStartEvent
   (start: BillingSagaStartEvent)
   (timestamp: DateTime)
   : BillingSaga
   =
   {
      Status = BillingSagaStatus.InProgress
      StartEvent = start
      Events = []
      ProcessingBillingStatement = None
      CorrelationId = start.CorrelationId
      ParentAccountId = start.ParentAccountId
      OrgId = start.OrgId
      BillingPeriod = start.BillingPeriod
      BillingCycleDate = start.BillingCycleDate
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init timestamp Activity.ProcessBillingStatement
               ActivityLifeCycle.init
                  timestamp
                  Activity.WaitForBillingStatementPersisted
            ]
            Completed = [
               {
                  Start = timestamp
                  End = Some timestamp
                  Activity = Activity.StartBilling
                  MaxAttempts = (Activity.StartBilling :> IActivity).MaxAttempts
                  Attempts = 1
               }
            ]
      }
   }

let applyEvent
   (saga: BillingSaga)
   (evt: BillingSagaEvent)
   (timestamp: DateTime)
   : BillingSaga
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | BillingSagaEvent.BillingStatementProcessing processing -> {
      saga with
         ProcessingBillingStatement = Some processing
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.ProcessBillingStatement
     }
   | BillingSagaEvent.BillingStatementPersisted -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.WaitForBillingStatementPersisted
            |> addActivity Activity.ProcessMaintenanceFee
     }
   | BillingSagaEvent.MaintenanceFeeProcessed -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.ProcessMaintenanceFee
            |> addActivity Activity.SendBillingEmail
     }
   | BillingSagaEvent.BillingEmailSent -> {
      saga with
         Status = BillingSagaStatus.Completed
         LifeCycle = saga.LifeCycle |> finishActivity Activity.SendBillingEmail
     }
   | BillingSagaEvent.EvaluateRemainingWork -> {
      saga with
         LifeCycle =
            SagaLifeCycle.retryActivitiesAfterInactivity
               timestamp
               saga.LifeCycle
     }
   | BillingSagaEvent.ResetInProgressActivityAttempts -> {
      saga with
         LifeCycle = SagaLifeCycle.resetInProgressActivities saga.LifeCycle
     }

let stateTransitionStart
   (start: BillingSagaStartEvent)
   (timestamp: DateTime)
   : Result<BillingSaga, SagaStateTransitionError>
   =
   Ok(applyStartEvent start timestamp)

let stateTransition
   (saga: BillingSaga)
   (evt: BillingSagaEvent)
   (timestamp: DateTime)
   : Result<BillingSaga, SagaStateTransitionError>
   =
   let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

   let invalidStepProgression =
      match evt with
      | BillingSagaEvent.EvaluateRemainingWork
      | BillingSagaEvent.ResetInProgressActivityAttempts -> false
      | BillingSagaEvent.BillingStatementProcessing _ ->
         activityIsDone Activity.ProcessBillingStatement
      | BillingSagaEvent.BillingStatementPersisted ->
         activityIsDone Activity.WaitForBillingStatementPersisted
      | BillingSagaEvent.MaintenanceFeeProcessed ->
         activityIsDone Activity.ProcessMaintenanceFee
      | BillingSagaEvent.BillingEmailSent ->
         activityIsDone Activity.SendBillingEmail

   if saga.Status = BillingSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

let onStartEventPersisted
   (registry: #IAccountActor)
   (start: BillingSagaStartEvent)
   =
   let msg =
      AccountMessage.ProcessBillingStatement(
         start.CorrelationId,
         start.BillingPeriod
      )

   registry.AccountActor start.ParentAccountId <! msg

let onEventPersisted
   (registry: #IAccountActor & #IEmailActor)
   (previousState: BillingSaga)
   (updatedState: BillingSaga)
   (evt: BillingSagaEvent)
   =
   let parentAccountId = updatedState.ParentAccountId
   let orgId = updatedState.OrgId
   let corrId = updatedState.CorrelationId
   let billingPeriod = updatedState.BillingPeriod

   let sendBillingEmail () =
      let emailMsg = EmailMessage.create orgId corrId EmailInfo.BillingStatement
      registry.EmailActor() <! emailMsg

   let processMaintenanceFee (processing: ProcessingBillingStatement) =
      let criteria = processing.MaintenanceFeeCriteria

      let compositeId =
         processing.PrimaryCheckingAccountId, parentAccountId, orgId

      if processing.MaintenanceFeeCriteria.CanSkipFee then
         let msg =
            SkipMaintenanceFeeCommand.create
               compositeId
               corrId
               criteria
               updatedState.BillingCycleDate
            |> AccountCommand.SkipMaintenanceFee
            |> AccountMessage.StateChange

         registry.AccountActor parentAccountId <! msg
      else
         let msg =
            MaintenanceFeeCommand.create
               compositeId
               corrId
               updatedState.BillingCycleDate
            |> AccountCommand.MaintenanceFee
            |> AccountMessage.StateChange

         registry.AccountActor parentAccountId <! msg

   match evt with
   | BillingSagaEvent.BillingStatementPersisted ->
      updatedState.ProcessingBillingStatement
      |> Option.iter processMaintenanceFee
   | BillingSagaEvent.MaintenanceFeeProcessed -> sendBillingEmail ()
   | BillingSagaEvent.BillingStatementProcessing _
   | BillingSagaEvent.BillingEmailSent
   | BillingSagaEvent.ResetInProgressActivityAttempts -> ()
   | BillingSagaEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.StartBilling
         | Activity.WaitForBillingStatementPersisted -> ()
         | Activity.ProcessBillingStatement ->
            let msg =
               AccountMessage.ProcessBillingStatement(corrId, billingPeriod)

            registry.AccountActor parentAccountId <! msg
         | Activity.ProcessMaintenanceFee ->
            updatedState.ProcessingBillingStatement
            |> Option.iter processMaintenanceFee
         | Activity.SendBillingEmail -> sendBillingEmail ()
