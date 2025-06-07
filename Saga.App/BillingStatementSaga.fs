module BillingSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Bank.Account.Domain
open MaintenanceFee
open Email
open Lib.Saga

[<RequireQualifiedAccess>]
type BillingSagaStatus =
   | InProgress
   | Completed
   | Failed

type BillingSagaStartEvent = {
   BillingPeriod: BillingPeriod
   BillingCycleDate: DateTime
   CorrelationId: CorrelationId
   ParentAccountId: ParentAccountId
   OrgId: OrgId
}

type ProcessingBillingStatement = {
   PrimaryCheckingAccountId: AccountId
   MaintenanceFeeCriteria: MaintenanceFeeCriteria
}

[<RequireQualifiedAccess>]
type BillingSagaEvent =
   | Start of BillingSagaStartEvent
   | BillingStatementProcessing of ProcessingBillingStatement
   | BillingStatementPersisted
   | MaintenanceFeeProcessed
   | BillingEmailSent
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess>]
type Activity =
   | StartBilling
   | ProcessBillingStatement
   | WaitForBillingStatementPersisted
   | ProcessMaintenanceFee
   | SendBillingEmail

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | StartBilling
         | WaitForBillingStatementPersisted -> 1
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | StartBilling
         | WaitForBillingStatementPersisted -> None
         | ProcessBillingStatement -> Some(TimeSpan.FromSeconds 10)
         | ProcessMaintenanceFee -> Some(TimeSpan.FromSeconds 10)
         | SendBillingEmail -> Some(TimeSpan.FromMinutes 4)

type BillingSaga = {
   CorrelationId: CorrelationId
   ParentAccountId: ParentAccountId
   OrgId: OrgId
   BillingPeriod: BillingPeriod
   BillingCycleDate: DateTime
   ProcessingBillingStatement: ProcessingBillingStatement option
   Events: BillingSagaEvent list
   Status: BillingSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
}

let applyEvent
   (state: BillingSaga option)
   (e: BillingSagaEvent)
   (timestamp: DateTime)
   : BillingSaga option
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp

   match state with
   | None ->
      match e with
      | BillingSagaEvent.Start start ->
         Some {
            Status = BillingSagaStatus.InProgress
            Events = [ e ]
            ProcessingBillingStatement = None
            CorrelationId = start.CorrelationId
            ParentAccountId = start.ParentAccountId
            OrgId = start.OrgId
            BillingPeriod = start.BillingPeriod
            BillingCycleDate = start.BillingCycleDate
            LifeCycle = {
               SagaLifeCycle.empty with
                  InProgress = [
                     ActivityLifeCycle.init
                        timestamp
                        Activity.ProcessBillingStatement
                     ActivityLifeCycle.init
                        timestamp
                        Activity.WaitForBillingStatementPersisted
                  ]
                  Completed = [
                     {
                        Start = timestamp
                        End = Some timestamp
                        Activity = Activity.StartBilling
                        MaxAttempts =
                           (Activity.StartBilling :> IActivity).MaxAttempts
                        Attempts = 1
                     }
                  ]
            }
         }
      | _ -> state
   | Some state ->
      let state =
         match e with
         | BillingSagaEvent.Start _ -> state
         | BillingSagaEvent.BillingStatementProcessing processing -> {
            state with
               ProcessingBillingStatement = Some processing
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.ProcessBillingStatement
           }
         | BillingSagaEvent.BillingStatementPersisted -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.WaitForBillingStatementPersisted
                  |> addActivity Activity.ProcessMaintenanceFee
           }
         | BillingSagaEvent.MaintenanceFeeProcessed -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.ProcessMaintenanceFee
                  |> addActivity Activity.SendBillingEmail
           }
         | BillingSagaEvent.BillingEmailSent -> {
            state with
               Status = BillingSagaStatus.Completed
               LifeCycle =
                  state.LifeCycle |> finishActivity Activity.SendBillingEmail
           }
         | BillingSagaEvent.EvaluateRemainingWork -> {
            state with
               LifeCycle =
                  SagaLifeCycle.retryActivitiesAfterInactivity
                     timestamp
                     state.LifeCycle
           }
         | BillingSagaEvent.ResetInProgressActivityAttempts -> {
            state with
               LifeCycle =
                  SagaLifeCycle.resetInProgressActivities state.LifeCycle
           }

      Some {
         state with
            Events = e :: state.Events
      }

let stateTransition
   (state: BillingSaga option)
   (evt: BillingSagaEvent)
   (timestamp: DateTime)
   : Result<BillingSaga option, SagaStateTransitionError>
   =
   match state with
   | None ->
      match evt with
      | BillingSagaEvent.Start _ -> Ok(applyEvent state evt timestamp)
      | _ -> Error SagaStateTransitionError.HasNotStarted
   | Some saga ->
      let eventIsStartEvent =
         match evt with
         | BillingSagaEvent.Start _ -> true
         | _ -> false

      let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

      let invalidStepProgression =
         match evt with
         | BillingSagaEvent.Start _
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
      elif eventIsStartEvent then
         Error SagaStateTransitionError.HasAlreadyStarted
      elif invalidStepProgression then
         Error SagaStateTransitionError.InvalidStepProgression
      else
         Ok(applyEvent state evt timestamp)

type PersistenceHandlerDependencies = {
   getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
}

let onStartEventPersisted
   (dep: PersistenceHandlerDependencies)
   (start: BillingSagaStartEvent)
   =
   let msg =
      AccountMessage.ProcessBillingStatement(
         start.CorrelationId,
         start.BillingPeriod
      )

   dep.getAccountRef start.ParentAccountId <! msg

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
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
      dep.getEmailRef () <! emailMsg

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

         dep.getAccountRef parentAccountId <! msg
      else
         let msg =
            MaintenanceFeeCommand.create
               compositeId
               corrId
               updatedState.BillingCycleDate
            |> AccountCommand.MaintenanceFee
            |> AccountMessage.StateChange

         dep.getAccountRef parentAccountId <! msg

   match evt with
   | BillingSagaEvent.BillingStatementPersisted ->
      updatedState.ProcessingBillingStatement
      |> Option.iter processMaintenanceFee
   | BillingSagaEvent.MaintenanceFeeProcessed -> sendBillingEmail ()
   | BillingSagaEvent.Start _
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

            dep.getAccountRef parentAccountId <! msg
         | Activity.ProcessMaintenanceFee ->
            updatedState.ProcessingBillingStatement
            |> Option.iter processMaintenanceFee
         | Activity.SendBillingEmail -> sendBillingEmail ()
