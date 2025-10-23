module EmployeeOnboardingSaga

open System
open Akkling

open Lib.SharedTypes
open Bank.Employee.Domain
open Bank.Org.Domain
open CommandApproval
open EmailMessage
open Lib.Saga
open EmployeeOnboardingSaga
open BankActorRegistry

let applyStartEvent
   (start: EmployeeOnboardingSagaStartEvent)
   (timestamp: DateTime)
   : EmployeeOnboardingSaga
   =
   match start with
   | EmployeeOnboardingSagaStartEvent.EmployeeCreated evt -> {
      Status = EmployeeOnboardingSagaStatus.InProgress
      StartEvent = start
      StartedAt = timestamp
      Events = []
      OrgId = evt.OrgId
      CorrelationId = evt.CorrelationId
      InitiatedBy = evt.InitiatedBy
      EmployeeId = EmployeeId.fromEntityId evt.EntityId
      EmployeeName = $"{evt.Data.FirstName} {evt.Data.LastName}"
      EmployeeEmail = evt.Data.Email
      CardInfo = evt.Data.CardInfo
      InviteToken = evt.Data.InviteToken
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               if evt.Data.OrgRequiresEmployeeInviteApproval.IsSome then
                  ActivityLifeCycle.init
                     timestamp
                     Activity.RequestAccessApproval
               else
                  ActivityLifeCycle.init
                     timestamp
                     Activity.SendEmployeeInviteNotification

                  ActivityLifeCycle.init
                     timestamp
                     Activity.WaitForInviteConfirmation
            ]
            Completed = [
               {
                  Start = timestamp
                  End = Some timestamp
                  Activity = Activity.CreateEmployee
                  MaxAttempts =
                     (Activity.CreateEmployee :> IActivity).MaxAttempts
                  Attempts = 1
               }
            ]
      }
     }
   | EmployeeOnboardingSagaStartEvent.AccountOwnerCreated evt -> {
      Status = EmployeeOnboardingSagaStatus.InProgress
      StartEvent = start
      StartedAt = timestamp
      Events = []
      OrgId = evt.OrgId
      CorrelationId = evt.CorrelationId
      InitiatedBy = evt.InitiatedBy
      EmployeeId = EmployeeId.fromEntityId evt.EntityId
      EmployeeName = $"{evt.Data.FirstName} {evt.Data.LastName}"
      EmployeeEmail = evt.Data.Email
      CardInfo = None
      InviteToken = evt.Data.InviteToken
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init
                  timestamp
                  Activity.SendEmployeeInviteNotification

               ActivityLifeCycle.init
                  timestamp
                  Activity.WaitForInviteConfirmation
            ]
            Completed = [
               {
                  Start = timestamp
                  End = Some timestamp
                  Activity = Activity.CreateEmployee
                  MaxAttempts =
                     (Activity.CreateEmployee :> IActivity).MaxAttempts
                  Attempts = 1
               }
            ]
      }
     }
   | EmployeeOnboardingSagaStartEvent.EmployeeAccessRestored o -> {
      Status = EmployeeOnboardingSagaStatus.InProgress
      StartEvent = start
      StartedAt = timestamp
      Events = []
      OrgId = o.Event.OrgId
      CorrelationId = o.Event.CorrelationId
      InitiatedBy = o.Event.InitiatedBy
      EmployeeId = EmployeeId.fromEntityId o.Event.EntityId
      EmployeeName = o.EmployeeName
      EmployeeEmail = o.EmployeeEmail
      CardInfo = None
      InviteToken = o.Event.Data.InviteToken
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init
                  timestamp
                  Activity.SendEmployeeInviteNotification

               ActivityLifeCycle.init
                  timestamp
                  Activity.WaitForInviteConfirmation
            ]
            Completed = [
               {
                  Start = timestamp
                  End = Some timestamp
                  Activity = Activity.RestoreEmployeeAccess
                  MaxAttempts =
                     (Activity.RestoreEmployeeAccess :> IActivity).MaxAttempts
                  Attempts = 1
               }
            ]
      }
     }

let applyEvent
   (saga: EmployeeOnboardingSaga)
   (evt: EmployeeOnboardingSagaEvent)
   (timestamp: DateTime)
   : EmployeeOnboardingSaga
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | EmployeeOnboardingSagaEvent.InviteTokenRefreshed token ->
      if saga.IsWaitingForInviteConfirmation then
         {
            saga with
               InviteToken = token
               LifeCycle =
                  saga.LifeCycle
                  |> finishActivity Activity.WaitForInviteConfirmation
                  |> addActivity Activity.SendEmployeeInviteNotification
         }
      else
         saga
   | EmployeeOnboardingSagaEvent.InviteNotificationSent -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.SendEmployeeInviteNotification
     }
   | EmployeeOnboardingSagaEvent.AccessRequestPending -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.RequestAccessApproval
            |> addActivity Activity.WaitForAccessApproval
     }
   | EmployeeOnboardingSagaEvent.AccessApproved -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.WaitForAccessApproval
            |> addActivity Activity.SendEmployeeInviteNotification
            |> addActivity Activity.WaitForInviteConfirmation
     }
   | EmployeeOnboardingSagaEvent.InviteCancelled reason -> {
      saga with
         Status = EmployeeOnboardingSagaStatus.Aborted reason
         LifeCycle = SagaLifeCycle.abortActivities timestamp saga.LifeCycle
     }
   | EmployeeOnboardingSagaEvent.InviteConfirmed ->
      let saga = {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.WaitForInviteConfirmation
      }

      match saga.CardInfo with
      | Some _ -> {
         saga with
            LifeCycle = saga.LifeCycle |> addActivity Activity.CardSetup
        }
      | None -> {
         saga with
            Status = EmployeeOnboardingSagaStatus.Completed
        }
   | EmployeeOnboardingSagaEvent.CardSetupSagaCompleted _ -> {
      saga with
         Status = EmployeeOnboardingSagaStatus.Completed
         LifeCycle = saga.LifeCycle |> finishActivity Activity.CardSetup
     }
   | EmployeeOnboardingSagaEvent.EvaluateRemainingWork -> {
      saga with
         LifeCycle =
            SagaLifeCycle.retryActivitiesAfterInactivity
               timestamp
               saga.LifeCycle
     }
   | EmployeeOnboardingSagaEvent.ResetInProgressActivityAttempts -> {
      saga with
         LifeCycle = SagaLifeCycle.resetInProgressActivities saga.LifeCycle
     }

let stateTransitionStart
   (evt: EmployeeOnboardingSagaStartEvent)
   (timestamp: DateTime)
   : Result<EmployeeOnboardingSaga, SagaStateTransitionError>
   =
   Ok(applyStartEvent evt timestamp)

let stateTransition
   (saga: EmployeeOnboardingSaga)
   (evt: EmployeeOnboardingSagaEvent)
   (timestamp: DateTime)
   : Result<EmployeeOnboardingSaga, SagaStateTransitionError>
   =
   let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

   let invalidStepProgression =
      match evt with
      | EmployeeOnboardingSagaEvent.EvaluateRemainingWork
      | EmployeeOnboardingSagaEvent.ResetInProgressActivityAttempts -> false
      | EmployeeOnboardingSagaEvent.InviteTokenRefreshed _ ->
         activityIsDone Activity.WaitForInviteConfirmation
      | EmployeeOnboardingSagaEvent.InviteNotificationSent ->
         activityIsDone Activity.SendEmployeeInviteNotification
      | EmployeeOnboardingSagaEvent.CardSetupSagaCompleted _ ->
         activityIsDone Activity.CardSetup
      | EmployeeOnboardingSagaEvent.AccessRequestPending ->
         activityIsDone Activity.RequestAccessApproval
      | EmployeeOnboardingSagaEvent.AccessApproved ->
         activityIsDone Activity.WaitForAccessApproval
      | EmployeeOnboardingSagaEvent.InviteConfirmed ->
         activityIsDone Activity.WaitForInviteConfirmation
      | EmployeeOnboardingSagaEvent.InviteCancelled _ ->
         activityIsDone Activity.WaitForInviteConfirmation

   if saga.Status = EmployeeOnboardingSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

// Org onboarding saga is started by a submitted application
// event coming from the Org actor.
let onStartEventPersisted
   (registry: #IOrgActor & #IEmailActor)
   (evt: EmployeeOnboardingSagaStartEvent)
   =
   match evt with
   | EmployeeOnboardingSagaStartEvent.AccountOwnerCreated e ->
      let emailMsg =
         EmailInfo.EmployeeInvite {
            Name = $"{e.Data.FirstName} {e.Data.LastName}"
            Email = e.Data.Email
            Token = string e.Data.InviteToken.Token
         }
         |> EmailMessage.create e.OrgId e.CorrelationId

      registry.EmailActor() <! emailMsg
   | EmployeeOnboardingSagaStartEvent.EmployeeCreated e ->
      if e.Data.OrgRequiresEmployeeInviteApproval.IsSome then
         let msg =
            ApproveAccessCommand.create
               (EmployeeId.fromEntityId e.EntityId, e.OrgId)
               e.InitiatedBy
               e.CorrelationId
               {
                  Name = $"{e.Data.FirstName} {e.Data.LastName}"
                  Reference = None
               }
            |> InviteEmployee
            |> ApprovableCommand.PerCommand
            |> OrgMessage.ApprovableRequest

         registry.OrgActor e.OrgId <! msg
      else
         let emailMsg =
            EmailInfo.EmployeeInvite {
               Name = $"{e.Data.FirstName} {e.Data.LastName}"
               Email = e.Data.Email
               Token = string e.Data.InviteToken.Token
            }
            |> EmailMessage.create e.OrgId e.CorrelationId

         registry.EmailActor() <! emailMsg
   | EmployeeOnboardingSagaStartEvent.EmployeeAccessRestored o ->
      let emailMsg =
         EmailInfo.EmployeeInvite {
            Name = o.EmployeeName
            Email = o.EmployeeEmail
            Token = string o.InviteToken.Token
         }
         |> EmailMessage.create o.Event.OrgId o.Event.CorrelationId

      registry.EmailActor() <! emailMsg

let onEventPersisted
   (registry:
      #IEmployeeGuaranteedDeliveryActor & #IOrgActor & #IEmailActor & #ICardIssuerServiceActor)
   (previousState: EmployeeOnboardingSaga)
   (updatedState: EmployeeOnboardingSaga)
   (evt: EmployeeOnboardingSagaEvent)
   =
   let employeeName = updatedState.EmployeeName
   let employeeId = updatedState.EmployeeId
   let orgId = updatedState.OrgId
   let corrId = updatedState.CorrelationId

   let sendEmployeeInviteEmail () =
      let emailMsg =
         EmailInfo.EmployeeInvite {
            Name = employeeName
            Email = updatedState.EmployeeEmail
            Token = string updatedState.InviteToken.Token
         }
         |> EmailMessage.create orgId corrId

      registry.EmailActor() <! emailMsg

   let requestAccessApproval () =
      let msg =
         ApproveAccessCommand.create
            (employeeId, orgId)
            updatedState.InitiatedBy
            corrId
            {
               Name = employeeName
               Reference = None
            }
         |> InviteEmployee
         |> ApprovableCommand.PerCommand
         |> OrgMessage.ApprovableRequest

      registry.OrgActor orgId <! msg

   let associateCardWithEmployee (info: EmployeeInviteSupplementaryCardInfo) =
      let msg =
         CreateCardCommand.create {
            AccountId = info.LinkedAccountId
            DailyPurchaseLimit = Some info.DailyPurchaseLimit
            MonthlyPurchaseLimit = Some info.MonthlyPurchaseLimit
            PersonName = employeeName
            CardNickname = None
            OrgId = orgId
            EmployeeId = employeeId
            CardId = CardId <| Guid.NewGuid()
            Virtual = true
            CardType = CardType.Debit
            InitiatedBy = updatedState.InitiatedBy
            OriginatedFromEmployeeOnboarding = Some corrId
         }
         |> EmployeeCommand.CreateCard
         |> EmployeeMessage.StateChange
         |> GuaranteedDelivery.message employeeId.Value

      registry.EmployeeGuaranteedDeliveryActor() <! msg

   match evt with
   | EmployeeOnboardingSagaEvent.InviteTokenRefreshed _ ->
      if updatedState.IsWaitingForInviteConfirmation then
         sendEmployeeInviteEmail ()
   | EmployeeOnboardingSagaEvent.InviteConfirmed ->
      updatedState.CardInfo |> Option.iter associateCardWithEmployee
   | EmployeeOnboardingSagaEvent.AccessApproved -> sendEmployeeInviteEmail ()
   | EmployeeOnboardingSagaEvent.InviteCancelled _
   | EmployeeOnboardingSagaEvent.InviteNotificationSent
   | EmployeeOnboardingSagaEvent.AccessRequestPending
   | EmployeeOnboardingSagaEvent.CardSetupSagaCompleted _
   | EmployeeOnboardingSagaEvent.ResetInProgressActivityAttempts -> ()
   | EmployeeOnboardingSagaEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.CreateEmployee
         | Activity.RestoreEmployeeAccess
         | Activity.WaitForAccessApproval
         | Activity.WaitForInviteConfirmation -> ()
         | Activity.RequestAccessApproval -> requestAccessApproval ()
         | Activity.SendEmployeeInviteNotification -> sendEmployeeInviteEmail ()
         | Activity.CardSetup ->
            updatedState.CardInfo |> Option.iter associateCardWithEmployee
