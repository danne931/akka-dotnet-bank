module EmployeeOnboardingSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Bank.Employee.Domain
open Bank.Org.Domain
open CommandApproval
open Email
open Lib.Saga
open CardIssuer.Service.Domain

[<RequireQualifiedAccess>]
type OnboardingFailureReason = | CardProviderCardCreateFail

[<RequireQualifiedAccess>]
type EmployeeOnboardingSagaStatus =
   | InProgress
   | Completed
   | Failed of OnboardingFailureReason
   | Aborted of reason: string option

[<RequireQualifiedAccess>]
type EmployeeOnboardingSagaStartEvent =
   | AccountOwnerCreated of BankEvent<CreatedAccountOwner>
   | EmployeeCreated of BankEvent<CreatedEmployee>
   | EmployeeAccessRestored of
      {|
         Event: BankEvent<AccessRestored>
         EmployeeName: string
         EmployeeEmail: Email
         InviteToken: InviteToken
      |}

[<RequireQualifiedAccess>]
type EmployeeOnboardingSagaEvent =
   | AccessRequestPending
   | AccessApproved
   | InviteNotificationSent
   | InviteTokenRefreshed of InviteToken
   | OnboardingFailNotificationSent
   | InviteConfirmed
   | InviteCancelled of reason: string option
   | CardCreateResponse of Result<ThirdPartyProviderCardId, string>
   | CardAssociatedWithEmployee
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess>]
type Activity =
   | CreateEmployee
   | RestoreEmployeeAccess
   | RequestAccessApproval
   | WaitForAccessApproval
   | SendEmployeeInviteNotification
   | WaitForInviteConfirmation
   | CreateCardViaThirdPartyProvider
   | AssociateCardWithEmployee
   | SendEmployeeOnboardingFailNotification

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForAccessApproval
         | WaitForInviteConfirmation -> 0
         | CreateEmployee -> 1
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | CreateEmployee
         | RestoreEmployeeAccess
         | WaitForAccessApproval
         | WaitForInviteConfirmation -> None
         | CreateCardViaThirdPartyProvider -> Some(TimeSpan.FromMinutes 2.)
         | SendEmployeeInviteNotification
         | SendEmployeeOnboardingFailNotification ->
            Some(TimeSpan.FromMinutes 4.)
         | RequestAccessApproval
         | AssociateCardWithEmployee -> Some(TimeSpan.FromSeconds 5.)

type EmployeeOnboardingSaga = {
   EmployeeId: EmployeeId
   OrgId: OrgId
   CorrelationId: CorrelationId
   InitiatedBy: Initiator
   EmployeeName: string
   EmployeeEmail: Email
   CardInfo: EmployeeInviteSupplementaryCardInfo option
   InviteToken: InviteToken
   StartEvent: EmployeeOnboardingSagaStartEvent
   Events: EmployeeOnboardingSagaEvent list
   Status: EmployeeOnboardingSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
   ProviderCardId: ThirdPartyProviderCardId option
} with

   member x.IsWaitingForInviteConfirmation =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsWaitForInviteConfirmation


let applyStartEvent
   (start: EmployeeOnboardingSagaStartEvent)
   (timestamp: DateTime)
   : EmployeeOnboardingSaga
   =
   match start with
   | EmployeeOnboardingSagaStartEvent.EmployeeCreated evt -> {
      Status = EmployeeOnboardingSagaStatus.InProgress
      StartEvent = start
      Events = []
      OrgId = evt.OrgId
      CorrelationId = evt.CorrelationId
      InitiatedBy = evt.InitiatedBy
      EmployeeId = EmployeeId.fromEntityId evt.EntityId
      EmployeeName = $"{evt.Data.FirstName} {evt.Data.LastName}"
      EmployeeEmail = evt.Data.Email
      CardInfo = evt.Data.CardInfo
      InviteToken = evt.Data.InviteToken
      ProviderCardId = None
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
      Events = []
      OrgId = evt.OrgId
      CorrelationId = evt.CorrelationId
      InitiatedBy = evt.InitiatedBy
      EmployeeId = EmployeeId.fromEntityId evt.EntityId
      EmployeeName = $"{evt.Data.FirstName} {evt.Data.LastName}"
      EmployeeEmail = evt.Data.Email
      CardInfo = None
      InviteToken = evt.Data.InviteToken
      ProviderCardId = None
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
      Events = []
      OrgId = o.Event.OrgId
      CorrelationId = o.Event.CorrelationId
      InitiatedBy = o.Event.InitiatedBy
      EmployeeId = EmployeeId.fromEntityId o.Event.EntityId
      EmployeeName = o.EmployeeName
      EmployeeEmail = o.EmployeeEmail
      CardInfo = None
      InviteToken = o.Event.Data.InviteToken
      ProviderCardId = None
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
   | EmployeeOnboardingSagaEvent.OnboardingFailNotificationSent -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.SendEmployeeOnboardingFailNotification
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
            LifeCycle =
               saga.LifeCycle
               |> addActivity Activity.CreateCardViaThirdPartyProvider
        }
      | None -> {
         saga with
            Status = EmployeeOnboardingSagaStatus.Completed
        }
   | EmployeeOnboardingSagaEvent.CardCreateResponse res ->
      let activity = Activity.CreateCardViaThirdPartyProvider

      match res with
      | Ok providerCardId -> {
         saga with
            ProviderCardId = Some providerCardId
            LifeCycle =
               saga.LifeCycle
               |> finishActivity activity
               |> addActivity Activity.AssociateCardWithEmployee
        }
      | Error _ -> {
         saga with
            Status =
               OnboardingFailureReason.CardProviderCardCreateFail
               |> EmployeeOnboardingSagaStatus.Failed
            LifeCycle =
               saga.LifeCycle
               |> SagaLifeCycle.failActivity timestamp activity
               |> addActivity Activity.SendEmployeeOnboardingFailNotification
        }
   | EmployeeOnboardingSagaEvent.CardAssociatedWithEmployee -> {
      saga with
         Status = EmployeeOnboardingSagaStatus.Completed
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.AssociateCardWithEmployee
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
      | EmployeeOnboardingSagaEvent.OnboardingFailNotificationSent ->
         activityIsDone Activity.SendEmployeeOnboardingFailNotification
      | EmployeeOnboardingSagaEvent.CardCreateResponse _ ->
         activityIsDone Activity.CreateCardViaThirdPartyProvider
      | EmployeeOnboardingSagaEvent.CardAssociatedWithEmployee ->
         activityIsDone Activity.AssociateCardWithEmployee
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

type PersistenceStartHandlerDependencies = {
   getOrgRef: OrgId -> IEntityRef<OrgMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
}

// Org onboarding saga is started by a submitted application
// event coming from the Org actor.
let onStartEventPersisted
   (dep: PersistenceStartHandlerDependencies)
   (evt: EmployeeOnboardingSagaStartEvent)
   =
   match evt with
   | EmployeeOnboardingSagaStartEvent.AccountOwnerCreated e ->
      let emailMsg =
         EmailInfo.EmployeeInvite {
            Name = $"{e.Data.FirstName} {e.Data.LastName}"
            Email = e.Data.Email
            Token = e.Data.InviteToken
         }
         |> EmailMessage.create e.OrgId e.CorrelationId

      dep.getEmailRef () <! emailMsg
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

         dep.getOrgRef e.OrgId <! msg
      else
         let emailMsg =
            EmailInfo.EmployeeInvite {
               Name = $"{e.Data.FirstName} {e.Data.LastName}"
               Email = e.Data.Email
               Token = e.Data.InviteToken
            }
            |> EmailMessage.create e.OrgId e.CorrelationId

         dep.getEmailRef () <! emailMsg
   | EmployeeOnboardingSagaStartEvent.EmployeeAccessRestored o ->
      let emailMsg =
         EmailInfo.EmployeeInvite {
            Name = o.EmployeeName
            Email = o.EmployeeEmail
            Token = o.InviteToken
         }
         |> EmailMessage.create o.Event.OrgId o.Event.CorrelationId

      dep.getEmailRef () <! emailMsg

type PersistenceHandlerDependencies = {
   getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>
   getOrgRef: OrgId -> IEntityRef<OrgMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   getCardIssuerServiceRef: unit -> IActorRef<CardIssuerMessage>
}

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
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
            Token = updatedState.InviteToken
         }
         |> EmailMessage.create orgId corrId

      dep.getEmailRef () <! emailMsg

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

      dep.getOrgRef orgId <! msg

   let associateCardWithEmployee
      (providerCardId: ThirdPartyProviderCardId)
      (info: EmployeeInviteSupplementaryCardInfo)
      =
      let msg =
         {
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
            } with
               CorrelationId = updatedState.CorrelationId
         }
         |> EmployeeCommand.CreateCard
         |> EmployeeMessage.StateChange

      dep.getEmployeeRef employeeId <! msg

   let sendEmployeeOnboardingFailEmail (reason: OnboardingFailureReason) =
      let emailMsg =
         EmailInfo.EmployeeOnboardingFail {
            Name = employeeName
            Reason = string reason
         }
         |> EmailMessage.create orgId corrId

      dep.getEmailRef () <! emailMsg

   let createCardViaThirdPartyProvider
      (info: EmployeeInviteSupplementaryCardInfo)
      =
      let request = {
         CardHolderName = employeeName
         CardType = info.CardType
         Metadata = {
            ReplyTo = SagaReplyTo.EmployeeOnboard
            OrgId = orgId
            CorrelationId = corrId
         }
      }

      dep.getCardIssuerServiceRef () <! CardIssuerMessage.CreateCard request

   match evt with
   | EmployeeOnboardingSagaEvent.InviteTokenRefreshed _ ->
      if updatedState.IsWaitingForInviteConfirmation then
         sendEmployeeInviteEmail ()
   | EmployeeOnboardingSagaEvent.InviteConfirmed ->
      updatedState.CardInfo |> Option.iter createCardViaThirdPartyProvider
   | EmployeeOnboardingSagaEvent.CardCreateResponse res ->
      match res with
      | Ok providerCardId ->
         updatedState.CardInfo
         |> Option.iter (associateCardWithEmployee providerCardId)
      | Error _ ->
         sendEmployeeOnboardingFailEmail
            OnboardingFailureReason.CardProviderCardCreateFail
   | EmployeeOnboardingSagaEvent.AccessApproved -> sendEmployeeInviteEmail ()
   | EmployeeOnboardingSagaEvent.InviteCancelled _
   | EmployeeOnboardingSagaEvent.InviteNotificationSent
   | EmployeeOnboardingSagaEvent.OnboardingFailNotificationSent
   | EmployeeOnboardingSagaEvent.AccessRequestPending
   | EmployeeOnboardingSagaEvent.CardAssociatedWithEmployee
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
         | Activity.SendEmployeeOnboardingFailNotification ->
            match updatedState.Status with
            | EmployeeOnboardingSagaStatus.Failed fail ->
               sendEmployeeOnboardingFailEmail fail
            | _ -> ()
         | Activity.CreateCardViaThirdPartyProvider ->
            updatedState.CardInfo |> Option.iter createCardViaThirdPartyProvider
         | Activity.AssociateCardWithEmployee ->
            match updatedState.ProviderCardId, updatedState.CardInfo with
            | Some providerCardId, Some cardInfo ->
               associateCardWithEmployee providerCardId cardInfo
            | _ -> ()
