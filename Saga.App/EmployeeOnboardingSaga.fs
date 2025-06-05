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
   | Start of EmployeeOnboardingSagaStartEvent
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
         | CreateEmployee
         | WaitForAccessApproval
         | WaitForInviteConfirmation -> 1
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | CreateEmployee
         | RestoreEmployeeAccess
         | WaitForAccessApproval
         | WaitForInviteConfirmation -> None
         | CreateCardViaThirdPartyProvider -> Some(TimeSpan.FromMinutes 2)
         | SendEmployeeInviteNotification
         | SendEmployeeOnboardingFailNotification ->
            Some(TimeSpan.FromMinutes 4)
         | RequestAccessApproval
         | AssociateCardWithEmployee -> Some(TimeSpan.FromSeconds 5)

type EmployeeOnboardingSaga = {
   EmployeeId: EmployeeId
   OrgId: OrgId
   CorrelationId: CorrelationId
   InitiatedBy: Initiator
   EmployeeName: string
   EmployeeEmail: Email
   CardInfo: EmployeeInviteSupplementaryCardInfo option
   InviteToken: InviteToken
   Events: EmployeeOnboardingSagaEvent list
   Status: EmployeeOnboardingSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
   ProviderCardId: ThirdPartyProviderCardId option
} with

   member x.IsWaitingForInviteConfirmation =
      x.LifeCycle.InProgress
      |> List.exists (fun a -> a.Activity = Activity.WaitForInviteConfirmation)

let applyEvent
   (state: EmployeeOnboardingSaga option)
   (e: EmployeeOnboardingSagaEvent)
   (timestamp: DateTime)
   : EmployeeOnboardingSaga option
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp

   match state with
   | None ->
      match e with
      | EmployeeOnboardingSagaEvent.Start startEvt ->
         match startEvt with
         | EmployeeOnboardingSagaStartEvent.EmployeeCreated evt ->
            Some {
               Status = EmployeeOnboardingSagaStatus.InProgress
               Events = [ e ]
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
                        if
                           evt.Data.OrgRequiresEmployeeInviteApproval.IsSome
                        then
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
         | EmployeeOnboardingSagaStartEvent.AccountOwnerCreated evt ->
            Some {
               Status = EmployeeOnboardingSagaStatus.InProgress
               Events = [ e ]
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
         | EmployeeOnboardingSagaStartEvent.EmployeeAccessRestored o ->
            Some {
               Status = EmployeeOnboardingSagaStatus.InProgress
               Events = [ e ]
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
                              (Activity.RestoreEmployeeAccess :> IActivity)
                                 .MaxAttempts
                           Attempts = 1
                        }
                     ]
               }
            }
      | _ -> state
   | Some state ->
      let state =
         match e with
         | EmployeeOnboardingSagaEvent.Start _ -> state
         | EmployeeOnboardingSagaEvent.InviteTokenRefreshed token ->
            if state.IsWaitingForInviteConfirmation then
               {
                  state with
                     InviteToken = token
                     LifeCycle =
                        state.LifeCycle
                        |> finishActivity Activity.WaitForInviteConfirmation
                        |> addActivity Activity.SendEmployeeInviteNotification
               }
            else
               state
         | EmployeeOnboardingSagaEvent.InviteNotificationSent -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.SendEmployeeInviteNotification
           }
         | EmployeeOnboardingSagaEvent.OnboardingFailNotificationSent -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity
                        Activity.SendEmployeeOnboardingFailNotification
           }
         | EmployeeOnboardingSagaEvent.AccessRequestPending -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.RequestAccessApproval
                  |> addActivity Activity.WaitForAccessApproval
           }
         | EmployeeOnboardingSagaEvent.AccessApproved -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.WaitForAccessApproval
                  |> addActivity Activity.SendEmployeeInviteNotification
                  |> addActivity Activity.WaitForInviteConfirmation
           }
         | EmployeeOnboardingSagaEvent.InviteCancelled reason -> {
            state with
               Status = EmployeeOnboardingSagaStatus.Aborted reason
               LifeCycle =
                  SagaLifeCycle.abortActivities timestamp state.LifeCycle
           }
         | EmployeeOnboardingSagaEvent.InviteConfirmed ->
            let state = {
               state with
                  LifeCycle =
                     state.LifeCycle
                     |> finishActivity Activity.WaitForInviteConfirmation
            }

            match state.CardInfo with
            | Some _ -> {
               state with
                  LifeCycle =
                     state.LifeCycle
                     |> addActivity Activity.CreateCardViaThirdPartyProvider
              }
            | None -> {
               state with
                  Status = EmployeeOnboardingSagaStatus.Completed
              }
         | EmployeeOnboardingSagaEvent.CardCreateResponse res ->
            let activity = Activity.CreateCardViaThirdPartyProvider

            match res with
            | Ok providerCardId -> {
               state with
                  ProviderCardId = Some providerCardId
                  LifeCycle =
                     state.LifeCycle
                     |> finishActivity activity
                     |> addActivity Activity.AssociateCardWithEmployee
              }
            | Error _ -> {
               state with
                  Status =
                     OnboardingFailureReason.CardProviderCardCreateFail
                     |> EmployeeOnboardingSagaStatus.Failed
                  LifeCycle =
                     state.LifeCycle
                     |> SagaLifeCycle.failActivity timestamp activity
                     |> addActivity
                           Activity.SendEmployeeOnboardingFailNotification
              }
         | EmployeeOnboardingSagaEvent.CardAssociatedWithEmployee -> {
            state with
               Status = EmployeeOnboardingSagaStatus.Completed
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.AssociateCardWithEmployee
           }
         | EmployeeOnboardingSagaEvent.EvaluateRemainingWork -> {
            state with
               LifeCycle =
                  SagaLifeCycle.retryActivitiesAfterInactivity
                     timestamp
                     state.LifeCycle
           }
         | EmployeeOnboardingSagaEvent.ResetInProgressActivityAttempts -> {
            state with
               LifeCycle =
                  SagaLifeCycle.resetInProgressActivities state.LifeCycle
           }

      Some {
         state with
            Events = e :: state.Events
      }

let stateTransition
   (state: EmployeeOnboardingSaga option)
   (evt: EmployeeOnboardingSagaEvent)
   (timestamp: DateTime)
   : Result<EmployeeOnboardingSaga option, SagaStateTransitionError>
   =
   match state with
   | None ->
      match evt with
      | EmployeeOnboardingSagaEvent.Start _ ->
         Ok(applyEvent state evt timestamp)
      | _ -> Error SagaStateTransitionError.HasNotStarted
   | Some saga ->
      let eventIsStartEvent =
         match evt with
         | EmployeeOnboardingSagaEvent.Start _ -> true
         | _ -> false

      let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

      let invalidStepProgression =
         match evt with
         | EmployeeOnboardingSagaEvent.Start _
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
      elif eventIsStartEvent then
         Error SagaStateTransitionError.HasAlreadyStarted
      elif invalidStepProgression then
         Error SagaStateTransitionError.InvalidStepProgression
      else
         Ok(applyEvent state evt timestamp)

type CardProviderRequest = {
   CardHolderName: string
   CardType: string
}

type PersistenceHandlerDependencies = {
   getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>
   getOrgRef: OrgId -> IEntityRef<OrgMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   createCardViaThirdPartyProvider:
      CardProviderRequest -> Async<EmployeeOnboardingSagaEvent>
   sendMessageToSelf:
      OrgId -> CorrelationId -> Async<EmployeeOnboardingSagaEvent> -> unit
}

// Org onboarding saga is started by a submitted application
// event coming from the Org actor.
let onStartEventPersisted
   (dep: PersistenceHandlerDependencies)
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
               ProviderCardId = Some providerCardId
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
         CardType = string info.CardType
      }

      dep.sendMessageToSelf
         orgId
         corrId
         (dep.createCardViaThirdPartyProvider request)

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
   | EmployeeOnboardingSagaEvent.Start _
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

let createCardViaThirdPartyProvider (_: CardProviderRequest) = async {
   do! Async.Sleep(1300)

   let response = Guid.NewGuid() |> ThirdPartyProviderCardId |> Ok

   return EmployeeOnboardingSagaEvent.CardCreateResponse response
}
