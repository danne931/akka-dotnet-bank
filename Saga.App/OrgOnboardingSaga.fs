module OrgOnboardingSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Org.Domain
open Email
open Lib.Saga
open PartnerBank.Service.Domain

[<RequireQualifiedAccess>]
type OrgOnboardingSagaStatus =
   | InProgress
   | Completed
   | Failed of OrgOnboardingFailureReason

[<RequireQualifiedAccess>]
type OrgOnboardingSagaStartEvent =
   | ApplicationSubmitted of BankEvent<OrgOnboardingApplicationSubmitted>

[<RequireQualifiedAccess>]
type OrgOnboardingSagaEvent =
   | ApplicationProcessingNotificationSent
   | KYCResponse of Result<unit, OrgOnboardingVerificationError>
   | ReceivedInfoFixDemandedByKYCService of OrgOnboardingApplicationSubmitted
   | LinkAccountToPartnerBankResponse of
      Result<PartnerBankAccountReference, string>
   | InitializedPrimaryVirtualAccount
   | OrgActivated
   | ApplicationAcceptedNotificationSent
   | ApplicationRejectedNotificationSent
   | ApplicationRequiresRevisionForKYCServiceNotificationSent
   | SupportTeamResolvedPartnerBankLink
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess>]
type Activity =
   | SubmitApplication
   | SendApplicationProcessingNotification
   | KYCVerification
   | LinkAccountToPartnerBank
   | InitializePrimaryVirtualAccount
   | ActivateOrg
   | SendApplicationAcceptedNotification
   | SendApplicationRejectedNotification
   | WaitForInfoFixDemandedByKYCService
   | SendApplicationRequiresRevisionForKYCServiceNotification
   | WaitForSupportTeamToResolvePartnerBankLink

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForInfoFixDemandedByKYCService
         | WaitForSupportTeamToResolvePartnerBankLink -> 0
         | SubmitApplication -> 1
         | LinkAccountToPartnerBank -> 4
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | SubmitApplication
         | WaitForInfoFixDemandedByKYCService
         | WaitForSupportTeamToResolvePartnerBankLink -> None
         | KYCVerification
         | LinkAccountToPartnerBank -> Some(TimeSpan.FromMinutes 2)
         | SendApplicationProcessingNotification
         | SendApplicationRequiresRevisionForKYCServiceNotification
         | SendApplicationAcceptedNotification
         | SendApplicationRejectedNotification -> Some(TimeSpan.FromMinutes 4)
         | InitializePrimaryVirtualAccount
         | ActivateOrg -> Some(TimeSpan.FromSeconds 5)

type OrgOnboardingSaga = {
   OrgId: OrgId
   CorrelationId: CorrelationId
   Application: OrgOnboardingApplicationSubmitted
   StartEvent: OrgOnboardingSagaStartEvent
   Events: OrgOnboardingSagaEvent list
   Status: OrgOnboardingSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
   ApplicationRequiresRevision:
      OrgOnboardingApplicationRequiresUpdateInfo option
} with

   member x.LinkedAccountToPartnerBank =
      x.Events
      |> List.tryPick (function
         | OrgOnboardingSagaEvent.LinkAccountToPartnerBankResponse res ->
            Result.toOption res
         | _ -> None)

let applyStartEvent
   (start: OrgOnboardingSagaStartEvent)
   (timestamp: DateTime)
   : OrgOnboardingSaga
   =
   match start with
   | OrgOnboardingSagaStartEvent.ApplicationSubmitted evt -> {
      OrgId = evt.OrgId
      CorrelationId = evt.CorrelationId
      Status = OrgOnboardingSagaStatus.InProgress
      StartEvent = start
      Events = []
      Application = evt.Data
      ApplicationRequiresRevision = None
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init
                  timestamp
                  Activity.SendApplicationProcessingNotification
               ActivityLifeCycle.init timestamp Activity.KYCVerification
            ]
            Completed = [
               {
                  Start = timestamp
                  End = Some timestamp
                  Activity = Activity.SubmitApplication
                  MaxAttempts =
                     (Activity.SubmitApplication :> IActivity).MaxAttempts
                  Attempts = 1
               }
            ]
      }
     }

let applyEvent
   (saga: OrgOnboardingSaga)
   (evt: OrgOnboardingSagaEvent)
   (timestamp: DateTime)
   : OrgOnboardingSaga
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
   | OrgOnboardingSagaEvent.ApplicationProcessingNotificationSent -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.SendApplicationProcessingNotification
     }
   | OrgOnboardingSagaEvent.KYCResponse response ->
      let activity = Activity.KYCVerification

      match response with
      | Ok _ -> {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> finishActivity activity
               |> addActivity Activity.LinkAccountToPartnerBank
        }
      | Error err ->
         match err with
         | OrgOnboardingVerificationError.RequiresUpdatedInfo info -> {
            saga with
               ApplicationRequiresRevision = Some info
               LifeCycle =
                  saga.LifeCycle
                  |> failActivity activity
                  |> addActivity Activity.WaitForInfoFixDemandedByKYCService
                  |> addActivity
                        Activity.SendApplicationRequiresRevisionForKYCServiceNotification
           }
         | OrgOnboardingVerificationError.Rejected reason -> {
            saga with
               Status =
                  OrgOnboardingFailureReason.KYCRejectedReason reason
                  |> OrgOnboardingSagaStatus.Failed
               LifeCycle =
                  saga.LifeCycle
                  |> failActivity activity
                  |> addActivity Activity.SendApplicationRejectedNotification
           }
   | OrgOnboardingSagaEvent.ApplicationRequiresRevisionForKYCServiceNotificationSent -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity
                  Activity.SendApplicationRequiresRevisionForKYCServiceNotification
     }
   | OrgOnboardingSagaEvent.ReceivedInfoFixDemandedByKYCService updatedInfo -> {
      saga with
         Application = updatedInfo
         ApplicationRequiresRevision = None
         Status = OrgOnboardingSagaStatus.InProgress
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.WaitForInfoFixDemandedByKYCService
            |> addActivity Activity.KYCVerification
     }
   | OrgOnboardingSagaEvent.LinkAccountToPartnerBankResponse res ->
      let activity = Activity.LinkAccountToPartnerBank

      match res with
      | Error err ->
         if saga.LifeCycle.ActivityHasRemainingAttempts activity then
            {
               saga with
                  LifeCycle = retryActivity activity saga.LifeCycle
            }
         else
            {
               saga with
                  Status =
                     OrgOnboardingFailureReason.PartnerBankLinkError err
                     |> OrgOnboardingSagaStatus.Failed
                  LifeCycle =
                     saga.LifeCycle
                     |> failActivity activity
                     |> addActivity
                           Activity.WaitForSupportTeamToResolvePartnerBankLink
            }
      | Ok _ -> {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> finishActivity activity
               |> addActivity Activity.InitializePrimaryVirtualAccount
        }
   | OrgOnboardingSagaEvent.InitializedPrimaryVirtualAccount -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.InitializePrimaryVirtualAccount
            |> addActivity Activity.ActivateOrg
     }
   | OrgOnboardingSagaEvent.OrgActivated -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.ActivateOrg
            |> addActivity Activity.SendApplicationAcceptedNotification
     }
   | OrgOnboardingSagaEvent.ApplicationAcceptedNotificationSent -> {
      saga with
         Status = OrgOnboardingSagaStatus.Completed
         LifeCycle =
            finishActivity
               Activity.SendApplicationAcceptedNotification
               saga.LifeCycle
     }
   | OrgOnboardingSagaEvent.ApplicationRejectedNotificationSent -> {
      saga with
         LifeCycle =
            finishActivity
               Activity.SendApplicationRejectedNotification
               saga.LifeCycle
     }
   | OrgOnboardingSagaEvent.SupportTeamResolvedPartnerBankLink -> {
      saga with
         Status = OrgOnboardingSagaStatus.InProgress
         LifeCycle =
            saga.LifeCycle
            |> finishActivity
                  Activity.WaitForSupportTeamToResolvePartnerBankLink
            |> addActivity Activity.LinkAccountToPartnerBank
     }
   | OrgOnboardingSagaEvent.EvaluateRemainingWork -> {
      saga with
         LifeCycle =
            SagaLifeCycle.retryActivitiesAfterInactivity
               timestamp
               saga.LifeCycle
     }
   | OrgOnboardingSagaEvent.ResetInProgressActivityAttempts -> {
      saga with
         LifeCycle = SagaLifeCycle.resetInProgressActivities saga.LifeCycle
     }

let stateTransitionStart
   (evt: OrgOnboardingSagaStartEvent)
   (timestamp: DateTime)
   : Result<OrgOnboardingSaga, SagaStateTransitionError>
   =
   Ok(applyStartEvent evt timestamp)

let stateTransition
   (saga: OrgOnboardingSaga)
   (evt: OrgOnboardingSagaEvent)
   (timestamp: DateTime)
   : Result<OrgOnboardingSaga, SagaStateTransitionError>
   =
   let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

   let invalidStepProgression =
      match evt with
      | OrgOnboardingSagaEvent.EvaluateRemainingWork
      | OrgOnboardingSagaEvent.ResetInProgressActivityAttempts -> false
      | OrgOnboardingSagaEvent.SupportTeamResolvedPartnerBankLink ->
         activityIsDone Activity.WaitForSupportTeamToResolvePartnerBankLink
      | OrgOnboardingSagaEvent.ApplicationProcessingNotificationSent ->
         activityIsDone Activity.SendApplicationProcessingNotification
      | OrgOnboardingSagaEvent.ApplicationRejectedNotificationSent ->
         activityIsDone Activity.SendApplicationRejectedNotification
      | OrgOnboardingSagaEvent.ApplicationAcceptedNotificationSent ->
         activityIsDone Activity.SendApplicationAcceptedNotification
      | OrgOnboardingSagaEvent.ApplicationRequiresRevisionForKYCServiceNotificationSent ->
         activityIsDone
            Activity.SendApplicationRequiresRevisionForKYCServiceNotification
      | OrgOnboardingSagaEvent.KYCResponse _ ->
         activityIsDone Activity.KYCVerification
      | OrgOnboardingSagaEvent.ReceivedInfoFixDemandedByKYCService _ ->
         activityIsDone Activity.WaitForInfoFixDemandedByKYCService
      | OrgOnboardingSagaEvent.LinkAccountToPartnerBankResponse _ ->
         activityIsDone Activity.LinkAccountToPartnerBank
      | OrgOnboardingSagaEvent.InitializedPrimaryVirtualAccount ->
         activityIsDone Activity.InitializePrimaryVirtualAccount
      | OrgOnboardingSagaEvent.OrgActivated ->
         activityIsDone Activity.ActivateOrg

   if saga.Status = OrgOnboardingSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

type PersistenceHandlerDependencies = {
   getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>
   getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>
   getOrgRef: OrgId -> IEntityRef<OrgMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   getKYCServiceRef: unit -> IActorRef<KYCMessage>
   getPartnerBankServiceRef: unit -> IActorRef<PartnerBankServiceMessage>
}

// Org onboarding saga is started by a submitted application
// event coming from the Org actor.
let onStartEventPersisted
   (dep: PersistenceHandlerDependencies)
   (evt: OrgOnboardingSagaStartEvent)
   =
   match evt with
   | OrgOnboardingSagaStartEvent.ApplicationSubmitted evt ->
      let info = evt.Data

      let emailMsg = {
         OrgId = evt.OrgId
         CorrelationId = evt.CorrelationId
         Info =
            EmailInfo.OrgOnboardingApplicationSubmitted {
               Email = info.AdminTeamEmail
               BusinessName = info.LegalBusinessName
            }
      }

      dep.getEmailRef () <! emailMsg

      dep.getKYCServiceRef ()
      <! KYCMessage.VerifyApplication {
         OrgId = evt.OrgId
         CorrelationId = evt.CorrelationId
         Application = info
      }

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
   (previousState: OrgOnboardingSaga)
   (updatedState: OrgOnboardingSaga)
   (evt: OrgOnboardingSagaEvent)
   =
   let application = updatedState.Application
   let orgId = updatedState.OrgId
   let corrId = updatedState.CorrelationId

   let sendApplicationAcceptedEmail () =
      let emailMsg = {
         OrgId = orgId
         CorrelationId = corrId
         Info =
            EmailInfo.OrgOnboardingApplicationAccepted {
               Email = application.AdminTeamEmail
               BusinessName = application.LegalBusinessName
            }
      }

      dep.getEmailRef () <! emailMsg

   let sendApplicationRejectedEmail
      (reason: OrgOnboardingApplicationRejectedReason)
      =
      let emailMsg = {
         OrgId = orgId
         CorrelationId = corrId
         Info =
            EmailInfo.OrgOnboardingApplicationRejected {
               Info = {
                  Email = application.AdminTeamEmail
                  BusinessName = application.LegalBusinessName
               }
               Reason = reason.Display
            }
      }

      dep.getEmailRef () <! emailMsg

   let sendApplicationRequiresRevisionForKYCServiceEmail
      (reason: OrgOnboardingApplicationRequiresUpdateInfo)
      =
      let emailMsg = {
         OrgId = orgId
         CorrelationId = corrId
         Info =
            EmailInfo.OrgOnboardingApplicationRequiresRevision {
               Info = {
                  Email = application.AdminTeamEmail
                  BusinessName = application.LegalBusinessName
               }
               Reason = reason.Display
            }
      }

      dep.getEmailRef () <! emailMsg

   let verifyOrg () =
      dep.getKYCServiceRef ()
      <! KYCMessage.VerifyApplication {
         OrgId = orgId
         CorrelationId = corrId
         Application = application
      }

   let linkAccountToPartnerBank () =
      dep.getPartnerBankServiceRef ()
      <! PartnerBankServiceMessage.LinkAccount {
         LegalBusinessName = application.LegalBusinessName
         EmployerIdentificationNumber = application.EmployerIdentificationNumber
         Metadata = {
            OrgId = orgId
            CorrelationId = corrId
         }
      }

   let initializeVirtualAccount partnerBankAccount =
      let parentAccountId = application.ParentAccountId

      let msg =
         InitializePrimaryCheckingAccountCommand.create {
            OrgId = orgId
            CorrelationId = corrId
            ParentAccountId = parentAccountId
            PartnerBankAccountNumber = partnerBankAccount.AccountNumber
            PartnerBankRoutingNumber = partnerBankAccount.RoutingNumber
         }
         |> AccountCommand.InitializePrimaryCheckingAccount
         |> AccountMessage.StateChange

      dep.getAccountRef parentAccountId <! msg

   let activateOrg () =
      let msg =
         FinishOrgOnboardingCommand.create {
            OrgId = orgId
            ParentAccountId = application.ParentAccountId
            CorrelationId = corrId
            InitiatedBy = Initiator.System
         }
         |> OrgCommand.FinishOrgOnboarding
         |> OrgMessage.StateChange

      dep.getOrgRef orgId <! msg

   match evt with
   | OrgOnboardingSagaEvent.KYCResponse res ->
      match res with
      | Ok _ -> linkAccountToPartnerBank ()
      | Error err ->
         match err with
         | OrgOnboardingVerificationError.RequiresUpdatedInfo infoRequired ->
            sendApplicationRequiresRevisionForKYCServiceEmail infoRequired
         | OrgOnboardingVerificationError.Rejected reason ->
            sendApplicationRejectedEmail reason
   | OrgOnboardingSagaEvent.ReceivedInfoFixDemandedByKYCService _ ->
      verifyOrg ()
   | OrgOnboardingSagaEvent.LinkAccountToPartnerBankResponse res ->
      match res with
      | Ok nums -> initializeVirtualAccount nums
      | Error reason ->
         if
            previousState.LifeCycle.ActivityHasRemainingAttempts
               Activity.LinkAccountToPartnerBank
         then
            linkAccountToPartnerBank ()
         else
            dep.getEmailRef ()
            <! {
                  OrgId = orgId
                  CorrelationId = corrId
                  Info = EmailInfo.ApplicationErrorRequiresSupport reason
               }
   | OrgOnboardingSagaEvent.SupportTeamResolvedPartnerBankLink ->
      // Support team resolved dispute with partner bank so
      // reattempt linking parent account with partner bank.
      linkAccountToPartnerBank ()
   | OrgOnboardingSagaEvent.InitializedPrimaryVirtualAccount -> activateOrg ()
   | OrgOnboardingSagaEvent.OrgActivated -> sendApplicationAcceptedEmail ()
   | OrgOnboardingSagaEvent.ApplicationProcessingNotificationSent
   | OrgOnboardingSagaEvent.ApplicationAcceptedNotificationSent
   | OrgOnboardingSagaEvent.ApplicationRejectedNotificationSent
   | OrgOnboardingSagaEvent.ApplicationRequiresRevisionForKYCServiceNotificationSent
   | OrgOnboardingSagaEvent.ResetInProgressActivityAttempts -> ()
   | OrgOnboardingSagaEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.SubmitApplication
         | Activity.WaitForSupportTeamToResolvePartnerBankLink
         | Activity.WaitForInfoFixDemandedByKYCService -> ()
         | Activity.SendApplicationProcessingNotification ->
            let emailMsg = {
               OrgId = orgId
               CorrelationId = corrId
               Info =
                  EmailInfo.OrgOnboardingApplicationSubmitted {
                     Email = application.AdminTeamEmail
                     BusinessName = application.LegalBusinessName
                  }
            }

            dep.getEmailRef () <! emailMsg
         | Activity.SendApplicationRejectedNotification ->
            match updatedState.Status with
            | OrgOnboardingSagaStatus.Failed(OrgOnboardingFailureReason.KYCRejectedReason reason) ->
               sendApplicationRejectedEmail reason
            | _ -> ()
         | Activity.SendApplicationRequiresRevisionForKYCServiceNotification ->
            updatedState.ApplicationRequiresRevision
            |> Option.iter sendApplicationRequiresRevisionForKYCServiceEmail
         | Activity.SendApplicationAcceptedNotification ->
            sendApplicationAcceptedEmail ()
         | Activity.KYCVerification -> verifyOrg ()
         | Activity.LinkAccountToPartnerBank -> linkAccountToPartnerBank ()
         | Activity.InitializePrimaryVirtualAccount ->
            updatedState.LinkedAccountToPartnerBank
            |> Option.iter initializeVirtualAccount
         | Activity.ActivateOrg -> activateOrg ()
