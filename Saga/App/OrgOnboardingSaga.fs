module OrgOnboardingSaga

open System
open Akkling

open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Org.Domain
open EmailMessage
open Lib.Saga
open PartnerBank.Service.Domain
open CachedOrgSettings
open OrgOnboardingSaga
open BankActorRegistry

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
               |> addActivity Activity.CreateLegalEntityWithPartnerBank
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
   | OrgOnboardingSagaEvent.CreateLegalEntityWithPartnerBankResponse res ->
      let activity = Activity.CreateLegalEntityWithPartnerBank

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
                     OrgOnboardingFailureReason.PartnerBankLegalEntityCreateError
                        err
                     |> OrgOnboardingSagaStatus.Failed
                  LifeCycle =
                     saga.LifeCycle
                     |> failActivity activity
                     |> addActivity Activity.WaitForSupportTeamToResolve
            }
      | Ok _ -> {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.CreateLegalEntityWithPartnerBank
               |> addActivity Activity.CreateInternalAccountWithPartnerBank
        }
   | OrgOnboardingSagaEvent.CreateInternalAccountWithPartnerBankResponse res ->
      let activity = Activity.CreateInternalAccountWithPartnerBank

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
                     OrgOnboardingFailureReason.PartnerBankInternalAccountCreateError
                        err
                     |> OrgOnboardingSagaStatus.Failed
                  LifeCycle =
                     saga.LifeCycle
                     |> failActivity activity
                     |> addActivity Activity.WaitForSupportTeamToResolve
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
            |> addActivity Activity.InitializeOrgSettingsCache
     }
   | OrgOnboardingSagaEvent.InitializeOrgSettingsCacheResponse res ->
      let activity = Activity.InitializeOrgSettingsCache
      let nextActivity = Activity.ActivateOrg

      match res with
      | Error _ ->
         if saga.LifeCycle.ActivityHasRemainingAttempts activity then
            {
               saga with
                  LifeCycle = retryActivity activity saga.LifeCycle
            }
         else
            // If cache setup fails then fail the activity but resume
            // normal processing.
            {
               saga with
                  LifeCycle =
                     saga.LifeCycle
                     |> failActivity activity
                     |> addActivity nextActivity
            }
      | Ok _ -> {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.InitializeOrgSettingsCache
               |> addActivity nextActivity
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
            |> finishActivity Activity.WaitForSupportTeamToResolve
            |> addActivity Activity.CreateInternalAccountWithPartnerBank
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
         activityIsDone Activity.WaitForSupportTeamToResolve
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
      | OrgOnboardingSagaEvent.CreateLegalEntityWithPartnerBankResponse _ ->
         activityIsDone Activity.CreateLegalEntityWithPartnerBank
      | OrgOnboardingSagaEvent.CreateInternalAccountWithPartnerBankResponse _ ->
         activityIsDone Activity.CreateInternalAccountWithPartnerBank
      | OrgOnboardingSagaEvent.InitializedPrimaryVirtualAccount ->
         activityIsDone Activity.InitializePrimaryVirtualAccount
      | OrgOnboardingSagaEvent.InitializeOrgSettingsCacheResponse _ ->
         activityIsDone Activity.InitializeOrgSettingsCache
      | OrgOnboardingSagaEvent.OrgActivated ->
         activityIsDone Activity.ActivateOrg

   if saga.Status = OrgOnboardingSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

// Org onboarding saga is started by a submitted application
// event coming from the Org actor.
let onStartEventPersisted
   (registry: #IEmailActor & #IKYCServiceActor)
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
               BusinessName = info.BusinessDetails.BusinessName
            }
      }

      registry.EmailActor() <! emailMsg

      registry.KYCServiceActor()
      <! KYCMessage.VerifyApplication {
         OrgId = evt.OrgId
         CorrelationId = evt.CorrelationId
         Application = info
      }

type OperationEnv = {
   logError: string -> unit
   sendEventToSelf:
      OrgId -> CorrelationId -> Async<OrgOnboardingSagaEvent> -> unit
   OrgSettingsCache: OrgSettingsCache
}

let onEventPersisted
   (registry:
      #IAccountActor & #IOrgActor & #IEmailActor & #IKYCServiceActor & #IPartnerBankServiceActor)
   (operationEnv: OperationEnv)
   (previousState: OrgOnboardingSaga)
   (updatedState: OrgOnboardingSaga)
   (evt: OrgOnboardingSagaEvent)
   =
   let application = updatedState.Application
   let orgId = updatedState.OrgId
   let corrId = updatedState.CorrelationId

   let sagaMetadata = {
      OrgId = orgId
      CorrelationId = corrId
   }

   let sendApplicationRequiresSupportEmail (reason: string) =
      let emailMsg = {
         OrgId = orgId
         CorrelationId = corrId
         Info = EmailInfo.ApplicationErrorRequiresSupport reason
      }

      registry.EmailActor() <! emailMsg

   let sendApplicationAcceptedEmail () =
      let emailMsg = {
         OrgId = orgId
         CorrelationId = corrId
         Info =
            EmailInfo.OrgOnboardingApplicationAccepted {
               Email = application.AdminTeamEmail
               BusinessName = application.BusinessDetails.BusinessName
            }
      }

      registry.EmailActor() <! emailMsg

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
                  BusinessName = application.BusinessDetails.BusinessName
               }
               Reason = reason.Display
            }
      }

      registry.EmailActor() <! emailMsg

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
                  BusinessName = application.BusinessDetails.BusinessName
               }
               Reason = reason.Display
            }
      }

      registry.EmailActor() <! emailMsg

   let verifyOrg () =
      registry.KYCServiceActor()
      <! KYCMessage.VerifyApplication {
         OrgId = orgId
         CorrelationId = corrId
         Application = application
      }

   let createLegalEntityWithPartnerBank () =
      registry.PartnerBankServiceActor()
      <! PartnerBankServiceMessage.CreateLegalEntity {
         Detail = application.BusinessDetails
         SagaMetadata = sagaMetadata
      }

   let createInternalAccountWithPartnerBank () =
      match updatedState.PartnerBankLegalEntity with
      | Some entity ->
         registry.PartnerBankServiceActor()
         <! PartnerBankServiceMessage.CreateInternalAccount {
            LegalEntityId = entity.Id
            AccountName = "Operations"
            SagaMetadata = sagaMetadata
         }
      | None ->
         operationEnv.logError "Attempted to create account before legal entity"

   let initializeVirtualAccount
      (partnerBankLink: PartnerBankInternalAccountLink)
      =
      let parentAccountId = application.ParentAccountId

      let msg =
         InitializePrimaryCheckingAccountCommand.create {
            OrgId = orgId
            CorrelationId = corrId
            ParentAccountId = parentAccountId
            PartnerBankLink = partnerBankLink
         }
         |> AccountCommand.InitializePrimaryCheckingAccount
         |> AccountMessage.StateChange

      registry.AccountActor parentAccountId <! msg

   let initOrgSettingsCache (partnerBankLink: PartnerBankInternalAccountLink) =
      let asyncEvt = async {
         let! res =
            operationEnv.OrgSettingsCache.Update orgId {
               AdminTeamEmail = application.AdminTeamEmail
               ParentAccountId = application.ParentAccountId
               PartnerBankInternalAccountLink = partnerBankLink
            }

         return OrgOnboardingSagaEvent.InitializeOrgSettingsCacheResponse res
      }

      operationEnv.sendEventToSelf orgId corrId asyncEvt

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

      registry.OrgActor orgId <! msg

   match evt with
   | OrgOnboardingSagaEvent.KYCResponse res ->
      match res with
      | Ok _ -> createLegalEntityWithPartnerBank ()
      | Error err ->
         match err with
         | OrgOnboardingVerificationError.RequiresUpdatedInfo infoRequired ->
            sendApplicationRequiresRevisionForKYCServiceEmail infoRequired
         | OrgOnboardingVerificationError.Rejected reason ->
            sendApplicationRejectedEmail reason
   | OrgOnboardingSagaEvent.ReceivedInfoFixDemandedByKYCService _ ->
      verifyOrg ()
   | OrgOnboardingSagaEvent.CreateLegalEntityWithPartnerBankResponse res ->
      match res with
      | Ok _ -> createInternalAccountWithPartnerBank ()
      | Error reason ->
         if
            previousState.LifeCycle.ActivityHasRemainingAttempts
               Activity.CreateLegalEntityWithPartnerBank
         then
            createLegalEntityWithPartnerBank ()
         else
            sendApplicationRequiresSupportEmail reason
   | OrgOnboardingSagaEvent.CreateInternalAccountWithPartnerBankResponse res ->
      match res with
      | Ok link -> initializeVirtualAccount link
      | Error reason ->
         if
            previousState.LifeCycle.ActivityHasRemainingAttempts
               Activity.CreateInternalAccountWithPartnerBank
         then
            createInternalAccountWithPartnerBank ()
         else
            sendApplicationRequiresSupportEmail reason
   | OrgOnboardingSagaEvent.SupportTeamResolvedPartnerBankLink ->
      // Support team resolved dispute with partner bank so
      // reattempt linking parent account with partner bank.
      createInternalAccountWithPartnerBank ()
   | OrgOnboardingSagaEvent.InitializedPrimaryVirtualAccount ->
      updatedState.PartnerBankInternalAccount
      |> Option.iter initOrgSettingsCache
   | OrgOnboardingSagaEvent.InitializeOrgSettingsCacheResponse res ->
      match res with
      | Ok() -> activateOrg ()
      | Error err ->
         operationEnv.logError
            $"Error initializing org settings cache {orgId} {err}"
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
         | Activity.WaitForSupportTeamToResolve
         | Activity.WaitForInfoFixDemandedByKYCService -> ()
         | Activity.SendApplicationProcessingNotification ->
            let emailMsg = {
               OrgId = orgId
               CorrelationId = corrId
               Info =
                  EmailInfo.OrgOnboardingApplicationSubmitted {
                     Email = application.AdminTeamEmail
                     BusinessName = application.BusinessDetails.BusinessName
                  }
            }

            registry.EmailActor() <! emailMsg
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
         | Activity.CreateLegalEntityWithPartnerBank ->
            createLegalEntityWithPartnerBank ()
         | Activity.CreateInternalAccountWithPartnerBank ->
            createInternalAccountWithPartnerBank ()
         | Activity.InitializePrimaryVirtualAccount ->
            updatedState.PartnerBankInternalAccount
            |> Option.iter initializeVirtualAccount
         | Activity.InitializeOrgSettingsCache ->
            updatedState.PartnerBankInternalAccount
            |> Option.iter initOrgSettingsCache
         | Activity.ActivateOrg -> activateOrg ()
