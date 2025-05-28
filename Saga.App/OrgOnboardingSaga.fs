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

[<RequireQualifiedAccess>]
type OrgOnboardingApplicationRequiresUpdateInfo =
   | InvalidBusinessName
   | InvalidAddress
   | InvalidEIN
   | Unknown of string

   member x.Display =
      match x with
      | InvalidBusinessName ->
         "The provided business name could not be verified."
      | InvalidAddress -> "The provided address could not be verified."
      | InvalidEIN -> "The provided EIN could not be validated with the IRS."
      | Unknown reason -> reason

[<RequireQualifiedAccess>]
type OrgOnboardingApplicationRejectedReason =
   | NotRegistered
   | NotInGoodStanding

   member x.Display =
      match x with
      | NotRegistered -> "The business is not legally registered."
      | NotInGoodStanding ->
         "The business has been determined to not be in good standing."

[<RequireQualifiedAccess>]
type OrgOnboardingVerificationError =
   | RequiresUpdatedInfo of OrgOnboardingApplicationRequiresUpdateInfo
   | Rejected of OrgOnboardingApplicationRejectedReason

[<RequireQualifiedAccess>]
type OrgOnboardingFailureReason =
   | KYCRejectedReason of OrgOnboardingApplicationRejectedReason
   | PartnerBankLinkError of string

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
   | Start of OrgOnboardingSagaStartEvent
   | ApplicationProcessingNotificationSent
   | KYCResponse of Result<unit, OrgOnboardingVerificationError>
   | ReceivedInfoFixDemandedByKYCService of OrgOnboardingApplicationSubmitted
   | LinkAccountToPartnerBankResponse of
      Result<AccountNumber * RoutingNumber, string>
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
         | SubmitApplication
         | WaitForInfoFixDemandedByKYCService
         | WaitForSupportTeamToResolvePartnerBankLink -> 1
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


let applyEvent
   (state: OrgOnboardingSaga option)
   (e: OrgOnboardingSagaEvent)
   (timestamp: DateTime)
   : OrgOnboardingSaga option
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp
   let failActivity = SagaLifeCycle.failActivity timestamp
   let retryActivity = SagaLifeCycle.retryActivity timestamp

   match state with
   | None ->
      match e with
      | OrgOnboardingSagaEvent.Start startEvt ->
         match startEvt with
         | OrgOnboardingSagaStartEvent.ApplicationSubmitted evt ->
            Some {
               OrgId = evt.OrgId
               CorrelationId = evt.CorrelationId
               Status = OrgOnboardingSagaStatus.InProgress
               Events = [ e ]
               Application = evt.Data
               ApplicationRequiresRevision = None
               LifeCycle = {
                  SagaLifeCycle.empty with
                     InProgress = [
                        ActivityLifeCycle.init
                           timestamp
                           Activity.SendApplicationProcessingNotification
                        ActivityLifeCycle.init
                           timestamp
                           Activity.KYCVerification
                     ]
                     Completed = [
                        {
                           Start = timestamp
                           End = Some timestamp
                           Activity = Activity.SubmitApplication
                           MaxAttempts =
                              (Activity.SubmitApplication :> IActivity)
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
         | OrgOnboardingSagaEvent.Start _ -> state
         | OrgOnboardingSagaEvent.ApplicationProcessingNotificationSent -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity
                        Activity.SendApplicationProcessingNotification
           }
         | OrgOnboardingSagaEvent.KYCResponse response ->
            let activity = Activity.KYCVerification

            match response with
            | Ok _ -> {
               state with
                  LifeCycle =
                     state.LifeCycle
                     |> finishActivity activity
                     |> addActivity Activity.LinkAccountToPartnerBank
              }
            | Error err ->
               match err with
               | OrgOnboardingVerificationError.RequiresUpdatedInfo info -> {
                  state with
                     ApplicationRequiresRevision = Some info
                     LifeCycle =
                        state.LifeCycle
                        |> failActivity activity
                        |> addActivity
                              Activity.WaitForInfoFixDemandedByKYCService
                        |> addActivity
                              Activity.SendApplicationRequiresRevisionForKYCServiceNotification
                 }
               | OrgOnboardingVerificationError.Rejected reason -> {
                  state with
                     Status =
                        OrgOnboardingFailureReason.KYCRejectedReason reason
                        |> OrgOnboardingSagaStatus.Failed
                     LifeCycle =
                        state.LifeCycle
                        |> failActivity activity
                        |> addActivity
                              Activity.SendApplicationRejectedNotification
                 }
         | OrgOnboardingSagaEvent.ApplicationRequiresRevisionForKYCServiceNotificationSent -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity
                        Activity.SendApplicationRequiresRevisionForKYCServiceNotification
           }
         | OrgOnboardingSagaEvent.ReceivedInfoFixDemandedByKYCService updatedInfo -> {
            state with
               Application = updatedInfo
               ApplicationRequiresRevision = None
               Status = OrgOnboardingSagaStatus.InProgress
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.WaitForInfoFixDemandedByKYCService
                  |> addActivity Activity.KYCVerification
           }
         | OrgOnboardingSagaEvent.LinkAccountToPartnerBankResponse res ->
            let activity = Activity.LinkAccountToPartnerBank

            match res with
            | Error err ->
               if state.LifeCycle.ActivityHasRemainingAttempts activity then
                  {
                     state with
                        LifeCycle = retryActivity activity state.LifeCycle
                  }
               else
                  {
                     state with
                        Status =
                           OrgOnboardingFailureReason.PartnerBankLinkError err
                           |> OrgOnboardingSagaStatus.Failed
                        LifeCycle =
                           state.LifeCycle
                           |> failActivity activity
                           |> addActivity
                                 Activity.WaitForSupportTeamToResolvePartnerBankLink
                  }
            | Ok _ -> {
               state with
                  LifeCycle =
                     state.LifeCycle
                     |> finishActivity activity
                     |> addActivity Activity.InitializePrimaryVirtualAccount
              }
         | OrgOnboardingSagaEvent.InitializedPrimaryVirtualAccount -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.InitializePrimaryVirtualAccount
                  |> addActivity Activity.ActivateOrg
           }
         | OrgOnboardingSagaEvent.OrgActivated -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.ActivateOrg
                  |> addActivity Activity.SendApplicationAcceptedNotification
           }
         | OrgOnboardingSagaEvent.ApplicationAcceptedNotificationSent -> {
            state with
               Status = OrgOnboardingSagaStatus.Completed
               LifeCycle =
                  finishActivity
                     Activity.SendApplicationAcceptedNotification
                     state.LifeCycle
           }
         | OrgOnboardingSagaEvent.ApplicationRejectedNotificationSent -> {
            state with
               LifeCycle =
                  finishActivity
                     Activity.SendApplicationRejectedNotification
                     state.LifeCycle
           }
         | OrgOnboardingSagaEvent.SupportTeamResolvedPartnerBankLink -> {
            state with
               Status = OrgOnboardingSagaStatus.InProgress
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity
                        Activity.WaitForSupportTeamToResolvePartnerBankLink
                  |> addActivity Activity.LinkAccountToPartnerBank
           }
         | OrgOnboardingSagaEvent.EvaluateRemainingWork -> {
            state with
               LifeCycle =
                  SagaLifeCycle.retryActivitiesAfterInactivity
                     timestamp
                     state.LifeCycle
           }
         | OrgOnboardingSagaEvent.ResetInProgressActivityAttempts -> {
            state with
               LifeCycle =
                  SagaLifeCycle.resetInProgressActivities state.LifeCycle
           }

      Some {
         state with
            Events = e :: state.Events
      }

let stateTransition
   (state: OrgOnboardingSaga option)
   (evt: OrgOnboardingSagaEvent)
   (timestamp: DateTime)
   : Result<OrgOnboardingSaga option, SagaStateTransitionError>
   =
   match state with
   | None ->
      match evt with
      | OrgOnboardingSagaEvent.Start _ -> Ok(applyEvent state evt timestamp)
      | _ -> Error SagaStateTransitionError.HasNotStarted
   | Some saga ->
      let eventIsStartEvent =
         match evt with
         | OrgOnboardingSagaEvent.Start _ -> true
         | _ -> false

      let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

      let invalidStepProgression =
         match evt with
         | OrgOnboardingSagaEvent.Start _
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
      elif eventIsStartEvent then
         Error SagaStateTransitionError.HasAlreadyStarted
      elif invalidStepProgression then
         Error SagaStateTransitionError.InvalidStepProgression
      else
         Ok(applyEvent state evt timestamp)

type PersistenceHandlerDependencies = {
   getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>
   getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>
   getOrgRef: OrgId -> IEntityRef<OrgMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   kycVerification:
      OrgOnboardingApplicationSubmitted -> Async<OrgOnboardingSagaEvent>
   linkAccountToPartnerBank:
      OrgOnboardingApplicationSubmitted -> Async<OrgOnboardingSagaEvent>
   sendMessageToSelf:
      OrgId -> CorrelationId -> Async<OrgOnboardingSagaEvent> -> unit
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

      dep.sendMessageToSelf
         evt.OrgId
         evt.CorrelationId
         (dep.kycVerification evt.Data)

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
   (previousState: OrgOnboardingSaga)
   (updatedState: OrgOnboardingSaga)
   (evt: OrgOnboardingSagaEvent)
   =
   let application = updatedState.Application

   let sendApplicationAcceptedEmail () =
      let emailMsg = {
         OrgId = updatedState.OrgId
         CorrelationId = updatedState.CorrelationId
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
         OrgId = updatedState.OrgId
         CorrelationId = updatedState.CorrelationId
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
         OrgId = updatedState.OrgId
         CorrelationId = updatedState.CorrelationId
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
      dep.sendMessageToSelf
         updatedState.OrgId
         updatedState.CorrelationId
         (dep.kycVerification application)

   let linkAccountToPartnerBank () =
      dep.sendMessageToSelf
         updatedState.OrgId
         updatedState.CorrelationId
         (dep.linkAccountToPartnerBank application)

   let initializeVirtualAccount (accountNumber, routingNumber) =
      let parentAccountId = application.ParentAccountId

      let msg =
         InitializePrimaryCheckingAccountCommand.create {
            OrgId = updatedState.OrgId
            CorrelationId = updatedState.CorrelationId
            ParentAccountId = parentAccountId
            PartnerBankAccountNumber = accountNumber
            PartnerBankRoutingNumber = routingNumber
         }
         |> AccountCommand.InitializePrimaryCheckingAccount
         |> AccountMessage.StateChange

      dep.getAccountRef parentAccountId <! msg

   let activateOrg () =
      let orgId = updatedState.OrgId

      let msg =
         FinishOrgOnboardingCommand.create {
            OrgId = updatedState.OrgId
            ParentAccountId = updatedState.Application.ParentAccountId
            CorrelationId = updatedState.CorrelationId
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
                  OrgId = updatedState.OrgId
                  CorrelationId = updatedState.CorrelationId
                  Info = EmailInfo.ApplicationErrorRequiresSupport reason
               }
   | OrgOnboardingSagaEvent.SupportTeamResolvedPartnerBankLink ->
      // Support team resolved dispute with partner bank so
      // reattempt linking parent account with partner bank.
      linkAccountToPartnerBank ()
   | OrgOnboardingSagaEvent.InitializedPrimaryVirtualAccount -> activateOrg ()
   | OrgOnboardingSagaEvent.OrgActivated -> sendApplicationAcceptedEmail ()
   | OrgOnboardingSagaEvent.Start _
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
               OrgId = updatedState.OrgId
               CorrelationId = updatedState.CorrelationId
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

let linkAccountToPartnerBank (info: OrgOnboardingApplicationSubmitted) = async {
   do! Async.Sleep(3000)

   // TODO: network request to partner bank

   let accountNum =
      AccountNumber.generate ()
      |> AccountNumber.fromString ""
      |> Result.toOption
      |> _.Value

   let res =
      (accountNum, RoutingNumber 123456789)
      |> Ok
      |> OrgOnboardingSagaEvent.LinkAccountToPartnerBankResponse

   return res
}

// Onboarding know-your-customer service requests & responses
// are based loosely on middesk.com API.
// TODO: Research & verify actual middesk API
module KnowYourCustomerService =
   type KYCServiceRequest = {
      name: string
      tax_id: string
      address: {|
         line1: string
         city: string
         state: string
         postal_code: string
      |}
   }

   type KYCServiceError = {
      code: string
      message: string
   } with

      member x.AsDomainError =
         match x.code with
         | "tax_id_invalid" ->
            OrgOnboardingApplicationRequiresUpdateInfo.InvalidEIN
         | "business_name_invalid" ->
            OrgOnboardingApplicationRequiresUpdateInfo.InvalidBusinessName
         | "business_address_invalid" ->
            OrgOnboardingApplicationRequiresUpdateInfo.InvalidAddress
         | other -> OrgOnboardingApplicationRequiresUpdateInfo.Unknown other

   type KYCServiceVerificationResponse = {
      registered: bool
      good_standing: bool
      errors: KYCServiceError list
   }

   let verifyOrg (info: OrgOnboardingApplicationSubmitted) = async {
      do! Async.Sleep(3000)

      // TODO: HTTP to middesk.com API

      return OrgOnboardingSagaEvent.KYCResponse(Ok())
   }
