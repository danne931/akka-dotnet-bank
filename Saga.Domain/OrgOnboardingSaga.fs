module OrgOnboardingSaga

open System

open Lib.SharedTypes
open Bank.Org.Domain
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
   | LinkAccountToPartnerBankResponse of Result<PartnerBankAccountLink, string>
   | InitializedPrimaryVirtualAccount
   | InitializeOrgSettingsCacheResponse of Result<unit, Err>
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
   | InitializeOrgSettingsCache
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
         | LinkAccountToPartnerBank -> Some(TimeSpan.FromMinutes 2.)
         | SendApplicationProcessingNotification
         | SendApplicationRequiresRevisionForKYCServiceNotification
         | SendApplicationAcceptedNotification
         | SendApplicationRejectedNotification -> Some(TimeSpan.FromMinutes 4.)
         | InitializePrimaryVirtualAccount
         | InitializeOrgSettingsCache
         | ActivateOrg -> Some(TimeSpan.FromSeconds 5.)

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
