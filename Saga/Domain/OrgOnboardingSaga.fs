module OrgOnboardingSaga

open System

open Lib.SharedTypes
open Bank.Org.Domain
open Bank.Account.Domain
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
   | CreateLegalEntityWithPartnerBankResponse of
      Result<LegalBusinessEntityCreateResponse, string>
   | CreateInternalAccountWithPartnerBankResponse of
      Result<PartnerBankInternalAccountLink, string>
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
   | CreateLegalEntityWithPartnerBank
   | CreateInternalAccountWithPartnerBank
   | InitializePrimaryVirtualAccount
   | InitializeOrgSettingsCache
   | ActivateOrg
   | SendApplicationAcceptedNotification
   | SendApplicationRejectedNotification
   | WaitForInfoFixDemandedByKYCService
   | SendApplicationRequiresRevisionForKYCServiceNotification
   | WaitForSupportTeamToResolve

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForInfoFixDemandedByKYCService
         | WaitForSupportTeamToResolve -> 0
         | SubmitApplication -> 1
         | CreateLegalEntityWithPartnerBank
         | CreateInternalAccountWithPartnerBank -> 4
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | SubmitApplication
         | WaitForInfoFixDemandedByKYCService
         | WaitForSupportTeamToResolve -> None
         | KYCVerification
         | CreateLegalEntityWithPartnerBank
         | CreateInternalAccountWithPartnerBank -> Some(TimeSpan.FromMinutes 2.)
         | SendApplicationProcessingNotification
         | SendApplicationRequiresRevisionForKYCServiceNotification
         | SendApplicationAcceptedNotification
         | SendApplicationRejectedNotification -> Some(TimeSpan.FromMinutes 4.)
         | InitializePrimaryVirtualAccount
         | InitializeOrgSettingsCache
         | ActivateOrg -> Some(TimeSpan.FromSeconds 5.)

type OutgoingCommandIdempotencyKeys = {
   InitPrimaryCheckingAccount: EventId
   FinishOrgOnboarding: EventId
}

type OrgOnboardingSaga = {
   OrgId: OrgId
   CorrelationId: CorrelationId
   Application: OrgOnboardingApplicationSubmitted
   StartEvent: OrgOnboardingSagaStartEvent
   StartedAt: DateTime
   Events: OrgOnboardingSagaEvent list
   Status: OrgOnboardingSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
   ApplicationRequiresRevision:
      OrgOnboardingApplicationRequiresUpdateInfo option
   OutgoingCommandIdempotencyKeys: OutgoingCommandIdempotencyKeys
} with

   member x.PartnerBankLegalEntity =
      x.Events
      |> List.tryPick (function
         | OrgOnboardingSagaEvent.CreateLegalEntityWithPartnerBankResponse res ->
            Result.toOption res
         | _ -> None)

   member x.PartnerBankInternalAccount =
      x.Events
      |> List.tryPick (function
         | OrgOnboardingSagaEvent.CreateInternalAccountWithPartnerBankResponse res ->
            Result.toOption res
         | _ -> None)
