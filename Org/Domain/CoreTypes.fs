namespace Bank.Org.Domain

open Lib.SharedTypes

[<RequireQualifiedAccess>]
type BusinessType =
   | LimitedPartnership
   | Trust
   | SoleProprietorship
   | Corporation
   | LLC
   | GeneralPartnership
   | ProfessionalAssociation
   | Government
   | NonProfit
   | Other

type Address = {
   City: string
   CountryCode: string
   Line1: string
   Line2: string
   PostalCode: string
   State: string
}

module Address =
   let empty = {
      City = ""
      CountryCode = ""
      Line1 = ""
      Line2 = ""
      PostalCode = ""
      State = ""
   }

type BusinessDetails = {
   Address: Address
   BusinessName: string
   Description: string
   EmployerIdentificationNumber: string
   LegalType: BusinessType
   Website: string option
}

type Merchant = {
   OrgId: OrgId
   Name: string
   Alias: string option
}

type FeatureFlagOrgSettings = {
   SocialTransferDiscoveryPrimaryAccountId: AccountId option
}

[<RequireQualifiedAccess>]
type OrgStatus =
   | InitialEmptyState
   | PendingOnboardingTasksFulfilled
   | Active

[<RequireQualifiedAccess>]
type OrgAccrualMetricEventType =
   | InternalTransferBetweenOrgs
   | DomesticTransfer

/// This is currently used to restrict command approval requests
/// with corresponding AmountDailyLimit rules configured but may
/// have additional uses in the future.
type OrgAccrualMetric = {
   TransactionAmount: decimal
   EventType: OrgAccrualMetricEventType
   CorrelationId: CorrelationId
   InitiatedBy: Initiator
   Timestamp: System.DateTime
}

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
   | PartnerBankLegalEntityCreateError of string
   | PartnerBankInternalAccountCreateError of string
