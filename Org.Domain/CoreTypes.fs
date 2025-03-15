namespace Bank.Org.Domain

open Lib.SharedTypes

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
   | PaymentPaid
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
