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

type DailyAccrual = {
   PaymentsPaid: decimal
   InternalTransferBetweenOrgs: decimal
   DomesticTransfer: decimal
}
