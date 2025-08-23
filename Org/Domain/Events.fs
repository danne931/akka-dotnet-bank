namespace Bank.Org.Domain

open Lib.SharedTypes

type OrgOnboardingApplicationSubmitted = {
   AdminTeamEmail: Email.Email
   ParentAccountId: ParentAccountId
   BusinessDetails: BusinessDetails
}

type OrgOnboardingFinished = { ParentAccountId: ParentAccountId }

type FeatureFlagConfigured = { Config: FeatureFlagOrgSettings }
