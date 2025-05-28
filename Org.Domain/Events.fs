namespace Bank.Org.Domain

open Lib.SharedTypes

type OrgOnboardingApplicationSubmitted = {
   LegalBusinessName: string
   AdminTeamEmail: Email
   ParentAccountId: ParentAccountId
   EmployerIdentificationNumber: string
}

type OrgOnboardingFinished = { ParentAccountId: ParentAccountId }

type FeatureFlagConfigured = { Config: FeatureFlagOrgSettings }
