namespace Bank.Org.Domain

open Lib.SharedTypes

type OrgCreated = { Name: string; AdminTeamEmail: Email }

type OrgOnboardingFinished = { EmployerIdentificationNumber: int }

type FeatureFlagConfigured = { Config: FeatureFlagOrgSettings }
