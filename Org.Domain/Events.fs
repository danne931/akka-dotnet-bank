namespace Bank.Org.Domain

type OrgCreated = { Name: string }

type OrgOnboardingFinished = { EmployerIdentificationNumber: int }

type FeatureFlagConfigured = { Config: FeatureFlagOrgSettings }
