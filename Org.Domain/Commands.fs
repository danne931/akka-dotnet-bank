namespace Bank.Org.Domain

open Validus

open Lib.SharedTypes

type CreateOrgInput = {
   Name: string
   OrgId: OrgId
   InitiatedBy: InitiatedById
}

type CreateOrgCommand = Command<CreateOrgInput>

module CreateOrgCommand =
   let create (data: CreateOrgInput) =
      Command.create
         (OrgId.toEntityId data.OrgId)
         data.OrgId
         (CorrelationId.create ())
         data.InitiatedBy
         data

   let toEvent
      (cmd: CreateOrgCommand)
      : ValidationResult<BankEvent<OrgCreated>>
      =
      BankEvent.create2<CreateOrgInput, OrgCreated> cmd { Name = cmd.Data.Name }
      |> Ok

type FinalizeOrgOnboardingInput = {
   OrgId: OrgId
   CorrelationId: CorrelationId
   EmployerIdentificationNumber: int
   InitiatedBy: InitiatedById
}

type FinalizeOrgOnboardingCommand = Command<FinalizeOrgOnboardingInput>

module FinalizeOrgOnboardingCommand =
   let create (data: FinalizeOrgOnboardingInput) =
      Command.create
         (OrgId.toEntityId data.OrgId)
         data.OrgId
         data.CorrelationId
         data.InitiatedBy
         data

   let toEvent
      (cmd: FinalizeOrgOnboardingCommand)
      : ValidationResult<BankEvent<OrgOnboardingFinished>>
      =
      BankEvent.create2<FinalizeOrgOnboardingInput, OrgOnboardingFinished> cmd {
         EmployerIdentificationNumber = cmd.Data.EmployerIdentificationNumber
      }
      |> Ok

type ConfigureFeatureFlagCommand = Command<FeatureFlagConfigured>

module ConfigureFeatureFlagCommand =
   let create orgId initiatedBy (data: FeatureFlagConfigured) =
      Command.create
         (OrgId.toEntityId orgId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: ConfigureFeatureFlagCommand)
      : ValidationResult<BankEvent<FeatureFlagConfigured>>
      =
      BankEvent.create<FeatureFlagConfigured> cmd |> Ok
