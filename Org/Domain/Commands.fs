namespace Bank.Org.Domain

open Validus

open Lib.SharedTypes

type SubmitOrgOnboardingApplicationInput = {
   LegalBusinessName: string
   AdminTeamEmail: Email
   EmployerIdentificationNumber: string
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   InitiatedBy: Initiator
}

type SubmitOrgOnboardingApplicationCommand =
   Command<SubmitOrgOnboardingApplicationInput>

module SubmitOrgOnboardingApplicationCommand =
   let create (data: SubmitOrgOnboardingApplicationInput) =
      Command.create
         (OrgId.toEntityId data.OrgId)
         data.OrgId
         (CorrelationId.create ())
         data.InitiatedBy
         data

   let toEvent
      (cmd: SubmitOrgOnboardingApplicationCommand)
      : ValidationResult<BankEvent<OrgOnboardingApplicationSubmitted>>
      =
      BankEvent.create2<
         SubmitOrgOnboardingApplicationInput,
         OrgOnboardingApplicationSubmitted
       >
         cmd
         {
            LegalBusinessName = cmd.Data.LegalBusinessName
            AdminTeamEmail = cmd.Data.AdminTeamEmail
            EmployerIdentificationNumber = cmd.Data.EmployerIdentificationNumber
            ParentAccountId = cmd.Data.ParentAccountId
         }
      |> Ok

type FinishOrgOnboardingInput = {
   OrgId: OrgId
   CorrelationId: CorrelationId
   InitiatedBy: Initiator
   ParentAccountId: ParentAccountId
}

type FinishOrgOnboardingCommand = Command<FinishOrgOnboardingInput>

module FinishOrgOnboardingCommand =
   let create (data: FinishOrgOnboardingInput) =
      Command.create
         (OrgId.toEntityId data.OrgId)
         data.OrgId
         data.CorrelationId
         data.InitiatedBy
         data

   let toEvent
      (cmd: FinishOrgOnboardingCommand)
      : ValidationResult<BankEvent<OrgOnboardingFinished>>
      =
      BankEvent.create2<FinishOrgOnboardingInput, OrgOnboardingFinished> cmd {
         ParentAccountId = cmd.Data.ParentAccountId
      }
      |> Ok

type ConfigureFeatureFlagCommand = Command<FeatureFlagConfigured>

module ConfigureFeatureFlagCommand =
   let create orgId (initiator: Initiator) (data: FeatureFlagConfigured) =
      Command.create
         (OrgId.toEntityId orgId)
         orgId
         (CorrelationId.create ())
         initiator
         data

   let toEvent
      (cmd: ConfigureFeatureFlagCommand)
      : ValidationResult<BankEvent<FeatureFlagConfigured>>
      =
      BankEvent.create<FeatureFlagConfigured> cmd |> Ok
