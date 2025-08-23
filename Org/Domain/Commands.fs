namespace Bank.Org.Domain

open Validus

open Lib.SharedTypes
open Email

type SubmitOrgOnboardingApplicationInput = {
   AdminTeamEmail: Email
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   BusinessDetails: BusinessDetails
   InitiatedBy: Initiator
}

type SubmitOrgOnboardingApplicationCommand =
   Command<SubmitOrgOnboardingApplicationInput>

module SubmitOrgOnboardingApplicationCommand =
   let create (data: SubmitOrgOnboardingApplicationInput) =
      Command.create
         data.OrgId.AsEntityId
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
            AdminTeamEmail = cmd.Data.AdminTeamEmail
            ParentAccountId = cmd.Data.ParentAccountId
            BusinessDetails = cmd.Data.BusinessDetails
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
         data.OrgId.AsEntityId
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
   let create
      (orgId: OrgId)
      (initiator: Initiator)
      (data: FeatureFlagConfigured)
      =
      Command.create
         orgId.AsEntityId
         orgId
         (CorrelationId.create ())
         initiator
         data

   let toEvent
      (cmd: ConfigureFeatureFlagCommand)
      : ValidationResult<BankEvent<FeatureFlagConfigured>>
      =
      BankEvent.create<FeatureFlagConfigured> cmd |> Ok
