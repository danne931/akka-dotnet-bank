namespace Bank.Org.Domain

open Lib.SharedTypes
open Bank.Account.Domain

type OrgCommand =
   | CreateOrg of CreateOrgCommand
   | FinalizeOrgOnboarding of FinalizeOrgOnboardingCommand
   | ConfigureFeatureFlag of ConfigureFeatureFlagCommand
   | ConfigureApprovalRule of CommandApprovalRule.ConfigureApprovalRuleCommand
   | DeleteApprovalRule of CommandApprovalRule.DeleteApprovalRuleCommand
   | RequestCommandApproval of CommandApprovalProgress.RequestCommandApproval
   | AcquireCommandApproval of CommandApprovalProgress.AcquireCommandApproval
   | DeclineCommandApproval of CommandApprovalProgress.DeclineCommandApproval
   | TerminateCommandApproval of
      CommandApprovalProgress.TerminateCommandApproval

type OrgEvent =
   | OrgCreated of BankEvent<OrgCreated>
   | OrgOnboardingFinished of BankEvent<OrgOnboardingFinished>
   | FeatureFlagConfigured of BankEvent<FeatureFlagConfigured>
   | CommandApprovalRuleConfigured of BankEvent<CommandApprovalRule.T>
   | CommandApprovalRuleDeleted of
      BankEvent<CommandApprovalRule.ApprovalRuleDeleted>
   | CommandApprovalRequested of
      BankEvent<CommandApprovalProgress.CommandApprovalRequested>
   | CommandApprovalAcquired of
      BankEvent<CommandApprovalProgress.CommandApprovalAcquired>
   | CommandApprovalProcessCompleted of
      BankEvent<CommandApprovalProgress.CommandApprovalProcessCompleted>
   | CommandApprovalDeclined of
      BankEvent<CommandApprovalProgress.CommandApprovalDeclined>
   | CommandApprovalTerminated of
      BankEvent<CommandApprovalProgress.CommandApprovalTerminated>

type OpenEventEnvelope = OrgEvent * Envelope

[<RequireQualifiedAccess>]
module OrgEnvelope =
   let get (evt: BankEvent<'E>) : Envelope = {
      Id = evt.Id
      EntityId = evt.EntityId
      OrgId = evt.OrgId
      CorrelationId = evt.CorrelationId
      InitiatedById = evt.InitiatedById
      Timestamp = evt.Timestamp
      EventName = evt.EventName
   }

   let wrap (o: BankEvent<_>) : OrgEvent =
      match box o with
      | :? BankEvent<OrgCreated> as evt -> OrgCreated evt
      | :? BankEvent<OrgOnboardingFinished> as evt -> OrgOnboardingFinished evt
      | :? BankEvent<FeatureFlagConfigured> as evt -> FeatureFlagConfigured evt
      | :? BankEvent<CommandApprovalRule.T> as evt ->
         CommandApprovalRuleConfigured evt
      | :? BankEvent<CommandApprovalRule.ApprovalRuleDeleted> as evt ->
         CommandApprovalRuleDeleted evt
      | :? BankEvent<CommandApprovalProgress.CommandApprovalRequested> as evt ->
         CommandApprovalRequested evt
      | :? BankEvent<CommandApprovalProgress.CommandApprovalAcquired> as evt ->
         CommandApprovalAcquired evt
      | :? BankEvent<CommandApprovalProgress.CommandApprovalProcessCompleted> as evt ->
         CommandApprovalProcessCompleted evt
      | :? BankEvent<CommandApprovalProgress.CommandApprovalDeclined> as evt ->
         CommandApprovalDeclined evt
      | :? BankEvent<CommandApprovalProgress.CommandApprovalTerminated> as evt ->
         CommandApprovalTerminated evt
      | _ -> failwith "Missing definition for OrgEvent message"

   let unwrap (o: OrgEvent) : OpenEventEnvelope =
      match o with
      | OrgCreated evt -> wrap evt, get evt
      | OrgOnboardingFinished evt -> wrap evt, get evt
      | FeatureFlagConfigured evt -> wrap evt, get evt
      | CommandApprovalRuleConfigured evt -> wrap evt, get evt
      | CommandApprovalRuleDeleted evt -> wrap evt, get evt
      | CommandApprovalRequested evt -> wrap evt, get evt
      | CommandApprovalAcquired evt -> wrap evt, get evt
      | CommandApprovalProcessCompleted evt -> wrap evt, get evt
      | CommandApprovalDeclined evt -> wrap evt, get evt
      | CommandApprovalTerminated evt -> wrap evt, get evt

type Org = {
   OrgId: OrgId
   Name: string
   Status: OrgStatus
   FeatureFlags: FeatureFlagOrgSettings
   CommandApprovalRules: Map<CommandApprovalRuleId, CommandApprovalRule.T>
   CommandApprovalProgress:
      Map<CommandApprovalProgressId, CommandApprovalProgress.T>
}

type OrgWithEvents = { Info: Org; Events: OrgEvent list }

type OrgWithAccountProfiles = {
   Org: Org
   AccountProfiles: Map<AccountId, AccountProfile>
   Balance: decimal
} with

   member x.Accounts: Map<AccountId, Account> =
      x.AccountProfiles |> Map.map (fun _ profile -> profile.Account)

type OrgMessage =
   | GetOrg
   | ApprovableEmployeeRequest of ApprovableCommand
   | StateChange of OrgCommand
   | Event of OrgEvent
