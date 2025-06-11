namespace Bank.Org.Domain

open System

open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Transfer.Domain
open CommandApproval

[<RequireQualifiedAccess>]
type OrgCommand =
   | SubmitOnboardingApplication of SubmitOrgOnboardingApplicationCommand
   | FinishOrgOnboarding of FinishOrgOnboardingCommand
   | ConfigureFeatureFlag of ConfigureFeatureFlagCommand
   | ConfigureApprovalRule of CommandApprovalRule.ConfigureApprovalRuleCommand
   | DeleteApprovalRule of CommandApprovalRule.DeleteApprovalRuleCommand
   | RequestCommandApproval of CommandApprovalProgress.RequestCommandApproval
   | AcquireCommandApproval of CommandApprovalProgress.AcquireCommandApproval
   | DeclineCommandApproval of CommandApprovalProgress.DeclineCommandApproval
   | TerminateCommandApproval of
      CommandApprovalProgress.TerminateCommandApproval

   member x.Envelope: Envelope =
      match x with
      | SubmitOnboardingApplication cmd -> Command.envelope cmd
      | FinishOrgOnboarding cmd -> Command.envelope cmd
      | ConfigureFeatureFlag cmd -> Command.envelope cmd
      | ConfigureApprovalRule cmd -> Command.envelope cmd
      | DeleteApprovalRule cmd -> Command.envelope cmd
      | RequestCommandApproval cmd -> Command.envelope cmd
      | AcquireCommandApproval cmd -> Command.envelope cmd
      | DeclineCommandApproval cmd -> Command.envelope cmd
      | TerminateCommandApproval cmd -> Command.envelope cmd

type OrgEvent =
   | OnboardingApplicationSubmitted of
      BankEvent<OrgOnboardingApplicationSubmitted>
   | OnboardingFinished of BankEvent<OrgOnboardingFinished>
   | FeatureFlagConfigured of BankEvent<FeatureFlagConfigured>
   | CommandApprovalRuleConfigured of
      BankEvent<CommandApprovalRule.ConfigureApprovalRule>
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
      InitiatedBy = evt.InitiatedBy
      Timestamp = evt.Timestamp
      EventName = evt.EventName
   }

   let wrap (o: BankEvent<_>) : OrgEvent =
      match box o with
      | :? BankEvent<OrgOnboardingApplicationSubmitted> as evt ->
         OnboardingApplicationSubmitted evt
      | :? BankEvent<OrgOnboardingFinished> as evt -> OnboardingFinished evt
      | :? BankEvent<FeatureFlagConfigured> as evt -> FeatureFlagConfigured evt
      | :? BankEvent<CommandApprovalRule.ConfigureApprovalRule> as evt ->
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
      | OnboardingApplicationSubmitted evt -> wrap evt, get evt
      | OnboardingFinished evt -> wrap evt, get evt
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
   ParentAccountId: ParentAccountId
   Name: string
   Status: OrgStatus
   FeatureFlags: FeatureFlagOrgSettings
   CommandApprovalRules: Map<CommandApprovalRuleId, CommandApprovalRule>
   CommandApprovalProgress:
      Map<CommandApprovalProgressId, CommandApprovalProgress.T>
   AdminTeamEmail: Email
   EmployerIdentificationNumber: string
}

module Org =
   let empty: Org = {
      OrgId = OrgId Guid.Empty
      ParentAccountId = ParentAccountId Guid.Empty
      Name = ""
      Status = OrgStatus.InitialEmptyState
      AdminTeamEmail = Email.empty
      FeatureFlags = {
         SocialTransferDiscoveryPrimaryAccountId = None
      }
      CommandApprovalRules = Map.empty
      CommandApprovalProgress = Map.empty
      EmployerIdentificationNumber = ""
   }

type OrgSnapshot = {
   Info: Org
   Events: OrgEvent list
   AccrualMetrics: Map<CorrelationId, OrgAccrualMetric>
}

module OrgSnapshot =
   let empty: OrgSnapshot = {
      Info = Org.empty
      Events = []
      AccrualMetrics = Map.empty
   }

type OrgWithAccountProfiles = {
   Org: Org
   AccountProfiles: Map<AccountId, AccountProfile>
   Balance: decimal
   DomesticTransferRecipients: Map<AccountId, DomesticTransferRecipient>
} with

   member x.Accounts: Map<AccountId, Account> =
      x.AccountProfiles |> Map.map (fun _ profile -> profile.Account)

   member x.CheckingAccounts: Map<AccountId, Account> =
      x.Accounts
      |> Map.filter (fun _ a -> a.Depository = AccountDepository.Checking)

   member x.Metrics: AccountMetrics =
      Seq.fold
         (fun acc profile -> {
            DailyInternalTransferWithinOrg =
               acc.DailyInternalTransferWithinOrg
               + profile.Metrics.DailyInternalTransferWithinOrg
            DailyInternalTransferBetweenOrgs =
               acc.DailyInternalTransferBetweenOrgs
               + profile.Metrics.DailyInternalTransferBetweenOrgs
            DailyDomesticTransfer =
               acc.DailyDomesticTransfer
               + profile.Metrics.DailyDomesticTransfer
            DailyPaymentPaid =
               acc.DailyPaymentPaid + profile.Metrics.DailyPaymentPaid
            DailyPurchase = acc.DailyPurchase + profile.Metrics.DailyPurchase
            MonthlyInternalTransferWithinOrg =
               acc.DailyInternalTransferWithinOrg
               + profile.Metrics.DailyInternalTransferWithinOrg
            MonthlyInternalTransferBetweenOrgs =
               acc.DailyInternalTransferBetweenOrgs
               + profile.Metrics.DailyInternalTransferBetweenOrgs
            MonthlyDomesticTransfer =
               acc.DailyDomesticTransfer
               + profile.Metrics.DailyDomesticTransfer
            MonthlyPaymentPaid =
               acc.DailyPaymentPaid + profile.Metrics.DailyPaymentPaid
            MonthlyPurchase = acc.DailyPurchase + profile.Metrics.DailyPurchase
         })
         AccountMetrics.empty
         x.AccountProfiles.Values

[<RequireQualifiedAccess>]
type OrgMessage =
   | GetOrg
   | GetCommandApprovalDailyAccrualByInitiatedBy of InitiatedById
   | ApprovableRequest of ApprovableCommand
   | StateChange of OrgCommand
   | Event of OrgEvent

type KYCApplication = {
   OrgId: OrgId
   CorrelationId: CorrelationId
   Application: OrgOnboardingApplicationSubmitted
}

/// Know Your Customer Third Party Org Verification Message
type KYCMessage = VerifyApplication of KYCApplication
