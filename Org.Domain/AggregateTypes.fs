namespace Bank.Org.Domain

open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Transfer.Domain

[<RequireQualifiedAccess>]
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
   | RegisterDomesticTransferRecipient of
      RegisterDomesticTransferRecipientCommand
   | EditDomesticTransferRecipient of EditDomesticTransferRecipientCommand
   | NicknameDomesticTransferRecipient of
      NicknameDomesticTransferRecipientCommand
   | FailDomesticTransferRecipient of FailDomesticTransferRecipientCommand
   | DomesticTransferRetryConfirmsRecipient of
      DomesticTransferRetryConfirmsRecipientCommand

type OrgEvent =
   | OrgCreated of BankEvent<OrgCreated>
   | OrgOnboardingFinished of BankEvent<OrgOnboardingFinished>
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
   | RegisteredDomesticTransferRecipient of
      BankEvent<RegisteredDomesticTransferRecipient>
   | EditedDomesticTransferRecipient of
      BankEvent<EditedDomesticTransferRecipient>
   | NicknamedDomesticTransferRecipient of
      BankEvent<NicknamedDomesticTransferRecipient>
   | DomesticTransferRecipientFailed of
      BankEvent<DomesticTransferRecipientFailed>
   | DomesticTransferRetryConfirmsRecipient of
      BankEvent<DomesticTransferRetryConfirmsRecipient>

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
      | :? BankEvent<RegisteredDomesticTransferRecipient> as evt ->
         RegisteredDomesticTransferRecipient evt
      | :? BankEvent<EditedDomesticTransferRecipient> as evt ->
         EditedDomesticTransferRecipient evt
      | :? BankEvent<NicknamedDomesticTransferRecipient> as evt ->
         NicknamedDomesticTransferRecipient evt
      | :? BankEvent<DomesticTransferRecipientFailed> as evt ->
         DomesticTransferRecipientFailed evt
      | :? BankEvent<DomesticTransferRetryConfirmsRecipient> as evt ->
         DomesticTransferRetryConfirmsRecipient evt
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
      | RegisteredDomesticTransferRecipient evt -> wrap evt, get evt
      | EditedDomesticTransferRecipient evt -> wrap evt, get evt
      | NicknamedDomesticTransferRecipient evt -> wrap evt, get evt
      | DomesticTransferRetryConfirmsRecipient evt -> wrap evt, get evt
      | DomesticTransferRecipientFailed evt -> wrap evt, get evt

type Org = {
   OrgId: OrgId
   Name: string
   Status: OrgStatus
   FeatureFlags: FeatureFlagOrgSettings
   CommandApprovalRules: Map<CommandApprovalRuleId, CommandApprovalRule.T>
   CommandApprovalProgress:
      Map<CommandApprovalProgressId, CommandApprovalProgress.T>
   DomesticTransferRecipients: Map<AccountId, DomesticTransferRecipient>
}

module Org =
   let empty: Org = {
      OrgId = OrgId System.Guid.Empty
      Name = ""
      Status = OrgStatus.InitialEmptyState
      FeatureFlags = {
         SocialTransferDiscoveryPrimaryAccountId = None
      }
      CommandApprovalRules = Map.empty
      CommandApprovalProgress = Map.empty
      DomesticTransferRecipients = Map.empty
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

type OrgHistory = {
   InitiatedByName: string
   Event: OrgEvent
}

type AccountHistory = {
   InitiatedByName: string
   Event: AccountEvent
}

type EmployeeHistory = {
   InitiatedByName: string
   EmployeeName: string
   Event: EmployeeEvent
}

[<RequireQualifiedAccess>]
type History =
   | Org of OrgHistory
   | Account of AccountHistory
   | Employee of EmployeeHistory
