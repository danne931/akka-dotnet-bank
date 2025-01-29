[<RequireQualifiedAccess>]
module Org

open Validus
open System

open Bank.Org.Domain
open Bank.Employee.Domain
open Lib.SharedTypes
open Lib.Time

let private hasActiveProgressWorkflow
   (org: Org)
   (progressId: CommandApprovalProgressId)
   =
   match Map.tryFind progressId org.CommandApprovalProgress with
   | None -> Error OrgStateTransitionError.ApprovalProgressWorklowNotActive
   | Some progress ->
      if progress.Status <> CommandApprovalProgress.Status.Pending then
         Error OrgStateTransitionError.ApprovalProgressWorklowNotActive
      else
         Ok progress

let private canManageApprovalProgress
   (org: Org)
   (ruleId: CommandApprovalRuleId)
   (progressId: CommandApprovalProgressId)
   (approver: EmployeeReference)
   =
   match Map.tryFind ruleId org.CommandApprovalRules with
   | None -> Error OrgStateTransitionError.ApprovalRuleNotFound
   | Some rule ->
      if
         CommandApprovalRule.isValidApprover
            (InitiatedById approver.EmployeeId)
            rule
      then
         hasActiveProgressWorkflow org progressId
         |> Result.map (fun progress -> rule, progress)
      else
         OrgStateTransitionError.ApproverUnrecognized(
            approver.EmployeeId,
            approver.EmployeeName
         )
         |> Error

let dailyAccrual (events: OrgEvent list) : DailyAccrual =
   List.fold
      (fun acc evt ->
         match evt with
         | CommandApprovalRequested e ->
            if DateTime.isToday e.Timestamp then
               match e.Data.Command with
               | ApprovableCommand.PerCommand _ -> acc
               | ApprovableCommand.AmountBased c ->
                  match c with
                  | FulfillPlatformPayment cmd -> {
                     acc with
                        PaymentsPaid =
                           acc.PaymentsPaid
                           + cmd.Data.RequestedPayment.BaseInfo.Amount
                    }
                  | DomesticTransfer cmd -> {
                     acc with
                        DomesticTransfer =
                           acc.DomesticTransfer + cmd.Data.Amount
                    }
                  | InternalTransferBetweenOrgs cmd -> {
                     acc with
                        InternalTransferBetweenOrgs =
                           acc.InternalTransferBetweenOrgs + cmd.Data.Amount
                    }
            else
               acc
         | _ -> acc)
      {
         PaymentsPaid = 0m
         InternalTransferBetweenOrgs = 0m
         DomesticTransfer = 0m
      }
      events

let commandRequiresApproval (cmd: ApprovableCommand) (state: OrgWithEvents) =
   CommandApprovalProgress.commandRequiresApproval
      cmd
      state.Info.CommandApprovalRules
      state.Info.CommandApprovalProgress
      (dailyAccrual state.Events)

// NOTE:
// Being able to reference events directly during state transitions
// is necessary for doing daily accrual computations for determining whether
// a command approval rule is to be applied (See dailyAccrual usage).
// No need to keep them around otherwise, especially as they are persisted
// as events in the akka_event_journal as well as a read model
// organization_event table.
let private trimExcess (state: OrgWithEvents) : OrgWithEvents =
   let dateWithinLookbackPeriod (date: DateTime) =
      date.ToUniversalTime() > DateTime.UtcNow.AddDays(-2.)

   {
      Events =
         state.Events
         |> List.filter (fun e ->
            let _, envelope = OrgEnvelope.unwrap e
            dateWithinLookbackPeriod envelope.Timestamp)
      Info = {
         state.Info with
            CommandApprovalProgress =
               state.Info.CommandApprovalProgress
               |> Map.filter (fun _ p ->
                  match dateWithinLookbackPeriod p.LastUpdate, p.Status with
                  | true, _ -> true
                  | false, status ->
                     match status with
                     | CommandApprovalProgress.Status.Pending -> true
                     | CommandApprovalProgress.Status.Approved -> false
                     | CommandApprovalProgress.Status.Declined -> false
                     | CommandApprovalProgress.Status.Terminated _ -> false)
      }
   }

let applyEvent (state: OrgWithEvents) (evt: OrgEvent) =
   let org = state.Info

   let updatedOrg =
      match evt with
      | OrgCreated e -> {
         OrgId = e.OrgId
         Name = e.Data.Name
         Status = OrgStatus.PendingOnboardingTasksFulfilled
         FeatureFlags = {
            SocialTransferDiscoveryPrimaryAccountId = None
         }
         CommandApprovalRules = Map.empty
         CommandApprovalProgress = Map.empty
        }
      | OrgOnboardingFinished _ -> { org with Status = OrgStatus.Active }
      | FeatureFlagConfigured e -> {
         org with
            FeatureFlags = e.Data.Config
        }
      | CommandApprovalRuleConfigured e ->
         let rule = e.Data.Rule

         {
            org with
               CommandApprovalRules =
                  if Map.containsKey rule.RuleId org.CommandApprovalRules then
                     org.CommandApprovalRules
                     |> Map.change
                           rule.RuleId
                           (Option.map (fun r -> {
                              r with
                                 Approvers = rule.Approvers
                                 Criteria = rule.Criteria
                           }))
                  else
                     org.CommandApprovalRules
                     |> Map.add rule.RuleId {
                        RuleId = rule.RuleId
                        OrgId = rule.OrgId
                        CommandType = rule.CommandType
                        Criteria = rule.Criteria
                        Approvers = rule.Approvers
                     }
         }
      | CommandApprovalRuleDeleted e -> {
         org with
            CommandApprovalRules =
               org.CommandApprovalRules |> Map.remove e.Data.RuleId
        }
      | CommandApprovalRequested e -> {
         org with
            CommandApprovalProgress =
               let progressId = CommandApprovalProgressId e.CorrelationId

               org.CommandApprovalProgress
               |> Map.add progressId {
                  ProgressId = progressId
                  RuleId = e.Data.RuleId
                  OrgId = e.OrgId
                  Status = CommandApprovalProgress.Status.Pending
                  RequestedBy = e.Data.Requester
                  ApprovedBy =
                     if e.Data.RequesterIsConfiguredAsAnApprover then
                        [ e.Data.Requester ]
                     else
                        []
                  DeclinedBy = None
                  CommandToInitiateOnApproval = e.Data.Command
                  CreatedAt = e.Timestamp
                  LastUpdate = e.Timestamp
               }
        }
      | CommandApprovalAcquired e -> {
         org with
            CommandApprovalProgress =
               org.CommandApprovalProgress
               |> Map.change
                     e.Data.ProgressId
                     (Option.map (fun progress -> {
                        progress with
                           ApprovedBy =
                              e.Data.ApprovedBy :: progress.ApprovedBy
                           LastUpdate = e.Timestamp
                     }))
        }
      | CommandApprovalProcessCompleted e -> {
         org with
            CommandApprovalProgress =
               org.CommandApprovalProgress
               |> Map.change
                     e.Data.ProgressId
                     (Option.map (fun progress -> {
                        progress with
                           Status = CommandApprovalProgress.Status.Approved
                           ApprovedBy =
                              e.Data.ApprovedBy :: progress.ApprovedBy
                           LastUpdate = e.Timestamp
                     }))
        }
      | CommandApprovalDeclined e -> {
         org with
            CommandApprovalProgress =
               org.CommandApprovalProgress
               |> Map.change
                     e.Data.ProgressId
                     (Option.map (fun progress -> {
                        progress with
                           DeclinedBy = Some e.Data.DeclinedBy
                           Status = CommandApprovalProgress.Status.Declined
                           LastUpdate = e.Timestamp
                     }))
        }
      | CommandApprovalTerminated e -> {
         org with
            CommandApprovalProgress =
               org.CommandApprovalProgress
               |> Map.change
                     e.Data.ProgressId
                     (Option.map (fun progress -> {
                        progress with
                           Status =
                              CommandApprovalProgress.Status.Terminated
                                 e.Data.Reason
                           LastUpdate = e.Timestamp
                     }))
        }

   {
      trimExcess state with
         Info = updatedOrg
         Events = evt :: state.Events
   }

module private StateTransition =
   let transitionErr (err: OrgStateTransitionError) =
      Error <| OrgStateTransitionError err

   let map
      (eventTransform: BankEvent<'t> -> OrgEvent)
      (state: OrgWithEvents)
      (eventValidation: ValidationResult<BankEvent<'t>>)
      =
      eventValidation
      |> Result.mapError ValidationError
      |> Result.map (fun evt ->
         let evt = eventTransform evt
         (evt, applyEvent state evt))

   let create (state: OrgWithEvents) (cmd: CreateOrgCommand) =
      if state.Info.Status <> OrgStatus.InitialEmptyState then
         transitionErr OrgStateTransitionError.OrgNotReadyToStartOnboarding
      else
         map OrgCreated state (CreateOrgCommand.toEvent cmd)

   let finalizeOnboarding
      (state: OrgWithEvents)
      (cmd: FinalizeOrgOnboardingCommand)
      =
      if state.Info.Status <> OrgStatus.PendingOnboardingTasksFulfilled then
         transitionErr OrgStateTransitionError.OrgNotReadyToActivate
      else
         map
            OrgOnboardingFinished
            state
            (FinalizeOrgOnboardingCommand.toEvent cmd)

   let configureFeatureFlag
      (state: OrgWithEvents)
      (cmd: ConfigureFeatureFlagCommand)
      =
      if state.Info.Status <> OrgStatus.Active then
         transitionErr OrgStateTransitionError.OrgNotActive
      else
         map
            FeatureFlagConfigured
            state
            (ConfigureFeatureFlagCommand.toEvent cmd)

   /// Create/Edit command approval rule
   let configureCommandApprovalRule
      (state: OrgWithEvents)
      (cmd: CommandApprovalRule.ConfigureApprovalRuleCommand)
      =
      let org = state.Info
      let existingRules = org.CommandApprovalRules.Values
      let rule = cmd.Data.Rule

      if org.Status <> OrgStatus.Active then
         transitionErr OrgStateTransitionError.OrgNotActive
      elif
         CommandApprovalRule.newRuleCommandTypeConflictsWithExistingRule
            existingRules
            rule
      then
         rule.CommandType.Display
         |> OrgStateTransitionError.ApprovalRuleMultipleOfType
         |> transitionErr
      elif
         CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
            existingRules
            rule
      then
         transitionErr
            OrgStateTransitionError.ApprovalRuleHasConflictingCriteria
      else
         let containsAmountBasedGaps =
            CommandApprovalRule.newRuleContainsAmountGapWithExistingRule
               existingRules
               rule

         match containsAmountBasedGaps with
         | Some gap -> transitionErr (CommandApprovalRule.RangeGap.toError gap)
         | None ->
            map
               CommandApprovalRuleConfigured
               state
               (CommandApprovalRule.ConfigureApprovalRuleCommand.toEvent cmd)

   let deleteCommandApprovalRule
      (state: OrgWithEvents)
      (cmd: CommandApprovalRule.DeleteApprovalRuleCommand)
      =
      let org = state.Info
      let data = cmd.Data

      if org.Status <> OrgStatus.Active then
         transitionErr OrgStateTransitionError.OrgNotActive
      else
         match org.CommandApprovalRules.TryFind data.RuleId with
         | None -> transitionErr OrgStateTransitionError.ApprovalRuleNotFound
         | Some _ ->
            map
               CommandApprovalRuleDeleted
               state
               (CommandApprovalRule.DeleteApprovalRuleCommand.toEvent cmd)

   let requestCommandApproval
      (state: OrgWithEvents)
      (cmd: CommandApprovalProgress.RequestCommandApproval)
      =
      let org = state.Info

      if org.Status <> OrgStatus.Active then
         transitionErr OrgStateTransitionError.OrgNotActive
      else
         match Map.tryFind cmd.Data.RuleId org.CommandApprovalRules with
         | None -> transitionErr OrgStateTransitionError.ApprovalRuleNotFound
         | Some _ ->
            map
               CommandApprovalRequested
               state
               (CommandApprovalProgress.RequestCommandApproval.toEvent cmd)

   let acquireCommandApproval
      (state: OrgWithEvents)
      (cmd: CommandApprovalProgress.AcquireCommandApproval)
      =
      let org = state.Info
      let data = cmd.Data

      if org.Status <> OrgStatus.Active then
         transitionErr OrgStateTransitionError.OrgNotActive
      else
         let approver = data.ApprovedBy

         let res =
            canManageApprovalProgress org data.RuleId data.ProgressId approver

         match res with
         | Error err -> transitionErr err
         | Ok(rule, progress) ->
            if
               CommandApprovalProgress.isNewApproval
                  approver.EmployeeId
                  progress
               |> not
            then
               OrgStateTransitionError.ApproverAlreadyApprovedCommand(
                  approver.EmployeeId,
                  approver.EmployeeName
               )
               |> transitionErr
            else if
               rule.Approvers.Length = (progress.ApprovedBy.Length + 1)
            then
               let evt =
                  cmd
                  |> CommandApprovalProgress.CompleteCommandApprovalProcess.create
                  |> CommandApprovalProgress.CompleteCommandApprovalProcess.toEvent

               map CommandApprovalProcessCompleted state evt
            else
               map
                  CommandApprovalAcquired
                  state
                  (CommandApprovalProgress.AcquireCommandApproval.toEvent cmd)

   let declineCommandApproval
      (state: OrgWithEvents)
      (cmd: CommandApprovalProgress.DeclineCommandApproval)
      =
      let org = state.Info
      let data = cmd.Data

      if org.Status <> OrgStatus.Active then
         transitionErr OrgStateTransitionError.OrgNotActive
      else
         let res =
            canManageApprovalProgress
               org
               data.RuleId
               data.ProgressId
               data.DeclinedBy

         match res with
         | Error err -> transitionErr err
         | Ok _ ->
            map
               CommandApprovalDeclined
               state
               (CommandApprovalProgress.DeclineCommandApproval.toEvent cmd)

   let terminateCommandApproval
      (state: OrgWithEvents)
      (cmd: CommandApprovalProgress.TerminateCommandApproval)
      =
      let org = state.Info
      let data = cmd.Data

      if org.Status <> OrgStatus.Active then
         transitionErr OrgStateTransitionError.OrgNotActive
      else
         let res = hasActiveProgressWorkflow org data.ProgressId

         match res with
         | Error err -> transitionErr err
         | Ok _ ->
            map
               CommandApprovalTerminated
               state
               (CommandApprovalProgress.TerminateCommandApproval.toEvent cmd)

let stateTransition (state: OrgWithEvents) (command: OrgCommand) =
   match command with
   | OrgCommand.CreateOrg cmd -> StateTransition.create state cmd
   | OrgCommand.FinalizeOrgOnboarding cmd ->
      StateTransition.finalizeOnboarding state cmd
   | OrgCommand.ConfigureFeatureFlag cmd ->
      StateTransition.configureFeatureFlag state cmd
   | OrgCommand.ConfigureApprovalRule cmd ->
      StateTransition.configureCommandApprovalRule state cmd
   | OrgCommand.DeleteApprovalRule cmd ->
      StateTransition.deleteCommandApprovalRule state cmd
   | OrgCommand.RequestCommandApproval cmd ->
      StateTransition.requestCommandApproval state cmd
   | OrgCommand.AcquireCommandApproval cmd ->
      StateTransition.acquireCommandApproval state cmd
   | OrgCommand.DeclineCommandApproval cmd ->
      StateTransition.declineCommandApproval state cmd
   | OrgCommand.TerminateCommandApproval cmd ->
      StateTransition.terminateCommandApproval state cmd

let empty: Org = {
   OrgId = OrgId System.Guid.Empty
   Name = ""
   Status = OrgStatus.InitialEmptyState
   FeatureFlags = {
      SocialTransferDiscoveryPrimaryAccountId = None
   }
   CommandApprovalRules = Map.empty
   CommandApprovalProgress = Map.empty
}
