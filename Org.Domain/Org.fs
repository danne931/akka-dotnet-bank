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

let private canApproveOrDeclineApprovalProcess
   (org: Org)
   (ruleId: CommandApprovalRuleId)
   (progressId: CommandApprovalProgressId)
   (approver: EmployeeReference)
   =
   match Map.tryFind ruleId org.CommandApprovalRules with
   | None -> Error OrgStateTransitionError.ApprovalRuleNotFound
   | Some rule ->
      match
         CommandApprovalRule.isValidApprover
            (InitiatedById approver.EmployeeId)
            rule
      with
      | None ->
         OrgStateTransitionError.ApproverUnrecognized(
            approver.EmployeeId,
            approver.EmployeeName
         )
         |> Error
      | Some _ ->
         hasActiveProgressWorkflow org progressId
         |> Result.map (fun progress -> rule, progress)

let numberOfApprovalsUserCanApprove (org: Org) (employeeId: EmployeeId) : int =
   org.CommandApprovalProgress.Values
   |> Seq.filter (fun progress ->
      org.CommandApprovalRules.TryFind progress.RuleId
      |> Option.exists (fun rule ->
         CommandApprovalProgress.mayApproveOrDeny rule progress employeeId))
   |> Seq.length

let dailyAccrual (events: OrgEvent list) : DailyAccrual =
   List.fold
      (fun acc evt ->
         match evt with
         | CommandApprovalRequested e ->
            if DateTime.isToday e.Timestamp then
               match e.Data.Command with
               | ApprovableCommand.FulfillPlatformPayment cmd -> {
                  acc with
                     PaymentsPaid =
                        acc.PaymentsPaid
                        + cmd.Data.RequestedPayment.BaseInfo.Amount
                 }
               | ApprovableCommand.DomesticTransfer cmd -> {
                  acc with
                     DomesticTransfer = acc.DomesticTransfer + cmd.Data.Amount
                 }
               | ApprovableCommand.InternalTransferBetweenOrgs cmd -> {
                  acc with
                     InternalTransferBetweenOrgs =
                        acc.InternalTransferBetweenOrgs + cmd.Data.Amount
                 }
               | _ -> acc
            else
               acc
         | _ -> acc)
      {
         PaymentsPaid = 0m
         InternalTransferBetweenOrgs = 0m
         DomesticTransfer = 0m
      }
      events

/// Detect if an incoming command requires additional approvals or
/// initiation into the approval workflow before issuing the command.
let commandRequiresApproval
   (command: ApprovableCommand)
   (state: OrgWithEvents)
   : CommandApprovalRule.T option
   =
   let accrual = dailyAccrual state.Events
   let org = state.Info

   let rulesForCommand =
      org.CommandApprovalRules.Values
      |> Seq.toList
      |> List.filter (fun rule ->
         let onlyApprover =
            CommandApprovalRule.isRequesterTheOnlyConfiguredApprover
               command.InitiatedBy
               rule

         rule.CommandType = command.CommandType && onlyApprover.IsNone)

   let requiresApproval (rule: CommandApprovalRule.T) =
      let criteria = rule.Criteria

      let progress =
         Map.tryFind
            (CommandApprovalProgressId command.CorrelationId)
            org.CommandApprovalProgress

      match progress with
      | Some progress when
         progress.Status = CommandApprovalProgress.Status.Approved
         ->
         None
      | _ ->
         match criteria with
         | CommandApprovalRule.Criteria.PerCommand ->
            match command with
            | ApprovableCommand.InviteEmployee _ -> Some rule
            | ApprovableCommand.UpdateEmployeeRole _ -> Some rule
            | ApprovableCommand.FulfillPlatformPayment _ -> None // should not be reached
            | ApprovableCommand.InternalTransferBetweenOrgs _ -> None // should not be reached
            | ApprovableCommand.DomesticTransfer _ -> None // should not be reached
         | CommandApprovalRule.Criteria.AmountPerCommand range ->
            match range.LowerBound, range.UpperBound with
            | None, None -> None // This case should not be reached
            | Some low, None ->
               if command.Amount >= low then Some rule else None
            | None, Some high ->
               if command.Amount <= high then Some rule else None
            | Some low, Some high ->
               if command.Amount >= low && command.Amount <= high then
                  Some rule
               else
                  None
         | CommandApprovalRule.Criteria.AmountDailyLimit limit ->
            match command with
            | ApprovableCommand.InviteEmployee _ -> None // Should not be reached
            | ApprovableCommand.UpdateEmployeeRole _ -> None // Should not be reached
            | ApprovableCommand.FulfillPlatformPayment cmd ->
               let overLimit =
                  cmd.Data.RequestedPayment.BaseInfo.Amount
                  + accrual.PaymentsPaid > limit

               if overLimit then Some rule else None
            | ApprovableCommand.InternalTransferBetweenOrgs cmd ->
               let overLimit =
                  cmd.Data.Amount + accrual.InternalTransferBetweenOrgs > limit

               if overLimit then Some rule else None
            | ApprovableCommand.DomesticTransfer cmd ->
               let overLimit =
                  cmd.Data.Amount + accrual.DomesticTransfer > limit

               if overLimit then Some rule else None

   match rulesForCommand with
   | [] -> None
   | [ rule ] -> requiresApproval rule
   | rules ->
      rules
      |> List.choose requiresApproval
      // Prioritize daily limit over range >=
      |> List.sortBy (fun rule ->
         match rule.Criteria with
         | CommandApprovalRule.Criteria.AmountDailyLimit _ -> 0
         | _ -> 1)
      |> function
         | [] -> None
         | head :: rest -> Some head

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
      | CommandApprovalRuleConfigured e -> {
         org with
            CommandApprovalRules =
               if Map.containsKey e.Data.RuleId org.CommandApprovalRules then
                  org.CommandApprovalRules
                  |> Map.change
                        e.Data.RuleId
                        (Option.map (fun rule -> {
                           rule with
                              Approvers = e.Data.Approvers
                              Criteria = e.Data.Criteria
                        }))
               else
                  org.CommandApprovalRules
                  |> Map.add e.Data.RuleId {
                     RuleId = e.Data.RuleId
                     OrgId = e.Data.OrgId
                     CommandType = e.Data.CommandType
                     Criteria = e.Data.Criteria
                     Approvers = e.Data.Approvers
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

   let configureCommandApprovalRule
      (state: OrgWithEvents)
      (cmd: CommandApprovalRule.ConfigureApprovalRuleCommand)
      =
      let org = state.Info
      let existingRules = org.CommandApprovalRules.Values

      if org.Status <> OrgStatus.Active then
         transitionErr OrgStateTransitionError.OrgNotActive
      elif
         CommandApprovalRule.newRuleCommandTypeConflictsWithExistingRule
            existingRules
            cmd.Data
         |> not
      then
         cmd.Data.CommandType.Display
         |> OrgStateTransitionError.ApprovalRuleMultipleOfType
         |> transitionErr
      elif
         CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
            existingRules
            cmd.Data
      then
         transitionErr
            OrgStateTransitionError.ApprovalRuleHasConflictingCriteria
      else
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
            canApproveOrDeclineApprovalProcess
               org
               data.RuleId
               data.ProgressId
               approver

         match res with
         | Error err -> transitionErr err
         | Ok(rule, progress) ->
            if
               not (CommandApprovalProgress.isNewApproval approver progress)
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
            canApproveOrDeclineApprovalProcess
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
   | CreateOrg cmd -> StateTransition.create state cmd
   | FinalizeOrgOnboarding cmd -> StateTransition.finalizeOnboarding state cmd
   | ConfigureFeatureFlag cmd -> StateTransition.configureFeatureFlag state cmd
   | ConfigureApprovalRule cmd ->
      StateTransition.configureCommandApprovalRule state cmd
   | DeleteApprovalRule cmd ->
      StateTransition.deleteCommandApprovalRule state cmd
   | RequestCommandApproval cmd ->
      StateTransition.requestCommandApproval state cmd
   | AcquireCommandApproval cmd ->
      StateTransition.acquireCommandApproval state cmd
   | DeclineCommandApproval cmd ->
      StateTransition.declineCommandApproval state cmd
   | TerminateCommandApproval cmd ->
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
