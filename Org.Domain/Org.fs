[<RequireQualifiedAccess>]
module Org

open Validus
open System

open Bank.Org.Domain
open Bank.Employee.Domain
open Lib.SharedTypes
open Lib.Time
open CommandApproval

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

let private validateApprovalRuleAgainstExistingRules
   (existingRules: CommandApprovalRule seq)
   (rule: CommandApprovalRule)
   =
   if
      CommandApprovalRule.newRuleCommandTypeConflictsWithExistingRule
         existingRules
         rule
   then
      rule.CommandType.Display
      |> OrgStateTransitionError.ApprovalRuleMultipleOfType
      |> Error
   elif
      CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
         existingRules
         rule
   then
      Error OrgStateTransitionError.ApprovalRuleHasConflictingCriteria
   else
      let containsAmountBasedGaps =
         CommandApprovalRule.newRuleContainsAmountGapWithExistingRule
            existingRules
            rule

      match containsAmountBasedGaps with
      | Some gap -> Error(RangeGap.toError gap)
      | None -> Ok()

let dailyAccrual
   (initiatedBy: InitiatedById)
   (metrics: Map<CorrelationId, OrgAccrualMetric>)
   : CommandApprovalDailyAccrual
   =
   metrics
   |> Map.fold
      (fun acc _ metrics ->
         if
            DateTime.isToday metrics.Timestamp
            && metrics.InitiatedBy.Id = initiatedBy
         then
            match metrics.EventType with
            | OrgAccrualMetricEventType.DomesticTransfer -> {
               acc with
                  DomesticTransfer =
                     acc.DomesticTransfer + metrics.TransactionAmount
              }
            | OrgAccrualMetricEventType.InternalTransferBetweenOrgs -> {
               acc with
                  InternalTransferBetweenOrgs =
                     acc.InternalTransferBetweenOrgs
                     + metrics.TransactionAmount
              }
         else
            acc)
      {
         InternalTransferBetweenOrgs = 0m
         DomesticTransfer = 0m
      }

let commandRequiresApproval (cmd: ApprovableCommand) (state: OrgSnapshot) =
   let accrual = dailyAccrual cmd.InitiatedBy.Id state.AccrualMetrics

   CommandApprovalProgress.commandRequiresApproval
      cmd
      state.Info.CommandApprovalRules
      state.Info.CommandApprovalProgress
      accrual

// NOTE:
// Being able to reference events directly during state transitions
// is necessary for doing daily accrual computations for determining whether
// a command approval rule is to be applied (See dailyAccrual usage).
// No need to keep them around otherwise, especially as they are persisted
// as events in the akka_event_journal as well as a read model
// organization_event table.
let private trimExcess (state: OrgSnapshot) : OrgSnapshot =
   let dateWithinLookbackPeriod (date: DateTime) =
      date.ToUniversalTime() > DateTime.UtcNow.AddDays -2.

   {
      AccrualMetrics =
         state.AccrualMetrics
         |> Map.filter (fun _ m -> dateWithinLookbackPeriod m.Timestamp)
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

let applyEvent (state: OrgSnapshot) (evt: OrgEvent) =
   let org = state.Info

   let updatedOrg =
      match evt with
      | OnboardingApplicationSubmitted e -> {
         OrgId = e.OrgId
         ParentAccountId = e.Data.ParentAccountId
         Name = e.Data.LegalBusinessName
         Status = OrgStatus.PendingOnboardingTasksFulfilled
         AdminTeamEmail = e.Data.AdminTeamEmail
         EmployerIdentificationNumber = e.Data.EmployerIdentificationNumber
         FeatureFlags = {
            SocialTransferDiscoveryPrimaryAccountId = None
         }
         CommandApprovalRules = Map.empty
         CommandApprovalProgress = Map.empty
        }
      | OnboardingFinished _ -> { org with Status = OrgStatus.Active }
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
            // If command approval workflow completed for
            // managing (create/edit/delete) an approval rule then
            // update the org's CommandApprovalRules accordingly.
            CommandApprovalRules =
               match e.Data.Command with
               | ApprovableCommand.PerCommand(ManageApprovalRule cmd) ->
                  match cmd.Data with
                  | ManageApprovalRuleInput.Delete(rule, _) ->
                     org.CommandApprovalRules |> Map.remove rule.RuleId
                  | ManageApprovalRuleInput.CreateOrEdit(rule, _) ->
                     org.CommandApprovalRules |> Map.add rule.RuleId rule
               | _ -> org.CommandApprovalRules
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
      (state: OrgSnapshot)
      (eventValidation: ValidationResult<BankEvent<'t>>)
      =
      eventValidation
      |> Result.mapError ValidationError
      |> Result.map (fun evt ->
         let evt = eventTransform evt
         (evt, applyEvent state evt))

   let submitOnboardingApplication
      (state: OrgSnapshot)
      (cmd: SubmitOrgOnboardingApplicationCommand)
      =
      if state.Info.Status <> OrgStatus.InitialEmptyState then
         transitionErr OrgStateTransitionError.OrgNotReadyToStartOnboarding
      else
         map
            OnboardingApplicationSubmitted
            state
            (SubmitOrgOnboardingApplicationCommand.toEvent cmd)

   let finishOnboarding (state: OrgSnapshot) (cmd: FinishOrgOnboardingCommand) =
      if state.Info.Status <> OrgStatus.PendingOnboardingTasksFulfilled then
         transitionErr OrgStateTransitionError.OrgNotReadyToActivate
      else
         map OnboardingFinished state (FinishOrgOnboardingCommand.toEvent cmd)

   let configureFeatureFlag
      (state: OrgSnapshot)
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
      (state: OrgSnapshot)
      (cmd: CommandApprovalRule.ConfigureApprovalRuleCommand)
      =
      let org = state.Info
      let existingRules = org.CommandApprovalRules.Values
      let rule = cmd.Data.Rule

      if org.Status <> OrgStatus.Active then
         transitionErr OrgStateTransitionError.OrgNotActive
      else
         match validateApprovalRuleAgainstExistingRules existingRules rule with
         | Error err -> transitionErr err
         | Ok() ->
            map
               CommandApprovalRuleConfigured
               state
               (CommandApprovalRule.ConfigureApprovalRuleCommand.toEvent cmd)

   let deleteCommandApprovalRule
      (state: OrgSnapshot)
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
      (state: OrgSnapshot)
      (cmd: CommandApprovalProgress.RequestCommandApproval)
      =
      let org = state.Info

      let validated () =
         map
            CommandApprovalRequested
            state
            (CommandApprovalProgress.RequestCommandApproval.toEvent cmd)

      if org.Status <> OrgStatus.Active then
         transitionErr OrgStateTransitionError.OrgNotActive
      else
         match Map.tryFind cmd.Data.RuleId org.CommandApprovalRules with
         | None -> transitionErr OrgStateTransitionError.ApprovalRuleNotFound
         | Some _ ->
            // Check for rule conflicts when command is ManageApprovalRule
            match cmd.Data.Command with
            | ApprovableCommand.PerCommand(ManageApprovalRule manageCmd) ->
               match manageCmd.Data with
               | ManageApprovalRuleInput.CreateOrEdit(rule, _) ->
                  let existingRules = org.CommandApprovalRules.Values

                  match
                     validateApprovalRuleAgainstExistingRules existingRules rule
                  with
                  | Error err -> transitionErr err
                  | Ok() -> validated ()
               | ManageApprovalRuleInput.Delete _ -> validated ()
            | _ -> validated ()

   let acquireCommandApproval
      (state: OrgSnapshot)
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
      (state: OrgSnapshot)
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
      (state: OrgSnapshot)
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

let stateTransition (state: OrgSnapshot) (command: OrgCommand) =
   match command with
   | OrgCommand.SubmitOnboardingApplication cmd ->
      StateTransition.submitOnboardingApplication state cmd
   | OrgCommand.FinishOrgOnboarding cmd ->
      StateTransition.finishOnboarding state cmd
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
