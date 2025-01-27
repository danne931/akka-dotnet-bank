namespace Bank.Org.Domain

open System
open Validus

open Lib.SharedTypes
open Bank.Employee.Domain
open Bank.Transfer.Domain

type ApprovablePerCommand =
   | InviteEmployeeCommandType
   | UpdateEmployeeRoleCommandType

type ApprovableAmountBased =
   | FulfillPlatformPaymentCommandType
   | InternalTransferBetweenOrgsCommandType
   | DomesticTransferCommandType

[<RequireQualifiedAccess>]
type ApprovableCommandType =
   | ApprovablePerCommand of ApprovablePerCommand
   | ApprovableAmountBased of ApprovableAmountBased

   override x.ToString() =
      match x with
      | ApprovablePerCommand x ->
         match x with
         | InviteEmployeeCommandType -> "InviteEmployee"
         | UpdateEmployeeRoleCommandType -> "UpdateEmployeeRole"
      | ApprovableAmountBased x ->
         match x with
         | FulfillPlatformPaymentCommandType -> "SendPayment"
         | InternalTransferBetweenOrgsCommandType ->
            "SendInternalTransferBetweenOrgs"
         | DomesticTransferCommandType -> "SendDomesticTransfer"

   member x.Display =
      match x with
      | ApprovablePerCommand x ->
         match x with
         | InviteEmployeeCommandType -> "Invite Employee"
         | UpdateEmployeeRoleCommandType -> "Update Employee Role"
      | ApprovableAmountBased x ->
         match x with
         | FulfillPlatformPaymentCommandType -> "Fulfill Platform Payment"
         | InternalTransferBetweenOrgsCommandType ->
            "Internal Transfer Between Orgs"
         | DomesticTransferCommandType -> "Domestic Transfer"

   static member fromString(o: string) : ApprovableCommandType option =
      if String.IsNullOrWhiteSpace o then
         None
      else
         match o with
         | "InviteEmployee" ->
            Some(ApprovablePerCommand InviteEmployeeCommandType)
         | "UpdateEmployeeRole" ->
            Some(ApprovablePerCommand UpdateEmployeeRoleCommandType)
         | "SendPayment" ->
            Some(ApprovableAmountBased FulfillPlatformPaymentCommandType)
         | "SendInternalTransferBetweenOrgs" ->
            Some(ApprovableAmountBased InternalTransferBetweenOrgsCommandType)
         | "SendDomesticTransfer" ->
            Some(ApprovableAmountBased DomesticTransferCommandType)
         | _ -> None

   static member fromStringUnsafe(o: string) : ApprovableCommandType =
      match ApprovableCommandType.fromString o with
      | None ->
         failwith "Error attempting to cast string to ApprovableCommandType"
      | Some o -> o

[<RequireQualifiedAccess>]
type ApprovableCommand =
   | InviteEmployee of ApproveAccessCommand
   | UpdateEmployeeRole of UpdateRoleCommand
   | FulfillPlatformPayment of FulfillPlatformPaymentCommand
   | InternalTransferBetweenOrgs of InternalTransferBetweenOrgsCommand
   | DomesticTransfer of DomesticTransferCommand

   static member envelope(cmd: ApprovableCommand) : CommandEnvelope =
      match cmd with
      | InviteEmployee o -> Command.envelope o
      | UpdateEmployeeRole o -> Command.envelope o
      | FulfillPlatformPayment o -> Command.envelope o
      | InternalTransferBetweenOrgs o -> Command.envelope o
      | DomesticTransfer o -> Command.envelope o

   member x.InitiatedBy = ApprovableCommand.envelope x |> _.InitiatedById

   member x.OrgId = ApprovableCommand.envelope x |> _.OrgId

   member x.CorrelationId = ApprovableCommand.envelope x |> _.CorrelationId

   member x.Amount =
      match x with
      | InviteEmployee _ -> 0m
      | UpdateEmployeeRole _ -> 0m
      | FulfillPlatformPayment o -> o.Data.RequestedPayment.BaseInfo.Amount
      | InternalTransferBetweenOrgs o -> o.Data.Amount
      | DomesticTransfer o -> o.Data.Amount

   member x.CommandType =
      match x with
      | InviteEmployee _ ->
         ApprovableCommandType.ApprovablePerCommand InviteEmployeeCommandType
      | UpdateEmployeeRole _ ->
         ApprovableCommandType.ApprovablePerCommand
            UpdateEmployeeRoleCommandType
      | FulfillPlatformPayment _ ->
         ApprovableCommandType.ApprovableAmountBased
            FulfillPlatformPaymentCommandType
      | InternalTransferBetweenOrgs _ ->
         ApprovableCommandType.ApprovableAmountBased
            InternalTransferBetweenOrgsCommandType
      | DomesticTransfer _ ->
         ApprovableCommandType.ApprovableAmountBased DomesticTransferCommandType

   member x.Display = x.CommandType.Display

module CommandApprovalRule =
   type AmountPerCommandRange = {
      LowerBound: decimal option
      UpperBound: decimal option
   }

   module AmountPerCommandRange =
      let hasOverlap
         (existingRange: AmountPerCommandRange)
         (range: AmountPerCommandRange)
         =
         match
            existingRange.LowerBound,
            existingRange.UpperBound,
            range.LowerBound,
            range.UpperBound
         with
         | None, None, _, _ -> true
         | _, _, None, None -> true
         | Some eLow, Some eHigh, Some low, Some high ->
            not (high <= eLow || low >= eHigh)
         | Some eLow, Some _, None, Some high -> high > eLow
         | Some _, Some eHigh, Some low, None -> low < eHigh
         | None, Some eHigh, Some low, Some _ -> low < eHigh
         | None, Some _, None, Some _ -> true
         | None, Some eHigh, Some low, None -> low < eHigh
         | Some eLow, None, Some _, Some high -> high > eLow
         | Some eLow, None, None, Some high -> high > eLow
         | Some _, None, Some _, None -> true

   [<RequireQualifiedAccess>]
   type Criteria =
      | AmountDailyLimit of limit: decimal
      | AmountPerCommand of AmountPerCommandRange
      | PerCommand

      override x.ToString() =
         match x with
         | AmountDailyLimit _ -> "AmountDailyLimit"
         | AmountPerCommand _ -> "AmountPerCommand"
         | PerCommand -> "PerCommand"

      static member validate
         (criteria: Criteria)
         : Result<Criteria, ValidationErrors> =
         match criteria with
         | Criteria.AmountPerCommand range ->
            match range.LowerBound, range.UpperBound with
            | None, None ->
               ValidationErrors.create "amount per command criteria" [
                  "Expecting lower bound, upper bound, or both."
               ]
               |> Error
            | _ -> Ok criteria
         | Criteria.AmountDailyLimit limit ->
            Lib.Validators.amountValidator "daily limit criteria" limit
            |> Result.map (fun _ -> criteria)
         | Criteria.PerCommand -> Ok criteria

      static member sortOrder(criteria: Criteria) =
         match criteria with
         | Criteria.AmountPerCommand range ->
            match range.LowerBound, range.UpperBound with
            | None, Some high -> 0, None, Some high
            | Some low, Some high -> 1, Some low, Some high
            | Some low, None -> 2, Some low, None
            // NOTE: Case should not occur.
            // Consider making a type which enforces either
            // lower or upper being Some.
            | None, None -> 3, None, None
         | Criteria.AmountDailyLimit _ -> 4, None, None
         | Criteria.PerCommand -> 5, None, None

   [<RequireQualifiedAccess>]
   type Approver =
      /// Any admin is considered a suitable approver.
      | AnyAdmin
      /// A particular admin is considered a suitable approver.
      | Admin of EmployeeReference

      member x.DisplayName =
         match x with
         | Approver.AnyAdmin -> "Any Admin"
         | Approver.Admin a -> a.EmployeeName

   type T = {
      RuleId: CommandApprovalRuleId
      OrgId: OrgId
      CommandType: ApprovableCommandType
      Criteria: Criteria
      Approvers: Approver list
   }

   let private isEmployeeAnApprover (id: InitiatedById) (approver: Approver) =
      match approver with
      | Approver.AnyAdmin -> true
      | Approver.Admin a -> InitiatedById a.EmployeeId = id

   /// Is the command approver configured as an approver in the
   /// rule or is there an AnyAdmin approver configured for the rule
   let isValidApprover (approvedBy: InitiatedById) (rule: T) : bool =
      rule.Approvers |> List.exists (isEmployeeAnApprover approvedBy)

   let isRequesterOneOfManyApprovers
      (requester: InitiatedById)
      (rule: T)
      : bool
      =
      rule.Approvers.Length > 1 && isValidApprover requester rule

   /// Approval not required if only 1 approver configured for the rule
   /// & the person requesting the command is the configured approver.
   let isRequesterTheOnlyConfiguredApprover
      (requester: InitiatedById)
      (rule: T)
      : bool
      =
      match rule.Approvers with
      | [ approver ] -> isEmployeeAnApprover requester approver
      | _ -> false

   let newRuleCommandTypeConflictsWithExistingRule
      (existingRules: T seq)
      (rule: T)
      =
      let existingRuleOfGivenType =
         existingRules
         |> Seq.tryPick (fun existing ->
            if existing.CommandType = rule.CommandType then
               Some existing
            else
               None)

      match existingRuleOfGivenType with
      | None -> false
      | Some existingRule ->
         // If we are editing an existing rule then this is not considered
         // a conflict.
         if existingRule.RuleId = rule.RuleId then
            false
         else
            match existingRule.Criteria, rule.Criteria with
            | Criteria.AmountDailyLimit _, Criteria.AmountDailyLimit _
            | Criteria.PerCommand, Criteria.PerCommand -> true
            | Criteria.AmountPerCommand _, Criteria.AmountPerCommand _ -> false
            | _ -> false

   let private rangeConflictsWithDailyLimit
      (limit: decimal)
      (range: AmountPerCommandRange)
      =
      match range.LowerBound, range.UpperBound with
      | None, None -> true
      | Some _, Some high -> limit <= high
      | None, Some high -> limit <= high
      | Some low, None -> limit <= low

   /// Check whether amount based rule criteria conflicts.
   /// This involves checking if AmountPerCommand rules conflict
   /// with AmountDailyLimit rules or if AmountPerCommand
   /// rules conflict with other AmountPerCommand rules.
   let newRuleCriteriaConflictsWithExistingRule
      (existingRules: T seq)
      (rule: T)
      =
      // Keep rules with AmountPerCommand or AmountDailyLimit criteria,
      // ignoring an existing rule if we are editing it's configuration.
      let keepAmountBasedCommandRules (existing: T) =
         existing.CommandType = rule.CommandType
         && existing.RuleId <> rule.RuleId
         && match existing.Criteria with
            | Criteria.AmountPerCommand _ -> true
            | Criteria.AmountDailyLimit _ -> true
            | _ -> false

      match rule.Criteria with
      | Criteria.PerCommand -> false
      | Criteria.AmountPerCommand range ->
         existingRules
         |> Seq.filter keepAmountBasedCommandRules
         |> Seq.exists (fun existing ->
            match existing.Criteria with
            | Criteria.AmountPerCommand existingRange ->
               AmountPerCommandRange.hasOverlap existingRange range
            | Criteria.AmountDailyLimit limit ->
               rangeConflictsWithDailyLimit limit range
            | _ -> false)
      | Criteria.AmountDailyLimit limit ->
         existingRules
         |> Seq.filter keepAmountBasedCommandRules
         |> Seq.exists (fun existing ->
            match existing.Criteria with
            | Criteria.AmountPerCommand range ->
               rangeConflictsWithDailyLimit limit range
            | _ -> false)

   type ApprovalRuleDeleted = {
      RuleId: CommandApprovalRuleId
      OrgId: OrgId
      CommandType: ApprovableCommandType
      DeletedBy: EmployeeReference
   }

   type DeleteApprovalRuleCommand = Command<ApprovalRuleDeleted>

   module DeleteApprovalRuleCommand =
      let create (data: ApprovalRuleDeleted) : DeleteApprovalRuleCommand =
         Command.create
            (OrgId.toEntityId data.OrgId)
            data.OrgId
            (CorrelationId.create ())
            (InitiatedById data.DeletedBy.EmployeeId)
            data

      let toEvent
         (cmd: DeleteApprovalRuleCommand)
         : ValidationResult<BankEvent<ApprovalRuleDeleted>>
         =
         BankEvent.create<ApprovalRuleDeleted> cmd |> Ok

   type ConfigureApprovalRule = { Rule: T }
   type ConfigureApprovalRuleCommand = Command<ConfigureApprovalRule>

   module ConfigureApprovalRuleCommand =
      let create
         (orgId: OrgId)
         (initiatedBy: InitiatedById)
         (data: T)
         : ConfigureApprovalRuleCommand
         =
         Command.create
            (OrgId.toEntityId orgId)
            orgId
            (CorrelationId.create ())
            initiatedBy
            { Rule = data }

      let toEvent
         (cmd: ConfigureApprovalRuleCommand)
         : ValidationResult<BankEvent<ConfigureApprovalRule>>
         =
         validate {
            let! _ = Criteria.validate cmd.Data.Rule.Criteria
            return BankEvent.create<ConfigureApprovalRule> cmd
         }

module CommandApprovalProgress =
   [<RequireQualifiedAccess>]
   type CommandApprovalTerminationReason =
      | AssociatedRuleDeleted
      | AssociatedRuleApproverDeleted

      member x.Display =
         match x with
         | AssociatedRuleDeleted -> "Deletion of Associated Rule"
         | AssociatedRuleApproverDeleted ->
            "Deletion of Associated Rule Approver"

   [<RequireQualifiedAccess>]
   type Status =
      | Pending
      | Approved
      | Declined
      | Terminated of CommandApprovalTerminationReason

      override x.ToString() =
         match x with
         | Pending -> "Pending"
         | Approved -> "Approved"
         | Declined -> "Declined"
         | Terminated _ -> "Terminated"

   type T = {
      ProgressId: CommandApprovalProgressId
      RuleId: CommandApprovalRuleId
      OrgId: OrgId
      Status: Status
      RequestedBy: EmployeeReference
      ApprovedBy: EmployeeReference list
      DeclinedBy: EmployeeReference option
      CommandToInitiateOnApproval: ApprovableCommand
      CreatedAt: DateTime
      LastUpdate: DateTime
   }

   /// Has the person approving the command already approved the
   /// command or is this a new approval?
   let isNewApproval (approvedBy: EmployeeId) (progress: T) : bool =
      progress.ApprovedBy
      |> List.exists (fun a -> a.EmployeeId = approvedBy)
      |> not

   /// Employee can approve or deny the command if they are a PermittedApprover
   /// for a given command type & they haven't already approved the command.
   let canManageProgress
      (rule: CommandApprovalRule.T)
      (progress: T)
      (eId: EmployeeId)
      : bool
      =
      progress.Status = Status.Pending
      && CommandApprovalRule.isValidApprover (InitiatedById eId) rule
      && isNewApproval eId progress

   /// Returns configured CommandApprovalRule Approvers which have
   /// not yet approved the command.
   let remainingApprovalRequiredBy
      (rule: CommandApprovalRule.T)
      (progress: T)
      : CommandApprovalRule.Approver list
      =
      List.fold
         (fun approversRequired (approvedBy: EmployeeReference) ->
            let adminFoundAtIndex =
               approversRequired
               |> List.tryFindIndex (function
                  | CommandApprovalRule.Approver.Admin a ->
                     a.EmployeeId = approvedBy.EmployeeId
                  | CommandApprovalRule.Approver.AnyAdmin -> false)
               // If approvedBy not found as one of the Approver.Admin items
               // then check for the existence of an Approver.AnyAdmin option.
               |> Option.orElse (
                  approversRequired
                  |> List.tryFindIndex (function
                     | CommandApprovalRule.Approver.Admin _ -> false
                     | CommandApprovalRule.Approver.AnyAdmin -> true)
               )

            // Remove the approversRequired item corresponding to approvedBy.
            match adminFoundAtIndex with
            | Some index -> List.removeAt index approversRequired
            | None -> approversRequired)
         rule.Approvers
         progress.ApprovedBy

   let requiresApprovalForPairOfRuleAndProgress
      (command: ApprovableCommand)
      (accrual: DailyAccrual)
      (progress: T option)
      (rule: CommandApprovalRule.T)
      =
      let criteria = rule.Criteria

      match progress with
      | Some progress when progress.Status <> Status.Pending -> None
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
            | Some low, None -> if command.Amount > low then Some rule else None
            | None, Some high ->
               if command.Amount < high then Some rule else None
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

   /// Detect if an incoming command requires additional approvals or
   /// initiation into the approval workflow before issuing the command.
   let commandRequiresApproval
      (command: ApprovableCommand)
      (rules: Map<CommandApprovalRuleId, CommandApprovalRule.T>)
      (progress: Map<CommandApprovalProgressId, T>)
      (accrual: DailyAccrual)
      : CommandApprovalRule.T option
      =
      let rulesForCommand =
         rules.Values
         |> Seq.toList
         |> List.filter (fun rule ->
            let onlyApprover =
               CommandApprovalRule.isRequesterTheOnlyConfiguredApprover
                  command.InitiatedBy
                  rule

            rule.CommandType = command.CommandType && not onlyApprover)

      let requiresApproval =
         requiresApprovalForPairOfRuleAndProgress
            command
            accrual
            (Map.tryFind
               (CommandApprovalProgressId command.CorrelationId)
               progress)

      match rulesForCommand with
      | [] -> None
      | [ rule ] -> requiresApproval rule
      | rules ->
         rules
         |> List.choose requiresApproval
         // Prioritize daily limit over range (Some, None) when a command
         // meets the criteria of multiple rules (AmountDailyLimit +
         // AmountPerCommand).
         // Prioritize multiple AmountPerCommandRange by their descending
         // sort order as well.
         //
         // (ex: DailyLimit 15
         //      { LowerBound = Some 7; UpperBound = None }
         //      { LowerBound = Some 4; UpperBound = Some 7 }
         //      { LowerBound = Some 2; UpperBound = Some 4 }
         // )
         |> List.sortByDescending (
            _.Criteria >> CommandApprovalRule.Criteria.sortOrder
         )
         |> function
            | [] -> None
            | head :: rest -> Some head

   let numberOfApprovalsUserCanManage
      (rules: Map<CommandApprovalRuleId, CommandApprovalRule.T>)
      (progress: Map<CommandApprovalProgressId, T>)
      (employeeId: EmployeeId)
      : int
      =
      progress.Values
      |> Seq.filter (fun progress ->
         rules.TryFind progress.RuleId
         |> Option.exists (fun rule ->
            canManageProgress rule progress employeeId))
      |> Seq.length

   type CommandApprovalRequested = {
      RuleId: CommandApprovalRuleId
      Command: ApprovableCommand
      Requester: EmployeeReference
      // The requester is configured in the associated rule as an approver.
      RequesterIsConfiguredAsAnApprover: bool
   }

   type CommandApprovalAcquired = {
      RuleId: CommandApprovalRuleId
      ApprovedBy: EmployeeReference
      ProgressId: CommandApprovalProgressId
      Command: ApprovableCommand
   }

   type CommandApprovalProcessCompleted = {
      RuleId: CommandApprovalRuleId
      ApprovedBy: EmployeeReference
      ProgressId: CommandApprovalProgressId
      Command: ApprovableCommand
   }

   type CommandApprovalDeclined = {
      RuleId: CommandApprovalRuleId
      DeclinedBy: EmployeeReference
      Command: ApprovableCommand
      ProgressId: CommandApprovalProgressId
   }

   type CommandApprovalTerminated = {
      RuleId: CommandApprovalRuleId
      ProgressId: CommandApprovalProgressId
      Command: ApprovableCommand
      Reason: CommandApprovalTerminationReason
   }

   type RequestCommandApproval = Command<CommandApprovalRequested>

   module RequestCommandApproval =
      let create
         (orgId: OrgId)
         (initiatedBy: InitiatedById)
         (correlationId: CorrelationId)
         (data: CommandApprovalRequested)
         =
         Command.create
            (OrgId.toEntityId orgId)
            orgId
            correlationId
            initiatedBy
            data

      let toEvent
         (cmd: RequestCommandApproval)
         : ValidationResult<BankEvent<CommandApprovalRequested>>
         =
         Ok <| BankEvent.create<CommandApprovalRequested> cmd

   type AcquireCommandApproval = Command<CommandApprovalAcquired>

   module AcquireCommandApproval =
      let create (orgId: OrgId) (data: CommandApprovalAcquired) =
         let (CommandApprovalProgressId correlationId) = data.ProgressId

         Command.create
            (OrgId.toEntityId orgId)
            orgId
            correlationId
            (InitiatedById data.ApprovedBy.EmployeeId)
            data

      let toEvent
         (cmd: AcquireCommandApproval)
         : ValidationResult<BankEvent<CommandApprovalAcquired>>
         =
         BankEvent.create<CommandApprovalAcquired> cmd |> Ok

   type CompleteCommandApprovalProcess =
      Command<CommandApprovalProcessCompleted>

   module CompleteCommandApprovalProcess =
      let create
         (cmd: AcquireCommandApproval)
         : Command<CommandApprovalProcessCompleted>
         =
         let (CommandApprovalProgressId correlationId) = cmd.Data.ProgressId

         Command.create
            (OrgId.toEntityId cmd.OrgId)
            cmd.OrgId
            correlationId
            (InitiatedById cmd.Data.ApprovedBy.EmployeeId)
            {
               RuleId = cmd.Data.RuleId
               ApprovedBy = cmd.Data.ApprovedBy
               ProgressId = cmd.Data.ProgressId
               Command = cmd.Data.Command
            }

      let toEvent
         (cmd: CompleteCommandApprovalProcess)
         : ValidationResult<BankEvent<CommandApprovalProcessCompleted>>
         =
         BankEvent.create<CommandApprovalProcessCompleted> cmd |> Ok

   type DeclineCommandApproval = Command<CommandApprovalDeclined>

   module DeclineCommandApproval =
      let create (orgId: OrgId) (data: CommandApprovalDeclined) =
         let (CommandApprovalProgressId correlationId) = data.ProgressId

         Command.create
            (OrgId.toEntityId orgId)
            orgId
            correlationId
            (InitiatedById data.DeclinedBy.EmployeeId)
            data

      let toEvent
         (cmd: DeclineCommandApproval)
         : ValidationResult<BankEvent<CommandApprovalDeclined>>
         =
         Ok <| BankEvent.create<CommandApprovalDeclined> cmd

   type TerminateCommandApproval = Command<CommandApprovalTerminated>

   module TerminateCommandApproval =
      let create (orgId: OrgId) (data: CommandApprovalTerminated) =
         let (CommandApprovalProgressId correlationId) = data.ProgressId

         Command.create
            (OrgId.toEntityId orgId)
            orgId
            correlationId
            (InitiatedById Constants.SYSTEM_USER_ID)
            data

      let toEvent
         (cmd: TerminateCommandApproval)
         : ValidationResult<BankEvent<CommandApprovalTerminated>>
         =
         BankEvent.create<CommandApprovalTerminated> cmd |> Ok
