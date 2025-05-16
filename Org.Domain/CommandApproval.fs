module CommandApproval

open System
open Validus

open Lib.SharedTypes
open Bank.Employee.Domain
open Bank.Transfer.Domain

/// Daily org metrics required to determine whether a
/// command requires approval based on AmountDailyLimit criteria.
type CommandApprovalDailyAccrual = {
   PaymentsPaid: decimal
   InternalTransferBetweenOrgs: decimal
   DomesticTransfer: decimal
}

type ApprovablePerCommand =
   | InviteEmployeeCommandType
   | UpdateEmployeeRoleCommandType
   | UnlockCardCommandType
   | ManageApprovalRuleCommandType

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
         | UnlockCardCommandType -> "UnlockCard"
         | ManageApprovalRuleCommandType -> "ManageApprovalRule"
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
         | UnlockCardCommandType -> "Unlock Card"
         | ManageApprovalRuleCommandType -> "Manage Approval Rules"
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
         | "UnlockCard" -> Some(ApprovablePerCommand UnlockCardCommandType)
         | "ManageApprovalRule" ->
            Some(ApprovablePerCommand ManageApprovalRuleCommandType)
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
type CommandApprover =
   /// Any admin is considered a suitable approver.
   | AnyAdmin
   /// A particular admin is considered a suitable approver.
   | Admin of EmployeeReference

   member x.DisplayName =
      match x with
      | CommandApprover.AnyAdmin -> "Any Admin"
      | CommandApprover.Admin a -> a.EmployeeName

type AmountPerCommandRange = {
   LowerBound: decimal option
   UpperBound: decimal option
}

[<RequireQualifiedAccess>]
type GapDirection =
   | Precedes
   | Follows

   override x.ToString() =
      match x with
      | Precedes -> "previous"
      | Follows -> "next"

type RangeGap = {
   Gap: decimal
   SetToAmountToCloseGap: decimal
   Direction: GapDirection
}

module RangeGap =
   let toError (gap: RangeGap) : OrgStateTransitionError =
      OrgStateTransitionError.ApprovalRuleHasGapInCriteria(
         gap.Gap,
         gap.SetToAmountToCloseGap,
         string gap.Direction
      )

module AmountPerCommandRange =
   let sortBy (range: AmountPerCommandRange) =
      match range.LowerBound, range.UpperBound with
      | None, Some high -> 0, None, Some high
      | Some low, Some high -> 1, Some low, Some high
      | Some low, None -> 2, Some low, None
      // NOTE: Case should not occur.
      // Consider making a type which enforces either
      // lower or upper being Some.
      | None, None -> 3, None, None

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

   /// Check whether a new AmountPerCommandRange has gaps with
   /// other AmountPerCommand ranges.
   /// Ex: existing: [{ high = Some 100; low = None }; { low = Some 100; high = 500 }]
   ///     new: { low = 605; high = none }
   ///     gap detected: 105 between new lower bound and preceding upper bound
   let hasGap
      (existingRanges: AmountPerCommandRange list)
      (range: AmountPerCommandRange)
      : RangeGap option
      =
      if existingRanges.Length = 0 then
         None
      else
         let ranges = range :: existingRanges |> List.sortBy sortBy

         ranges
         |> List.tryFindIndex (fun r -> r = range)
         |> Option.bind (function
            | 0 ->
               // New range is at front of list

               let rangeUpperBound =
                  ranges |> List.tryItem 0 |> Option.bind _.UpperBound

               let followingRangeLowerBound =
                  ranges |> List.tryItem 1 |> Option.bind _.LowerBound

               match rangeUpperBound, followingRangeLowerBound with
               | Some up, Some lowFollowing ->
                  let diff = lowFollowing - up

                  if diff > 0m then
                     Some {
                        Gap = diff
                        SetToAmountToCloseGap = lowFollowing
                        Direction = GapDirection.Follows
                     }
                  else
                     None
               | _ -> None
            | index when index = ranges.Length - 1 ->
               // New range is at end of list

               let rangeLowerBound =
                  ranges |> List.tryItem index |> Option.bind _.LowerBound

               let precedingRangeUpperBound =
                  ranges |> List.tryItem (index - 1) |> Option.bind _.UpperBound

               match rangeLowerBound, precedingRangeUpperBound with
               | Some low, Some upPreceding ->
                  let diff = low - upPreceding

                  if diff > 0m then
                     Some {
                        Gap = diff
                        SetToAmountToCloseGap = upPreceding
                        Direction = GapDirection.Precedes
                     }
                  else
                     None
               | _ -> None
            | index ->
               // New range is somewhere in the middle

               let range = ranges |> List.tryItem index
               let rangeLowerBound = range |> Option.bind _.LowerBound
               let rangeUpperBound = range |> Option.bind _.UpperBound

               let precedingRangeUpperBound =
                  ranges |> List.tryItem (index - 1) |> Option.bind _.UpperBound

               let followingRangeLowerBound =
                  ranges |> List.tryItem (index + 1) |> Option.bind _.LowerBound

               match
                  rangeLowerBound,
                  rangeUpperBound,
                  precedingRangeUpperBound,
                  followingRangeLowerBound
               with
               | Some low, Some up, Some upPreceding, Some lowFollowing ->
                  let diffBetweenPreceding = low - upPreceding
                  let diffBetweenFollowing = lowFollowing - up

                  if diffBetweenPreceding > 0m then
                     Some {
                        Gap = diffBetweenPreceding
                        SetToAmountToCloseGap = upPreceding
                        Direction = GapDirection.Precedes
                     }
                  elif diffBetweenFollowing > 0m then
                     Some {
                        Gap = diffBetweenFollowing
                        SetToAmountToCloseGap = lowFollowing
                        Direction = GapDirection.Follows
                     }
                  else
                     None
               | _ -> None)

[<RequireQualifiedAccess>]
type ApprovalCriteria =
   | AmountDailyLimit of limit: decimal
   | AmountPerCommand of AmountPerCommandRange
   | PerCommand

   override x.ToString() =
      match x with
      | AmountDailyLimit _ -> "AmountDailyLimit"
      | AmountPerCommand _ -> "AmountPerCommand"
      | PerCommand -> "PerCommand"

   static member validate
      (criteria: ApprovalCriteria)
      : Result<ApprovalCriteria, ValidationErrors> =
      match criteria with
      | ApprovalCriteria.AmountPerCommand range ->
         match range.LowerBound, range.UpperBound with
         | None, None ->
            ValidationErrors.create "amount per command criteria" [
               "Expecting lower bound, upper bound, or both."
            ]
            |> Error
         | _ -> Ok criteria
      | ApprovalCriteria.AmountDailyLimit limit ->
         Lib.Validators.amountValidator "daily limit criteria" limit
         |> Result.map (fun _ -> criteria)
      | ApprovalCriteria.PerCommand -> Ok criteria

   static member sortBy(criteria: ApprovalCriteria) =
      match criteria with
      | ApprovalCriteria.AmountPerCommand range ->
         AmountPerCommandRange.sortBy range
      | ApprovalCriteria.AmountDailyLimit _ -> 4, None, None
      | ApprovalCriteria.PerCommand -> 5, None, None

type CommandApprovalRule = {
   RuleId: CommandApprovalRuleId
   OrgId: OrgId
   CommandType: ApprovableCommandType
   Criteria: ApprovalCriteria
   Approvers: CommandApprover list
}

[<RequireQualifiedAccess>]
type ManageApprovalRuleInput =
   | CreateOrEdit of Rule: CommandApprovalRule * Initiator
   | Delete of Rule: CommandApprovalRule * Initiator

   member x.CommandType =
      match x with
      | CreateOrEdit(rule, _)
      | Delete(rule, _) -> rule.CommandType

type ManageApprovalRuleCommand = Command<ManageApprovalRuleInput>

module ManageApprovalRuleCommand =
   let create (data: ManageApprovalRuleInput) =
      let (rule, initiator) =
         match data with
         | ManageApprovalRuleInput.CreateOrEdit(rule, initiator)
         | ManageApprovalRuleInput.Delete(rule, initiator) -> rule, initiator

      Command.create
         (OrgId.toEntityId rule.OrgId)
         rule.OrgId
         (CorrelationId.create ())
         initiator
         data

type ApprovableCommandPerCommand =
   | InviteEmployee of ApproveAccessCommand
   | UpdateEmployeeRole of UpdateRoleCommand
   | UnlockCard of UnlockCardCommand
   | ManageApprovalRule of ManageApprovalRuleCommand

type ApprovableCommandAmountBased =
   | FulfillPlatformPayment of FulfillPlatformPaymentCommand
   | InternalTransferBetweenOrgs of InternalTransferBetweenOrgsCommand
   | DomesticTransfer of DomesticTransferCommand

[<RequireQualifiedAccess>]
type ApprovableCommand =
   | PerCommand of ApprovableCommandPerCommand
   | AmountBased of ApprovableCommandAmountBased

   static member envelope(cmd: ApprovableCommand) : Envelope =
      match cmd with
      | ApprovableCommand.PerCommand c ->
         match c with
         | InviteEmployee o -> Command.envelope o
         | UpdateEmployeeRole o -> Command.envelope o
         | UnlockCard o -> Command.envelope o
         | ManageApprovalRule o -> Command.envelope o
      | ApprovableCommand.AmountBased c ->
         match c with
         | FulfillPlatformPayment o -> Command.envelope o
         | InternalTransferBetweenOrgs o -> Command.envelope o
         | DomesticTransfer o -> Command.envelope o

   member x.InitiatedBy = ApprovableCommand.envelope x |> _.InitiatedBy

   member x.OrgId = ApprovableCommand.envelope x |> _.OrgId

   member x.CorrelationId = ApprovableCommand.envelope x |> _.CorrelationId

   member x.Timestamp = ApprovableCommand.envelope x |> _.Timestamp

   member x.Amount =
      match x with
      | ApprovableCommand.PerCommand _ -> 0m
      | ApprovableCommand.AmountBased c ->
         match c with
         | FulfillPlatformPayment o -> o.Data.RequestedPayment.BaseInfo.Amount
         | InternalTransferBetweenOrgs o -> o.Data.Amount
         | DomesticTransfer o -> o.Data.Amount

   member x.CommandType =
      match x with
      | ApprovableCommand.PerCommand c ->
         match c with
         | InviteEmployee _ ->
            ApprovableCommandType.ApprovablePerCommand InviteEmployeeCommandType
         | UpdateEmployeeRole _ ->
            ApprovableCommandType.ApprovablePerCommand
               UpdateEmployeeRoleCommandType
         | UnlockCard _ ->
            ApprovableCommandType.ApprovablePerCommand UnlockCardCommandType
         | ManageApprovalRule _ ->
            ApprovableCommandType.ApprovablePerCommand
               ManageApprovalRuleCommandType
      | ApprovableCommand.AmountBased c ->
         match c with
         | FulfillPlatformPayment _ ->
            ApprovableCommandType.ApprovableAmountBased
               FulfillPlatformPaymentCommandType
         | InternalTransferBetweenOrgs _ ->
            ApprovableCommandType.ApprovableAmountBased
               InternalTransferBetweenOrgsCommandType
         | DomesticTransfer _ ->
            ApprovableCommandType.ApprovableAmountBased
               DomesticTransferCommandType

   member x.Display =
      match x with
      | ApprovableCommand.PerCommand(ManageApprovalRule cmd) ->
         match cmd.Data with
         | ManageApprovalRuleInput.Delete(rule, _) ->
            $"Delete {rule.CommandType.Display} Rule"
         | ManageApprovalRuleInput.CreateOrEdit(rule, _) ->
            $"Configure {rule.CommandType.Display} Rule"
      | _ -> x.CommandType.Display

module CommandApprovalRule =
   let private isEmployeeAnApprover
      (id: InitiatedById)
      (approver: CommandApprover)
      =
      match approver with
      | CommandApprover.AnyAdmin -> true
      | CommandApprover.Admin a -> InitiatedById a.EmployeeId = id

   /// Is the command approver configured as an approver in the
   /// rule or is there an AnyAdmin approver configured for the rule
   let isValidApprover
      (approvedBy: InitiatedById)
      (rule: CommandApprovalRule)
      : bool
      =
      rule.Approvers |> List.exists (isEmployeeAnApprover approvedBy)

   let isRequesterOneOfManyApprovers
      (requester: InitiatedById)
      (rule: CommandApprovalRule)
      : bool
      =
      rule.Approvers.Length > 1 && isValidApprover requester rule

   /// Approval not required if only 1 approver configured for the rule
   /// & the person requesting the command is the configured approver.
   let isRequesterTheOnlyConfiguredApprover
      (requester: InitiatedById)
      (rule: CommandApprovalRule)
      : bool
      =
      match rule.Approvers with
      | [ approver ] -> isEmployeeAnApprover requester approver
      | _ -> false

   let newRuleCommandTypeConflictsWithExistingRule
      (existingRules: CommandApprovalRule seq)
      (rule: CommandApprovalRule)
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
            | ApprovalCriteria.AmountDailyLimit _,
              ApprovalCriteria.AmountDailyLimit _
            | ApprovalCriteria.PerCommand, ApprovalCriteria.PerCommand -> true
            | ApprovalCriteria.AmountPerCommand _,
              ApprovalCriteria.AmountPerCommand _ -> false
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
      (existingRules: CommandApprovalRule seq)
      (rule: CommandApprovalRule)
      =
      // Keep rules with AmountPerCommand or AmountDailyLimit criteria,
      // ignoring an existing rule if we are editing it's configuration.
      let keepAmountBasedCommandRules (existing: CommandApprovalRule) =
         existing.CommandType = rule.CommandType
         && existing.RuleId <> rule.RuleId // Editing existing rule
         && match existing.Criteria with
            | ApprovalCriteria.AmountPerCommand _ -> true
            | ApprovalCriteria.AmountDailyLimit _ -> true
            | _ -> false

      match rule.Criteria with
      | ApprovalCriteria.PerCommand -> false
      | ApprovalCriteria.AmountPerCommand range ->
         existingRules
         |> Seq.filter keepAmountBasedCommandRules
         |> Seq.exists (fun existing ->
            match existing.Criteria with
            | ApprovalCriteria.AmountPerCommand existingRange ->
               AmountPerCommandRange.hasOverlap existingRange range
            | ApprovalCriteria.AmountDailyLimit limit ->
               rangeConflictsWithDailyLimit limit range
            | _ -> false)
      | ApprovalCriteria.AmountDailyLimit limit ->
         existingRules
         |> Seq.filter keepAmountBasedCommandRules
         |> Seq.exists (fun existing ->
            match existing.Criteria with
            | ApprovalCriteria.AmountPerCommand range ->
               rangeConflictsWithDailyLimit limit range
            | _ -> false)

   /// Check whether a new rule contains gaps with other AmountPerCommand
   /// rules for the same command type.
   let newRuleContainsAmountGapWithExistingRule
      (existingRules: CommandApprovalRule seq)
      (rule: CommandApprovalRule)
      : RangeGap option
      =
      match rule.Criteria with
      | ApprovalCriteria.AmountPerCommand range ->
         let existingRanges =
            existingRules
            |> Seq.fold
               (fun acc r ->
                  match
                     rule.RuleId <> r.RuleId, // Editing an existing rule
                     r.CommandType = rule.CommandType,
                     r.Criteria
                  with
                  | true, true, ApprovalCriteria.AmountPerCommand range ->
                     range :: acc
                  | _ -> acc)
               []

         AmountPerCommandRange.hasGap existingRanges range
      | _ -> None

   // It is possible for a command type to require approval from
   // multiple rules (Ex: A transaction amount may meet the
   // AmountDailyLimit criteria as well as an AmountPerCommand criteria
   // given an AmountPerCommand criteria with range
   // { LowerBound = Some; UpperBound = None }).
   // In such cases the AmountDailyLimit rule will be applied.
   let selectOneOfAssociatedRules (rules: CommandApprovalRule list) =
      rules
      |> List.sortBy (fun r ->
         match r.Criteria with
         | ApprovalCriteria.AmountDailyLimit _ -> 0
         | _ -> 1)
      |> List.tryHead

   /// Determines if a rule requires approval for a command type given
   /// the requester is not the only configured approver for that rule.
   let ruleDemandsApprovalForCommandType
      (commandType: ApprovableCommandType)
      (requester: InitiatedById)
      (rule: CommandApprovalRule)
      : bool
      =
      let onlyApprover = isRequesterTheOnlyConfiguredApprover requester rule
      rule.CommandType = commandType && not onlyApprover

   let ruleDemandsApprovalForCriteria
      (command: ApprovableCommand)
      (accrual: CommandApprovalDailyAccrual)
      (rule: CommandApprovalRule)
      : bool
      =
      match rule.Criteria with
      | ApprovalCriteria.PerCommand ->
         match command with
         | ApprovableCommand.PerCommand _ -> true
         | ApprovableCommand.AmountBased _ -> false
      | ApprovalCriteria.AmountPerCommand range ->
         match range.LowerBound, range.UpperBound with
         | None, None -> false // This case should not be reached
         | Some low, None -> command.Amount >= low
         | None, Some high -> command.Amount < high
         | Some low, Some high -> command.Amount >= low && command.Amount < high
      | ApprovalCriteria.AmountDailyLimit limit ->
         match command with
         | ApprovableCommand.PerCommand _ -> false
         | ApprovableCommand.AmountBased c ->
            match c with
            | FulfillPlatformPayment cmd ->
               let overLimit =
                  cmd.Data.RequestedPayment.BaseInfo.Amount
                  + accrual.PaymentsPaid > limit

               overLimit
            | InternalTransferBetweenOrgs cmd ->
               let overLimit =
                  cmd.Data.Amount + accrual.InternalTransferBetweenOrgs > limit


               overLimit
            | DomesticTransfer cmd ->
               let overLimit =
                  cmd.Data.Amount + accrual.DomesticTransfer > limit

               overLimit

   /// Detect if a command type requires initiation into the
   /// approval workflow before issuing the command.
   let commandTypeRequiresApproval
      (commandType: ApprovableCommandType)
      (requester: InitiatedById)
      (rules: Map<CommandApprovalRuleId, CommandApprovalRule>)
      : CommandApprovalRule option
      =
      rules.Values
      |> Seq.toList
      |> List.filter (ruleDemandsApprovalForCommandType commandType requester)
      |> selectOneOfAssociatedRules

   /// Create/Edit/Delete of a rule requires approval
   let ruleManagementRequiresApproval
      (requester: InitiatedById)
      (rules: Map<CommandApprovalRuleId, CommandApprovalRule>)
      : CommandApprovalRule option
      =
      commandTypeRequiresApproval
         (ApprovableCommandType.ApprovablePerCommand
            ManageApprovalRuleCommandType)
         requester
         rules

   /// Get all relevant command approval rules applying
   /// to a given command.
   let associatedCommandApprovalRulesForCommand
      (command: ApprovableCommand)
      (accrual: CommandApprovalDailyAccrual)
      (rules: Map<CommandApprovalRuleId, CommandApprovalRule>)
      : CommandApprovalRule list
      =
      rules.Values
      |> Seq.toList
      |> List.filter (fun rule ->
         let forCommandType =
            ruleDemandsApprovalForCommandType
               command.CommandType
               command.InitiatedBy.Id
               rule

         let forCriteria = ruleDemandsApprovalForCriteria command accrual rule

         forCommandType && forCriteria)

   /// Detect if a command requires initiation into the
   /// approval workflow before issuing the command.
   let commandRequiresApproval
      (command: ApprovableCommand)
      (accrual: CommandApprovalDailyAccrual)
      (rules: Map<CommandApprovalRuleId, CommandApprovalRule>)
      : CommandApprovalRule option
      =
      associatedCommandApprovalRulesForCommand command accrual rules
      |> selectOneOfAssociatedRules

   type ApprovalRuleDeleted = {
      RuleId: CommandApprovalRuleId
      OrgId: OrgId
      CommandType: ApprovableCommandType
   }

   type DeleteApprovalRuleCommand = Command<ApprovalRuleDeleted>

   module DeleteApprovalRuleCommand =
      let create
         initiatedBy
         (data: ApprovalRuleDeleted)
         : DeleteApprovalRuleCommand
         =
         Command.create
            (OrgId.toEntityId data.OrgId)
            data.OrgId
            (CorrelationId.create ())
            initiatedBy
            data

      let toEvent
         (cmd: DeleteApprovalRuleCommand)
         : ValidationResult<BankEvent<ApprovalRuleDeleted>>
         =
         BankEvent.create<ApprovalRuleDeleted> cmd |> Ok

   type ConfigureApprovalRule = { Rule: CommandApprovalRule }
   type ConfigureApprovalRuleCommand = Command<ConfigureApprovalRule>

   module ConfigureApprovalRuleCommand =
      let create
         (orgId: OrgId)
         (initiatedBy: Initiator)
         (data: CommandApprovalRule)
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
            let! _ = ApprovalCriteria.validate cmd.Data.Rule.Criteria
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

   // Edit or deletion of a rule is currently pending approval.
   let managementOfRuleIsPendingApproval
      (progress: Map<CommandApprovalProgressId, T>)
      (commandType: ApprovableCommandType)
      =
      progress
      |> Map.exists (fun _ p ->
         match p.Status, p.CommandToInitiateOnApproval with
         | Status.Pending, ApprovableCommand.PerCommand(ManageApprovalRule c) ->
            c.Data.CommandType = commandType
         | _ -> false)

   /// Has the person approving the command already approved the
   /// command or is this a new approval?
   let isNewApproval (approvedBy: EmployeeId) (progress: T) : bool =
      progress.ApprovedBy
      |> List.exists (fun a -> a.EmployeeId = approvedBy)
      |> not

   /// Employee can approve or deny the command if they are a PermittedApprover
   /// for a given command type & they haven't already approved the command.
   let canManageProgress
      (rule: CommandApprovalRule)
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
      (rule: CommandApprovalRule)
      (progress: T)
      : CommandApprover list
      =
      List.fold
         (fun approversRequired (approvedBy: EmployeeReference) ->
            let adminFoundAtIndex =
               approversRequired
               |> List.tryFindIndex (function
                  | CommandApprover.Admin a ->
                     a.EmployeeId = approvedBy.EmployeeId
                  | CommandApprover.AnyAdmin -> false)
               // If approvedBy not found as one of the CommandApprover.Admin items
               // then check for the existence of an CommandApprover.AnyAdmin option.
               |> Option.orElse (
                  approversRequired
                  |> List.tryFindIndex (function
                     | CommandApprover.Admin _ -> false
                     | CommandApprover.AnyAdmin -> true)
               )

            // Remove the approversRequired item corresponding to approvedBy.
            match adminFoundAtIndex with
            | Some index -> List.removeAt index approversRequired
            | None -> approversRequired)
         rule.Approvers
         progress.ApprovedBy

   /// Detect if an incoming command requires additional approvals or
   /// initiation into the approval workflow before issuing the command.
   let commandRequiresApproval
      (command: ApprovableCommand)
      (rules: Map<CommandApprovalRuleId, CommandApprovalRule>)
      (progress: Map<CommandApprovalProgressId, T>)
      (accrual: CommandApprovalDailyAccrual)
      : CommandApprovalRule option
      =
      let p =
         Map.tryFind (CommandApprovalProgressId command.CorrelationId) progress

      match p with
      | Some p when p.Status <> Status.Pending -> None
      | _ -> CommandApprovalRule.commandRequiresApproval command accrual rules

   let numberOfApprovalsUserCanManage
      (rules: Map<CommandApprovalRuleId, CommandApprovalRule>)
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
         (initiatedBy: Initiator)
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
            {
               Id = InitiatedById data.ApprovedBy.EmployeeId
               Name = data.ApprovedBy.EmployeeName
            }
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
            {
               Id = InitiatedById cmd.Data.ApprovedBy.EmployeeId
               Name = cmd.Data.ApprovedBy.EmployeeName
            }
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
            {
               Id = InitiatedById data.DeclinedBy.EmployeeId
               Name = data.DeclinedBy.EmployeeName
            }
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
            Initiator.System
            data

      let toEvent
         (cmd: TerminateCommandApproval)
         : ValidationResult<BankEvent<CommandApprovalTerminated>>
         =
         BankEvent.create<CommandApprovalTerminated> cmd |> Ok
