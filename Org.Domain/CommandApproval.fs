namespace Bank.Org.Domain

open System
open Validus

open Lib.SharedTypes
open Bank.Employee.Domain
open Bank.Transfer.Domain

[<RequireQualifiedAccess>]
type ApprovableCommandType =
   | InviteEmployee
   | UpdateEmployeeRole
   | FulfillPlatformPayment
   | InternalTransferBetweenOrgs
   | DomesticTransfer

   override x.ToString() =
      match x with
      | InviteEmployee -> "InviteEmployee"
      | UpdateEmployeeRole -> "UpdateEmployeeRole"
      | FulfillPlatformPayment -> "SendPayment"
      | InternalTransferBetweenOrgs -> "SendInternalTransferBetweenOrgs"
      | DomesticTransfer -> "SendDomesticTransfer"

   member x.Display =
      match x with
      | InviteEmployee -> "Invite Employee"
      | UpdateEmployeeRole -> "Update Employee Role"
      | FulfillPlatformPayment -> "Fulfill Platform Payment"
      | InternalTransferBetweenOrgs -> "Internal Transfer Between Orgs"
      | DomesticTransfer -> "Domestic Transfer"

   static member fromString(o: string) : ApprovableCommandType option =
      if String.IsNullOrWhiteSpace o then
         None
      else
         match o with
         | "InviteEmployee" -> Some InviteEmployee
         | "UpdateEmployeeRole" -> Some UpdateEmployeeRole
         | "SendPayment" -> Some FulfillPlatformPayment
         | "SendInternalTransferBetweenOrgs" -> Some InternalTransferBetweenOrgs
         | "SendDomesticTransfer" -> Some DomesticTransfer
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
      | InviteEmployee _ -> ApprovableCommandType.InviteEmployee
      | UpdateEmployeeRole _ -> ApprovableCommandType.UpdateEmployeeRole
      | FulfillPlatformPayment _ -> ApprovableCommandType.FulfillPlatformPayment
      | InternalTransferBetweenOrgs _ ->
         ApprovableCommandType.InternalTransferBetweenOrgs
      | DomesticTransfer _ -> ApprovableCommandType.DomesticTransfer

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
            not (high < eLow || low > eHigh)
         | Some eLow, Some _, None, Some high -> high >= eLow
         | Some _, Some eHigh, Some low, None -> low <= eHigh
         | None, Some eHigh, Some low, Some _ -> low <= eHigh
         | None, Some _, None, Some _ -> true
         | None, Some eHigh, Some low, None -> low <= eHigh
         | Some eLow, None, Some _, Some high -> high >= eLow
         | Some eLow, None, None, Some high -> high >= eLow
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

   type Approver = { Name: string; EmployeeId: EmployeeId }

   type T = {
      RuleId: CommandApprovalRuleId
      OrgId: OrgId
      CommandType: ApprovableCommandType
      Criteria: Criteria
      Approvers: Approver list
   }

   let isValidApprover (approver: Approver) (rule: T) =
      rule.Approvers
      |> List.exists (fun a -> a.EmployeeId = approver.EmployeeId)

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
      | None -> true
      | Some existingRule ->
         if existingRule.RuleId = rule.RuleId then
            true
         else
            let commandCriteriaForbidsMoreThanOneRule =
               match existingRule.Criteria, rule.Criteria with
               | Criteria.AmountDailyLimit _, Criteria.AmountDailyLimit _
               | Criteria.PerCommand, Criteria.PerCommand -> true
               | Criteria.AmountPerCommand _, Criteria.AmountPerCommand _ ->
                  false
               | _ -> false

            not commandCriteriaForbidsMoreThanOneRule

   let newRuleCriteriaConflictsWithExistingRule
      (existingRules: T seq)
      (rule: T)
      =
      // Keep rules with AmountPerCommand criteria, ignoring an existing rule
      // if we are editing it's configuration.
      let keepAmountPerCommandRules (existing: T) =
         existing.CommandType = rule.CommandType
         && existing.RuleId <> rule.RuleId
         && match existing.Criteria with
            | Criteria.AmountPerCommand _ -> true
            | _ -> false

      match rule.Criteria with
      | Criteria.PerCommand -> false
      | Criteria.AmountPerCommand range ->
         existingRules
         |> Seq.filter keepAmountPerCommandRules
         |> Seq.exists (fun existing ->
            match existing.Criteria with
            | Criteria.AmountPerCommand existingRange ->
               AmountPerCommandRange.hasOverlap existingRange range
            | _ -> false)
      | Criteria.AmountDailyLimit limit ->
         existingRules
         |> Seq.filter keepAmountPerCommandRules
         |> Seq.exists (fun existing ->
            match existing.Criteria with
            | Criteria.AmountPerCommand range ->
               match range.LowerBound, range.UpperBound with
               | None, None -> true
               | Some _, Some high -> limit <= high
               | None, Some high -> limit <= high
               | Some low, None -> limit <= low
            | _ -> false)

   type ConfigureApprovalRuleCommand = Command<T>

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
            data

      let toEvent
         (cmd: ConfigureApprovalRuleCommand)
         : ValidationResult<BankEvent<T>>
         =
         validate {
            let! _ = Criteria.validate cmd.Data.Criteria
            return BankEvent.create<T> cmd
         }

module CommandApprovalProgress =
   type Requester = { Name: string; Id: InitiatedById }

   [<RequireQualifiedAccess>]
   type Status =
      | Pending
      | Approved
      | Declined
      | Terminated

      override x.ToString() =
         match x with
         | Pending -> "Pending"
         | Approved -> "Approved"
         | Declined -> "Declined"
         | Terminated -> "Terminated"

   type T = {
      ProgressId: CommandApprovalProgressId
      RuleId: CommandApprovalRuleId
      OrgId: OrgId
      Status: Status
      ApprovedBy: CommandApprovalRule.Approver list
      DeclinedBy: CommandApprovalRule.Approver option
      CommandToInitiateOnApproval: ApprovableCommand
   }

   let isNewApproval (approver: CommandApprovalRule.Approver) (progress: T) =
      progress.ApprovedBy
      |> List.exists (fun a -> a.EmployeeId = approver.EmployeeId)
      |> not

   type CommandApprovalRequested = {
      RuleId: CommandApprovalRuleId
      Command: ApprovableCommand
   }

   type CommandApprovalAcquired = {
      RuleId: CommandApprovalRuleId
      ApprovedBy: CommandApprovalRule.Approver
      ProgressId: CommandApprovalProgressId
      Command: ApprovableCommand
   }

   type CommandApprovalProcessCompleted = {
      RuleId: CommandApprovalRuleId
      ApprovedBy: CommandApprovalRule.Approver
      ProgressId: CommandApprovalProgressId
      Command: ApprovableCommand
   }

   type CommandApprovalDeclined = {
      RuleId: CommandApprovalRuleId
      DeclinedBy: CommandApprovalRule.Approver
      Command: ApprovableCommand
      ProgressId: CommandApprovalProgressId
   }

   [<RequireQualifiedAccess>]
   type CommandApprovalTerminationReason =
      | AssociatedRuleDeleted
      | AssociatedRuleApproverDeleted

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

type CommandApprovalProgressWithRule = {
   RuleId: CommandApprovalRuleId
   CommandProgressId: CommandApprovalProgressId
   Command: ApprovableCommand
   Criteria: CommandApprovalRule.Criteria
   PermittedApprovers: CommandApprovalRule.Approver list
   ApprovedBy: CommandApprovalRule.Approver list option
   DeclinedBy: CommandApprovalRule.Approver option
   RequestedBy: CommandApprovalProgress.Requester
   Status: CommandApprovalProgress.Status
   LastUpdate: DateTime
}

module CommandApprovalProgressWithRule =
   /// Employee may approve or deny the command if they are a PermittedApprover
   /// for a given command type & they haven't already approved the command.
   let mayApproveOrDeny
      (progress: CommandApprovalProgressWithRule)
      (eId: EmployeeId)
      =
      progress.Status = CommandApprovalProgress.Status.Pending
      && progress.PermittedApprovers
         |> List.exists (fun o -> o.EmployeeId = eId)
      && (progress.ApprovedBy
          |> Option.map (List.exists (fun o -> o.EmployeeId <> eId))
          |> Option.defaultValue true)
