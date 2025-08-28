module UIDomain.Org

open Bank.Org.Domain
open Bank.Transfer.Domain
open Bank.Employee.Domain
open Lib.SharedTypes
open CommandApproval

type OrgCommandReceipt = {
   PendingCommand: OrgCommand
   PendingEvent: OrgEvent
   PendingState: Org
   Envelope: Envelope
}

type CommandApprovalProgressMaybe =
   Result<Map<CommandApprovalProgressId, CommandApprovalProgress.T> option, Err>

module CommandApprovalProgress =
   module RequestCommandApproval =
      let fromApprovableCommand
         (session: UserSession)
         (rule: CommandApprovalRule)
         (command: ApprovableCommand)
         : CommandApprovalProgress.RequestCommandApproval
         =
         CommandApprovalProgress.RequestCommandApproval.create
            session.OrgId
            session.AsInitiator
            command.CorrelationId
            {
               RuleId = rule.RuleId
               Command = command
               Requester = {
                  EmployeeName = session.Name
                  EmployeeId = session.EmployeeId
               }
               RequesterIsConfiguredAsAnApprover =
                  CommandApprovalRule.isRequesterOneOfManyApprovers
                     session.AsInitiator.Id
                     rule
            }

let displayApprovers (approvers: CommandApprover list) =
   approvers |> List.map _.DisplayName |> String.concat ", "

module CommandApprovalRule =
   /// Get list of approvers and detailed criteria details if any.
   let displayVerbose (rule: CommandApprovalRule) =
      let approvers = displayApprovers rule.Approvers
      let approverMsg = $"requires approval from {approvers}"

      match rule.Criteria with
      | ApprovalCriteria.PerCommand -> approverMsg
      | ApprovalCriteria.AmountDailyLimit limit ->
         "Daily limit (per employee) of "
         + Money.format limit
         + $" {approverMsg}"
      | ApprovalCriteria.AmountPerCommand range ->
         "Amount "
         + match range.LowerBound, range.UpperBound with
           | Some low, Some high ->
              $">= {Money.format low} and < {Money.format high}"
           | Some low, None -> $">= {Money.format low}"
           | None, Some high -> $"< {Money.format high}"
           | None, None -> ""
         + $" {approverMsg}"

module ApprovableCommand =
   let displayVerbose (cmd: ApprovableCommand) =
      match cmd with
      | ApprovableCommand.PerCommand c ->
         match c with
         | InviteEmployee c -> $"Invite employee {c.Data.Name}"
         | UpdateEmployeeRole c ->
            $"Update {c.Data.EmployeeName}'s role from {c.Data.PriorRole} to {c.Data.Role}"
         | UnlockCard c ->
            $"Unlock {c.Data.EmployeeName}'s {c.Data.CardName} **{c.Data.CardNumberLast4} card"
         | ManageApprovalRule c ->
            match c.Data with
            | ManageApprovalRuleInput.Delete(rule, _) ->
               $"Delete {rule.CommandType.Display} command approval rule"
            | ManageApprovalRuleInput.CreateOrEdit(rule, _) ->
               $"Configure {rule.CommandType.Display} command approval rule:"
               + $" {CommandApprovalRule.displayVerbose rule}"
      | ApprovableCommand.AmountBased c ->
         match c with
         | DomesticTransfer c ->
            $"{Money.format c.Data.Amount} domestic transfer from
            {c.Data.Originator.Name} to {c.Data.Counterparty.Name}"
         | InternalTransferBetweenOrgs c ->
            let p = c.Data

            match p.OriginatedFromPaymentRequest with
            | Some _ ->
               $"{Money.format p.Amount} payment fulfillment to {p.Recipient.Name}"
            | None -> $"{Money.format p.Amount} transfer to {p.Recipient.Name}"

/// Determines if there is a pending command approval for updating an employee
/// role. If there is then returns that pending role.
let employeeRolePendingApproval
   (progress: CommandApprovalProgress.T seq)
   (employeeId: EmployeeId)
   : Role option
   =
   progress
   |> Seq.tryPick (fun p ->
      match p.Status, p.CommandToInitiateOnApproval with
      | CommandApprovalProgress.Status.Pending,
        ApprovableCommand.PerCommand(UpdateEmployeeRole cmd) ->
         if EmployeeId.fromEntityId cmd.EntityId = employeeId then
            Some cmd.Data.Role
         else
            None
      | _ -> None)

/// Determines if there is a pending command approval for
/// a payment fulfillment.
let paymentFulfillmentPendingApproval
   (progress: CommandApprovalProgress.T seq)
   (paymentId: PaymentRequestId)
   : CommandApprovalProgress.T option
   =
   progress
   |> Seq.tryPick (fun p ->
      match p.Status, p.CommandToInitiateOnApproval with
      | CommandApprovalProgress.Status.Pending,
        ApprovableCommand.AmountBased(InternalTransferBetweenOrgs cmd) ->
         match cmd.Data.OriginatedFromPaymentRequest with
         | Some id when id = paymentId -> Some p
         | _ -> None
      | _ -> None)
