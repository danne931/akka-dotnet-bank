module UIDomain.Org

open Bank.Org.Domain
open Bank.Transfer.Domain
open Bank.Employee.Domain
open Lib.SharedTypes

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
         (rule: CommandApprovalRule.T)
         (command: ApprovableCommand)
         : CommandApprovalProgress.RequestCommandApproval
         =
         let initiatedBy = InitiatedById session.EmployeeId

         CommandApprovalProgress.RequestCommandApproval.create
            session.OrgId
            initiatedBy
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
                     initiatedBy
                     rule
            }

module ApprovableCommand =
   let displayVerbose (cmd: ApprovableCommand) =
      match cmd with
      | ApprovableCommand.PerCommand c ->
         match c with
         | InviteEmployee c -> $"Invite employee {c.Data.Name}"
         | UpdateEmployeeRole c ->
            $"Update {c.Data.Name}'s role from {c.Data.PriorRole} to {c.Data.Role}"
         | UnlockCard c ->
            $"Unlock {c.Data.EmployeeName}'s {c.Data.CardName} **{c.Data.CardNumberLast4} card"
      | ApprovableCommand.AmountBased c ->
         match c with
         | DomesticTransfer c ->
            $"{Money.format c.Data.Amount} domestic transfer from
            {c.Data.Sender.Name} to {c.Data.Recipient.Name}"
         | FulfillPlatformPayment c ->
            let pay = c.Data.RequestedPayment.BaseInfo
            $"{Money.format pay.Amount} payment fulfillment to {pay.Payee.OrgName}"
         | InternalTransferBetweenOrgs c ->
            $"{Money.format c.Data.Amount} transfer to {c.Data.Recipient.Name}"

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
         if (EmployeeId.fromEntityId cmd.EntityId) = employeeId then
            Some cmd.Data.Role
         else
            None
      | _ -> None)

/// Determines if there is a pending command approval for
/// a payment fulfillment.
let paymentFulfillmentPendingApproval
   (progress: CommandApprovalProgress.T seq)
   (paymentId: PaymentId)
   : CommandApprovalProgress.T option
   =
   progress
   |> Seq.tryPick (fun p ->
      match p.Status, p.CommandToInitiateOnApproval with
      | CommandApprovalProgress.Status.Pending,
        ApprovableCommand.AmountBased(FulfillPlatformPayment cmd) ->
         if cmd.Data.RequestedPayment.BaseInfo.Id = paymentId then
            Some p
         else
            None
      | _ -> None)
