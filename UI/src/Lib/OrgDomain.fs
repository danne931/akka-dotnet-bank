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

/// Determines if there is a pending command approval for updating an employee
/// role. If there is then returns that pending role.
let employeeRolePendingApproval
   (progress: CommandApprovalProgress.T seq)
   (employee: Employee)
   : Role option
   =
   progress
   |> Seq.tryPick (fun p ->
      match p.Status, p.CommandToInitiateOnApproval with
      | CommandApprovalProgress.Status.Pending,
        ApprovableCommand.PerCommand(UpdateEmployeeRole cmd) ->
         if (EmployeeId.fromEntityId cmd.EntityId) = employee.EmployeeId then
            Some cmd.Data.Role
         else
            None
      | _ -> None)

/// Determines if there is a pending command approval for
/// a payment fulfillment.
let paymentFulfillmentPendingApproval
   (progress: CommandApprovalProgress.T seq)
   (payment: PaymentBaseInfo)
   : CommandApprovalProgress.T option
   =
   progress
   |> Seq.tryPick (fun p ->
      match p.Status, p.CommandToInitiateOnApproval with
      | CommandApprovalProgress.Status.Pending,
        ApprovableCommand.AmountBased(FulfillPlatformPayment cmd) ->
         if cmd.Data.RequestedPayment.BaseInfo.Id = payment.Id then
            Some p
         else
            None
      | _ -> None)
