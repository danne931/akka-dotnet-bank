[<RequireQualifiedAccess>]
module OrganizationStub

open System

open Lib.SharedTypes
open Bank.Org.Domain
open Bank.Employee.Domain

let orgId = Guid.NewGuid() |> OrgId
let ruleId () = Guid.NewGuid() |> CommandApprovalRuleId

let progressId () =
   Guid.NewGuid() |> CorrelationId |> CommandApprovalProgressId

let updateRoleCommand =
   UpdateRoleCommand.create
      (Guid.NewGuid() |> EmployeeId, orgId)
      (Guid.NewGuid() |> EmployeeId |> InitiatedById)
      {
         Name = ""
         Role = Role.Admin
         PriorRole = Role.Scholar
         CardInfo = None
      }
   |> ApprovableCommand.UpdateEmployeeRole

let progress: CommandApprovalProgress.T = {
   ProgressId = progressId ()
   RuleId = ruleId ()
   OrgId = orgId
   Status = CommandApprovalProgress.Status.Pending
   RequestedBy = {
      EmployeeId = Guid.NewGuid() |> EmployeeId
      EmployeeName = "A"
   }
   ApprovedBy = []
   DeclinedBy = None
   CreatedAt = DateTime.UtcNow
   LastUpdate = DateTime.UtcNow
   CommandToInitiateOnApproval = updateRoleCommand
}

let accrual: DailyAccrual = {
   PaymentsPaid = 0m
   DomesticTransfer = 0m
   InternalTransferBetweenOrgs = 0m
}
