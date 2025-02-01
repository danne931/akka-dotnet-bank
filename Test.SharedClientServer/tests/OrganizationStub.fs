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
   |> UpdateEmployeeRole
   |> ApprovableCommand.PerCommand

let inviteEmployeeCommand =
   ApproveAccessCommand.create
      (Guid.NewGuid() |> EmployeeId, orgId)
      (Guid.NewGuid() |> EmployeeId |> InitiatedById)
      (Guid.NewGuid() |> CorrelationId)
      { Name = "Dan E"; Reference = None }
   |> InviteEmployee
   |> ApprovableCommand.PerCommand

let createOrgCommand =
   CreateOrgCommand.create {
      OrgId = orgId
      Name = "new org"
      InitiatedBy = Guid.NewGuid() |> EmployeeId |> InitiatedById
   }

let progress (cmd: ApprovableCommand) : CommandApprovalProgress.T = {
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
   CommandToInitiateOnApproval = cmd
}

let accrual: CommandApprovalDailyAccrual = {
   PaymentsPaid = 0m
   DomesticTransfer = 0m
   InternalTransferBetweenOrgs = 0m
}

let orgStateWithEvents: OrgWithEvents = {
   Info = {
      Name = "Sapphire Health"
      OrgId = orgId
      Status = OrgStatus.Active
      FeatureFlags = {
         SocialTransferDiscoveryPrimaryAccountId = None
      }
      CommandApprovalRules = Map.empty
      CommandApprovalProgress = Map.empty
   }
   Events = []
   AccrualMetrics = Map.empty
}

let commandTypes = {|
   InviteEmployee =
      ApprovableCommandType.ApprovablePerCommand InviteEmployeeCommandType
   UpdateEmployeeRole =
      ApprovableCommandType.ApprovablePerCommand UpdateEmployeeRoleCommandType
   DomesticTransfer =
      ApprovableCommandType.ApprovableAmountBased DomesticTransferCommandType
   Payment =
      ApprovableCommandType.ApprovableAmountBased
         FulfillPlatformPaymentCommandType
   InternalTransfer =
      ApprovableCommandType.ApprovableAmountBased
         InternalTransferBetweenOrgsCommandType
|}
