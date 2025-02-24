[<RequireQualifiedAccess>]
module OrganizationStub

open System

open Lib.SharedTypes
open Bank.Org.Domain
open Bank.Employee.Domain
open Bank.Transfer.Domain
open CommandApproval

let orgId = Guid.NewGuid() |> OrgId
let ruleId () = Guid.NewGuid() |> CommandApprovalRuleId

let initiator = {
   Id = Guid.NewGuid() |> EmployeeId |> InitiatedById
   Name = "Devon E"
}

let progressId () =
   Guid.NewGuid() |> CorrelationId |> CommandApprovalProgressId

let domesticRecipient: DomesticTransferRecipient = {
   SenderOrgId = orgId
   LastName = "fish"
   FirstName = "big"
   Nickname = None
   AccountNumber = AccountNumber <| Int64.Parse "123456789123456"
   RoutingNumber = RoutingNumber 123456789
   Status = RecipientRegistrationStatus.Confirmed
   RecipientAccountId = Guid.NewGuid() |> AccountId
   Depository = DomesticRecipientAccountDepository.Checking
   PaymentNetwork = PaymentNetwork.ACH
   CreatedAt = DateTime.UtcNow
}

let command = {|
   updateRole =
      UpdateRoleCommand.create (Guid.NewGuid() |> EmployeeId, orgId) initiator {
         Name = ""
         Role = Role.Admin
         PriorRole = Role.Scholar
         CardInfo = None
      }
      |> UpdateEmployeeRole
      |> ApprovableCommand.PerCommand

   inviteEmployee =
      ApproveAccessCommand.create
         (Guid.NewGuid() |> EmployeeId, orgId)
         initiator
         (Guid.NewGuid() |> CorrelationId)
         { Name = "Dan E"; Reference = None }
      |> InviteEmployee
      |> ApprovableCommand.PerCommand
   createOrg =
      CreateOrgCommand.create {
         OrgId = orgId
         Name = "new org"
         InitiatedBy = initiator
      }
   registerDomesticRecipient =
      RegisterDomesticTransferRecipientCommand.create orgId initiator {
         AccountId = Guid.NewGuid() |> AccountId
         FirstName = domesticRecipient.FirstName
         LastName = domesticRecipient.LastName
         AccountNumber = string domesticRecipient.AccountNumber
         RoutingNumber = string domesticRecipient.RoutingNumber
         Depository = DomesticRecipientAccountDepository.Checking
         PaymentNetwork = PaymentNetwork.ACH
      }
|}

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

let orgStateWithEvents: OrgSnapshot = {
   Info = {
      Name = "Sapphire Health"
      OrgId = orgId
      Status = OrgStatus.Active
      FeatureFlags = {
         SocialTransferDiscoveryPrimaryAccountId = None
      }
      CommandApprovalRules = Map.empty
      CommandApprovalProgress = Map.empty
      DomesticTransferRecipients = Map.empty
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
