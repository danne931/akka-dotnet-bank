[<RequireQualifiedAccess>]
module OrganizationStub

open System

open Lib.SharedTypes
open Bank.Org.Domain
open Bank.Employee.Domain
open Bank.Transfer.Domain
open CommandApproval
open Email

let orgId = Guid.NewGuid() |> OrgId
let ruleId () = Guid.NewGuid() |> CommandApprovalRuleId

let initiator = {
   Id = Guid.NewGuid() |> EmployeeId |> InitiatedById
   Name = "Devon E"
}

let progressId () =
   Guid.NewGuid() |> CorrelationId |> CommandApprovalProgressId

let domesticRecipient: Counterparty = {
   Kind = CounterpartyType.TradingPartner
   OrgId = orgId
   LastName = "fish"
   FirstName = "big"
   Nickname = None
   AccountNumber = AccountNumber <| Int64.Parse "123456789123456"
   RoutingNumber = RoutingNumber "123456789"
   CounterpartyId = Guid.NewGuid() |> CounterpartyId
   PartnerBankCounterpartyId = PartnerBankCounterpartyId "123456789"
   Depository = CounterpartyAccountDepository.Checking
   PaymentNetwork = PaymentNetwork.ACH
   CreatedAt = DateTime.UtcNow
   Address = Address.empty
}

let command = {|
   updateRole =
      UpdateRoleCommand.create (Guid.NewGuid() |> EmployeeId, orgId) initiator {
         EmployeeName = ""
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
      SubmitOrgOnboardingApplicationCommand.create {
         OrgId = orgId
         AdminTeamEmail = Email.deserialize "test-org@test-org.com"
         ParentAccountId = Guid.NewGuid() |> ParentAccountId
         BusinessDetails = {
            BusinessName = "new org"
            EmployerIdentificationNumber = "123456789"
            Address = Address.empty
            LegalType = BusinessType.LimitedPartnership
            Description = "new org"
            Website = None
         }
         InitiatedBy = initiator
      }
   registerDomesticRecipient =
      RegisterCounterpartyCommand.create initiator {
         CounterpartyId = Guid.NewGuid() |> CounterpartyId
         PartnerBankCounterpartyId = PartnerBankCounterpartyId "123456789"
         Kind = CounterpartyType.TradingPartner
         FirstName = domesticRecipient.FirstName
         LastName = domesticRecipient.LastName
         AccountNumber = string domesticRecipient.AccountNumber
         RoutingNumber = string domesticRecipient.RoutingNumber
         Depository = CounterpartyAccountDepository.Checking
         PaymentNetwork = PaymentNetwork.ACH
         Address = Address.empty
         Sender = {|
            OrgId = orgId
            ParentAccountId = Guid.NewGuid() |> ParentAccountId
         |}
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
      AdminTeamEmail = Email.empty
      EmployerIdentificationNumber = ""
      Address = Address.empty
      BusinessType = BusinessType.LimitedPartnership
      Description = ""
      Website = None
      ParentAccountId = Guid.NewGuid() |> ParentAccountId
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
   InternalTransfer =
      ApprovableCommandType.ApprovableAmountBased
         InternalTransferBetweenOrgsCommandType
|}
