namespace Bank.Employee.Domain

open System

open Lib.SharedTypes

type EmployeeReference = {
   EmployeeName: string
   EmployeeId: EmployeeId
}

type EmployeeInviteSupplementaryCardInfo = {
   DailyPurchaseLimit: decimal
   MonthlyPurchaseLimit: decimal
   LinkedAccountId: AccountId
   CardType: CardType
}

type InviteToken = {
   Token: Guid
   Expiration: DateTime
} with

   member x.IsExpired() =
      x.Expiration >= DateTime.UtcNow.AddDays 7.

   static member generate() = {
      Token = Guid.NewGuid()
      Expiration = DateTime.UtcNow.AddDays 7.
   }

type InviteApproval = {
   RuleId: CommandApprovalRuleId
   ProgressId: CommandApprovalProgressId
}

type PendingInviteConfirmation = {
   Token: InviteToken
   CorrelationId: CorrelationId
}

type EmployeePendingInviteConfirmation = {
   Email: Email
   Name: string
   InviteConfirmation: PendingInviteConfirmation
   EmployeeId: EmployeeId
   OrgId: OrgId
}

[<RequireQualifiedAccess>]
type EmployeeStatus =
   | InitialEmptyState
   /// Requires approval from other admins before sending an invite.
   | PendingInviteApproval of InviteApproval
   /// Approval obtained (or no approval rule configured) so employee is ready
   /// for invite email.
   | PendingInviteConfirmation of PendingInviteConfirmation
   /// Employee has confirmed their invitation.
   | Active
   | Closed
   | PendingRestoreAccessApproval
   | ReadyForDelete

   member x.Display =
      match x with
      | InitialEmptyState -> ""
      | PendingInviteApproval _ -> "Pending Invite Approval"
      | PendingInviteConfirmation _ -> "Pending Invite Confirmation"
      | Active -> "Active"
      | Closed -> "Closed"
      | PendingRestoreAccessApproval -> "Pending Restore Approval"
      | ReadyForDelete -> "Processing Removal"

   override x.ToString() =
      match x with
      | InitialEmptyState -> "InitialEmptyState"
      | PendingInviteApproval _ -> "PendingInviteApproval"
      | PendingInviteConfirmation _ -> "PendingInviteConfirmation"
      | Active -> "Active"
      | Closed -> "Closed"
      | PendingRestoreAccessApproval -> "PendingRestoreAccessApproval"
      | ReadyForDelete -> "ReadyForDelete"

type UserSession = {
   OrgId: OrgId
   EmployeeId: EmployeeId
   Name: string
   Email: Email
   Role: Role
} with

   member x.AsInitiator: Initiator = {
      Id = InitiatedById x.EmployeeId
      Name = x.Name
   }
