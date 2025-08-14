namespace Bank.Employee.Domain

open System
open Validus

open Lib.SharedTypes

type ThirdPartyProviderCardId =
   | ThirdPartyProviderCardId of Guid

   override x.ToString() =
      let (ThirdPartyProviderCardId id) = x
      string id

   static member get(ThirdPartyProviderCardId id) = id

type EmployeeReference = {
   EmployeeName: string
   EmployeeId: EmployeeId
}

[<RequireQualifiedAccess>]
type CardType =
   | Credit
   | Debit

   static member fromString(status: string) : CardType option =
      if String.IsNullOrEmpty status then
         None
      else
         match status.ToLower() with
         | "credit" -> Some Credit
         | "debit" -> Some Debit
         | _ -> None

   static member fromStringUnsafe(cardType: string) : CardType =
      match CardType.fromString cardType with
      | None -> failwith "Error attempting to cast string to CardType"
      | Some status -> status

[<RequireQualifiedAccess>]
type CardFrozenReason =
   | UserRequested
   | SuspectedFraud

[<RequireQualifiedAccess>]
type CardStatus =
   | Pending
   | Active
   | Frozen of CardFrozenReason
   | Closed

   override x.ToString() =
      match x with
      | Pending -> "Pending"
      | Active -> "Active"
      | Frozen _ -> "Frozen"
      | Closed -> "Closed"

type CardExpiration = { Month: int; Year: int }

module CardExpiration =
   let create () : CardExpiration =
      let exp = DateTime.Now.AddYears 3
      { Month = exp.Month; Year = exp.Year }

type Card = {
   CardType: CardType
   CardNumberLast4: string
   DailyPurchaseLimit: decimal
   MonthlyPurchaseLimit: decimal
   Virtual: bool // virtual vs. physical card
   Status: CardStatus
   CardNickname: string option
   LastPurchaseAt: DateTime option
   Expiration: CardExpiration
   CardId: CardId
   AccountId: AccountId
   ThirdPartyProviderCardId: ThirdPartyProviderCardId option
} with

   member x.IsExpired() =
      DateTime(x.Expiration.Year, x.Expiration.Month, 1) <= DateTime.UtcNow

   member x.IsPending =
      match x.Status with
      | CardStatus.Pending -> true
      | _ -> false

   member x.IsFrozen =
      match x.Status with
      | CardStatus.Frozen reason -> Some reason
      | _ -> None

   member x.Display =
      $"""
      {x.CardNickname |> Option.defaultValue ""}
      **{x.CardNumberLast4}
      """

module Card =
   let dailyPurchaseLimitValidator =
      Check.Decimal.between 0m Constants.DAILY_PURCHASE_LIMIT_DEFAULT

   let monthlyPurchaseLimitValidator =
      Check.Decimal.between 0m Constants.MONTHLY_PURCHASE_LIMIT_DEFAULT

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

module InviteToken =
   let generate () = {
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
