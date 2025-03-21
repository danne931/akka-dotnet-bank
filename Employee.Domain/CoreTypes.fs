namespace Bank.Employee.Domain

open System
open Validus

open Lib.SharedTypes
open Lib.NetworkQuery

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
type CardStatus =
   //| Pending
   | Active
   | Frozen
   | Closed

   override x.ToString() =
      match x with
      | Active -> "Active"
      | Frozen -> "Frozen"
      | Closed -> "Closed"

   static member fromString(status: string) : CardStatus option =
      if String.IsNullOrEmpty status then
         None
      else
         match status.ToLower() with
         | "active" -> Some Active
         | "frozen" -> Some Frozen
         | "closed" -> Some Closed
         | _ -> None

   static member fromStringUnsafe(status: string) : CardStatus =
      match CardStatus.fromString status with
      | None -> failwith "Error attempting to cast string to CardStatus"
      | Some status -> status

type CardExpiration = { Month: int; Year: int }

module CardExpiration =
   let create () : CardExpiration =
      let exp = DateTime.Now.AddYears(3)
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
} with

   member x.IsExpired() =
      DateTime(x.Expiration.Year, x.Expiration.Month, 1) <= DateTime.UtcNow

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

[<RequireQualifiedAccess>]
type EmployeeStatus =
   | InitialEmptyState
   /// Requires approval from other admins before sending an invite.
   | PendingInviteApproval of InviteApproval
   /// Approval obtained (or no approval rule configured) so employee is ready
   /// for invite email.
   | PendingInviteConfirmation of InviteToken
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

type PurchaseInfo = {
   AccountId: AccountId
   InitiatedBy: Initiator
   CorrelationId: CorrelationId
   CardId: CardId
   CardNumberLast4: string
   Date: DateTime
   Amount: decimal
   Merchant: string
   Reference: string option
}

[<RequireQualifiedAccess>]
type PurchaseFailReason =
   | InsufficientAccountFunds of balance: decimal * accountName: string
   | ExceededDailyCardLimit of limit: decimal * accrued: decimal
   | ExceededMonthlyCardLimit of limit: decimal * accrued: decimal

module PurchaseFailReason =
   let display =
      function
      | PurchaseFailReason.InsufficientAccountFunds(balance, accountName) ->
         $"Account {accountName} has insufficient funds.  The current balance is ${balance}."
      | PurchaseFailReason.ExceededDailyCardLimit(limit, accrued) ->
         $"You have spent ${accrued} today. 
           Your daily purchase limit is set to ${limit}."
      | PurchaseFailReason.ExceededMonthlyCardLimit(limit, accrued) ->
         $"You have spent ${accrued} this month. 
           Your monthly purchase limit is set to ${limit}."

/// Tasks to initiate upon employee invite confirmation.
type EmployeeOnboardingTask = CreateCard of EmployeeInviteSupplementaryCardInfo

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

[<RequireQualifiedAccess>]
type EmployeeEventGroupFilter =
   | Invitation
   | CreatedCard
   | Purchase
   | PurchaseLimitUpdated
   | CardFrozenUnfrozen
   | UpdatedRole
   | AccessRestored

   member x.Display =
      match x with
      | EmployeeEventGroupFilter.Invitation -> "Invitations"
      | EmployeeEventGroupFilter.CreatedCard -> "Cards Issued"
      | EmployeeEventGroupFilter.Purchase -> "Purchases"
      | EmployeeEventGroupFilter.PurchaseLimitUpdated ->
         "Purchase Limit Applied"
      | EmployeeEventGroupFilter.CardFrozenUnfrozen -> "Card Frozen/Unfrozen"
      | EmployeeEventGroupFilter.UpdatedRole -> "Employee Role Altered"
      | EmployeeEventGroupFilter.AccessRestored -> "Employee Access Restored"

   static member All = [
      EmployeeEventGroupFilter.Invitation
      EmployeeEventGroupFilter.Purchase
      EmployeeEventGroupFilter.CreatedCard
      EmployeeEventGroupFilter.UpdatedRole
      EmployeeEventGroupFilter.CardFrozenUnfrozen
      EmployeeEventGroupFilter.PurchaseLimitUpdated
      EmployeeEventGroupFilter.AccessRestored
   ]

module EmployeeEventGroupFilter =
   let fromString =
      function
      | "Invitation" -> Some EmployeeEventGroupFilter.Invitation
      | "CreatedCard" -> Some EmployeeEventGroupFilter.CreatedCard
      | "Purchase" -> Some EmployeeEventGroupFilter.Purchase
      | "PurchaseLimitUpdated" ->
         Some EmployeeEventGroupFilter.PurchaseLimitUpdated
      | "CardFrozenUnfrozen" -> Some EmployeeEventGroupFilter.CardFrozenUnfrozen
      | "UpdatedRole" -> Some EmployeeEventGroupFilter.UpdatedRole
      | "AccessRestored" -> Some EmployeeEventGroupFilter.AccessRestored
      | _ -> None

   let fromQueryString: string -> EmployeeEventGroupFilter list option =
      listFromQueryString fromString

   let listToDisplay (items: EmployeeEventGroupFilter list) =
      List.fold
         (fun acc (filter: EmployeeEventGroupFilter) ->
            if acc = "" then
               filter.Display
            else
               $"{acc}, {filter.Display}")
         ""
         items

type EmployeeQuery = {
   EmployeeIds: (EmployeeId list) option
   Roles: (Role list) option
   Status: EmployeeStatus option
}

module EmployeeQuery =
   let rolesFromQueryString = listFromQueryString Role.fromString

   let rolesToDisplay =
      List.fold
         (fun acc (filter: Role) ->
            if acc = "" then
               filter.Display
            else
               $"{acc}, {filter.Display}")
         ""

   let employeeIdsFromQueryString: string -> EmployeeId list option =
      listFromQueryString (Guid.parseOptional >> Option.map EmployeeId)

type CardQuery = {
   AccountIds: (AccountId list) option
   EmployeeIds: (EmployeeId list) option
   CreatedAtDateRange: (DateTime * DateTime) option
   Amount: AmountFilter option
}

module CardQuery =
   let employeeIdsFromQueryString = EmployeeQuery.employeeIdsFromQueryString

   let accountIdsFromQueryString =
      listFromQueryString (Guid.parseOptional >> Option.map AccountId)
