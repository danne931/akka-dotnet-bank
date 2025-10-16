namespace Bank.Employee.Domain

open System

open Lib.SharedTypes
open Bank.Purchase.Domain
open Email

type CreatedAccountOwner = {
   Email: Email
   FirstName: string
   LastName: string
   InviteToken: InviteToken
   ParentAccountId: ParentAccountId
}

type CreatedEmployee = {
   Role: Role
   Email: Email
   FirstName: string
   LastName: string
   OrgRequiresEmployeeInviteApproval: CommandApprovalRuleId option
   CardInfo: EmployeeInviteSupplementaryCardInfo option
   InviteToken: InviteToken
   ParentAccountId: ParentAccountId
}

type InvitationConfirmed = {
   Email: Email
   AuthProviderUserId: Guid
   Reference: string option
}

type InvitationTokenRefreshed = {
   InviteToken: InviteToken
   Reason: string option
}

type InvitationCancelled = { Reason: string option }

/// Initialize card in context of application
type CreatedCard = {
   PersonName: string
   Card: Card
   /// Refers to the employee onboarding saga id if this card
   /// creation originated as an activity in the employee onboarding saga.
   OriginatedFromEmployeeOnboarding: CorrelationId option
}

/// Initialize card in context of card issuer
type CardLinked = {
   Link: CardIssuerLink
   CardNumberLast4: string
}

type CardPurchasePending = { Info: PurchaseInfo }

type CardIssuerUpdatedPurchaseProgress = { Info: CardIssuerPurchaseProgress }

type CardPurchaseSettled = { Info: PurchaseInfo }

type CardPurchaseFailed = {
   Info: PurchaseInfo
   Reason: PurchaseFailReason
}

type CardPurchaseRefunded = {
   Info: PurchaseInfo
   Reason: PurchaseRefundReason
}

type ConfiguredRollingPurchaseLimit = {
   CardId: CardId
   CardNumberLast4: string
   PriorDailyLimit: decimal
   PriorMonthlyLimit: decimal
   DailyLimit: decimal
   MonthlyLimit: decimal
}

type LockedCard = {
   CardId: CardId
   CardName: string
   CardNumberLast4: string
   EmployeeName: string
   Reference: string option
}

type UnlockedCard = {
   CardId: CardId
   CardName: string
   CardNumberLast4: string
   EmployeeName: string
   Reference: string option
}

type RoleUpdated = {
   EmployeeName: string
   Role: Role
   PriorRole: Role
   CardInfo: EmployeeInviteSupplementaryCardInfo option
}

type CardNicknamed = {
   Name: string
   PriorName: string option
   CardId: CardId
}

type AccessApproved = {
   Name: string
   Reference: string option
   InviteToken: InviteToken
}

type AccessRestored = {
   Name: string
   Reference: string option
   InviteToken: InviteToken
}
