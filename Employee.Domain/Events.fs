namespace Bank.Employee.Domain

open System

open Lib.SharedTypes

type CreatedAccountOwner = {
   Email: Email
   FirstName: string
   LastName: string
   InviteToken: InviteToken
}

type CreatedEmployee = {
   Role: Role
   Email: Email
   FirstName: string
   LastName: string
   OrgRequiresEmployeeInviteApproval: CommandApprovalRuleId option
   CardInfo: EmployeeInviteSupplementaryCardInfo option
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

type CreatedCard = { PersonName: string; Card: Card }

type DebitRequested = { Info: DebitInfo }

type DebitApproved = { Info: DebitInfo }

type DebitDeclined = {
   Info: DebitInfo
   Reason: PurchaseDeclinedReason
}

type DailyDebitLimitUpdated = {
   CardId: CardId
   CardNumberLast4: string
   PriorLimit: decimal
   DebitLimit: decimal
}

type MonthlyDebitLimitUpdated = {
   CardId: CardId
   CardNumberLast4: string
   PriorLimit: decimal
   DebitLimit: decimal
}

type LockedCard = {
   CardId: CardId
   CardNumberLast4: string
   Reference: string option
}

type UnlockedCard = {
   CardId: CardId
   CardNumberLast4: string
   Reference: string option
}

type RoleUpdated = {
   Name: string
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
}

type AccessRestored = {
   Name: string
   Reference: string option
}
