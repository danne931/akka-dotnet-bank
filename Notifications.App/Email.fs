module Email

open Lib.SharedTypes
open Bank.Employee.Domain

type EmployeeInviteEmailInfo = {
   Name: string
   Email: Email
   Token: InviteToken
   OrgId: OrgId
}

type TransferDepositEmailInfo = {
   AccountName: string
   Amount: decimal
   SenderBusinessName: string
   OrgId: OrgId
}

type PurchaseFailEmailInfo = {
   Email: Email
   Reason: PurchaseFailReason
   OrgId: OrgId
}

[<RequireQualifiedAccess>]
type EmailMessage =
   | AccountOpen of accountName: string * OrgId
   | AccountClose of accountName: string * OrgId
   | BillingStatement of accountName: string * OrgId
   | PurchaseFailed of PurchaseFailEmailInfo
   | InternalTransferBetweenOrgsDeposited of TransferDepositEmailInfo
   | ApplicationErrorRequiresSupport of error: string * OrgId
   | EmployeeInvite of EmployeeInviteEmailInfo
