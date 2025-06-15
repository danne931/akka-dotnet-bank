namespace Bank.Account.Domain

open System

open Lib.SharedTypes
open MaintenanceFee

type PrimaryCheckingAccountInfo = {
   AccountId: AccountId
   Name: string
   AccountNumber: AccountNumber
   RoutingNumber: RoutingNumber
}

type InitializedPrimaryCheckingAccount = {
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   PartnerBankRoutingNumber: ParentRoutingNumber
   PartnerBankAccountNumber: ParentAccountNumber
   PrimaryChecking: PrimaryCheckingAccountInfo
}

type CreatedVirtualAccount = {
   AccountId: AccountId
   Name: string
   Depository: AccountDepository
   Balance: decimal
   Currency: Currency
   AccountNumber: AccountNumber
   RoutingNumber: RoutingNumber
}

type DepositedCash = {
   AccountId: AccountId
   Amount: decimal
   Origin: string
}

type EmployeePurchaseReference = {
   EmployeeName: string
   EmployeeCardNumberLast4: string
   EmployeeId: EmployeeId
   CardId: CardId
}

type DebitedAccount = {
   AccountId: AccountId
   Date: DateTime
   Amount: decimal
   Merchant: string
   Reference: string option
   EmployeePurchaseReference: EmployeePurchaseReference
}

type PurchaseSettled = {
   AccountId: AccountId
   SettlementId: SettlementId
   Merchant: string
   Amount: decimal
   EmployeePurchaseReference: EmployeePurchaseReference
}

type RefundedDebit = {
   AccountId: AccountId
   EmployeePurchaseReference: EmployeePurchaseReference
   Merchant: string
   Amount: decimal
   Reason: PurchaseRefundReason
}

type MaintenanceFeeDebited = {
   AccountId: AccountId
   Amount: decimal
   BillingDate: DateTime
}

type MaintenanceFeeSkipped = {
   AccountId: AccountId
   Reason: MaintenanceFeeCriteria
   BillingDate: DateTime
}

type AccountClosed = {
   AccountId: AccountId
   Reference: string option
}
