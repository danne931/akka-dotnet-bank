namespace Bank.Purchase.Domain

open System

open Lib.SharedTypes

type PurchaseInfo = {
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   AccountId: AccountId
   EmployeeId: EmployeeId
   CardId: CardId
   InitiatedBy: Initiator
   CorrelationId: CorrelationId
   EmployeeName: string
   EmployeeEmail: Email.Email
   CardNumberLast4: string
   Date: DateTime
   Amount: decimal
   Merchant: string
   CurrencyMerchant: Currency
   CurrencyCardHolder: Currency
   Reference: string option
   CardIssuerCardId: CardIssuerCardId
   CardIssuerTransactionId: CardIssuerTransactionId
   CardNickname: string option
}

type Purchase = {
   Info: PurchaseInfo
   Events: PurchaseEvent list
   Status: PurchaseStatus
}
