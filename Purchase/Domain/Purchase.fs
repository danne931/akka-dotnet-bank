namespace Bank.Purchase.Domain

open System

open Lib.SharedTypes

type PurchaseClearedId =
   | PurchaseClearedId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (PurchaseClearedId id) = x in id

/// A single CardIssuerPurchaseProgress received from Lithic may
/// present us with many PurchaseEvents that update the purchase amount
/// such as multiple clearings, AuthReversal, or AuthExpiry.
/// We will combine all amount-altering PurchaseEvents within a
/// a CardIssuerPurchaseProgress saga event into a single
/// PurchaseClearing. This PurchaseClearing will be referenced
/// when sending commands to the Account and Employee entity
/// actors to apply settled amounts.
type PurchaseClearing = {
   PurchaseClearedId: PurchaseClearedId
   ClearedAmount: Money
}

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
   Merchant: NonEmptyString
   CurrencyMerchant: Currency
   CurrencyCardHolder: Currency
   Reference: string option
   CardIssuerCardId: CardIssuerCardId
   CardIssuerTransactionId: CardIssuerTransactionId
   CardNickname: string option
   AuthorizationType: PurchaseAuthType
}

type Purchase = {
   Info: PurchaseInfo
   Events: PurchaseEvent list
   Status: PurchaseStatus
}
