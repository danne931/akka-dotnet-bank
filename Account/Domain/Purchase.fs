namespace Bank.Account.Domain

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

[<RequireQualifiedAccess>]
type PurchaseCardFailReason =
   | CardNotFound
   | CardExpired
   | CardLocked
   | ExceededDailyCardLimit of limit: decimal * accrued: decimal
   | ExceededMonthlyCardLimit of limit: decimal * accrued: decimal

   member x.Display =
      match x with
      | PurchaseCardFailReason.CardNotFound -> "Card not found."
      | PurchaseCardFailReason.CardExpired -> "Card expired."
      | PurchaseCardFailReason.CardLocked -> "Card locked."
      | PurchaseCardFailReason.ExceededDailyCardLimit(limit, accrued) ->
         $"You have spent ${accrued} today.
           Your daily purchase limit is set to ${limit}."
      | PurchaseCardFailReason.ExceededMonthlyCardLimit(limit, accrued) ->
         $"You have spent ${accrued} this month.
           Your monthly purchase limit is set to ${limit}."

[<RequireQualifiedAccess>]
type PurchaseAccountFailReason =
   | AccountNotActive of accountName: string
   | InsufficientAccountFunds of balance: decimal * accountName: string

   member x.Display =
      match x with
      | PurchaseAccountFailReason.AccountNotActive accountName ->
         $"Account {accountName} is not active."
      | PurchaseAccountFailReason.InsufficientAccountFunds(balance, accountName) ->
         $"Account {accountName} has insufficient funds.  The current balance is ${balance}."

[<RequireQualifiedAccess>]
type CardNetworkFailReason =
   | Declined
   | Expired
   | Voided

[<RequireQualifiedAccess>]
type PurchaseFailReason =
   | Card of PurchaseCardFailReason
   | Account of PurchaseAccountFailReason
   | CardNetwork of CardNetworkFailReason
   | PartnerBankSync of string

   member x.Display =
      match x with
      | PurchaseFailReason.Card reason -> reason.Display
      | PurchaseFailReason.Account reason -> reason.Display
      | PurchaseFailReason.CardNetwork _ ->
         "Card network declined the purchase."
      | PurchaseFailReason.PartnerBankSync _ ->
         "Unable to sync to partner bank."

[<RequireQualifiedAccess>]
type PurchaseRefundReason =
   | FraudulentActivity
   | DuplicateCharge
   | UserRequested of reason: string
