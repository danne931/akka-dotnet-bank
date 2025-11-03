namespace Bank.Purchase.Domain

open System

open Lib.SharedTypes

[<RequireQualifiedAccess>]
type PurchaseAuthType =
   | Debit
   //| Credit
   /// SMS (Single Message System): Purchase Authorization which
   /// translates immediately into a settled transaction.
   | DebitSMS
   //| CreditSMS
   /// It is sometimes possible to receive a purchase progress update from Lithic without
   /// first receiving the purchase intent at the authorization stream access
   /// webhook.  This poses some risk in that they may allow a purchase to settle
   /// for an amount above the cardholder's balance, without affording the user
   /// the opportunity to decline.  Such situations may be subject to chargeback.
   /// See Purchase/Domain/PurchaseLifecycleEvent.fs cases 16-20.
   /// When this is received we will create a purchase intent that bypasses
   /// validation mechanisms.
   | BypassAuth

type PurchaseAuthorization = {
   Type: PurchaseAuthType
   CardId: CardId
   CardIssuerCardId: CardIssuerCardId
   CardIssuerTransactionId: CardIssuerTransactionId
   Amount: decimal
   MerchantCategoryCode: int
   MerchantName: string
   CurrencyCardHolder: Currency
   CurrencyMerchant: Currency
   CreatedAt: DateTime
}
