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
