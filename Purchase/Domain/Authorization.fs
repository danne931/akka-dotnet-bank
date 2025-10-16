namespace Bank.Purchase.Domain

open System

open Lib.SharedTypes

type PurchaseAuthorization = {
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
