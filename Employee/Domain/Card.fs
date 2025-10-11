namespace Bank.Employee.Domain

open System
open Validus

open Lib.SharedTypes

[<RequireQualifiedAccess>]
type CardIssuerName =
   | Lithic
   //| Marqueta
   static member fromString(status: string) : CardIssuerName option =
      if String.IsNullOrEmpty status then
         None
      else
         match status.ToLower() with
         | "lithic" -> Some Lithic
         //| "marqueta" -> Some Marqueta
         | _ -> None

   static member fromStringUnsafe(cardIssuerName: string) : CardIssuerName =
      match CardIssuerName.fromString cardIssuerName with
      | None -> failwith "Error attempting to cast string to CardIssuerName"
      | Some status -> status

[<RequireQualifiedAccess>]
type CardType =
   | Credit
   | Debit

   static member fromString(status: string) : CardType option =
      if String.IsNullOrEmpty status then
         None
      else
         match status.ToLower() with
         | "credit" -> Some Credit
         | "debit" -> Some Debit
         | _ -> None

   static member fromStringUnsafe(cardType: string) : CardType =
      match CardType.fromString cardType with
      | None -> failwith "Error attempting to cast string to CardType"
      | Some status -> status

[<RequireQualifiedAccess>]
type CardFrozenReason =
   | UserRequested
   /// The card is temporarily paused pending further review.
   | CardIssuerReview
   /// The card has one or more suspicious transactions or activities that require review.
   /// This can involve prompting the cardholder to confirm legitimate use or report confirmed fraud.
   | SuspectedFraud

[<RequireQualifiedAccess>]
type CardClosedReason =
   /// The physical card is no longer in the cardholder's possession due to
   /// being lost or never received by the cardholder.
   | Lost
   /// Card information has been exposed, potentially leading to unauthorized access.
   /// This may involve physical card theft, cloning, or online data breaches.
   | Compromised
   /// The physical card is not functioning properly, such as having chip failures
   /// or a demagnetized magnetic stripe.
   | Damaged
   /// The cardholder requested the closure of the card for reasons unrelated to
   /// fraud or damage, such as switching to a different product or closing the account.
   | EndUserRequest
   /// The issuer closed the card for reasons unrelated to fraud or damage, such
   /// as account inactivity, product or policy changes, or technology upgrades.
   | IssuerRequested
   | Other of string

[<RequireQualifiedAccess>]
type CardStatus =
   | Pending
   | Active
   | Frozen of CardFrozenReason
   | Closed of CardClosedReason

   override x.ToString() =
      match x with
      | Pending -> "Pending"
      | Active -> "Active"
      | Frozen _ -> "Frozen"
      | Closed _ -> "Closed"

type CardExpiration = {
   Month: int
   Year: int
} with

   static member create() : CardExpiration =
      let exp = DateTime.Now.AddYears 3
      { Month = exp.Month; Year = exp.Year }

type Card = {
   CardType: CardType
   CardNumberLast4: string
   DailyPurchaseLimit: decimal
   MonthlyPurchaseLimit: decimal
   Virtual: bool // virtual vs. physical card
   Status: CardStatus
   CardNickname: string option
   LastPurchaseAt: DateTime option
   Expiration: CardExpiration
   CardId: CardId
   AccountId: AccountId
} with

   member x.IsExpired() =
      DateTime(x.Expiration.Year, x.Expiration.Month, 1) <= DateTime.UtcNow

   member x.IsPending =
      match x.Status with
      | CardStatus.Pending -> true
      | _ -> false

   member x.IsFrozen =
      match x.Status with
      | CardStatus.Frozen reason -> Some reason
      | _ -> None

   member x.Display =
      $"""
      {x.CardNickname |> Option.defaultValue ""}
      **{x.CardNumberLast4}
      """

   static member dailyPurchaseLimitValidator =
      Check.Decimal.between 0m Constants.DAILY_PURCHASE_LIMIT_DEFAULT

   static member monthlyPurchaseLimitValidator =
      Check.Decimal.between 0m Constants.MONTHLY_PURCHASE_LIMIT_DEFAULT

type CardIssuerLink = {
   CardId: CardId
   CardIssuerCardId: CardIssuerCardId
   CardIssuerName: CardIssuerName
}

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

type PurchaseRuleEnforced = {
   Id: Guid
   Result: string
   Name: string
   Explanation: string
}
// There are four possible message types that can occur after the initial authorization (shown in the events.type field): CLEARING, AUTHORIZATION_REVERSAL, AUTHORIZATION_EXPIRY and AUTHORIZATION_ADVICE.
[<RequireQualifiedAccess>]
type PurchaseEventType =
   | Auth
   | AuthAdvice
   | AuthExpiry
   | AuthReversal
   | BalanceInquiry
   | Clearing
   | CorrectionCredit
   | CorrectionDebit
   | CreditAuth
   | CreditAuthAdvice
   | FinancialAuth
   | FinancialCreditAuth
   | Return
   | ReturnReversal

type PurchaseEvent = {
   Type: PurchaseEventType
   Amount: decimal
   //amounts: TransactionAmountsDTO
   Flow: MoneyFlow
   EnforcedRules: PurchaseRuleEnforced list
   EventId: Guid
   CreatedAt: DateTime
}

[<RequireQualifiedAccess>]
type PurchaseStatus =
   | Declined
   | Expired
   | Pending
   | Settled
   | Voided

type PurchaseAmount = { Amount: decimal; Currency: Currency }

type PurchaseCardHolderAmount = {
   ConversionRate: decimal
   Amount: decimal
   Currency: Currency
}

type PurchaseAmounts = {
   Hold: PurchaseAmount
   Cardholder: PurchaseCardHolderAmount
   Merchant: PurchaseAmount
   Settlement: PurchaseAmount
}

type CardIssuerPurchaseProgress = {
   Amounts: PurchaseAmounts
   Events: PurchaseEvent list
   //network: string
   //network_risk_score: string
   (*
    * ACCOUNT_PAUSED
    * ACCOUNT_STATE_TRANSACTION_FAIL
    * APPROVED
    * BANK_CONNECTION_ERROR
    * BANK_NOT_VERIFIED
    * CARD_CLOSED
    * CARD_PAUSED
    * DECLINED
    * FRAUD_ADVICE
    * IGNORED_TTL_EXPIRY
    * INACTIVE_ACCOUNT
    * INCORRECT_PIN
    * INVALID_CARD_DETAILS
    * INSUFFICIENT_FUNDS
    * INSUFFICIENT_FUNDS_PRELOAD
    * INVALID_TRANSACTION
    * MERCHANT_BLACKLIST
    * ORIGINAL_NOT_FOUND
    * PREVIOUSLY_COMPLETED
    * SINGLE_USE_RECHARGED
    * SWITCH_INOPERATIVE_ADVICE
    * UNAUTHORIZED_MERCHANT
    * UNKNOWN_HOST_TIMEOUT
    * USER_TRANSACTION_LIMIT
   *)
   Result: string
   Status: PurchaseStatus
   PurchaseId: CardIssuerTransactionId
   CardIssuerCardId: CardIssuerCardId
}

type Purchase = {
   Info: Bank.Account.Domain.PurchaseInfo
   Events: PurchaseEvent list
   Status: PurchaseStatus
}
