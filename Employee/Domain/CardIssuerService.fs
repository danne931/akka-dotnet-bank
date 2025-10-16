module CardIssuer.Service.Domain

open System
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Bank.Employee.Domain
open Bank.Purchase.Domain

[<RequireQualifiedAccess>]
type CardIssuerMetadata = {
   OrgId: OrgId
   CorrelationId: CorrelationId
}

type CardIssuerCreateCardRequest = {
   CardType: CardType
   CardNickname: string option
   Expiration: CardExpiration
   Metadata: CardIssuerMetadata
} with

   member x.AsDTO: obj =
      let dto = {|
         ``type`` = "VIRTUAL"
         exp_year = string x.Expiration.Year
         exp_month = sprintf "%02i" x.Expiration.Month
      |}

      match x.CardNickname with
      | None -> dto
      | Some name -> {| dto with memo = name |}

type CardCreateResponse = {
   CardIssuerCardId: CardIssuerCardId
   CardNumberLast4: string
   CardIssuerName: CardIssuerName
}

type CardCreateResponseDTO = {
   token: Guid
   state: string
   ``type``: string
   last_four: string
   exp_month: string
   exp_year: string
} with

   member x.AsEntity = {
      CardIssuerCardId = CardIssuerCardId x.token
      CardNumberLast4 = x.last_four
      CardIssuerName = CardIssuerName.Lithic
   }

type CardIssuerCloseCardRequest = {
   CardIssuerCardId: CardIssuerCardId
   Metadata: CardIssuerMetadata
}

type SimulatePurchaseRequest = {
   Amount: decimal
   Descriptor: string
   CardNumber: string
   MerchantCurrency: Currency
   Metadata: CardIssuerMetadata
} with

   member x.AsDTO =
      let amount = Math.Round(x.Amount, 2) * 100m |> int

      {|
         amount = amount
         descriptor = x.Descriptor
         pan = x.CardNumber
         merchant_currency = string x.MerchantCurrency
         merchant_amount = amount
      |}

type SimulatePurchaseResponse = {
   CardIssuerTransactionId: CardIssuerTransactionId
}

type SimulatePurchaseResponseDTO = {
   token: Guid
} with

   member x.AsEntity: SimulatePurchaseResponse = {
      CardIssuerTransactionId = CardIssuerTransactionId x.token
   }

type CardGetResponse = {
   CardIssuerCardId: CardIssuerCardId
   NumberLast4: string
   Number: string
   Expiration: CardExpiration
}

type CardGetResponseDTO = {
   token: Guid
   state: string
   ``type``: string
   last_four: string
   exp_month: string
   exp_year: string
   pan: string
} with

   member x.AsEntity = {
      CardIssuerCardId = CardIssuerCardId x.token
      NumberLast4 = x.last_four
      Number = x.pan
      Expiration = {
         Month = int x.exp_month
         Year = int x.exp_year
      }
   }

[<RequireQualifiedAccess>]
type AuthorizationStreamAction =
   | Auth
   //| CreditAuth
   | FinancialAuth
   //| FinancialCreditAuth
   //| BalanceInquiry

   static member fromString(action) =
      match action with
      | "AUTHORIZATION" -> Ok Auth
      //| "CREDIT_AUTHORIZATION" -> Ok CreditAuth
      | "FINANCIAL_AUTHORIZATION" -> Ok FinancialAuth
      //| "FINANCIAL_CREDIT_AUTHORIZATION" -> Ok FinancialCreditAuth
      //| "BALANCE_INQUIRY" -> Ok BalanceInquiry
      | _ -> Error "Invalid ASA action"

type AuthStreamAccessWebhookRequest = {
   Action: AuthorizationStreamAction
   CardIssuerTransactionId: CardIssuerTransactionId
   CardIssuerCardId: CardIssuerCardId
   Amount: decimal
   MerchantCategoryCode: int
   MerchantName: string
   CurrencyCardHolder: Currency
   CurrencyMerchant: Currency
}

type MerchantDTO = {
   /// Unique alphanumeric identifier for the payment card acceptor (merchant)
   acceptor_id: string
   /// Unique numeric identifier of the acquiring institution
   acquiring_institution_id: string
   /// In many cases, particularly in card-not-present transactions, merchants
   /// may send through a phone number or URL in this field.
   city: string
   /// ISO 3166-1 alpha-3 country codes (Ex: THA for Thailand)
   country: string
   /// Ex: Di BOSCO Coffee Specialist
   descriptor: string
   /// Classifies a business by the types of goods or services it provides.
   /// 4 digit number listed in ISO 18245
   mcc: string
   /// Ex: CA for California
   state: string
}

type AuthStreamAccessWebhookRequestDTO = {
   token: Guid
   status: string
   card: {| token: Guid |}
   merchant: MerchantDTO
   /// Amount merchant will receive, denominated in merchant_currency
   /// and in the smallest currency unit.  Includes acquirer fee.
   /// Value may be different from amount field below if merchant
   /// is taking payment in a different currency.
   merchant_amount: int64
   /// 3-character ISO 4217 code (Ex: USD)
   /// TODO: Application domain is currently not handling currency
   ///       differences.  Database is not considering currency either
   ///       so everything is currently stored assuming transactions occur in USD.
   merchant_currency: string
   /// 3-character ISO 4217 code (Ex: USD) for cardholder's billing currency
   cardholder_currency: string
   // Authorization amount of txn (in cents), including any
   // acquirer fees and cash back.
   // TODO: Research whether cash_amount and acquirer_fee
   // could be interesting info to store.  There is currently
   // no application domain concept representing any other amount
   // field other than the total amount represented in the amount
   // field.
   amount: int64
   // Portion of the txn requested as cash back.
   cash_amount: int64
   // Fee (in cents) assessed by the merchant & paid for by the
   // holder.  Rebates may be transmitted as a negative value to indicate
   // credited fees.
   acquirer_fee: int64
   // Card network of the authorization (AMEX, VISA, etc.)
   network: string
   transaction_initiator: string
} with

   member x.AsEntity = result {
      let! action = AuthorizationStreamAction.fromString x.status
      let! cardholderCurrency = Currency.create x.cardholder_currency
      let! merchantCurrency = Currency.create x.merchant_currency

      return {
         Action = action
         CardIssuerTransactionId = CardIssuerTransactionId x.token
         Amount = decimal x.amount / 100m
         CardIssuerCardId = CardIssuerCardId x.card.token
         MerchantCategoryCode = int x.merchant.mcc
         MerchantName = x.merchant.descriptor
         CurrencyCardHolder = cardholderCurrency
         CurrencyMerchant = merchantCurrency
      }
   }

[<RequireQualifiedAccess>]
type PurchaseAuthorizationStatus =
   | Approved
   | AccountInactive
   | AVSInvalid
   | CardPaused
   | InsufficientFunds
   | UnauthorizedMerchant
   | VelocityExceeded
   | DriverNumberInvalid
   | VehicleNumberInvalid
   | Challenge

   override x.ToString() =
      match x with
      | Approved -> "APPROVED"
      | AccountInactive -> "ACCOUNT_INACTIVE"
      | AVSInvalid -> "AVS_INVALID"
      | CardPaused -> "CARD_PAUSED"
      | InsufficientFunds -> "INSUFFICIENT_FUNDS"
      | UnauthorizedMerchant -> "UNAUTHORIZED_MERCHANT"
      | VelocityExceeded -> "VELOCITY_EXCEEDED"
      | DriverNumberInvalid -> "DRIVER_NUMBER_INVALID"
      | VehicleNumberInvalid -> "VEHICLE_NUMBER_INVALID"
      | Challenge -> "CHALLENGE"

   static member fromAccountFailReason =
      function
      | PurchaseAccountFailReason.AccountNotActive _ -> AccountInactive
      | PurchaseAccountFailReason.InsufficientAccountFunds _ ->
         InsufficientFunds

   static member fromCardFailReason =
      function
      | PurchaseCardFailReason.CardNotFound -> CardPaused
      | PurchaseCardFailReason.CardExpired -> CardPaused
      | PurchaseCardFailReason.CardLocked -> CardPaused
      | PurchaseCardFailReason.ExceededDailyCardLimit _ -> VelocityExceeded
      | PurchaseCardFailReason.ExceededMonthlyCardLimit _ -> VelocityExceeded

type AuthStreamAccessWebhookResponseDTO = { token: Guid; result: string }

type AuthStreamAccessWebhookResponse = {
   TransactionId: CardIssuerTransactionId
   Result: PurchaseAuthorizationStatus
} with

   member x.AsDTO: AuthStreamAccessWebhookResponseDTO = {
      token = x.TransactionId.Value
      result = string x.Result
   }

type TransactionAmountDTO = { amount: decimal; currency: string }

type TransactionCardHolderAmountDTO = {
   conversion_rate: decimal
   amount: decimal
   currency: string
}

type TransactionAmountsDTO = {
   hold: TransactionAmountDTO
   cardholder: TransactionCardHolderAmountDTO
   merchant: TransactionAmountDTO
   settlement: TransactionAmountDTO
}

type RuleResultDTO = {
   // The Auth Rule Token associated with the rule from which the decline originated.
   // If this is set to null, then the decline was not associated with a customer-configured Auth Rule.
   // This may happen in cases where a transaction is declined due to a Lithic-configured security or compliance rule, for example.
   auth_rule_token: Guid
   (*
    * The detailed_result associated with this rule's decline.
    *
    * ACCOUNT_DAILY_SPEND_LIMIT_EXCEEDED
    * ACCOUNT_DELINQUENT
    * ACCOUNT_INACTIVE
    * ACCOUNT_LIFETIME_SPEND_LIMIT_EXCEEDED
    * ACCOUNT_MONTHLY_SPEND_LIMIT_EXCEEDED
    * ACCOUNT_PAUSED
    * ACCOUNT_UNDER_REVIEW
    * ADDRESS_INCORRECT
    * APPROVED
    * AUTH_RULE_ALLOWED_COUNTRY
    * AUTH_RULE_ALLOWED_MCC
    * AUTH_RULE_BLOCKED_COUNTRY
    * AUTH_RULE_BLOCKED_MCC
    * CARD_CLOSED
    * CARD_CRYPTOGRAM_VALIDATION_FAILURE
    * CARD_EXPIRED
    * CARD_EXPIRY_DATE_INCORRECT
    * CARD_INVALID
    * CARD_NOT_ACTIVATED
    * CARD_PAUSED
    * CARD_PIN_INCORRECT
    * CARD_RESTRICTED
    * CARD_SECURITY_CODE_INCORRECT
    * CARD_SPEND_LIMIT_EXCEEDED
    * CONTACT_CARD_ISSUER
    * CUSTOMER_ASA_TIMEOUT
    * CUSTOM_ASA_RESULT
    * DECLINED
    * DO_NOT_HONOR
    * DRIVER_NUMBER_INVALID
    * FORMAT_ERROR
    * INSUFFICIENT_FUNDING_SOURCE_BALANCE
    * INSUFFICIENT_FUNDS
    * LITHIC_SYSTEM_ERROR
    * LITHIC_SYSTEM_RATE_LIMIT
    * MALFORMED_ASA_RESPONSE
    * MERCHANT_INVALID
    * MERCHANT_LOCKED_CARD_ATTEMPTED_ELSEWHERE
    * MERCHANT_NOT_PERMITTED
    * OVER_REVERSAL_ATTEMPTED
    * PIN_BLOCKED
    * PROGRAM_CARD_SPEND_LIMIT_EXCEEDED
    * PROGRAM_SUSPENDED
    * PROGRAM_USAGE_RESTRICTION
    * REVERSAL_UNMATCHED
    * SECURITY_VIOLATION
    * SINGLE_USE_CARD_REATTEMPTED
    * TRANSACTION_INVALID
    * TRANSACTION_NOT_PERMITTED_TO_ACQUIRER_OR_TERMINAL
    * TRANSACTION_NOT_PERMITTED_TO_ISSUER_OR_CARDHOLDER
    * TRANSACTION_PREVIOUSLY_COMPLETED
    * UNAUTHORIZED_MERCHANT
    * VEHICLE_NUMBER_INVALID
    *)
   result: string
   // The name of the rule, if any was configured.
   name: string
   // A human-readable explanation outlining the motivation for the rule's decline.
   explanation: string
}

type CardTransactionEventDTO = {
   ``type``: string
   amount: decimal
   //amounts: TransactionAmountsDTO
   effective_polarity: string
   rule_results: RuleResultDTO[]
   token: Guid
   created: DateTime
} with

   member x.AsEntity = result {
      let! evtType =
         match x.``type`` with
         | "AUTHORIZATION" -> Ok PurchaseEventType.Auth
         | "AUTHORIZATION_ADVICE" -> Ok PurchaseEventType.AuthAdvice
         | "AUTHORIZATION_EXPIRY" -> Ok PurchaseEventType.AuthExpiry
         | "AUTHORIZATION_REVERSAL" -> Ok PurchaseEventType.AuthReversal
         //| "BALANCE_INQUIRY" -> Ok PurchaseEventType.BalanceInquiry
         | "CLEARING" -> Ok PurchaseEventType.Clearing
         //| "CORRECTION_CREDIT" -> Ok PurchaseEventType.CorrectionCredit
         //| "CORRECTION_DEBIT" -> Ok PurchaseEventType.CorrectionDebit
         //| "CREDIT_AUTHORIZATION" -> Ok PurchaseEventType.CreditAuth
         //| "CREDIT_AUTHORIZATION_ADVICE" ->
         //   Ok PurchaseEventType.CreditAuthAdvice
         | "FINANCIAL_AUTHORIZATION" -> Ok PurchaseEventType.FinancialAuth
         //| "FINANCIAL_CREDIT_AUTHORIZATION" ->
         //   Ok PurchaseEventType.FinancialCreditAuth
         | "RETURN" -> Ok PurchaseEventType.Return
         | "RETURN_REVERSAL" -> Ok PurchaseEventType.ReturnReversal
         | _ -> Error "Unknown Purchase Event Type"

      let! flow =
         match x.effective_polarity with
         | "DEBIT" -> Ok MoneyFlow.Out
         | "CREDIT" -> Ok MoneyFlow.In
         | _ -> Error "Unknown Purchase Polarity"

      return {
         Type = evtType
         Amount = x.amount / 100m
         Flow = flow
         EnforcedRules = []
         EventId = x.token
         CreatedAt = x.created
      }
   }

type CardTransactionDTO = {
   amounts: TransactionAmountsDTO
   acquirer_fee: decimal
   events: CardTransactionEventDTO list
   merchant: MerchantDTO
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
   result: string
   status: string
   card_token: Guid
   token: Guid
   created: DateTime
} with

   member x.AsEntity = result {
      let! currencyHold = Currency.create x.amounts.hold.currency
      let! currencyMerchant = Currency.create x.amounts.merchant.currency
      let! currencyCardholder = Currency.create x.amounts.cardholder.currency
      let! currencySettlement = Currency.create x.amounts.settlement.currency

      let! status =
         match x.status with
         | "DECLINED" -> Ok PurchaseStatus.Declined
         | "EXPIRED" -> Ok PurchaseStatus.Expired
         | "PENDING" -> Ok PurchaseStatus.Pending
         | "SETTLED" -> Ok PurchaseStatus.Settled
         | "VOIDED" -> Ok PurchaseStatus.Voided
         | _ -> Error "Invalid Purchase Status"

      let! events = x.events |> List.traverseResultM _.AsEntity

      let formattedAmount = abs >> fun num -> num / 100m

      return {
         Result = x.result
         Status = status
         Events = events
         PurchaseId = CardIssuerTransactionId x.token
         CardIssuerCardId = CardIssuerCardId x.card_token
         Amounts = {
            Hold = {
               Amount = formattedAmount x.amounts.hold.amount
               Currency = currencyHold
            }
            Cardholder = {
               Amount = formattedAmount x.amounts.cardholder.amount
               Currency = currencyCardholder
               ConversionRate = x.amounts.cardholder.conversion_rate
            }
            Merchant = {
               Amount = formattedAmount x.amounts.merchant.amount
               Currency = currencyMerchant
            }
            Settlement = {
               Amount = formattedAmount x.amounts.settlement.amount
               Currency = currencySettlement
            }
         }
      }
   }

[<RequireQualifiedAccess>]
type CardIssuerMessage =
   | CreateCard of CardIssuerCreateCardRequest
   | CloseCard of CardIssuerCloseCardRequest

   member x.Metadata =
      match x with
      | CreateCard req -> req.Metadata
      | CloseCard req -> req.Metadata

type CardCloseResponse = {
   Customer: obj
   CardIssuerCardId: CardIssuerCardId
}

[<RequireQualifiedAccess>]
type CardIssuerResponse =
   | CreateCard of CardCreateResponse
   | CloseCard of CardCloseResponse
