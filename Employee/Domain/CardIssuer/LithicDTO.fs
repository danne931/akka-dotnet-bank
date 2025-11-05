namespace CardIssuer.Service.Domain

open System
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Bank.Employee.Domain
open Bank.Purchase.Domain

module private Util =
   let formattedTxnAmount num = abs num / 100m

module LithicCardCreateRequestDTO =
   let fromEntity (req: CardIssuerCreateCardRequest) : obj =
      let dto = {|
         ``type`` = "VIRTUAL"
         exp_year = string req.Expiration.Year
         exp_month = sprintf "%02i" req.Expiration.Month
      |}

      match req.CardNickname with
      | None -> dto
      | Some name -> {| dto with memo = name |}

type LithicCardCreateResponseDTO = {
   token: Guid
   state: string
   ``type``: string
   last_four: string
   exp_month: string
   exp_year: string
} with

   member x.AsEntity: CardCreateResponse = {
      CardIssuerCardId = CardIssuerCardId x.token
      CardNumberLast4 = x.last_four
      CardIssuerName = CardIssuerName.Lithic
   }

module LithicSimulatePurchaseRequestDTO =
   let fromEntity (x: SimulatePurchaseRequest) =
      let amount = Math.Round(x.Amount, 2) * 100m |> int

      {|
         amount = amount
         descriptor = x.Descriptor.Value
         pan = x.CardNumber
         merchant_currency = string x.MerchantCurrency
         merchant_amount = amount
      |}

type LithicSimulatePurchaseResponseDTO = {
   token: Guid
} with

   member x.AsEntity: SimulatePurchaseResponse = {
      CardIssuerTransactionId = CardIssuerTransactionId x.token
   }

type LithicCardGetResponseDTO = {
   token: Guid
   state: string
   ``type``: string
   last_four: string
   exp_month: string
   exp_year: string
   pan: string
} with

   member x.AsEntity: CardGetResponse = {
      CardIssuerCardId = CardIssuerCardId x.token
      NumberLast4 = x.last_four
      Number = x.pan
      Expiration = {
         Month = int x.exp_month
         Year = int x.exp_year
      }
   }

type LithicMerchantDTO = {
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

type LithicAuthStreamAccessWebhookRequestDTO = {
   token: Guid
   status: string
   card: {| token: Guid |}
   merchant: LithicMerchantDTO
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

   member x.AsEntity: Result<AuthStreamAccessWebhookRequest, string> = result {
      let! action = x.ActionParsed
      let! cardholderCurrency = Currency.create x.cardholder_currency
      let! merchantCurrency = Currency.create x.merchant_currency
      let! merchantName = NonEmptyString.create x.merchant.descriptor

      return {
         Action = action
         CardIssuerTransactionId = CardIssuerTransactionId x.token
         Amount = decimal x.amount / 100m
         CardIssuerCardId = CardIssuerCardId x.card.token
         MerchantCategoryCode = int x.merchant.mcc
         MerchantName = merchantName
         CurrencyCardHolder = cardholderCurrency
         CurrencyMerchant = merchantCurrency
      }
   }

   member private x.ActionParsed =
      match x.status with
      | "AUTHORIZATION" -> Ok AuthorizationStreamAction.Auth
      //| "CREDIT_AUTHORIZATION" -> Ok CreditAuth
      | "FINANCIAL_AUTHORIZATION" -> Ok AuthorizationStreamAction.FinancialAuth
      //| "FINANCIAL_CREDIT_AUTHORIZATION" -> Ok FinancialCreditAuth
      //| "BALANCE_INQUIRY" -> Ok BalanceInquiry
      | _ -> Error "Invalid ASA action"

type LithicAuthStreamAccessWebhookResponseDTO = {
   token: Guid
   result: string
} with

   static member fromEntity(entity: AuthStreamAccessWebhookResponse) =
      let result =
         match entity.Result with
         | PurchaseAuthorizationStatus.Approved -> "APPROVED"
         | PurchaseAuthorizationStatus.AccountInactive -> "ACCOUNT_INACTIVE"
         | PurchaseAuthorizationStatus.AVSInvalid -> "AVS_INVALID"
         | PurchaseAuthorizationStatus.CardPaused -> "CARD_PAUSED"
         | PurchaseAuthorizationStatus.InsufficientFunds -> "INSUFFICIENT_FUNDS"
         | PurchaseAuthorizationStatus.UnauthorizedMerchant ->
            "UNAUTHORIZED_MERCHANT"
         | PurchaseAuthorizationStatus.VelocityExceeded -> "VELOCITY_EXCEEDED"
         | PurchaseAuthorizationStatus.DriverNumberInvalid ->
            "DRIVER_NUMBER_INVALID"
         | PurchaseAuthorizationStatus.VehicleNumberInvalid ->
            "VEHICLE_NUMBER_INVALID"
         | PurchaseAuthorizationStatus.Challenge -> "CHALLENGE"

      {
         token = entity.TransactionId.Value
         result = result
      }

type LithicTransactionAmountDTO = { amount: decimal; currency: string }

type LithicTransactionCardHolderAmountDTO = {
   conversion_rate: decimal
   amount: decimal
   currency: string
}

type LithicTransactionAmountsDTO = {
   hold: LithicTransactionAmountDTO
   cardholder: LithicTransactionCardHolderAmountDTO
   merchant: LithicTransactionAmountDTO
   settlement: LithicTransactionAmountDTO
}

type LithicRuleResultDTO = {
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

type LithicCardTransactionEventDTO = {
   ``type``: string
   amount: decimal
   //amounts: TransactionAmountsDTO
   effective_polarity: string
   rule_results: LithicRuleResultDTO[]
   token: Guid
   created: DateTime
} with

   member x.AsEntity: Result<PurchaseEvent, string> = result {
      let! evtType = x.EventTypeParsed
      let! flow = x.MoneyFlowParsed

      return {
         Type = evtType
         Money = {
            Amount = Util.formattedTxnAmount x.amount
            Flow = flow
         }
         EnforcedRules = []
         EventId = x.token
         CreatedAt = x.created
      }
   }

   member private x.EventTypeParsed =
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

   member private x.MoneyFlowParsed =
      match x.effective_polarity with
      | "DEBIT" -> Ok MoneyFlow.Out
      | "CREDIT" -> Ok MoneyFlow.In
      | _ -> Error "Unknown Purchase Polarity"

type LithicCardTransactionDTO = {
   amounts: LithicTransactionAmountsDTO
   acquirer_fee: decimal
   events: LithicCardTransactionEventDTO list
   merchant: LithicMerchantDTO
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

   member x.AsEntity: Result<CardIssuerPurchaseProgress, string> = result {
      let! amounts = x.AmountsParsed
      let! status = x.StatusParsed
      let! merchantName = NonEmptyString.create x.merchant.descriptor

      let! events =
         x.events
         |> List.traverseResultM _.AsEntity
         |> Result.bind NonEmptyList.fromList

      return {
         Result = x.result
         Status = status
         Events = events
         PurchaseId = CardIssuerTransactionId x.token
         CardIssuerCardId = CardIssuerCardId x.card_token
         Amounts = amounts
         MerchantName = merchantName
      }
   }

   member private x.StatusParsed =
      match x.status with
      | "DECLINED" -> Ok PurchaseStatus.Declined
      | "EXPIRED" -> Ok PurchaseStatus.Expired
      | "PENDING" -> Ok PurchaseStatus.Pending
      | "SETTLED" -> Ok PurchaseStatus.Settled
      | "VOIDED" -> Ok PurchaseStatus.Voided
      | _ -> Error "Invalid Purchase Status"

   member private x.AmountsParsed = result {
      let! currencyHold = Currency.create x.amounts.hold.currency
      let! currencyMerchant = Currency.create x.amounts.merchant.currency
      let! currencyCardholder = Currency.create x.amounts.cardholder.currency
      let! currencySettlement = Currency.create x.amounts.settlement.currency

      return {
         Hold = {
            Amount = Util.formattedTxnAmount x.amounts.hold.amount
            Currency = currencyHold
         }
         Cardholder = {
            Amount = Util.formattedTxnAmount x.amounts.cardholder.amount
            Currency = currencyCardholder
            ConversionRate = x.amounts.cardholder.conversion_rate
         }
         Merchant = {
            Amount = Util.formattedTxnAmount x.amounts.merchant.amount
            Currency = currencyMerchant
         }
         Settlement = {
            Amount = Util.formattedTxnAmount x.amounts.settlement.amount
            Currency = currencySettlement
         }
      }
   }
