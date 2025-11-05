namespace Bank.Purchase.Domain

open System

open Lib.SharedTypes

(*
 * NOTE:
 * Not currently handling credit related auths
 * (Credit Authorization, Financial Credit Authorization, Credit Authorization Advice)
 *
 * ----- Message Types Originating wih an ASA Request ------
 *
 *
 * There are 4 possible message types that can occur after the initial auth:
 * CLEARING, AUTHORIZATION_REVERSAL, AUTHORIZATION_EXPIRY and AUTHORIZATION_ADVICE.
 *
 * Typical Scenarios:
 * 1. Transaction cleared for same amount as was authorized
 *    - Most common scenario
 *    - Auth $20 -> Clearing $20
 * 2. Amount is updated to a higher amount than what we authorized
 *    - Auth $1 -> AuthAdvice (Approve $15) -> Clearing $15
 *    - An initial auth is sent before the gas is pumped & updated to the
 *      true amount when finished pumping gas
 * 3. Amount is updated to a lower amount than what we authorized
 *    - Typically occurs as part of a "multiple completion" in which a merchant
 *      fulfills a transaction multiple times (merchant ships 2 packages
 *      separately from a single online order & sends a clearing for each
 *      package)
 *    - Auth $20 -> AuthAdvice (Approve $19.50) -> Clearing $17.50 -> Clearing $2.00
 *
 * NOTE: Only a single Clearing, AuthReversal, or AuthExpiry is expected to
 *       follow an AuthAdvice
 *
 * NOTE: In the 3 above cases, the status will be SETTLED.  
 *       Additional Clearing, AuthReversal, and AuthExpiry messages may still
 *       occur on a SETTLED transaction.
 *
 * 4. Merchant cancels transaction before it is settled
 *    - Auth $25 -> AuthReversal $25 -> (Status: VOIDED)
 * 5. Merchant reverses a portion of the authorized amount
 *    - Auth $25 -> AuthReversal $10 -> (Status: PENDING) (amounts.hold.amount = -$15.00)
 *    - Transaction will remain PENDING until another message arrives to either
 *      clear or reverse the remaining amount.
 * 6. Merchant reverses a greater amount than the authorized amount
 *    - Happens when:
 *      1. Foreign exchange rate has changed since initial auth
 *      2. Merchant makes an error & enters an incorrect reversal amount
 *         (rare occurrence as the networks have checks to prevent over-reversals)
 *    - If there is an over-reversal, Lithic will cap the amounts.hold.amount to $0
 *    - Auth $25 -> AuthReversal $27 -> (Status: DECLINED)
 * 
 * NOTE: 
 *     Additional Clearing, AuthReversal, and AuthExpiry events can
 *     occur after an Authorization Reversal event.
 *
 * 7. Authorization validity window passes
 *   - Each auth has an auth validity window set by the card network that
 *     indicates how long the merchant has to clear (or settle) the txn.
 *   - Once this window passes, holds placed on cardholder funds should be lifted
 *   - Typically, auths that expire DO SO IN FULL, meaning no clearing occurred
 *   - Auth $10 -> AuthExpiry $10 -> (Status: EXPIRED)
 *   - Once Lithic expires an auth, additional events may still occur, such as a
 *     merchant sending an AuthReversal. Sometimes a merchant may clear a txn
 *     after Lithic has expired the auth.  Lithic cannot reject this clearing
 *     event because the rules Lithic follows to expire an auth are independent
 *     of the merchant's own timing with clearing a txn.
 * 
 * 8. AuthAdvice (Decline) can occur when networks "stand-in" to respond to an
 *    auth request on behalf of Lithic & its customers.  Will occur when
 *    Lithic does not respond to the auth request in time (time-out)
 *   - Auth $10 -> AuthAdvice (Decline $10) -> (Status: DECLINED)
 *   - No further messages are expected from Lithic after an AuthAdvice (Decline)
 *
 * ----------- Multiple Completion ------------------

 * 9. Once a transaction has been partially cleared, there can be any permutation of 
 *    Clearing, Return, AuthReversal, and AuthExpiry messages on this same transaction.
 *  - Auth $10 -> Clearing $8 -> AuthReversal $1 -> Clearing $1
 *    -> (Status: SETTLED) (amounts.settlement.amount = $9)
 *
 * 10. A typical auth validity window is 7 days from the initial auth but can be
 *     longer for some types of txns (auto rentals, hotel reservations)
 *   - If a txn pending amount exceeds its settled amount in magnitude 
 *     (amounts.hold.amount > amounts.settlement.amount) when its auth validity
 *     window passes, Lithic will initiate an AuthExpiry message to release the
 *     remaining authorized amount.
 *   _ Sometimes, the merchant may explicitly signal to Lithic that it will not
 *     send any further clearings on that txn, so Lithic will expire the
 *     remaining auth amount that same day.
 *   - Auth $10 -> Clearing $7 -> Clearing $1 -> AuthExpiry $2 
 *     -> (Status: SETTLED) (amounts.settlement.amount = $8)
 *
 * 11. Once a txn has been partially or fully cleared, a return on a purchase or
 *     a return reversal on a return, may occur in the same txn or in a new txn.
 *   - Ex: if the above txn is refunded the user may see a subsequent standalone
 *         txn like so:
 *     Return $8 -> (Status: SETTLED) (settled amount will be positive instead of negative)
 *
 * 12. For txns in foreign currencies, it is possible that a full return in the
 *     local currency results in a Return amount that differs from the
 *     clearing amount in the settlement currency.
 *   - A transaction in France clears for €10 in the local currency, Euro. 
 *     The foreign exchange rate of Euro to USD is 0.80. 
 *     This results in a Clearing message for $8 in the settlement currency, USD.
 *   - A week later, the transaction is refunded for the full €10 in the local currency, Euro. 
 *     However, the foreign exchange rate from Euro to USD has since changed to 1.
 *     This results in a Return message for $10 in the settlement currency of USD.
 *     The Return message materializes in a new transaction.
 *   - Accounting for both transactions, the cardholder receives a net credit of $2 due
 *     to the fluctuation of the foreign exchange rate.
 *
 * --------------- Financial Authorization ------------------
 * 13. Financial Authorization requests are single-event txns with no subsequent
 *     clearings. These are considered SMS requests, wherein auth & clearing are
 *     combined into a single message.
 *   - If the auth is declined then we will receive a single event
 *     { type: "FINANCIAL_AUTHORIZATION", "result": "DECLINED" } and no more
 *     events will be received.
 *   - If the auth is approved then we will receive a single event
 *     { type: "FINANCIAL_AUTHORIZATION", "result": "APPROVED" }.
 *     At this point, the txn is considered settled & no further Clearing
 *     messages are expected.
 * 14. A financial auth can be returned.  It will be either associated with the
 *     original txn (and appear as a new event under the original auth) or
 *     materialize in a new transaction.
 *   - FinancialAuth $10 (Approved) -> Return $10 -> (Status: VOIDED) (amounts.settlement.amount = 0)
 *   - No additional messages will be received after a return.
 *
 * 15. As with regular Auth, FinancialAuth may result in a time-out if Lithic
 *     does not respond to the card networks in time.  The networks will
 *     stand-in to decline a FinancialAuth by emitting a Clearing message with
 *     status of DECLINED.
 *   - FinancialAuth $10 (Approved) -> Clearing $10 (Declined)
 *   - No further messages are expected from Lithic after this clearing
 *
 * -------- Message Types Originating with a Transaction Webhook --------
 *
 * 16. Authorization
 *   - If the user receives an Auth message at the txn webhook endpoint without
 *     a prior ASA request, this signifies that there was a decline due to a
 *     check performed by Lithic before the ASA request was sent.
 *     Ex: Txn attempted on paused card, spend limit exceeded, auth rule triggered
 *     { status: "DECLINED", events: [{ type: "AUTHORIZATION", result: "CARD_PAUSED" }] }
 *   - No further messages related to the txn are expected
 *   - Auth messages signifying approvals should never be received without a
 *     prior ASA request.
 *
 * 17. Clearing
 *   - When a Clearing message occurs but there is no previously authorized txn,
 *     funds will immediately be designated as settled.
 *   - This "Force Post" poses some risk to card programs in that they allow a
 *     txn to settle for an amount above the cardholder's balance, without
 *     affording the user an opportunity to decline.
 *   - Force posts can be indicative of bad merchant behavior and, when the user's
 *     balance is exceeded, may be subject to chargeback.
 *   - No messages are expected from Lithic after a force post
 *
 * 18. Return
 *   - When a return is initiated on a settled txn, the return oftentimes cannot
 *     be mapped to the original txn so will appear in a standalone txn.
 *   - It is possible to build a system to match these returns to the original transaction 
 *     using a combination of transaction amount and merchant information, but such systems 
 *     can be unreliable and risk over-crediting the user.
 *   - { status: "SETTLED", events: [{ type: "RETURN", result: "APPROVED" }] }
 *   - In some cases, Lithic may mark a return txn as so:
 *     { status: "DECLINED", events: [{ type: "RETURN", result: "ORIGINAL_NOT_FOUND" }] }
 *     This typically results from a prior financial auth being missed, so YOU
 *     SHOULD IGNORE the return event to avoid over-crediting the cardholder.
 *
 * 19. Authorization Advice
 *   - 2 known cases where this can occur:
 *     1. Card is used at an offline POS system (buying food on a plane)
 *     2. Merchant sends an adjustment due to a POS error; while some merchants
 *        will send this through as a Force Post (see above remarks on Force
 *        Post), others will send this as an AuthAdvice and follow up with a Clearing.
 *   - Because the issuer does not get to decision on any authorization, these
 *     txns may be subject to chargeback.
 *   - Users can expect the same subsequent events that they would for txns
 *     beginning with Auth followed by AuthAdvice.
 *   - AuthAdvice $10 (Approved) -> (Status: PENDING) (amounts.hold.amount: $10)
 *
 *   - Txns can also start with a declined AuthAdvice, which typically occurs
 *     when a party upstream of Lithic - either acquirer or card network - has
 *     already declined the auth.
 *     { type: AuthAdvice, result: "INCORRECT_PIN" } ->  (Status: DECLINED)
 *
 * 20. Authorization Reversal
 *   - Usually takes place when a cardholder initiates a return on a settled
 *     purchase, & the merchant attempts to reverse the auth (as they would with
 *     an outstanding, uncleared auth) instead of initiating a standard
 *     credit authorization flow.  The merchant may do this by accident or
 *     because it is unaware that its acquirer has already cleared the auth.
 *   - Lithic is unable to locate an outstanding auth tied to the card on
 *     which a refund is taking place, so it will not link the auth reversal
 *     event to the original txn, & will instead surface the event in its own
 *     txn with a declined status.
 *   - This webhook will serve as a notification that this event has taken place
 *     on your card program, but no action is needed as no money movement has
 *     transpired.
 *   - { type: "AUTHORIZATION_REVERSAL", result: "ORIGINAL_NOT_FOUND" } -> (status: "DECLINED")
 *   - The typical next step to ensure that the cardholder will receive the credit is that the
 *     merchant and acquirer will send the refund through as a credit authorization,
 *     financial credit authorization, or force posted return. Credit should not be extended
 *     to the end cardholder until one of the above sequences occur and the transaction is in
 *     a SETTLED status (as indication that money has moved).
 *)
[<RequireQualifiedAccess>]
type PurchaseEventType =
   | Auth
   | AuthAdvice
   | AuthExpiry
   | AuthReversal
   //| BalanceInquiry
   | Clearing
   //| CorrectionCredit
   //| CorrectionDebit
   //| CreditAuth
   //| CreditAuthAdvice
   | FinancialAuth
   //| FinancialCreditAuth
   | Return
   | ReturnReversal

type PurchaseRuleEnforced = {
   Id: Guid
   Result: string
   Name: string
   Explanation: string
}

type PurchaseEvent = {
   Type: PurchaseEventType
   Money: Money
   //amounts: TransactionAmountsDTO
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
} with

   static member Empty =
      let o = { Amount = 0m; Currency = Currency.USD }

      {
         Hold = o
         Merchant = o
         Settlement = o
         Cardholder = {
            Amount = o.Amount
            Currency = o.Currency
            ConversionRate = 1m
         }
      }

type CardIssuerPurchaseProgress = {
   Amounts: PurchaseAmounts
   Events: PurchaseEvent NonEmptyList
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
   MerchantName: NonEmptyString
} with

   member x.InitiatedViaSMSAuth =
      x.Events
      |> NonEmptyList.toList
      |> List.exists (fun e ->
         match e.Type with
         | PurchaseEventType.FinancialAuth -> true
         | _ -> false)

   member x.IsSMSApproval = x.InitiatedViaSMSAuth && x.Result = "APPROVED"

   member x.IsSMSDecline = x.InitiatedViaSMSAuth && x.Result = "DECLINED"

   member x.EventsTimeOrdered = x.Events |> NonEmptyList.sortBy _.CreatedAt

   member x.OriginatingEvent = NonEmptyList.head x.EventsTimeOrdered
