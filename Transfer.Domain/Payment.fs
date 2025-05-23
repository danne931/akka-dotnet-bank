namespace Bank.Transfer.Domain

open System

open Lib.SharedTypes

type PaymentId =
   | PaymentId of Guid

   override x.ToString() =
      let (PaymentId id) = x
      string id

module PaymentId =
   let get (payId: PaymentId) =
      let (PaymentId id) = payId
      id

   let toCorrelationId (PaymentId payId) = CorrelationId payId

[<RequireQualifiedAccess>]
type PaymentType =
   | Platform
   | ThirdParty

module PaymentType =
   let fromString (str: string) : PaymentType option =
      match str.ToLower() with
      | "platform" -> Some PaymentType.Platform
      | "thirdparty" -> Some PaymentType.ThirdParty
      | _ -> None

   let fromStringUnsafe str : PaymentType =
      match fromString str with
      | Some s -> s
      | None -> failwith "Error attempting to cast string to PaymentType"

[<RequireQualifiedAccess>]
type PlatformPaymentFailReason =
   | AccountClosed
   | PartnerBankSync of string

[<RequireQualifiedAccess>]
type PlatformPaymentRefundReason = PaymentFailed of PlatformPaymentFailReason

[<RequireQualifiedAccess>]
type PlatformPaymentStatus =
   | Unpaid
   | Paid
   | Deposited
   | Cancelled
   | Declined
   | Failed of PlatformPaymentFailReason

[<RequireQualifiedAccess>]
type ThirdPartyPaymentStatus =
   | Unpaid
   | Deposited
   | Cancelled

module ThirdPartyPaymentStatus =
   let fromString (str: string) : ThirdPartyPaymentStatus option =
      match str.ToLower() with
      | "unpaid" -> Some ThirdPartyPaymentStatus.Unpaid
      | "deposited" -> Some ThirdPartyPaymentStatus.Deposited
      | "cancelled" -> Some ThirdPartyPaymentStatus.Cancelled
      | _ -> None

   let fromStringUnsafe str : ThirdPartyPaymentStatus =
      match fromString str with
      | Some s -> s
      | None ->
         failwith "Error attempting to cast string to ThirdPartyPaymentStatus"

type ThirdPartyPayer = { Name: string; Email: Email }

type PlatformPayer = {
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   OrgName: string
}

[<RequireQualifiedAccess>]
type Payer =
   // Payment is made from an org on the platform.
   | Platform of PlatformPayer
   // Payment may be made out via card or ACH for orgs which do not
   // have an account on the platform.  We must first reach out to the
   // prospective payer via email.
   | ThirdParty of ThirdPartyPayer

// The org which requested the payment and the account they
// wish the payment to be made out to.
type Payee = {
   OrgId: OrgId
   OrgName: string
   ParentAccountId: ParentAccountId
   AccountId: AccountId
}

// TODO:
// Research Plaid to see what data should be included in these payment types.
[<RequireQualifiedAccess>]
type ThirdPartyPaymentMethod =
   | ACH
   | Card

[<RequireQualifiedAccess>]
type PaymentMethod =
   | Platform of AccountId
   | ThirdParty of ThirdPartyPaymentMethod

type PaymentBaseInfo = {
   Id: PaymentId
   InitiatedBy: InitiatedById
   Amount: decimal
   Type: PaymentType
   Payee: Payee
   CreatedAt: DateTime
   Expiration: DateTime
   Memo: string
}

[<RequireQualifiedAccess>]
type PlatformPayment = {
   BaseInfo: PaymentBaseInfo
   Status: PlatformPaymentStatus
   Payer: PlatformPayer
   PaidBy: PaymentMethod option
}

[<RequireQualifiedAccess>]
type ThirdPartyPayment = {
   BaseInfo: PaymentBaseInfo
   Status: ThirdPartyPaymentStatus
   Payer: ThirdPartyPayer
   PaidBy: ThirdPartyPaymentMethod
}

[<RequireQualifiedAccess>]
type Payment =
   | Platform of PlatformPayment
   | ThirdParty of ThirdPartyPayment

module Payment =
   let baseInfo =
      function
      | Payment.Platform p -> p.BaseInfo
      | Payment.ThirdParty p -> p.BaseInfo

   let payer =
      function
      | Payment.Platform p -> p.Payer.OrgName
      | Payment.ThirdParty p -> p.Payer.Name

   let isExpired =
      function
      | Payment.Platform p -> p.BaseInfo.Expiration <= DateTime.UtcNow
      | Payment.ThirdParty p -> p.BaseInfo.Expiration <= DateTime.UtcNow

   let isUnpaid (payment: Payment) =
      match payment with
      | Payment.Platform p -> p.Status = PlatformPaymentStatus.Unpaid
      | Payment.ThirdParty p -> p.Status = ThirdPartyPaymentStatus.Unpaid

   let canManage (payment: Payment) =
      (not (isExpired payment)) && isUnpaid payment

   let displayPriority (payment: Payment) =
      match payment with
      | Payment.Platform p ->
         match p.Status with
         | PlatformPaymentStatus.Unpaid when isExpired payment -> 6
         | PlatformPaymentStatus.Deposited -> 5
         | PlatformPaymentStatus.Paid -> 4
         | PlatformPaymentStatus.Cancelled -> 3
         | PlatformPaymentStatus.Declined -> 3
         | PlatformPaymentStatus.Failed _ -> 2
         | PlatformPaymentStatus.Unpaid -> 1
      | Payment.ThirdParty p ->
         match p.Status with
         | ThirdPartyPaymentStatus.Unpaid when isExpired payment -> 4
         | ThirdPartyPaymentStatus.Deposited -> 3
         | ThirdPartyPaymentStatus.Cancelled -> 2
         | ThirdPartyPaymentStatus.Unpaid -> 1

   let statusDisplay =
      function
      | Payment.Platform p ->
         if
            p.Status = PlatformPaymentStatus.Unpaid
            && p.BaseInfo.Expiration < DateTime.UtcNow
         then
            "Expired"
         else
            string p.Status
      | Payment.ThirdParty p ->
         if
            p.Status = ThirdPartyPaymentStatus.Unpaid
            && p.BaseInfo.Expiration < DateTime.UtcNow
         then
            "Expired"
         else
            string p.Status

type PaymentSummary = {
   // Payment requests to orgs on the platform or outside the platform.
   OutgoingRequests: Payment list
   // Payment request from orgs on the platform.
   IncomingRequests: PlatformPayment list
}
