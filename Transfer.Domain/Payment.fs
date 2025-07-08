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
type PaymentRequestType =
   | Platform
   | ThirdParty

module PaymentRequestType =
   let fromString (str: string) : PaymentRequestType option =
      match str.ToLower() with
      | "platform" -> Some PaymentRequestType.Platform
      | "thirdparty" -> Some PaymentRequestType.ThirdParty
      | _ -> None

   let fromStringUnsafe str : PaymentRequestType =
      match fromString str with
      | Some s -> s
      | None -> failwith "Error attempting to cast string to PaymentRequestType"

[<RequireQualifiedAccess>]
type PlatformPaymentFailReason =
   | AccountClosed
   | InsufficientFunds

type PaymentFulfilled = {
   TransferId: TransferId
   FulfilledAt: DateTime
}

[<RequireQualifiedAccess>]
type PaymentRequestStatus =
   | Requested
   | Fulfilled of PaymentFulfilled
   | Cancelled
   | Declined
   | Failed of PlatformPaymentFailReason

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

type PaymentBaseInfo = {
   Id: PaymentId
   InitiatedBy: InitiatedById
   Amount: decimal
   Type: PaymentRequestType
   Payee: Payee
   Status: PaymentRequestStatus
   CreatedAt: DateTime
   Expiration: DateTime
   Memo: string
}

[<RequireQualifiedAccess>]
type PlatformPayment = {
   BaseInfo: PaymentBaseInfo
   Payer: PlatformPayer
}

[<RequireQualifiedAccess>]
type ThirdPartyPayment = {
   BaseInfo: PaymentBaseInfo
   Payer: ThirdPartyPayer
}

[<RequireQualifiedAccess>]
type Payment =
   | Platform of PlatformPayment
   | ThirdParty of ThirdPartyPayment

   member x.BaseInfo =
      match x with
      | Platform p -> p.BaseInfo
      | ThirdParty p -> p.BaseInfo

   member x.Status = x.BaseInfo.Status

   member x.Payer =
      match x with
      | Platform p -> p.Payer.OrgName
      | ThirdParty p -> p.Payer.Name

   member x.IsExpired =
      match x with
      | Payment.Platform p -> p.BaseInfo.Expiration <= DateTime.UtcNow
      | Payment.ThirdParty p -> p.BaseInfo.Expiration <= DateTime.UtcNow

   member x.IsUnpaid = x.Status = PaymentRequestStatus.Requested

   member x.CanManage = (not (x.IsExpired)) && x.IsUnpaid

   member x.DisplayPriority =
      match x.Status with
      | PaymentRequestStatus.Requested when x.IsExpired -> 6
      | PaymentRequestStatus.Fulfilled _ -> 5
      | PaymentRequestStatus.Cancelled -> 4
      | PaymentRequestStatus.Declined -> 3
      | PaymentRequestStatus.Failed _ -> 2
      | PaymentRequestStatus.Requested -> 1

   member x.StatusDisplay =
      match x.Status with
      | PaymentRequestStatus.Requested when x.IsExpired -> "Expired"
      | PaymentRequestStatus.Requested -> "Requested"
      | PaymentRequestStatus.Fulfilled _ -> "Fulfilled"
      | PaymentRequestStatus.Cancelled -> "Cancelled"
      | PaymentRequestStatus.Declined -> "Declined"
      | PaymentRequestStatus.Failed _ -> "Failed"

type PaymentSummary = {
   // Payment requests to orgs on the platform or outside the platform.
   OutgoingRequests: Payment list
   // Payment request from orgs on the platform.
   IncomingRequests: PlatformPayment list
}
