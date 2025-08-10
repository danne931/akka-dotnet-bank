namespace Bank.Payment.Domain

open System

open Lib.SharedTypes
open RecurringPaymentSchedule

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
type PaymentFailReason =
   | AccountClosed
   | InsufficientFunds

type PaymentFulfillment = {
   TransferId: TransferId
   FulfilledAt: DateTime
}

[<RequireQualifiedAccess>]
type PaymentRequestStatus =
   | Requested
   | Fulfilled of PaymentFulfillment
   | Cancelled
   | Declined
   | Failed of PaymentFailReason

// The org which requested the payment and the account they
// wish the payment to be made out to.
type Payee = {
   OrgId: OrgId
   OrgName: string
   ParentAccountId: ParentAccountId
   AccountId: AccountId
}

type PlatformPayer = {
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   OrgName: string
}

type ThirdPartyPayer = { Name: string; Email: Email }

[<RequireQualifiedAccess>]
type Payer =
   // Payment is made from an org on the platform.
   | Platform of PlatformPayer
   // Payment may be made out via card or ACH for orgs which do not
   // have an account on the platform.  We must first reach out to the
   // prospective payer via email.
   | ThirdParty of ThirdPartyPayer

type PaymentRequestSharedDetails = {
   Id: PaymentRequestId
   InitiatedBy: InitiatedById
   Amount: decimal
   Payee: Payee
   Memo: string
   DueAt: DateTime
   CreatedAt: DateTime
   Status: PaymentRequestStatus
   RecurrenceSettings: RecurrenceSettings option
   Invoice: Invoice option
}

type PlatformPaymentRequest = {
   SharedDetails: PaymentRequestSharedDetails
   Payer: PlatformPayer
}

module PlatformPaymentRequest =
   let toTransferCommand
      (initiator: Initiator)
      (payment: PlatformPaymentRequest)
      selectedAccountId
      =
      let shared = payment.SharedDetails
      let payer = payment.Payer
      let payee = shared.Payee

      Bank.Transfer.Domain.InternalTransferBetweenOrgsCommand.create initiator {
         ScheduledDateSeedOverride = None
         Amount = shared.Amount
         Sender = {
            OrgId = payer.OrgId
            ParentAccountId = payer.ParentAccountId
            AccountId = selectedAccountId
            Name = payer.OrgName
         }
         Recipient = {
            OrgId = payee.OrgId
            ParentAccountId = payee.ParentAccountId
            AccountId = payee.AccountId
            Name = payee.OrgName
         }
         Memo = Some shared.Memo
         OriginatedFromSchedule = false
         OriginatedFromPaymentRequest = Some shared.Id
      }

type PaymentPortalShortId =
   | ShortId of string

   override x.ToString() =
      match x with
      | ShortId s -> s

   static member create() = NanoId.nanoid () |> ShortId

   member x.AsUrl = $"/payment/{x}"

[<RequireQualifiedAccess>]
type ThirdPartyPaymentRequest = {
   SharedDetails: PaymentRequestSharedDetails
   Payer: ThirdPartyPayer
   ShortId: PaymentPortalShortId
}

[<RequireQualifiedAccess>]
type PaymentRequest =
   | Platform of PlatformPaymentRequest
   | ThirdParty of ThirdPartyPaymentRequest

   member x.SharedDetails =
      match x with
      | Platform p -> p.SharedDetails
      | ThirdParty p -> p.SharedDetails

   member x.Status = x.SharedDetails.Status

   member x.Payer =
      match x with
      | Platform p -> p.Payer.OrgName
      | ThirdParty p -> p.Payer.Name

   member x.IsExpired =
      match x with
      | Platform p -> p.SharedDetails.DueAt <= DateTime.UtcNow
      | ThirdParty p -> p.SharedDetails.DueAt <= DateTime.UtcNow

   member x.IsUnpaid = x.Status = PaymentRequestStatus.Requested

   member x.CanManage = not x.IsExpired && x.IsUnpaid

   member x.DisplayPriority =
      let sortByStatus =
         match x.Status with
         | PaymentRequestStatus.Requested when x.IsExpired -> 6
         | PaymentRequestStatus.Fulfilled _ -> 5
         | PaymentRequestStatus.Cancelled -> 4
         | PaymentRequestStatus.Declined -> 3
         | PaymentRequestStatus.Failed _ -> 2
         | PaymentRequestStatus.Requested -> 1

      sortByStatus, x.SharedDetails.DueAt

   member x.StatusDisplay =
      match x.Status with
      | PaymentRequestStatus.Requested when x.IsExpired -> "Overdue"
      | PaymentRequestStatus.Requested -> "Requested"
      | PaymentRequestStatus.Fulfilled _ -> "Fulfilled"
      | PaymentRequestStatus.Cancelled -> "Cancelled"
      | PaymentRequestStatus.Declined -> "Declined"
      | PaymentRequestStatus.Failed _ -> "Failed"

type PaymentAnalytics = {
   UnpaidMoney: decimal
   UnpaidCount: int
   OverdueMoney: decimal
   OverdueCount: int
}

type PaymentRequestSummary = {
   // Payment requests to orgs on the platform or outside the platform.
   OutgoingRequests: PaymentRequest list
   // Payment request from orgs on the platform (TODO: or entities outside the platform)
   IncomingRequests: PaymentRequest list
} with

   static member private computeAnalytics
      (requests: PaymentRequest list)
      : PaymentAnalytics
      =
      List.fold
         (fun acc (payment: PaymentRequest) ->
            if payment.IsUnpaid then
               {
                  acc with
                     UnpaidMoney =
                        acc.UnpaidMoney + payment.SharedDetails.Amount
                     UnpaidCount = acc.UnpaidCount + 1
               }
            elif payment.IsExpired then
               {
                  acc with
                     OverdueMoney =
                        acc.OverdueMoney + payment.SharedDetails.Amount
                     OverdueCount = acc.OverdueCount + 1
               }
            else
               acc)
         {
            UnpaidMoney = 0m
            UnpaidCount = 0
            OverdueMoney = 0m
            OverdueCount = 0
         }
         requests

   member x.AnalyticsOutgoing =
      PaymentRequestSummary.computeAnalytics x.OutgoingRequests

   member x.AnalyticsIncoming =
      PaymentRequestSummary.computeAnalytics x.IncomingRequests
