namespace Bank.Payment.Domain

open System

open Lib.SharedTypes

type PaymentRequestSharedEventDetails = {
   Id: PaymentRequestId
   Amount: decimal
   Payee: Payee
   DueAt: DateTime
   Memo: string
}

module PaymentRequestSharedEventDetails =
   let fromPaymentRequest
      (p: PaymentRequest)
      : PaymentRequestSharedEventDetails
      =
      {
         Id = p.SharedDetails.Id
         Amount = p.SharedDetails.Amount
         Payee = p.SharedDetails.Payee
         DueAt = p.SharedDetails.DueAt
         Memo = p.SharedDetails.Memo
      }

/// The existing PaymentRequestId & recurrence settings
/// that this next recurring payment will be based on.
type RecurringPaymentReference = {
   OriginPaymentId: PaymentRequestId
   Settings: RecurringPaymentSchedule.RecurrenceSettings
}

type PlatformPaymentRequested = {
   SharedDetails: PaymentRequestSharedEventDetails
   Payer: PlatformPayer
   RecurringPaymentReference: RecurringPaymentReference option
   Invoice: Invoice option
}

type ThirdPartyPaymentRequested = {
   SharedDetails: PaymentRequestSharedEventDetails
   Payer: ThirdPartyPayer
   ShortId: PaymentPortalShortId
   RecurringPaymentReference: RecurringPaymentReference option
   Invoice: Invoice option
}

type PaymentRequested =
   | Platform of PlatformPaymentRequested
   | ThirdParty of ThirdPartyPaymentRequested

   member x.SharedDetails =
      match x with
      | Platform p -> p.SharedDetails
      | ThirdParty p -> p.SharedDetails

   member x.PayerName =
      match x with
      | Platform p -> p.Payer.OrgName
      | ThirdParty p -> p.Payer.Name

   member x.RecurringPaymentReference =
      match x with
      | Platform p -> p.RecurringPaymentReference
      | ThirdParty p -> p.RecurringPaymentReference

   member x.Invoice =
      match x with
      | Platform p -> p.Invoice
      | ThirdParty p -> p.Invoice

module PaymentRequested =
   let toPaymentRequest (e: BankEvent<PaymentRequested>) : PaymentRequest =
      let sharedDetails: PaymentRequestSharedDetails = {
         Id = e.Data.SharedDetails.Id
         InitiatedBy = e.InitiatedBy.Id
         Amount = e.Data.SharedDetails.Amount
         Payee = e.Data.SharedDetails.Payee
         DueAt = e.Data.SharedDetails.DueAt
         Memo = e.Data.SharedDetails.Memo
         CreatedAt = e.Timestamp
         Status = PaymentRequestStatus.Requested
         RecurrenceSettings =
            e.Data.RecurringPaymentReference |> Option.map _.Settings
         Invoice = e.Data.Invoice
      }

      match e.Data with
      | Platform p ->
         PaymentRequest.Platform {
            SharedDetails = sharedDetails
            Payer = p.Payer
         }
      | ThirdParty p ->
         PaymentRequest.ThirdParty {
            SharedDetails = sharedDetails
            Payer = p.Payer
            ShortId = p.ShortId
         }

   let updateRecurrenceSettings
      (transform: RecurringPaymentReference -> RecurringPaymentReference)
      (pay: PaymentRequested)
      =
      match pay with
      | PaymentRequested.Platform p ->
         PaymentRequested.Platform {
            p with
               RecurringPaymentReference =
                  p.RecurringPaymentReference |> Option.map transform
         }
      | PaymentRequested.ThirdParty p ->
         PaymentRequested.ThirdParty {
            p with
               RecurringPaymentReference =
                  p.RecurringPaymentReference |> Option.map transform
         }

type PaymentRequestCancelled = {
   SharedDetails: PaymentRequestSharedEventDetails
   PayerName: string
   Reason: string option
}

type PaymentRequestDeclined = {
   SharedDetails: PaymentRequestSharedEventDetails
   PayerName: string
   Reason: string option
}
