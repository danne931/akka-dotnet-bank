namespace Bank.Payment.Domain

open System

open Lib.SharedTypes

type PaymentRequestSharedEventDetails = {
   Id: PaymentRequestId
   Amount: decimal
   Payee: Payee
   Expiration: DateTime
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
         Expiration = p.SharedDetails.Expiration
         Memo = p.SharedDetails.Memo
      }

type PlatformPaymentRequested = {
   SharedDetails: PaymentRequestSharedEventDetails
   Payer: PlatformPayer
}

type ThirdPartyPaymentRequested = {
   SharedDetails: PaymentRequestSharedEventDetails
   Payer: ThirdPartyPayer
   ShortId: PaymentPortalShortId
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

module PaymentRequested =
   let toPaymentRequest (e: BankEvent<PaymentRequested>) : PaymentRequest =
      let sharedDetails: PaymentRequestSharedDetails = {
         Id = e.Data.SharedDetails.Id
         InitiatedBy = e.InitiatedBy.Id
         Amount = e.Data.SharedDetails.Amount
         Payee = e.Data.SharedDetails.Payee
         Expiration = e.Data.SharedDetails.Expiration
         Memo = e.Data.SharedDetails.Memo
         CreatedAt = e.Timestamp
         Status = PaymentRequestStatus.Requested
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
