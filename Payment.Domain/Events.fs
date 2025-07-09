namespace Bank.Payment.Domain

open System

open Lib.SharedTypes

type PlatformPaymentBaseInfo = {
   Id: PaymentRequestId
   Payee: Payee
   Payer: PlatformPayer
   InitiatedById: InitiatedById
   Amount: decimal
}

module PlatformPaymentBaseInfo =
   let fromPayment (p: PlatformPaymentRequest) : PlatformPaymentBaseInfo = {
      Id = p.BaseInfo.Id
      Payee = p.BaseInfo.Payee
      Payer = p.Payer
      InitiatedById = p.BaseInfo.InitiatedBy
      Amount = p.BaseInfo.Amount
   }

type PlatformPaymentRequested = {
   BaseInfo: PlatformPaymentBaseInfo
   Expiration: DateTime
   Memo: string
}

module PlatformPaymentRequested =
   let toPayment
      (e: BankEvent<PlatformPaymentRequested>)
      : PlatformPaymentRequest
      =
      let info = e.Data.BaseInfo

      {
         BaseInfo = {
            Id = info.Id
            InitiatedBy = info.InitiatedById
            Amount = info.Amount
            Type = PaymentRequestType.Platform
            Payee = info.Payee
            Status = PaymentRequestStatus.Requested
            CreatedAt = e.Timestamp
            Expiration = e.Data.Expiration
            Memo = e.Data.Memo
         }
         Payer = e.Data.BaseInfo.Payer
      }

type PlatformPaymentRequestCancelled = {
   BaseInfo: PlatformPaymentBaseInfo
   Reason: string option
}

type PlatformPaymentRequestDeclined = {
   BaseInfo: PlatformPaymentBaseInfo
   Reason: string option
}

type ThirdPartyPaymentBaseInfo = {
   Id: PaymentRequestId
   Payee: Payee
   Payer: ThirdPartyPayer
   InitiatedById: InitiatedById
}

type ThirdPartyPaymentRequested = {
   BaseInfo: ThirdPartyPaymentBaseInfo
   Amount: decimal
   Expiration: DateTime
   Memo: string
}

type ThirdPartyPaymentRequestCancelled = {
   BaseInfo: ThirdPartyPaymentBaseInfo
   Reason: string option
}
