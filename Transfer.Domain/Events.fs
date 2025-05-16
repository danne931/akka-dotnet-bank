namespace Bank.Transfer.Domain

open System

open Lib.SharedTypes

type InternalTransferWithinOrgPending = { BaseInfo: BaseInternalTransferInfo }

type InternalTransferWithinOrgCompleted = { BaseInfo: BaseInternalTransferInfo }

type InternalTransferWithinOrgFailed = {
   BaseInfo: BaseInternalTransferInfo
   Reason: InternalTransferFailReason
}

type InternalTransferBetweenOrgsScheduled = {
   BaseInfo: BaseInternalTransferInfo
}

type InternalTransferBetweenOrgsPending = {
   BaseInfo: BaseInternalTransferInfo
   /// Indicates whether this transfer originated from a scheduled job.
   FromSchedule: bool
}

type InternalTransferBetweenOrgsCompleted = {
   BaseInfo: BaseInternalTransferInfo
}

type InternalTransferBetweenOrgsFailed = {
   BaseInfo: BaseInternalTransferInfo
   Reason: InternalTransferFailReason
}

type DomesticTransferScheduled = { BaseInfo: BaseDomesticTransferInfo }

type DomesticTransferPending = {
   /// Indicates whether this transfer originated from a scheduled job.
   FromSchedule: bool
   BaseInfo: BaseDomesticTransferInfo
}

type DomesticTransferProgressUpdate = {
   BaseInfo: BaseDomesticTransferInfo
   InProgressInfo: DomesticTransferServiceProgress
}

type DomesticTransferCompleted = {
   BaseInfo: BaseDomesticTransferInfo
   /// Indicates the transfer was completed after previously failing
   /// and then retrying.
   FromRetry: DomesticTransferFailReason option
}

type DomesticTransferFailed = {
   BaseInfo: BaseDomesticTransferInfo
   Reason: DomesticTransferFailReason
}

type RegisteredDomesticTransferRecipient = {
   Recipient: DomesticTransferRecipient
}

type EditedDomesticTransferRecipient = { Recipient: DomesticTransferRecipient }

type DomesticTransferRecipientFailed = {
   RecipientId: AccountId
   TransferId: TransferId
   Reason: DomesticTransferRecipientFailReason
}

/// Successful retry of a previously failed domestic transfer infers that
/// the recipient info has been corrected.
type DomesticTransferRetryConfirmsRecipient = {
   RecipientId: AccountId
   TransferId: TransferId
}

type InternalTransferWithinOrgDeposited = { BaseInfo: BaseInternalTransferInfo }

type InternalTransferBetweenOrgsDeposited = {
   BaseInfo: BaseInternalTransferInfo
}

type NicknamedDomesticTransferRecipient = {
   RecipientId: AccountId
   RecipientAccountEnvironment: RecipientAccountEnvironment
   Nickname: string option
}

module TransferEventToDomesticTransfer =
   let fromPending
      (evt: BankEvent<DomesticTransferPending>)
      : DomesticTransfer
      =
      let info = evt.Data.BaseInfo

      {
         Sender = info.Sender
         TransferId = evt.Data.BaseInfo.TransferId
         Recipient = info.Recipient
         InitiatedBy = evt.InitiatedBy
         Amount = info.Amount
         ScheduledDate = info.ScheduledDate
         Memo = info.Memo
         Status = DomesticTransferProgress.WaitingForTransferServiceAck
      }

   let fromFailure (evt: BankEvent<DomesticTransferFailed>) : DomesticTransfer =
      let info = evt.Data.BaseInfo

      {
         Sender = info.Sender
         TransferId = evt.Data.BaseInfo.TransferId
         Recipient = info.Recipient
         InitiatedBy = evt.InitiatedBy
         Amount = info.Amount
         ScheduledDate = info.ScheduledDate
         Memo = info.Memo
         Status = DomesticTransferProgress.Failed evt.Data.Reason
      }

type PlatformPaymentBaseInfo = {
   Id: PaymentId
   Payee: Payee
   Payer: PlatformPayer
   InitiatedById: InitiatedById
   Amount: decimal
}

type PlatformPaymentRequested = {
   BaseInfo: PlatformPaymentBaseInfo
   Expiration: DateTime
   Memo: string
}

module PlatformPaymentRequested =
   let fromPayment (p: PlatformPayment) : PlatformPaymentRequested = {
      BaseInfo = {
         Id = p.BaseInfo.Id
         Payee = p.BaseInfo.Payee
         Payer = p.Payer
         InitiatedById = p.BaseInfo.InitiatedBy
         Amount = p.BaseInfo.Amount
      }
      Memo = p.BaseInfo.Memo
      Expiration = p.BaseInfo.Expiration
   }

   let toPayment (e: BankEvent<PlatformPaymentRequested>) : PlatformPayment =
      let info = e.Data.BaseInfo

      {
         BaseInfo = {
            Id = info.Id
            InitiatedBy = info.InitiatedById
            Amount = info.Amount
            Type = PaymentType.Platform
            Payee = info.Payee
            CreatedAt = e.Timestamp
            Expiration = e.Data.Expiration
            Memo = e.Data.Memo
         }
         Status = PlatformPaymentStatus.Unpaid
         Payer = e.Data.BaseInfo.Payer
         PaidBy = None
      }

type PlatformPaymentPaid = {
   BaseInfo: PlatformPaymentBaseInfo
   PaymentMethod: PaymentMethod
}

type PlatformPaymentDeposited = {
   BaseInfo: PlatformPaymentBaseInfo
   PaymentMethod: PaymentMethod
}

type PlatformPaymentCancelled = {
   BaseInfo: PlatformPaymentBaseInfo
   Reason: string option
}

type PlatformPaymentDeclined = {
   BaseInfo: PlatformPaymentBaseInfo
   Reason: string option
}

type PlatformPaymentRefunded = {
   BaseInfo: PlatformPaymentBaseInfo
   Reason: PlatformPaymentRefundReason
}

type ThirdPartyPaymentBaseInfo = {
   Id: PaymentId
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

type ThirdPartyPaymentDeposited = {
   BaseInfo: ThirdPartyPaymentBaseInfo
   PaymentMethod: ThirdPartyPaymentMethod
}

type ThirdPartyPaymentCancelled = {
   BaseInfo: ThirdPartyPaymentBaseInfo
   Reason: string option
}

type AutomaticTransferRuleConfigured = {
   Config: AutomaticTransfer.AutomaticTransferConfig
}

type AutomaticTransferRuleDeleted = { RuleId: Guid }

type InternalAutomatedTransferPending = {
   BaseInfo: BaseInternalTransferInfo
   Rule: AutomaticTransfer.AutomaticTransferRule
}

type InternalAutomatedTransferCompleted = {
   BaseInfo: BaseInternalTransferInfo
   Rule: AutomaticTransfer.AutomaticTransferRule
}

type InternalAutomatedTransferFailed = {
   BaseInfo: BaseInternalTransferInfo
   Reason: InternalTransferFailReason
   Rule: AutomaticTransfer.AutomaticTransferRule
}

type InternalAutomatedTransferDeposited = {
   BaseInfo: BaseInternalTransferInfo
   Rule: AutomaticTransfer.AutomaticTransferRule
}
