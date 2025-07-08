namespace Bank.Transfer.Domain

open System

open Lib.SharedTypes

type InternalTransferWithinOrgDeducted = {
   BaseInfo: BaseInternalTransferWithinOrgInfo
}

type InternalTransferWithinOrgDeposited = {
   BaseInfo: BaseInternalTransferWithinOrgInfo
}

type InternalTransferBetweenOrgsScheduled = {
   BaseInfo: BaseInternalTransferBetweenOrgsInfo
}

type InternalTransferBetweenOrgsPending = {
   BaseInfo: BaseInternalTransferBetweenOrgsInfo
   /// Indicates whether this transfer originated from a scheduled job.
   FromSchedule: bool
}

type InternalTransferBetweenOrgsDeposited = {
   BaseInfo: BaseInternalTransferBetweenOrgsInfo
}

type InternalTransferBetweenOrgsSettled = {
   BaseInfo: BaseInternalTransferBetweenOrgsInfo
   SettlementId: SettlementId
}

type InternalTransferBetweenOrgsFailed = {
   BaseInfo: BaseInternalTransferBetweenOrgsInfo
   Reason: InternalTransferFailReason
}

type DomesticTransferScheduled = {
   BaseInfo: BaseDomesticTransferInfo
   ExpectedSettlementDate: DateTime
}

type DomesticTransferPending = {
   /// Indicates whether this transfer originated from a scheduled job.
   FromSchedule: bool
   ExpectedSettlementDate: DateTime
   BaseInfo: BaseDomesticTransferInfo
}

type DomesticTransferProgressUpdated = {
   BaseInfo: BaseDomesticTransferInfo
   InProgressInfo: DomesticTransferThirdPartyUpdate
   NewExpectedSettlementDate: DateTime option
}

type DomesticTransferSettled = {
   BaseInfo: BaseDomesticTransferInfo
   /// Indicates the transfer was settled after previously
   /// failing and then retrying.
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
         ExpectedSettlementDate = evt.Data.ExpectedSettlementDate
         Memo = info.Memo
         Status = DomesticTransferProgress.WaitingForTransferServiceAck
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

type ThirdPartyPaymentRequestCancelled = {
   BaseInfo: ThirdPartyPaymentBaseInfo
   Reason: string option
}

type AutomaticTransferRuleConfigured = {
   AccountId: AccountId
   Config: AutomaticTransfer.AutomaticTransferConfig
}

type AutomaticTransferRuleDeleted = { AccountId: AccountId; RuleId: Guid }

type InternalAutomatedTransferDeducted = {
   BaseInfo: BaseInternalTransferWithinOrgInfo
   Rule: AutomaticTransfer.AutomaticTransferRule
}

type InternalAutomatedTransferDeposited = {
   BaseInfo: BaseInternalTransferWithinOrgInfo
   Rule: AutomaticTransfer.AutomaticTransferRule
}
