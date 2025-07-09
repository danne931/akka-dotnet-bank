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
