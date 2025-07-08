namespace Bank.Transfer.Domain

open System

open Lib.SharedTypes

type InternalTransferSender = {
   Name: string
   AccountId: AccountId
   ParentAccountId: ParentAccountId
   OrgId: OrgId
}

type InternalTransferRecipient = {
   Name: string
   AccountId: AccountId
   ParentAccountId: ParentAccountId
   OrgId: OrgId
}

[<RequireQualifiedAccess>]
type InternalTransferFailReason =
   | AccountNotActive
   | InsufficientFunds
   | PartnerBankSync of string

[<RequireQualifiedAccess>]
type InternalTransferWithinOrgStatus =
   | Pending
   | Settled

type BaseInternalTransferWithinOrgInfo = {
   TransferId: TransferId
   InitiatedBy: Initiator
   Sender: InternalTransferSender
   Recipient: InternalTransferRecipient
   Amount: decimal
   ScheduledDate: DateTime
   Memo: string option
}

type InProgressInternalWithinOrgTransfer = {
   TransferId: TransferId
   Info: BaseInternalTransferWithinOrgInfo
   IsAutomated: bool
   Status: InternalTransferWithinOrgStatus
}

[<RequireQualifiedAccess>]
type InternalTransferBetweenOrgsStatus =
   | Scheduled
   | Pending
   | Deposited
   | Settled
   | Failed of InternalTransferFailReason

type BaseInternalTransferBetweenOrgsInfo = {
   TransferId: TransferId
   InitiatedBy: Initiator
   Sender: InternalTransferSender
   Recipient: InternalTransferRecipient
   Amount: decimal
   ScheduledDate: DateTime
   FromPaymentRequest: PaymentId option
   Memo: string option
}
