namespace Bank.Transfer.Domain

open System

open Lib.SharedTypes

type InternalTransferRecipient = {
   Name: string
   AccountId: AccountId
   ParentAccountId: ParentAccountId
   OrgId: OrgId
}

type InternalTransferSender = {
   Name: string
   AccountId: AccountId
   ParentAccountId: ParentAccountId
   OrgId: OrgId
}

[<RequireQualifiedAccess>]
type InternalTransferFailReason =
   | AccountClosed
   | InvalidAccountInfo
   | PartnerBankSync of string

[<RequireQualifiedAccess>]
type InternalTransferStatus =
   | Scheduled
   | Pending
   | Deposited
   | Settled
   | Failed of InternalTransferFailReason

type BaseInternalTransferInfo = {
   TransferId: TransferId
   InitiatedBy: Initiator
   Recipient: InternalTransferRecipient
   Amount: decimal
   ScheduledDate: DateTime
   Sender: InternalTransferSender
   Memo: string option
}

type InProgressInternalTransfer = {
   TransferId: TransferId
   Info: BaseInternalTransferInfo
}
