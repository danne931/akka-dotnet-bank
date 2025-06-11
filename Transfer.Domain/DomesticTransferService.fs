module DomesticTransfer.Service.Domain

open Bank.Transfer.Domain

[<RequireQualifiedAccess>]
type DomesticTransferServiceFailReason =
   | CorruptData
   | InvalidAction
   | InvalidPaymentNetwork
   | InvalidDepository
   | InvalidAmount
   | AccountClosed
   | InvalidAccountInfo
   | Unknown of string

[<RequireQualifiedAccess>]
type DomesticTransferServiceAction =
   | TransferAck
   | ProgressCheck

type DomesticTransferServiceSender = {
   Name: string
   AccountNumber: string
   RoutingNumber: string
}

type DomesticTransferServiceRecipient = {
   Name: string
   AccountNumber: string
   RoutingNumber: string
   Depository: string
}

type DomesticTransferServiceResponse = {
   Sender: DomesticTransferServiceSender
   Recipient: DomesticTransferServiceRecipient
   Ok: bool
   Status: string
   Reason: string
   TransactionId: string
}

[<RequireQualifiedAccess>]
type DomesticTransferServiceMessage =
   | TransferRequest of DomesticTransferServiceAction * DomesticTransfer
