namespace Bank.Transfer.Domain

open System
open System.Threading.Tasks

open Lib.SharedTypes

[<RequireQualifiedAccess>]
type RecipientRegistrationStatus =
   | Confirmed
   | InvalidAccount
   | Closed

module RecipientRegistrationStatus =
   let fromString (str: string) : RecipientRegistrationStatus option =
      match str.ToLower() with
      | "confirmed" -> Some RecipientRegistrationStatus.Confirmed
      | "invalidaccount" -> Some RecipientRegistrationStatus.InvalidAccount
      | "closed" -> Some RecipientRegistrationStatus.Closed
      | _ -> None

   let fromStringUnsafe str : RecipientRegistrationStatus =
      match fromString str with
      | Some s -> s
      | None ->
         failwith
            "Error attempting to cast string to RecipientRegistrationStatus"

[<RequireQualifiedAccess>]
type DomesticRecipientAccountDepository =
   | Checking
   | Savings

module DomesticRecipientAccountDepository =
   let fromString (str: string) : DomesticRecipientAccountDepository option =
      match str.ToLower() with
      | "checking" -> Some DomesticRecipientAccountDepository.Checking
      | "savings" -> Some DomesticRecipientAccountDepository.Savings
      | _ -> None

   let fromStringUnsafe str : DomesticRecipientAccountDepository =
      match fromString str with
      | Some s -> s
      | None ->
         failwith
            "Error attempting to cast string to DomesticRecipientAccountDepository"

[<RequireQualifiedAccess>]
type PaymentNetwork = | ACH
//| FedNow

module PaymentNetwork =
   let fromString (str: string) : PaymentNetwork option =
      match str.ToLower() with
      | "ach" -> Some PaymentNetwork.ACH
      | _ -> None

   let fromStringUnsafe str : PaymentNetwork =
      match fromString str with
      | Some s -> s
      | None -> failwith "Error attempting to cast string to PaymentNetwork"

type DomesticTransferRecipient = {
   LastName: string
   FirstName: string
   Nickname: string option
   AccountNumber: AccountNumber
   RoutingNumber: RoutingNumber
   Status: RecipientRegistrationStatus
   RecipientAccountId: AccountId
   SenderOrgId: OrgId
   Depository: DomesticRecipientAccountDepository
   PaymentNetwork: PaymentNetwork
   CreatedAt: DateTime
} with

   member x.Name = $"{x.FirstName} {x.LastName}"

   member x.FullName =
      $"{x.Nickname |> Option.defaultValue x.Name} **{x.AccountNumber.Last4}"

[<RequireQualifiedAccess>]
type TransferProgressTrackingMessage = | ProgressCheck

[<RequireQualifiedAccess>]
type DomesticTransferRecipientFailReason =
   | InvalidAccountInfo
   | ClosedAccount

[<RequireQualifiedAccess>]
type DomesticTransferFailReason =
   | SenderAccountNotActive
   | SenderAccountInsufficientFunds
   | CorruptData
   | InvalidAction
   | InvalidPaymentNetwork
   | InvalidDepository
   | InvalidAmount
   | AccountClosed
   | InvalidAccountInfo
   | Unknown of string

   member x.Display =
      match x with
      | SenderAccountNotActive -> "Sender Account Not Active"
      | SenderAccountInsufficientFunds ->
         "Sender Account Has Insufficient Funds"
      | CorruptData -> "Corrupt Data"
      | InvalidAction -> "Invalid Action"
      | InvalidPaymentNetwork -> "Invalid Payment Network"
      | InvalidDepository -> "Invalid Depository"
      | InvalidAmount -> "Invalid Amount"
      | AccountClosed -> "Account Closed"
      | InvalidAccountInfo -> "Invalid Account Info"
      | Unknown r -> r

[<RequireQualifiedAccess>]
type DomesticTransferServiceProgress =
   | InitialHandshakeAck
   | InProgress of string
   | Settled
   | Failed of DomesticTransferFailReason

[<RequireQualifiedAccess>]
type DomesticTransferProgress =
   | Scheduled
   | ProcessingSenderAccountDeduction
   | WaitingForTransferServiceAck
   | InProgress of DomesticTransferServiceProgress
   | Completed
   | Failed of DomesticTransferFailReason

type DomesticTransferSender = {
   Name: string
   AccountNumber: AccountNumber
   RoutingNumber: RoutingNumber
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   AccountId: AccountId
}

// Info received from the initial domestic transfer request will
// carry over unaltered for all event progressions
// (ProgressUpdate/Completed/Rejected/Retry).
type BaseDomesticTransferInfo = {
   Sender: DomesticTransferSender
   Recipient: DomesticTransferRecipient
   Amount: decimal
   TransferId: TransferId
   InitiatedBy: Initiator
   Memo: string option
   ScheduledDate: DateTime
}

type DomesticTransfer = {
   Sender: DomesticTransferSender
   Recipient: DomesticTransferRecipient
   TransferId: TransferId
   InitiatedBy: Initiator
   Amount: decimal
   ScheduledDate: DateTime
   Status: DomesticTransferProgress
   Memo: string option
}

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

type DomesticTransferRequest =
   DomesticTransferServiceAction
      -> DomesticTransfer
      -> Task<Result<DomesticTransferServiceResponse, Err>>

[<RequireQualifiedAccess>]
type DomesticTransferMessage =
   | TransferRequest of DomesticTransferServiceAction * DomesticTransfer
