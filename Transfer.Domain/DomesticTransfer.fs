namespace Bank.Transfer.Domain

open System

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
type DomesticTransferInfraFailReason =
   | CorruptData
   | InvalidAction
   | InvalidPaymentNetwork
   | RecipientAccountInvalidDepository
   | Unknown of string

   member x.Display =
      match x with
      | CorruptData -> "Corrupt Data"
      | InvalidAction -> "Invalid Action"
      | InvalidPaymentNetwork -> "Invalid Payment Network"
      | RecipientAccountInvalidDepository ->
         "Invalid Recipient Account Depository"
      | Unknown r -> r

[<RequireQualifiedAccess>]
type DomesticTransferThirdPartyFailReason =
   | InvalidAmount
   | RecipientAccountInvalidInfo
   | RecipientAccountNotActive
   | NoTransferFound
   | Infra of DomesticTransferInfraFailReason

   member x.Display =
      match x with
      | InvalidAmount -> "Invalid Amount"
      | RecipientAccountInvalidInfo -> "Invalid Recipient Account Info Provided"
      | RecipientAccountNotActive -> "Recipient Account Not Active"
      | NoTransferFound -> "No Transfer Found During Progress Check"
      | Infra infraRelated -> infraRelated.Display

[<RequireQualifiedAccess>]
type DomesticTransferThirdPartyUpdate =
   | ServiceAckReceived
   | ProgressDetail of string
   | Settled
   | Failed of DomesticTransferThirdPartyFailReason

[<RequireQualifiedAccess>]
type DomesticTransferFailReason =
   | SenderAccountNotActive
   | SenderAccountInsufficientFunds
   | ThirdParty of DomesticTransferThirdPartyFailReason

   member x.Display =
      match x with
      | SenderAccountNotActive -> "Sender Account Not Active"
      | SenderAccountInsufficientFunds ->
         "Sender Account Has Insufficient Funds"
      | ThirdParty tp -> tp.Display

[<RequireQualifiedAccess>]
type DomesticTransferProgress =
   | Scheduled
   | ProcessingSenderAccountDeduction
   | WaitingForTransferServiceAck
   | ThirdParty of DomesticTransferThirdPartyUpdate
   | Settled
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
// (ProgressUpdate/Settled/Failed/Retry).
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
