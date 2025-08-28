namespace Bank.Transfer.Domain

open System

open Lib.SharedTypes

[<RequireQualifiedAccess>]
type CounterpartyAccountDepository =
   | Checking
   | Savings

module CounterpartyAccountDepository =
   let fromString (str: string) : CounterpartyAccountDepository option =
      match str.ToLower() with
      | "checking" -> Some CounterpartyAccountDepository.Checking
      | "savings" -> Some CounterpartyAccountDepository.Savings
      | _ -> None

   let fromStringUnsafe str : CounterpartyAccountDepository =
      match fromString str with
      | Some s -> s
      | None ->
         failwith
            "Error attempting to cast string to CounterpartyAccountDepository"

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

type Counterparty = {
   LastName: string
   FirstName: string
   Nickname: string option
   AccountNumber: AccountNumber
   RoutingNumber: RoutingNumber
   CounterpartyId: AccountId
   OrgId: OrgId
   Depository: CounterpartyAccountDepository
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
   | InvalidDepository
   | Unknown of string

   member x.Display =
      match x with
      | CorruptData -> "Corrupt Data"
      | InvalidAction -> "Invalid Action"
      | InvalidPaymentNetwork -> "Invalid Payment Network"
      | InvalidDepository -> "Invalid Counterparty Account Depository"
      | Unknown r -> r

[<RequireQualifiedAccess>]
type DomesticTransferPartnerBankFailReason =
   | InvalidAmount
   | CounterpartyAccountInvalidInfo
   | CounterpartyAccountNotActive
   | NoTransferFound
   | Infra of DomesticTransferInfraFailReason

   member x.Display =
      match x with
      | InvalidAmount -> "Invalid Amount"
      | CounterpartyAccountInvalidInfo ->
         "Invalid Counterparty Account Info Provided"
      | CounterpartyAccountNotActive -> "Counterparty Account Not Active"
      | NoTransferFound -> "No Transfer Found During Progress Check"
      | Infra infraRelated -> infraRelated.Display

type DomesticTransferPartnerBankProgressDetail = {
   Detail: string
   ExpectedSettlementDate: DateTime
}

[<RequireQualifiedAccess>]
type DomesticTransferPartnerBankUpdate =
   | ServiceAckReceived
   | ProgressDetail of DomesticTransferPartnerBankProgressDetail
   | Settled
   | Failed of DomesticTransferPartnerBankFailReason

[<RequireQualifiedAccess>]
type DomesticTransferFailReason =
   | AccountNotActive
   | AccountInsufficientFunds
   | PartnerBank of DomesticTransferPartnerBankFailReason

   member x.Display =
      match x with
      | AccountNotActive -> "Account Not Active"
      | AccountInsufficientFunds -> "Account Has Insufficient Funds"
      | PartnerBank p -> p.Display

[<RequireQualifiedAccess>]
type DomesticTransferProgress =
   | Scheduled
   | ProcessingAccountDeduction
   | WaitingForTransferServiceAck
   | PartnerBank of DomesticTransferPartnerBankUpdate
   | Settled
   | Failed of DomesticTransferFailReason

type DomesticTransferOriginator = {
   Name: string
   AccountNumber: AccountNumber
   RoutingNumber: RoutingNumber
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   PartnerBankAccountId: PartnerBankAccountId
   AccountId: AccountId
}

type DomesticTransferOriginatorReference = {
   Name: string
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   AccountId: AccountId
}

// Info received from the initial domestic transfer request will
// carry over unaltered for all event progressions
// (ProgressUpdate/Settled/Failed/Retry).
type BaseDomesticTransferInfo = {
   Originator: DomesticTransferOriginatorReference
   Counterparty: Counterparty
   Amount: decimal
   TransferId: TransferId
   InitiatedBy: Initiator
   Memo: string option
   ScheduledDate: DateTime
}

type DomesticTransfer = {
   Originator: DomesticTransferOriginator
   Counterparty: Counterparty
   TransferId: TransferId
   InitiatedBy: Initiator
   Amount: decimal
   ScheduledDate: DateTime
   ExpectedSettlementDate: DateTime
   Status: DomesticTransferProgress
   Memo: string option
}
