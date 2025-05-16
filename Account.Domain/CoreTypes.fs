namespace Bank.Account.Domain

open System

open Lib.SharedTypes

[<RequireQualifiedAccess>]
type Currency =
   | USD
   | EUR
   | THB
   | VND

[<RequireQualifiedAccess>]
type AccountStatus =
   | InitialEmptyState
   | Pending
   | Active
   | Closed
   | ReadyForDelete

   override x.ToString() =
      match x with
      | AccountStatus.InitialEmptyState -> "InitialEmptyState"
      | AccountStatus.Pending -> "Pending"
      | AccountStatus.Active -> "Active"
      | AccountStatus.Closed -> "Closed"
      | AccountStatus.ReadyForDelete -> "ReadyForDelete"

   static member fromString(status: string) : AccountStatus option =
      if String.IsNullOrEmpty status then
         None
      else
         match status.ToLower() with
         | "pending" -> Some AccountStatus.Pending
         | "active" -> Some AccountStatus.Active
         | "closed" -> Some AccountStatus.Closed
         | "readyfordelete" -> Some AccountStatus.ReadyForDelete
         | _ -> None

   static member fromStringUnsafe(status: string) : AccountStatus =
      match AccountStatus.fromString status with
      | None -> failwith "Error attempting to cast string to AccountStatus"
      | Some status -> status

[<RequireQualifiedAccess>]
type AccountDepository =
   | Checking
   | Savings

   override x.ToString() =
      match x with
      | AccountDepository.Checking -> "Checking"
      | AccountDepository.Savings -> "Savings"

   static member fromString(dep: string) : AccountDepository option =
      if String.IsNullOrEmpty dep then
         None
      else
         match dep.ToLower() with
         | "checking" -> Some AccountDepository.Checking
         | "savings" -> Some AccountDepository.Savings
         | _ -> None

   static member fromStringUnsafe(dep: string) : AccountDepository =
      match AccountDepository.fromString dep with
      | None -> failwith "Error attempting to cast string to AccountDepository"
      | Some dep -> dep

[<RequireQualifiedAccess>]
type MoneyFlow =
   | In
   | Out

module MoneyFlow =
   let fromString (flow: string) : MoneyFlow option =
      if String.IsNullOrEmpty flow then
         None
      else
         match flow.ToLower() with
         | "in" -> Some MoneyFlow.In
         | "out" -> Some MoneyFlow.Out
         | _ -> None

type PurchaseInfo = {
   OrgId: OrgId
   AccountId: AccountId
   EmployeeId: EmployeeId
   CardId: CardId
   InitiatedBy: Initiator
   CorrelationId: CorrelationId
   EmployeeName: string
   EmployeeEmail: Email
   CardNumberLast4: string
   Date: DateTime
   Amount: decimal
   Merchant: string
   Reference: string option
   // Represents ID of transaction coming from simulated card network.
   CardNetworkTransactionId: Guid
}

[<RequireQualifiedAccess>]
type PurchaseCardFailReason =
   | CardNotFound
   | CardExpired
   | CardLocked
   | ExceededDailyCardLimit of limit: decimal * accrued: decimal
   | ExceededMonthlyCardLimit of limit: decimal * accrued: decimal

   member x.Display =
      match x with
      | PurchaseCardFailReason.CardNotFound -> "Card not found."
      | PurchaseCardFailReason.CardExpired -> "Card expired."
      | PurchaseCardFailReason.CardLocked -> "Card locked."
      | PurchaseCardFailReason.ExceededDailyCardLimit(limit, accrued) ->
         $"You have spent ${accrued} today.
           Your daily purchase limit is set to ${limit}."
      | PurchaseCardFailReason.ExceededMonthlyCardLimit(limit, accrued) ->
         $"You have spent ${accrued} this month.
           Your monthly purchase limit is set to ${limit}."

[<RequireQualifiedAccess>]
type PurchaseAccountFailReason =
   | AccountNotActive of accountName: string
   | InsufficientAccountFunds of balance: decimal * accountName: string

   member x.Display =
      match x with
      | PurchaseAccountFailReason.AccountNotActive accountName ->
         $"Account {accountName} is not active."
      | PurchaseAccountFailReason.InsufficientAccountFunds(balance, accountName) ->
         $"Account {accountName} has insufficient funds.  The current balance is ${balance}."

[<RequireQualifiedAccess>]
type PurchaseFailReason =
   | Card of PurchaseCardFailReason
   | Account of PurchaseAccountFailReason
   | CardNetwork of string
   | PartnerBankSync of string

   member x.Display =
      match x with
      | PurchaseFailReason.Card reason -> reason.Display
      | PurchaseFailReason.Account reason -> reason.Display
      | PurchaseFailReason.CardNetwork _ ->
         "Card network declined the purchase."
      | PurchaseFailReason.PartnerBankSync _ ->
         "Unable to sync to partner bank."

[<RequireQualifiedAccess>]
type PurchaseRefundReason =
   | FraudulentActivity
   | DuplicateCharge
   | AccountStateInvalid of PurchaseAccountFailReason
   | CardNetworkError of string
