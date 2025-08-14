namespace Bank.Org.Domain

open System

open Lib.SharedTypes
open Lib.NetworkQuery

[<RequireQualifiedAccess>]
type AccountEventGroupFilter =
   | Purchase
   | Deposit
   | InternalTransferWithinOrg
   | InternalTransferBetweenOrgs
   | InternalAutomatedTransfer
   | DomesticTransfer
   | PaymentRequest

   member x.Display =
      match x with
      | AccountEventGroupFilter.Purchase -> "Purchases"
      | AccountEventGroupFilter.Deposit -> "Deposits"
      | AccountEventGroupFilter.InternalTransferWithinOrg ->
         "Transfers within your org"
      | AccountEventGroupFilter.InternalTransferBetweenOrgs ->
         "Transfers between orgs on the platform"
      | AccountEventGroupFilter.InternalAutomatedTransfer ->
         "Automated balance management transfers"
      | AccountEventGroupFilter.DomesticTransfer ->
         "Domestic transfers outside the platform"
      | AccountEventGroupFilter.PaymentRequest -> "Payment requests"

   static member All = [
      AccountEventGroupFilter.Purchase
      AccountEventGroupFilter.Deposit
      AccountEventGroupFilter.InternalTransferWithinOrg
      AccountEventGroupFilter.InternalTransferBetweenOrgs
      AccountEventGroupFilter.InternalAutomatedTransfer
      AccountEventGroupFilter.DomesticTransfer
      AccountEventGroupFilter.PaymentRequest
   ]

   static member fromString =
      function
      | "Purchase" -> Some AccountEventGroupFilter.Purchase
      | "Deposit" -> Some AccountEventGroupFilter.Deposit
      | "InternalTransferWithinOrg" ->
         Some AccountEventGroupFilter.InternalTransferWithinOrg
      | "InternalTransferBetweenOrgs" ->
         Some AccountEventGroupFilter.InternalTransferBetweenOrgs
      | "InternalAutomatedTransfer" ->
         Some AccountEventGroupFilter.InternalAutomatedTransfer
      | "DomesticTransfer" -> Some AccountEventGroupFilter.DomesticTransfer
      | "PaymentRequest" -> Some AccountEventGroupFilter.PaymentRequest
      | _ -> None

   static member fromQueryString: string -> AccountEventGroupFilter list option =
      listFromQueryString AccountEventGroupFilter.fromString

   static member listToDisplay(items: AccountEventGroupFilter list) =
      List.fold
         (fun acc (filter: AccountEventGroupFilter) ->
            if acc = "" then
               filter.Display
            else
               $"{acc}, {filter.Display}")
         ""
         items

/// Indicates the oldest Transaction within a "page" of transactions.
type TransactionCursor = {
   Timestamp: DateTime
   TransactionId: TransactionId
}

type TransactionQuery = {
   OrgId: OrgId
   AccountIds: (AccountId list) option
   PageLimit: int
   Cursor: TransactionCursor option
   MoneyFlow: Bank.Account.Domain.MoneyFlow option
   Category: CategoryFilter option
   Amount: AmountFilter option
   DateRange: (DateTime * DateTime) option
   CardIds: (CardId list) option
   InitiatedByIds: (InitiatedById list) option
   EventType: (AccountEventGroupFilter list) option
}

module TransactionQuery =
   let accountIdsFromQueryString: string -> AccountId list option =
      listFromQueryString (Guid.parseOptional >> Option.map AccountId)

   let cardIdsFromQueryString: string -> CardId list option =
      listFromQueryString (Guid.parseOptional >> Option.map CardId)

   let initiatedByIdsFromQueryString: string -> InitiatedById list option =
      listFromQueryString (
         Guid.parseOptional >> Option.map (EmployeeId >> InitiatedById)
      )
