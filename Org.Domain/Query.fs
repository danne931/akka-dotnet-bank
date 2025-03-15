namespace Bank.Org.Domain

open System

open Lib.SharedTypes
open Lib.NetworkQuery

[<RequireQualifiedAccess>]
type OrgEventGroupFilter =
   | Onboarding
   | FeatureFlagConfigured
   | CommandApprovalRule
   | CommandApprovalProgress
   | DomesticTransferRecipient

   member x.Display =
      match x with
      | OrgEventGroupFilter.Onboarding -> "Org Onboarding"
      | OrgEventGroupFilter.FeatureFlagConfigured ->
         "Org Feature Flag Configured"
      | OrgEventGroupFilter.CommandApprovalRule -> "Command Approval Rule"
      | OrgEventGroupFilter.CommandApprovalProgress ->
         "Command Approval Progress"
      | OrgEventGroupFilter.DomesticTransferRecipient ->
         "Domestic Transfer Recipient"

   static member All = [
      OrgEventGroupFilter.Onboarding
      OrgEventGroupFilter.FeatureFlagConfigured
      OrgEventGroupFilter.CommandApprovalRule
      OrgEventGroupFilter.CommandApprovalProgress
      OrgEventGroupFilter.DomesticTransferRecipient
   ]

   static member fromString =
      function
      | "Onboarding" -> Some OrgEventGroupFilter.Onboarding
      | "FeatureFlagConfigured" ->
         Some OrgEventGroupFilter.FeatureFlagConfigured
      | "CommandApprovalRule" -> Some OrgEventGroupFilter.CommandApprovalRule
      | "CommandApprovalProgress" ->
         Some OrgEventGroupFilter.CommandApprovalProgress
      | "DomesticTransferRecipient" ->
         Some OrgEventGroupFilter.DomesticTransferRecipient
      | _ -> None

   static member fromQueryString: string -> OrgEventGroupFilter list option =
      listFromQueryString OrgEventGroupFilter.fromString

   static member listToDisplay(items: OrgEventGroupFilter list) =
      List.fold
         (fun acc (filter: OrgEventGroupFilter) ->
            if acc = "" then
               filter.Display
            else
               $"{acc}, {filter.Display}")
         ""
         items

[<RequireQualifiedAccess>]
type TransactionGroupFilter =
   | Purchase
   | Deposit
   | InternalTransferWithinOrg
   | InternalTransferBetweenOrgs
   | InternalAutomatedTransfer
   | DomesticTransfer
   | PlatformPayment
   //| ThirdPartyPayment

   member x.Display =
      match x with
      | TransactionGroupFilter.Purchase -> "Purchases"
      | TransactionGroupFilter.Deposit -> "Deposits"
      | TransactionGroupFilter.InternalTransferWithinOrg ->
         "Transfers within your org"
      | TransactionGroupFilter.InternalTransferBetweenOrgs ->
         "Transfers between orgs on the platform"
      | TransactionGroupFilter.InternalAutomatedTransfer ->
         "Automated balance management transfers"
      | TransactionGroupFilter.DomesticTransfer ->
         "Domestic transfers outside the platform"
      | TransactionGroupFilter.PlatformPayment ->
         "Payments between orgs on the platform"
   //| TransactionGroupFilter.ThirdPartyPayment ->
   //   "Payments outside the platform"

   static member All = [
      TransactionGroupFilter.Purchase
      TransactionGroupFilter.Deposit
      TransactionGroupFilter.InternalTransferWithinOrg
      TransactionGroupFilter.InternalTransferBetweenOrgs
      TransactionGroupFilter.InternalAutomatedTransfer
      TransactionGroupFilter.DomesticTransfer
      TransactionGroupFilter.PlatformPayment
   ]

   static member fromString =
      function
      | "Purchase" -> Some TransactionGroupFilter.Purchase
      | "Deposit" -> Some TransactionGroupFilter.Deposit
      | "InternalTransferWithinOrg" ->
         Some TransactionGroupFilter.InternalTransferWithinOrg
      | "InternalTransferBetweenOrgs" ->
         Some TransactionGroupFilter.InternalTransferBetweenOrgs
      | "InternalAutomatedTransfer" ->
         Some TransactionGroupFilter.InternalAutomatedTransfer
      | "DomesticTransfer" -> Some TransactionGroupFilter.DomesticTransfer
      | "PlatformPayment" -> Some TransactionGroupFilter.PlatformPayment
      //| "ThirdPartyPayment" -> Some TransactionGroupFilter.ThirdPartyPayment
      | _ -> None

   static member fromQueryString: string -> TransactionGroupFilter list option =
      listFromQueryString TransactionGroupFilter.fromString

   static member listToDisplay(items: TransactionGroupFilter list) =
      List.fold
         (fun acc (filter: TransactionGroupFilter) ->
            if acc = "" then
               filter.Display
            else
               $"{acc}, {filter.Display}")
         ""
         items

/// Indicates the oldest History item within a "page".
type HistoryCursor = {
   Timestamp: DateTime
   EventId: EventId
}

type HistoryQuery = {
   PageLimit: int
   Cursor: HistoryCursor option
   DateRange: (DateTime * DateTime) option
   EmployeeEventType:
      (Bank.Employee.Domain.EmployeeEventGroupFilter list) option
   AccountEventType: (TransactionGroupFilter list) option
   OrgEventType: (OrgEventGroupFilter list) option
   InitiatedByIds: (InitiatedById list) option
}

module HistoryQuery =
   let initiatedByIdsFromQueryString: string -> InitiatedById list option =
      listFromQueryString (
         Guid.parseOptional >> Option.map (EmployeeId >> InitiatedById)
      )

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
   EventType: (TransactionGroupFilter list) option
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
