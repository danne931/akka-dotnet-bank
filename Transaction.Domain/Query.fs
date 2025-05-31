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

   member x.Display =
      match x with
      | OrgEventGroupFilter.Onboarding -> "Org Onboarding"
      | OrgEventGroupFilter.FeatureFlagConfigured ->
         "Org Feature Flag Configured"
      | OrgEventGroupFilter.CommandApprovalRule -> "Command Approval Rule"
      | OrgEventGroupFilter.CommandApprovalProgress ->
         "Command Approval Progress"

   static member All = [
      OrgEventGroupFilter.Onboarding
      OrgEventGroupFilter.FeatureFlagConfigured
      OrgEventGroupFilter.CommandApprovalRule
      OrgEventGroupFilter.CommandApprovalProgress
   ]

   static member fromString =
      function
      | "Onboarding" -> Some OrgEventGroupFilter.Onboarding
      | "FeatureFlagConfigured" ->
         Some OrgEventGroupFilter.FeatureFlagConfigured
      | "CommandApprovalRule" -> Some OrgEventGroupFilter.CommandApprovalRule
      | "CommandApprovalProgress" ->
         Some OrgEventGroupFilter.CommandApprovalProgress
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
type ParentAccountEventGroupFilter =
   | DomesticTransferRecipient

   member x.Display =
      match x with
      | ParentAccountEventGroupFilter.DomesticTransferRecipient ->
         "Domestic Transfer Recipient"

   static member All = [
      ParentAccountEventGroupFilter.DomesticTransferRecipient
   ]

   static member fromString =
      function
      | "DomesticTransferRecipient" ->
         Some ParentAccountEventGroupFilter.DomesticTransferRecipient
      | _ -> None

   static member fromQueryString
      : string -> ParentAccountEventGroupFilter list option =
      listFromQueryString ParentAccountEventGroupFilter.fromString

   static member listToDisplay(items: ParentAccountEventGroupFilter list) =
      List.fold
         (fun acc (filter: ParentAccountEventGroupFilter) ->
            if acc = "" then
               filter.Display
            else
               $"{acc}, {filter.Display}")
         ""
         items

[<RequireQualifiedAccess>]
type AccountEventGroupFilter =
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
      | AccountEventGroupFilter.PlatformPayment ->
         "Payments between orgs on the platform"
   //| AccountEventGroupFilter.ThirdPartyPayment ->
   //   "Payments outside the platform"

   static member All = [
      AccountEventGroupFilter.Purchase
      AccountEventGroupFilter.Deposit
      AccountEventGroupFilter.InternalTransferWithinOrg
      AccountEventGroupFilter.InternalTransferBetweenOrgs
      AccountEventGroupFilter.InternalAutomatedTransfer
      AccountEventGroupFilter.DomesticTransfer
      AccountEventGroupFilter.PlatformPayment
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
      | "PlatformPayment" -> Some AccountEventGroupFilter.PlatformPayment
      //| "ThirdPartyPayment" -> Some AccountEventGroupFilter.ThirdPartyPayment
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
   AccountEventType: (AccountEventGroupFilter list) option
   ParentAccountEventType: (ParentAccountEventGroupFilter list) option
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
