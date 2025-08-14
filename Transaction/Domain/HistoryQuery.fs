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
