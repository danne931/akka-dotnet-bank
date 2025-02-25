namespace Bank.Org.Domain

open System

open Lib.SharedTypes
open Lib.NetworkQuery

type Merchant = {
   OrgId: OrgId
   Name: string
   Alias: string option
}

type FeatureFlagOrgSettings = {
   SocialTransferDiscoveryPrimaryAccountId: AccountId option
}

[<RequireQualifiedAccess>]
type OrgStatus =
   | InitialEmptyState
   | PendingOnboardingTasksFulfilled
   | Active

[<RequireQualifiedAccess>]
type OrgAccrualMetricEventType =
   | PaymentPaid
   | InternalTransferBetweenOrgs
   | DomesticTransfer

/// This is currently used to restrict command approval requests
/// with corresponding AmountDailyLimit rules configured but may
/// have additional uses in the future.
type OrgAccrualMetric = {
   TransactionAmount: decimal
   EventType: OrgAccrualMetricEventType
   CorrelationId: CorrelationId
   InitiatedBy: Initiator
   Timestamp: System.DateTime
}

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

module OrgEventGroupFilter =
   let fromString =
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

   let fromQueryString: string -> OrgEventGroupFilter list option =
      listFromQueryString fromString

   let listToDisplay (items: OrgEventGroupFilter list) =
      List.fold
         (fun acc (filter: OrgEventGroupFilter) ->
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
   AccountEventType: (Bank.Account.Domain.TransactionGroupFilter list) option
   OrgEventType: (OrgEventGroupFilter list) option
   InitiatedByIds: (InitiatedById list) option
}

module HistoryQuery =
   let initiatedByIdsFromQueryString: string -> InitiatedById list option =
      listFromQueryString (
         Guid.parseOptional >> Option.map (EmployeeId >> InitiatedById)
      )
