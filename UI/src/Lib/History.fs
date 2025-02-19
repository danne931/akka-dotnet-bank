module History

open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Employee.Domain
open UIDomain.Org
open Lib.SharedTypes
open Lib.NetworkQuery

type HistoryMaybe = Result<History list option, Err>

type HistoryUIFriendly = {
   Id: EventId
   Name: string
   Date: string
   Amount: string option
   Info: string
   MoneyFlow: MoneyFlow option
   Initiator: string
}

let orgHistoryUIFriendly (org: Org) (history: OrgHistory) : HistoryUIFriendly =
   let _, envelope = OrgEnvelope.unwrap history.Event

   let props = {
      Id = envelope.Id
      Name = envelope.EventName
      Date = DateTime.dateUIFriendlyWithSeconds envelope.Timestamp
      Amount = None
      Info = ""
      MoneyFlow = None
      Initiator = history.InitiatedByName
   }

   let domesticRecipientName (recipientAccountId: AccountId) =
      org.DomesticTransferRecipients
      |> Map.tryFind recipientAccountId
      |> Option.map _.FullName

   match history.Event with
   | RegisteredDomesticTransferRecipient evt ->
      let name =
         domesticRecipientName evt.Data.Recipient.RecipientAccountId
         |> Option.defaultValue evt.Data.Recipient.FullName

      {
         props with
            Info = $"Created domestic recipient: {name}"
      }
   | OrgEvent.DomesticTransferRetryConfirmsRecipient evt -> {
      props with
         Info =
            $"Recipient confirmed upon transfer retry: {domesticRecipientName evt.Data.RecipientId}"
     }
   | OrgEvent.DomesticTransferRecipientFailed evt -> {
      props with
         Info =
            $"Recipient {domesticRecipientName evt.Data.RecipientId} invalid due to {evt.Data.Reason}"
     }
   | EditedDomesticTransferRecipient evt ->
      let name =
         domesticRecipientName evt.Data.Recipient.RecipientAccountId
         |> Option.defaultValue evt.Data.Recipient.FullName

      {
         props with
            Info = $"Edited recipient: {name}"
      }
   | NicknamedDomesticTransferRecipient evt -> {
      props with
         Info =
            match evt.Data.Nickname with
            | None -> "Removed recipient nickname."
            | Some name -> $"Updated recipient nickname to {name}"
     }
   | OrgEvent.OrgCreated _ -> {
      props with
         Info = "Organization created"
     }
   | OrgEvent.OrgOnboardingFinished _ -> {
      props with
         Info = "Organization onboarding finished"
     }
   | OrgEvent.FeatureFlagConfigured e -> {
      props with
         Info = $"Organization feature flag updated: {e.Data}"
     }
   | OrgEvent.CommandApprovalRuleConfigured e -> {
      props with
         Info =
            $"Configured command approval rule for {e.Data.Rule.CommandType.Display}"
     }
   | OrgEvent.CommandApprovalRuleDeleted e -> {
      props with
         Info = $"Deleted {e.Data.CommandType.Display} command approval rule"
     }
   | OrgEvent.CommandApprovalRequested e -> {
      props with
         Info =
            $"Requested approval for {ApprovableCommand.displayVerbose e.Data.Command}"
     }
   | OrgEvent.CommandApprovalAcquired e -> {
      props with
         Info =
            $"Approval acquired for {ApprovableCommand.displayVerbose e.Data.Command}"
     }
   | OrgEvent.CommandApprovalDeclined e -> {
      props with
         Info =
            $"Approval declined for {ApprovableCommand.displayVerbose e.Data.Command}"
     }
   | OrgEvent.CommandApprovalProcessCompleted e -> {
      props with
         Info =
            $"Approval complete for {ApprovableCommand.displayVerbose e.Data.Command}"
     }
   | OrgEvent.CommandApprovalTerminated e -> {
      props with
         Info =
            $"Approval terminated early for {ApprovableCommand.displayVerbose e.Data.Command} due to {e.Data.Reason}"
     }

let employeeHistoryUIFriendly (txn: EmployeeHistory) : HistoryUIFriendly =
   let _, envelope = EmployeeEnvelope.unwrap txn.Event

   let props = {
      Id = envelope.Id
      Date = DateTime.dateUIFriendlyWithSeconds envelope.Timestamp
      Name = ""
      Amount = None
      MoneyFlow = None
      Initiator = txn.InitiatedByName
      Info = ""
   }

   match txn.Event with
   | EmployeeEvent.PurchasePending e -> {
      props with
         Name = "Purchase Pending"
         Info =
            $"Purchase requested by {txn.EmployeeName} at {e.Data.Info.Merchant} with card {e.Data.Info.CardNumberLast4}"
         Amount = Some <| Money.format e.Data.Info.Amount
     }
   | EmployeeEvent.PurchaseConfirmedByAccount e -> {
      props with
         Name = "Purchase Confirmed by Account"
         Info =
            $"Purchase confirmed by account for {txn.EmployeeName} at {e.Data.Info.Merchant} with card {e.Data.Info.CardNumberLast4}"
         Amount = Some <| Money.format e.Data.Info.Amount
     }
   | EmployeeEvent.PurchaseRejectedByAccount e -> {
      props with
         Name = "Purchase Rejected by Account"
         Info =
            $"Purchase rejected by account for {txn.EmployeeName} at {e.Data.Info.Merchant} with card {e.Data.Info.CardNumberLast4}"
         Amount = Some <| Money.format e.Data.Info.Amount
     }
   | EmployeeEvent.CreatedEmployee e -> {
      props with
         Name =
            match e.Data.OrgRequiresEmployeeInviteApproval with
            | Some _ -> "Employee Creation Pending Approval"
            | None -> "Employee Created"
         Info =
            match e.Data.OrgRequiresEmployeeInviteApproval with
            | Some _ ->
               $"Employee creation for {txn.EmployeeName} with role {e.Data.Role.Display} pending approval"
            | None ->
               $"Created employee {txn.EmployeeName} with role {e.Data.Role.Display}"
     }
   | EmployeeEvent.CreatedCard e -> {
      props with
         Name = "Card Created"
         Info =
            $"Created card **{e.Data.Card.CardNumberLast4} for {txn.EmployeeName}"
     }
   | EmployeeEvent.CreatedAccountOwner e -> {
      props with
         Name = "Account Owner Created"
         Info = $"Created account owner {txn.EmployeeName}"
     }
   | EmployeeEvent.UpdatedRole e -> {
      props with
         Name = "Updated Role"
         Info =
            $"Updated role of {txn.EmployeeName} from {e.Data.PriorRole.Display} to {e.Data.Role.Display}"
     }
   | EmployeeEvent.LockedCard e -> {
      props with
         Name = "Locked Card"
         Info = $"Locked card {e.Data.CardNumberLast4} for {txn.EmployeeName}"
     }
   | EmployeeEvent.UnlockedCard e -> {
      props with
         Name = "Unlocked Card"
         Info = $"Unlocked card {e.Data.CardNumberLast4} for {txn.EmployeeName}"
     }
   | EmployeeEvent.AccessRestored e -> {
      props with
         Name = "Access Restored"
         Info = $"Employee access restored for {txn.EmployeeName}"
     }
   | EmployeeEvent.DailyDebitLimitUpdated e -> {
      props with
         Name = "Daily Purchase Limit Updated"
         Info =
            $"Updated daily purchase limit from ${e.Data.PriorLimit} to ${e.Data.DebitLimit} for {txn.EmployeeName}'s card **{e.Data.CardNumberLast4}"
     }
   | EmployeeEvent.MonthlyDebitLimitUpdated e -> {
      props with
         Name = "Monthly Purchase Limit Updated"
         Info =
            $"Updated monthly purchase limit from ${e.Data.PriorLimit} to ${e.Data.DebitLimit} for {txn.EmployeeName}'s card **{e.Data.CardNumberLast4}"
     }
   | EmployeeEvent.CardNicknamed e -> {
      props with
         Name = "Card Nickname Updated"
         Info =
            $"Card nickname updated from {e.Data.PriorName} to {e.Data.Name} for {txn.EmployeeName}'s card"
     }
   | EmployeeEvent.InvitationConfirmed e -> {
      props with
         Name = "Invitation Confirmed"
         Info = "Invitation confirmed"
     }
   | EmployeeEvent.InvitationCancelled e -> {
      props with
         Name = "Invitation Cancelled"
         Info = "Invitation cancelled"
     }
   | EmployeeEvent.InvitationTokenRefreshed e -> {
      props with
         Name = "Invitation Token Refreshed"
         Info = "Invitation token refreshed"
     }
   | EmployeeEvent.AccessApproved _ -> {
      props with
         Name = "Employee Access Approved"
         Info = $"Employee access approved for {txn.EmployeeName}"
     }

let accountHistoryUIFriendly
   (org: OrgWithAccountProfiles)
   (history: AccountHistory)
   : HistoryUIFriendly
   =
   let props =
      UIDomain.Account.transactionUIFriendlyFromAccountEvent org history.Event

   let _, envelope = AccountEnvelope.unwrap history.Event

   {
      Id = envelope.Id
      Name = props.Name
      Date = props.Date
      Amount = props.Amount
      Info = props.Info
      MoneyFlow = props.MoneyFlow
      Initiator = history.InitiatedByName
   }

let historyUIFriendly
   (org: OrgWithAccountProfiles)
   (history: History)
   : HistoryUIFriendly
   =
   match history with
   | History.Org h -> orgHistoryUIFriendly org.Org h
   | History.Employee h -> employeeHistoryUIFriendly h
   | History.Account h -> accountHistoryUIFriendly org h

type HistoryBrowserQuery = {
   Date: UIDomain.DateFilter option
   OrgEventType: (OrgEventGroupFilter list) option
   EmployeeEventType: (EmployeeEventGroupFilter list) option
   AccountEventType: (TransactionGroupFilter list) option
   SelectedInitiatedBy: (UIDomain.Employee.SelectedEmployee list) option
}

module HistoryBrowserQuery =
   let toQueryParams (query: HistoryBrowserQuery) : (string * string) list =
      let agg = []

      let agg =
         match query.SelectedInitiatedBy with
         | None -> agg
         | Some initiatedBy ->
            ("initiatedBy", Serialization.serialize initiatedBy) :: agg

      let agg =
         match query.OrgEventType with
         | None -> agg
         | Some filters -> ("orgEventFilters", listToQueryString filters) :: agg

      let agg =
         match query.EmployeeEventType with
         | None -> agg
         | Some filters ->
            ("employeeEventFilters", listToQueryString filters) :: agg

      let agg =
         match query.AccountEventType with
         | None -> agg
         | Some filters ->
            ("accountEventFilters", listToQueryString filters) :: agg

      // If custom date range selected, date query param will consist
      // of a start & end date.  Otherwise it will be something like
      // date=Last30Days; date=LastMonth; etc.
      let agg =
         match query.Date with
         | None -> agg
         | Some(UIDomain.DateFilter.Custom(startDate, endDate)) ->
            ("date", DateTime.rangeAsQueryString startDate endDate) :: agg
         | Some filter -> ("date", string filter) :: agg

      agg

   let fromQueryParams
      (queryParams: (string * string) list)
      : HistoryBrowserQuery
      =
      let queryParams = Map.ofList queryParams

      {
         Date =
            Map.tryFind "date" queryParams
            |> Option.bind UIDomain.DateFilter.fromString
         OrgEventType =
            Map.tryFind "orgEventFilters" queryParams
            |> Option.bind OrgEventGroupFilter.fromQueryString
         EmployeeEventType =
            Map.tryFind "employeeEventFilters" queryParams
            |> Option.bind EmployeeEventGroupFilter.fromQueryString
         AccountEventType =
            Map.tryFind "accountEventFilters" queryParams
            |> Option.bind TransactionGroupFilter.fromQueryString
         SelectedInitiatedBy =
            Map.tryFind "initiatedBy" queryParams
            |> Option.bind UIDomain.Employee.parseEmployees
      }

   let empty: HistoryBrowserQuery = {
      Date = None
      OrgEventType = None
      EmployeeEventType = None
      AccountEventType = None
      SelectedInitiatedBy = None
   }
