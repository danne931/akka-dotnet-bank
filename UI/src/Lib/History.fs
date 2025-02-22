module History

open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Employee.Domain
open UIDomain.Org
open Lib.SharedTypes
open Lib.NetworkQuery
open CommandApproval

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

let amountFromApprovableCommand cmd =
   match cmd with
   | ApprovableCommand.AmountBased _ -> Some(Money.format cmd.Amount)
   | _ -> None

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
            $"Configured command approval rule for {e.Data.Rule.CommandType.Display}: "
            + $" {CommandApprovalRule.displayVerbose e.Data.Rule}"
     }
   | OrgEvent.CommandApprovalRuleDeleted e -> {
      props with
         Info = $"Deleted {e.Data.CommandType.Display} command approval rule"
     }
   | OrgEvent.CommandApprovalRequested e -> {
      props with
         Info =
            $"Requested approval for {ApprovableCommand.displayVerbose e.Data.Command}"
         Amount = amountFromApprovableCommand e.Data.Command
     }
   | OrgEvent.CommandApprovalAcquired e -> {
      props with
         Info =
            $"Approval acquired for {ApprovableCommand.displayVerbose e.Data.Command}"
         Amount = amountFromApprovableCommand e.Data.Command
     }
   | OrgEvent.CommandApprovalDeclined e -> {
      props with
         Info =
            $"Approval declined for {ApprovableCommand.displayVerbose e.Data.Command}"
         Amount = amountFromApprovableCommand e.Data.Command
     }
   | OrgEvent.CommandApprovalProcessCompleted e -> {
      props with
         Info =
            $"Approval complete for {ApprovableCommand.displayVerbose e.Data.Command}"
         Amount = amountFromApprovableCommand e.Data.Command
     }
   | OrgEvent.CommandApprovalTerminated e -> {
      props with
         Info =
            $"Approval terminated early for {ApprovableCommand.displayVerbose e.Data.Command} due to {e.Data.Reason}"
         Amount = amountFromApprovableCommand e.Data.Command
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
         Info =
            $"Locked card {e.Data.CardName} **{e.Data.CardNumberLast4} for {txn.EmployeeName}"
     }
   | EmployeeEvent.UnlockedCard e -> {
      props with
         Name = "Unlocked Card"
         Info =
            $"Unlocked card {e.Data.CardName} **{e.Data.CardNumberLast4} for {txn.EmployeeName}"
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

let private matchesOrgEventFilter
   (event: OrgEvent)
   (filter: OrgEventGroupFilter)
   =
   match filter with
   | OrgEventGroupFilter.Onboarding ->
      match event with
      | OrgEvent.OrgCreated _
      | OrgEvent.OrgOnboardingFinished _ -> true
      | _ -> false
   | OrgEventGroupFilter.FeatureFlagConfigured ->
      match event with
      | OrgEvent.FeatureFlagConfigured _ -> true
      | _ -> false
   | OrgEventGroupFilter.CommandApprovalRule ->
      match event with
      | OrgEvent.CommandApprovalRuleConfigured _
      | OrgEvent.CommandApprovalRuleDeleted _ -> true
      | _ -> false
   | OrgEventGroupFilter.CommandApprovalProgress ->
      match event with
      | OrgEvent.CommandApprovalRequested _
      | OrgEvent.CommandApprovalAcquired _
      | OrgEvent.CommandApprovalDeclined _
      | OrgEvent.CommandApprovalTerminated _
      | OrgEvent.CommandApprovalProcessCompleted _ -> true
      | _ -> false
   | OrgEventGroupFilter.DomesticTransferRecipient ->
      match event with
      | OrgEvent.RegisteredDomesticTransferRecipient _
      | OrgEvent.EditedDomesticTransferRecipient _
      | OrgEvent.NicknamedDomesticTransferRecipient _
      | OrgEvent.DomesticTransferRetryConfirmsRecipient _
      | OrgEvent.DomesticTransferRecipientFailed _ -> true
      | _ -> false

let private matchesAccountEventFilter
   (event: AccountEvent)
   (filter: TransactionGroupFilter)
   =
   match filter with
   | TransactionGroupFilter.Purchase ->
      match event with
      | AccountEvent.DebitedAccount _ -> true
      | _ -> false
   | TransactionGroupFilter.Deposit ->
      match event with
      | AccountEvent.DepositedCash _ -> true
      | _ -> false
   | TransactionGroupFilter.InternalTransferWithinOrg ->
      match event with
      | AccountEvent.InternalTransferWithinOrgPending _
      | AccountEvent.InternalTransferWithinOrgCompleted _
      | AccountEvent.InternalTransferWithinOrgFailed _
      | AccountEvent.InternalTransferWithinOrgDeposited _ -> true
      | _ -> false
   | TransactionGroupFilter.InternalTransferBetweenOrgs ->
      match event with
      | AccountEvent.InternalTransferBetweenOrgsPending _
      | AccountEvent.InternalTransferBetweenOrgsCompleted _
      | AccountEvent.InternalTransferBetweenOrgsFailed _
      | AccountEvent.InternalTransferBetweenOrgsDeposited _ -> true
      | _ -> false
   | TransactionGroupFilter.InternalAutomatedTransfer ->
      match event with
      | AccountEvent.InternalAutomatedTransferPending _
      | AccountEvent.InternalAutomatedTransferCompleted _
      | AccountEvent.InternalAutomatedTransferFailed _
      | AccountEvent.InternalAutomatedTransferDeposited _ -> true
      | _ -> false
   | TransactionGroupFilter.DomesticTransfer ->
      match event with
      | AccountEvent.DomesticTransferPending _
      | AccountEvent.DomesticTransferCompleted _
      | AccountEvent.DomesticTransferFailed _
      | AccountEvent.DomesticTransferProgress _ -> true
      | _ -> false
   | TransactionGroupFilter.PlatformPayment ->
      match event with
      | AccountEvent.PlatformPaymentRequested _
      | AccountEvent.PlatformPaymentPaid _
      | AccountEvent.PlatformPaymentDeposited _
      | AccountEvent.PlatformPaymentDeclined _
      | AccountEvent.PlatformPaymentCancelled _ -> true
      | _ -> false

let private matchesEmployeeEventFilter
   (event: EmployeeEvent)
   (filter: EmployeeEventGroupFilter)
   =
   match filter with
   | EmployeeEventGroupFilter.Invitation ->
      match event with
      | EmployeeEvent.CreatedEmployee _
      | EmployeeEvent.InvitationConfirmed _
      | EmployeeEvent.InvitationCancelled _ -> true
      | _ -> false
   | EmployeeEventGroupFilter.CardFrozenUnfrozen ->
      match event with
      | EmployeeEvent.LockedCard _
      | EmployeeEvent.UnlockedCard _ -> true
      | _ -> false
   | EmployeeEventGroupFilter.AccessRestored ->
      match event with
      | EmployeeEvent.AccessRestored _ -> true
      | _ -> false
   | EmployeeEventGroupFilter.Purchase ->
      match event with
      | EmployeeEvent.PurchasePending _
      | EmployeeEvent.PurchaseConfirmedByAccount _
      | EmployeeEvent.PurchaseRejectedByAccount _ -> true
      | _ -> false
   | EmployeeEventGroupFilter.CreatedCard ->
      match event with
      | EmployeeEvent.CreatedCard _ -> true
      | _ -> false
   | EmployeeEventGroupFilter.UpdatedRole ->
      match event with
      | EmployeeEvent.UpdatedRole _ -> true
      | _ -> false
   | EmployeeEventGroupFilter.PurchaseLimitUpdated ->
      match event with
      | EmployeeEvent.DailyDebitLimitUpdated _
      | EmployeeEvent.MonthlyDebitLimitUpdated _ -> true
      | _ -> false

/// Apply the selected filter logic to events arriving via SignalR.
/// Events fetched from the network query will be filtered via the
/// database query but still need to apply an in-browser filter for
/// events arriving via SignalR.
let keepRealtimeEventsCorrespondingToSelectedFilter
   (query: HistoryQuery)
   (history: History)
   =
   let envelope =
      match history with
      | History.Account o -> AccountEnvelope.unwrap o.Event |> snd
      | History.Employee o -> EmployeeEnvelope.unwrap o.Event |> snd
      | History.Org o -> OrgEnvelope.unwrap o.Event |> snd

   let qualifiedDate =
      match query.DateRange with
      | None -> true
      | Some(start, finish) ->
         let timestamp = envelope.Timestamp.ToLocalTime()
         timestamp >= start && timestamp <= finish

   let qualifiedInitiator =
      match query.InitiatedByIds with
      | None -> true
      | Some initiators ->
         initiators
         |> List.exists (fun initiatedById ->
            initiatedById = envelope.InitiatedById)

   let qualifiedEventType =
      match
         history,
         query.OrgEventType,
         query.AccountEventType,
         query.EmployeeEventType
      with
      | _, None, None, None -> true
      | History.Org o, orgEventFilter, _, _ ->
         match orgEventFilter with
         | None -> false
         | Some filters ->
            filters |> List.exists (matchesOrgEventFilter o.Event)
      | History.Account o, _, accountEventFilter, _ ->
         match accountEventFilter with
         | None -> false
         | Some filters ->
            filters |> List.exists (matchesAccountEventFilter o.Event)
      | History.Employee o, _, _, employeeEventFilter ->
         match employeeEventFilter with
         | None -> false
         | Some filters ->
            filters |> List.exists (matchesEmployeeEventFilter o.Event)

   qualifiedDate && qualifiedInitiator && qualifiedEventType

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
