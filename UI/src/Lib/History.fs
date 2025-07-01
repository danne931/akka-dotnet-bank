module UIDomain.History

open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Employee.Domain
open UIDomain.Org
open Lib.SharedTypes
open Lib.NetworkQuery
open Lib.Time
open CommandApproval
open Transaction

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

let orgHistoryUIFriendly (history: OrgHistory) : HistoryUIFriendly =
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

   match history.Event with
   | OrgEvent.OnboardingApplicationSubmitted _ -> {
      props with
         Info = "Organization onboarding application submitted"
     }
   | OrgEvent.OnboardingFinished _ -> {
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
   | EmployeeEvent.PurchasePending e ->
      let info = e.Data.Info

      {
         props with
            Name = "Purchase Pending"
            Info =
               $"Purchase processing at {info.Merchant} with card {info.CardNickname} **{info.CardNumberLast4}"
            Amount = Some <| Money.format info.Amount
      }
   | EmployeeEvent.PurchaseSettled e ->
      let info = e.Data.Info

      {
         props with
            Name = "Purchase Settled"
            Info =
               $"Purchase by {txn.EmployeeName} at {info.Merchant} deducted from card {info.CardNickname} **{info.CardNumberLast4}"
            Amount = Some <| Money.format info.Amount
      }
   | EmployeeEvent.PurchaseFailed e ->
      let info = e.Data.Info
      let card = $"{info.CardNickname} **{info.CardNumberLast4}"

      {
         props with
            Name = "Purchase Failed"
            Info =
               $"Failed purchase for {txn.EmployeeName}'s card {card} at {info.Merchant} due to {e.Data.Reason}"
            Amount = Some <| Money.format info.Amount
      }
   | EmployeeEvent.PurchaseRefunded e ->
      let info = e.Data.Info
      let card = $"{info.CardNickname} **{info.CardNumberLast4}"

      {
         props with
            Name = "Purchase Refunded to Card"
            Info =
               $"Refunded purchase for {txn.EmployeeName}'s card {card} at {info.Merchant} due to {e.Data.Reason}"
            Amount = Some <| Money.format info.Amount
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
   | EmployeeEvent.ThirdPartyProviderCardLinked e -> {
      props with
         Name = "Provider Card Linked"
         Info =
            $"Provider card linked to **{e.Data.CardNumberLast4} for {txn.EmployeeName}"
     }
   | EmployeeEvent.CreatedAccountOwner _ -> {
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
   | EmployeeEvent.AccessRestored _ -> {
      props with
         Name = "Access Restored"
         Info = $"Employee access restored for {txn.EmployeeName}"
     }
   | EmployeeEvent.ConfiguredRollingPurchaseLimit e ->
      let o = e.Data

      let daily =
         if o.PriorDailyLimit <> o.DailyLimit then
            $" Daily ({Money.format o.PriorDailyLimit} -> {Money.format o.DailyLimit})"
         else
            ""

      let monthly =
         if o.PriorMonthlyLimit <> o.MonthlyLimit then
            $" Monthly ({Money.format o.PriorMonthlyLimit} -> {Money.format o.MonthlyLimit})"
         else
            ""

      {
         props with
            Name = "Purchase Limits Updated"
            Info =
               $"Updated purchase limits {daily} {monthly} for {txn.EmployeeName}'s card **{o.CardNumberLast4}"
      }
   | EmployeeEvent.CardNicknamed e -> {
      props with
         Name = "Card Nickname Updated"
         Info =
            $"Card nickname updated from {e.Data.PriorName} to {e.Data.Name} for {txn.EmployeeName}'s card"
     }
   | EmployeeEvent.InvitationConfirmed _ -> {
      props with
         Name = "Invitation Confirmed"
         Info = "Invitation confirmed"
     }
   | EmployeeEvent.InvitationCancelled _ -> {
      props with
         Name = "Invitation Cancelled"
         Info = "Invitation cancelled"
     }
   | EmployeeEvent.InvitationTokenRefreshed _ -> {
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
   let _, envelope = AccountEnvelope.unwrap history.Event

   let props = {
      Id = envelope.Id
      Name = envelope.EventName
      Date = DateTime.dateUIFriendlyWithSeconds envelope.Timestamp
      Initiator = history.InitiatedByName
      Amount = None
      Info = ""
      MoneyFlow = None
   }

   let domesticRecipientName (recipientFromEvt: DomesticTransferRecipient) =
      org.DomesticTransferRecipients
      |> Map.tryFind recipientFromEvt.RecipientAccountId
      |> Option.map _.FullName
      |> Option.defaultValue recipientFromEvt.FullName

   let accountName =
      org.AccountProfiles.TryFind history.Event.AccountId
      |> Option.map (fun a -> a.Account.Name)
      |> Option.defaultValue "Account"

   match history.Event with
   | InitializedPrimaryCheckingAccount _ -> {
      props with
         Info = "Initialized Primary Checking Account"
     }
   | CreatedVirtualAccount _ -> { props with Info = "Created Account" }
   | DepositedCash evt -> {
      props with
         Name = "Deposit Received"
         Info = $"Deposited money into {accountName}."
         MoneyFlow = Some MoneyFlow.In
         Amount = Some <| Money.format evt.Data.Amount
     }
   | DebitPending evt ->
      let em = evt.Data.EmployeePurchaseReference
      let card = $"{em.CardNickname} **{em.EmployeeCardNumberLast4}"

      {
         props with
            Name = "Purchase"
            Info =
               $"Pending purchase from {evt.Data.Merchant} with card {card} to account ({accountName})"
            Amount = Some <| Money.format evt.Data.Amount
      }
   | DebitSettled evt ->
      let em = evt.Data.EmployeePurchaseReference

      {
         props with
            Name = "Purchase Settled"
            Info =
               $"Purchase by {em.EmployeeName} at {evt.Data.Merchant} deducted from account {accountName} "
            Amount = Some <| Money.format evt.Data.Amount
            MoneyFlow = Some MoneyFlow.Out
      }
   | DebitFailed e -> {
      props with
         Name = "Debit Failed"
         Info = $"Failed debit to account {accountName} due to {e.Data.Reason}"
         Amount = Some <| Money.format e.Data.Amount
     }
   | DebitRefunded e -> {
      props with
         Name = "Debit Refunded to Account"
         Info =
            $"Refunded debit to account {accountName} at {e.Data.Merchant} due to {e.Data.Reason}."
         Amount = Some <| Money.format e.Data.Amount
         MoneyFlow = Some MoneyFlow.In
     }
   | MaintenanceFeeDebited evt -> {
      props with
         Info = "Maintenance Fee"
         Amount = Some <| Money.format evt.Data.Amount
         MoneyFlow = Some MoneyFlow.Out
     }
   | MaintenanceFeeSkipped _ -> {
      props with
         Info = "Skipped Maintenance Fee"
     }
   | InternalTransferWithinOrgDeducted evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Internal Transfer"
            Info =
               $"Moved money from {info.Sender.Name} to {info.Recipient.Name}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.Out
      }
   | InternalTransferWithinOrgDeposited evt ->
      let info = evt.Data.BaseInfo
      let sender = info.Sender.Name

      {
         props with
            Name = "Transfer Deposit Within Org"
            Info = $"Transfer received from {sender} into {accountName}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.In
      }
   | InternalTransferBetweenOrgsScheduled evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Transfer Between Orgs Scheduled"
            Info =
               $"Transfer scheduled from {accountName} to {info.Recipient.Name} for {DateTime.formatShort info.ScheduledDate}"
            Amount = Some <| Money.format info.Amount
      }
   | InternalTransferBetweenOrgsPending evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Transfer Between Orgs"
            Info =
               $"Transfer processing from {accountName} to {info.Recipient.Name}"
            Amount = Some <| Money.format info.Amount
      }
   | InternalTransferBetweenOrgsDeposited evt ->
      let info = evt.Data.BaseInfo
      let sender = info.Sender.Name

      {
         props with
            Name = "Transfer Deposit Between Orgs"
            Info = $"Transfer received from {sender} into {accountName}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.In
      }
   | InternalTransferBetweenOrgsSettled evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Transfer Between Orgs Settled"
            Info =
               $"Transfer settled from {accountName} to {info.Recipient.Name}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.Out
      }
   | InternalTransferBetweenOrgsFailed evt ->
      let info = evt.Data.BaseInfo

      {
         props with
            Name = "Transfer Between Orgs Failed"
            Info =
               $"Transfer failed from {accountName} to {info.Recipient.Name}
              - Reason: {evt.Data.Reason} 
              - Account refunded"
            Amount = Some <| Money.format info.Amount
      }
   | DomesticTransferScheduled evt ->
      let info = evt.Data.BaseInfo
      let recipientName = domesticRecipientName info.Recipient
      let payNetwork = info.Recipient.PaymentNetwork

      {
         props with
            Name = "Domestic Transfer"
            Info =
               $"{payNetwork} transfer scheduled from {info.Sender.Name} to {recipientName} for {DateTime.formatShort info.ScheduledDate}"
            Amount = Some <| Money.format info.Amount
      }
   | DomesticTransferPending evt ->
      let info = evt.Data.BaseInfo
      let recipientName = domesticRecipientName info.Recipient
      let payNetwork = info.Recipient.PaymentNetwork

      {
         props with
            Name = "Domestic Transfer"
            Info =
               $"{payNetwork} transfer processing from {info.Sender.Name} to {recipientName}"
            Amount = Some <| Money.format info.Amount
      }
   | DomesticTransferSettled evt ->
      let info = evt.Data.BaseInfo
      let payNetwork = info.Recipient.PaymentNetwork

      {
         props with
            Name = "Domestic Transfer Settled"
            Info =
               $"{payNetwork} transfer settled from {info.Sender.Name} to {domesticRecipientName info.Recipient}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.Out
      }
   | DomesticTransferFailed evt ->
      let info = evt.Data.BaseInfo

      let recipientName = domesticRecipientName info.Recipient
      let payNetwork = info.Recipient.PaymentNetwork

      {
         props with
            Name = "Domestic Transfer Failed"
            Info =
               $"{payNetwork} transfer failed from {accountName} to {recipientName}
               - Reason: {evt.Data.Reason.Display}"
            Amount = Some <| Money.format info.Amount
      }
   | DomesticTransferProgress evt ->
      let info = evt.Data.BaseInfo
      let payNetwork = info.Recipient.PaymentNetwork
      let recipientName = domesticRecipientName info.Recipient

      {
         props with
            Info =
               $"Progress update received for {payNetwork} transfer 
                 to {recipientName} from {accountName}
                 - {evt.Data.InProgressInfo}"
            Amount = Some <| Money.format info.Amount
      }
   | AccountClosed evt -> {
      props with
         Info = $"Closed Account - Reference: {evt.Data.Reference}"
     }
   | PlatformPaymentRequested evt ->
      let p = evt.Data.BaseInfo

      {
         props with
            Name = "Payment Requested"
            Info =
               if evt.OrgId = org.Org.OrgId then
                  $"Payment request sent to {p.Payer.OrgName}"
               else
                  $"Received payment request from {p.Payee.OrgName}"
            Amount = Some <| Money.format p.Amount
      }
   | PlatformPaymentPending evt ->
      let p = evt.Data.BaseInfo

      {
         props with
            Name = "Payment Pending"
            Info = $"Payment pending to {p.Payee.OrgName} from {accountName}."
            Amount = Some <| Money.format p.Amount
      }
   | PlatformPaymentDeposited evt ->
      let p = evt.Data.BaseInfo

      {
         props with
            Name = "Payment Deposited"
            Info =
               $"Payment received from {p.Payer.OrgName} into {accountName}."
            Amount = Some <| Money.format p.Amount
            MoneyFlow = Some MoneyFlow.In
      }
   | PlatformPaymentFailed evt ->
      let p = evt.Data.BaseInfo

      {
         props with
            Name = "Payment Refunded"
            Info =
               $"Payment refunded to {p.Payer.OrgName} due to {evt.Data.Reason}."
            Amount = Some <| Money.format p.Amount
      }
   | PlatformPaymentCancelled evt ->
      let p = evt.Data.BaseInfo

      {
         props with
            Name = "Payment Cancelled"
            Info = $"Payment request cancelled to {p.Payer.OrgName}"
            Amount = Some <| Money.format p.Amount
      }
   | PlatformPaymentDeclined evt ->
      let p = evt.Data.BaseInfo

      {
         props with
            Name = "Payment Declined"
            Info = $"Payment request declined by {p.Payer.OrgName}"
            Amount = Some <| Money.format p.Amount
      }
   | PlatformPaymentSettled evt ->
      let p = evt.Data.BaseInfo

      {
         props with
            Name = "Payment Settled"
            Info = $"Payment settled to {p.Payee.OrgName}"
            Amount = Some <| Money.format p.Amount
            MoneyFlow = Some MoneyFlow.Out
      }
   | AutoTransferRuleConfigured evt -> {
      props with
         Name = "Auto Transfer Rule Configured"
         Info = $"Created auto transfer rule {evt.Data.Config.Info.Display}"
     }
   | AutoTransferRuleDeleted _ -> {
      props with
         Name = "Auto Transfer Rule Deleted"
         Info = $"Deleted auto transfer rule"
     }
   | InternalAutomatedTransferDeducted evt ->
      let info = evt.Data.BaseInfo
      let reason = UIDomain.Account.autoTransferRuleDisplay evt.Data.Rule

      {
         props with
            Name = "Auto Balance Management"
            Info =
               $"Automatically moved money from {info.Sender.Name} to {info.Recipient.Name}
               - Reason: {reason}"
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.Out
      }
   | InternalAutomatedTransferDeposited evt ->
      let info = evt.Data.BaseInfo
      let sender = info.Sender.Name

      {
         props with
            Name = "Internal Automated Transfer Deposit"
            Info =
               $"Auto Balance Management: {accountName} received transfer from {sender}."
            Amount = Some <| Money.format info.Amount
            MoneyFlow = Some MoneyFlow.In
      }
   | ParentAccount evt ->
      match evt with
      | ParentAccountEvent.RegisteredDomesticTransferRecipient evt ->
         let name = domesticRecipientName evt.Data.Recipient

         {
            props with
               Info = $"Created domestic recipient: {name}"
         }
      | ParentAccountEvent.EditedDomesticTransferRecipient evt ->
         let name = domesticRecipientName evt.Data.Recipient

         {
            props with
               Info = $"Edited recipient: {name}"
         }
      | ParentAccountEvent.NicknamedDomesticTransferRecipient evt -> {
         props with
            Info =
               match evt.Data.Nickname with
               | None -> "Removed recipient nickname."
               | Some name -> $"Updated recipient nickname to {name}"
        }

let historyUIFriendly
   (org: OrgWithAccountProfiles)
   (history: History)
   : HistoryUIFriendly
   =
   match history with
   | History.Org h -> orgHistoryUIFriendly h
   | History.Employee h -> employeeHistoryUIFriendly h
   | History.Account h -> accountHistoryUIFriendly org h
   | History.ParentAccount h ->
      let h: AccountHistory = {
         InitiatedByName = h.InitiatedByName
         Event = AccountEvent.ParentAccount h.Event
      }

      accountHistoryUIFriendly org h

let private matchesOrgEventFilter
   (event: OrgEvent)
   (filter: OrgEventGroupFilter)
   =
   match filter with
   | OrgEventGroupFilter.Onboarding ->
      match event with
      | OrgEvent.OnboardingApplicationSubmitted _
      | OrgEvent.OnboardingFinished _ -> true
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

let private matchesParentAccountEventFilter
   (event: ParentAccountEvent)
   (filter: ParentAccountEventGroupFilter)
   =
   match filter with
   | ParentAccountEventGroupFilter.DomesticTransferRecipient ->
      match event with
      | ParentAccountEvent.RegisteredDomesticTransferRecipient _
      | ParentAccountEvent.EditedDomesticTransferRecipient _
      | ParentAccountEvent.NicknamedDomesticTransferRecipient _ -> true

let private matchesAccountEventFilter
   (event: AccountEvent)
   (filter: AccountEventGroupFilter)
   =
   match filter with
   | AccountEventGroupFilter.Purchase ->
      match event with
      | AccountEvent.DebitPending _
      | AccountEvent.DebitSettled _
      | AccountEvent.DebitRefunded _
      | AccountEvent.DebitFailed _ -> true
      | _ -> false
   | AccountEventGroupFilter.Deposit ->
      match event with
      | AccountEvent.DepositedCash _ -> true
      | _ -> false
   | AccountEventGroupFilter.InternalTransferWithinOrg ->
      match event with
      | AccountEvent.InternalTransferWithinOrgDeducted _
      | AccountEvent.InternalTransferWithinOrgDeposited _ -> true
      | _ -> false
   | AccountEventGroupFilter.InternalTransferBetweenOrgs ->
      match event with
      | AccountEvent.InternalTransferBetweenOrgsScheduled _
      | AccountEvent.InternalTransferBetweenOrgsPending _
      | AccountEvent.InternalTransferBetweenOrgsFailed _
      | AccountEvent.InternalTransferBetweenOrgsDeposited _
      | AccountEvent.InternalTransferBetweenOrgsSettled _ -> true
      | _ -> false
   | AccountEventGroupFilter.InternalAutomatedTransfer ->
      match event with
      | AccountEvent.InternalAutomatedTransferDeducted _
      | AccountEvent.InternalAutomatedTransferDeposited _ -> true
      | _ -> false
   | AccountEventGroupFilter.DomesticTransfer ->
      match event with
      | AccountEvent.DomesticTransferScheduled _
      | AccountEvent.DomesticTransferPending _
      | AccountEvent.DomesticTransferSettled _
      | AccountEvent.DomesticTransferFailed _
      | AccountEvent.DomesticTransferProgress _ -> true
      | _ -> false
   | AccountEventGroupFilter.PlatformPayment ->
      match event with
      | AccountEvent.PlatformPaymentRequested _
      | AccountEvent.PlatformPaymentPending _
      | AccountEvent.PlatformPaymentDeposited _
      | AccountEvent.PlatformPaymentFailed _
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
      | EmployeeEvent.PurchaseSettled _
      | EmployeeEvent.PurchaseFailed _
      | EmployeeEvent.PurchaseRefunded _ -> true
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
      | EmployeeEvent.ConfiguredRollingPurchaseLimit _ -> true
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
      | History.ParentAccount o ->
         o.Event |> AccountEvent.ParentAccount |> AccountEnvelope.unwrap |> snd
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
            initiatedById = envelope.InitiatedBy.Id)

   let qualifiedEventType =
      match
         history,
         query.OrgEventType,
         query.AccountEventType,
         query.ParentAccountEventType,
         query.EmployeeEventType
      with
      | _, None, None, None, None -> true
      | History.Org o, orgEventFilter, _, _, _ ->
         match orgEventFilter with
         | None -> false
         | Some filters ->
            filters |> List.exists (matchesOrgEventFilter o.Event)
      | History.Account o, _, accountEventFilter, _, _ ->
         match accountEventFilter with
         | None -> false
         | Some filters ->
            filters |> List.exists (matchesAccountEventFilter o.Event)
      | History.ParentAccount o, _, _, eventFilter, _ ->
         match eventFilter with
         | None -> false
         | Some filters ->
            filters |> List.exists (matchesParentAccountEventFilter o.Event)
      | History.Employee o, _, _, _, employeeEventFilter ->
         match employeeEventFilter with
         | None -> false
         | Some filters ->
            filters |> List.exists (matchesEmployeeEventFilter o.Event)

   qualifiedDate && qualifiedInitiator && qualifiedEventType

type HistoryBrowserQuery = {
   Date: UIDomain.DateFilter option
   OrgEventType: (OrgEventGroupFilter list) option
   EmployeeEventType: (EmployeeEventGroupFilter list) option
   AccountEventType: (AccountEventGroupFilter list) option
   ParentAccountEventType: (ParentAccountEventGroupFilter list) option
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

      let agg =
         match query.ParentAccountEventType with
         | None -> agg
         | Some filters ->
            ("parentAccountEventFilters", listToQueryString filters) :: agg

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
            |> Option.bind AccountEventGroupFilter.fromQueryString
         ParentAccountEventType =
            Map.tryFind "parentAccountEventFilters" queryParams
            |> Option.bind ParentAccountEventGroupFilter.fromQueryString
         SelectedInitiatedBy =
            Map.tryFind "initiatedBy" queryParams
            |> Option.bind UIDomain.Employee.parseEmployees
      }

   let empty: HistoryBrowserQuery = {
      Date = None
      OrgEventType = None
      EmployeeEventType = None
      AccountEventType = None
      ParentAccountEventType = None
      SelectedInitiatedBy = None
   }
