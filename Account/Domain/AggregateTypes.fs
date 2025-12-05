namespace Bank.Account.Domain

open System

open Lib.SharedTypes
open Bank.Transfer.Domain
open Bank.Payment.Domain
open MaintenanceFee
open AutomaticTransfer

/// Commands which pertain to the parent account rather
/// than one of the subacounts.
[<RequireQualifiedAccess>]
type ParentAccountCommand =
   | RegisterCounterparty of RegisterCounterpartyCommand
   | EditCounterparty of EditCounterpartyCommand
   | NicknameCounterparty of NicknameCounterpartyCommand

[<RequireQualifiedAccess>]
type AccountCommand =
   | InitializePrimaryCheckingAccount of InitializePrimaryCheckingAccountCommand
   | CreateVirtualAccount of CreateVirtualAccountCommand
   | DepositCash of DepositCashCommand
   | Debit of DebitCommand
   | SettleDebit of SettleDebitCommand
   | FailDebit of FailDebitCommand
   | RefundDebit of RefundDebitCommand
   | MaintenanceFee of MaintenanceFeeCommand
   | SkipMaintenanceFee of SkipMaintenanceFeeCommand
   | InternalTransfer of InternalTransferWithinOrgCommand
   | DepositTransferWithinOrg of DepositInternalTransferWithinOrgCommand
   | ScheduleInternalTransferBetweenOrgs of
      ScheduleInternalTransferBetweenOrgsCommand
   | InternalTransferBetweenOrgs of InternalTransferBetweenOrgsCommand
   | FailInternalTransferBetweenOrgs of FailInternalTransferBetweenOrgsCommand
   | DepositTransferBetweenOrgs of DepositInternalTransferBetweenOrgsCommand
   | SettleInternalTransferBetweenOrgs of
      SettleInternalTransferBetweenOrgsCommand
   | ScheduleDomesticTransfer of ScheduleDomesticTransferCommand
   | DomesticTransfer of DomesticTransferCommand
   | UpdateDomesticTransferProgress of UpdateDomesticTransferProgressCommand
   | SettleDomesticTransfer of SettleDomesticTransferCommand
   | FailDomesticTransfer of FailDomesticTransferCommand
   | RequestPayment of RequestPaymentCommand
   | CancelPaymentRequest of CancelPaymentRequestCommand
   | DeclinePaymentRequest of DeclinePaymentRequestCommand
   | CloseAccount of CloseAccountCommand
   | ConfigureAutoTransferRule of ConfigureAutoTransferRuleCommand
   | DeleteAutoTransferRule of DeleteAutoTransferRuleCommand
   | InternalAutoTransfer of InternalAutoTransferCommand
   | DepositInternalAutoTransfer of DepositInternalAutoTransferCommand
   | ParentAccount of ParentAccountCommand

   member x.Envelope: Envelope =
      match x with
      | InitializePrimaryCheckingAccount cmd -> Command.envelope cmd
      | CreateVirtualAccount cmd -> Command.envelope cmd
      | DepositCash cmd -> Command.envelope cmd
      | Debit cmd -> Command.envelope cmd
      | SettleDebit cmd -> Command.envelope cmd
      | FailDebit cmd -> Command.envelope cmd
      | RefundDebit cmd -> Command.envelope cmd
      | MaintenanceFee cmd -> Command.envelope cmd
      | SkipMaintenanceFee cmd -> Command.envelope cmd
      | InternalTransfer cmd -> Command.envelope cmd
      | ScheduleInternalTransferBetweenOrgs cmd -> Command.envelope cmd
      | InternalTransferBetweenOrgs cmd -> Command.envelope cmd
      | SettleInternalTransferBetweenOrgs cmd -> Command.envelope cmd
      | FailInternalTransferBetweenOrgs cmd -> Command.envelope cmd
      | DepositTransferWithinOrg cmd -> Command.envelope cmd
      | DepositTransferBetweenOrgs cmd -> Command.envelope cmd
      | ScheduleDomesticTransfer cmd -> Command.envelope cmd
      | DomesticTransfer cmd -> Command.envelope cmd
      | UpdateDomesticTransferProgress cmd -> Command.envelope cmd
      | SettleDomesticTransfer cmd -> Command.envelope cmd
      | FailDomesticTransfer cmd -> Command.envelope cmd
      | RequestPayment cmd -> Command.envelope cmd
      | CancelPaymentRequest cmd -> Command.envelope cmd
      | DeclinePaymentRequest cmd -> Command.envelope cmd
      | CloseAccount cmd -> Command.envelope cmd
      | ConfigureAutoTransferRule cmd -> Command.envelope cmd
      | DeleteAutoTransferRule cmd -> Command.envelope cmd
      | InternalAutoTransfer cmd -> Command.envelope cmd
      | DepositInternalAutoTransfer cmd -> Command.envelope cmd
      | ParentAccount cmd ->
         match cmd with
         | ParentAccountCommand.RegisterCounterparty cmd -> Command.envelope cmd
         | ParentAccountCommand.EditCounterparty cmd -> Command.envelope cmd
         | ParentAccountCommand.NicknameCounterparty cmd -> Command.envelope cmd

   member x.AccountId =
      match x with
      | InitializePrimaryCheckingAccount _ -> AccountId Guid.Empty
      | CreateVirtualAccount cmd -> cmd.Data.AccountId
      | DepositCash cmd -> cmd.Data.AccountId
      | Debit cmd -> cmd.Data.AccountId
      | SettleDebit cmd -> cmd.Data.AccountId
      | FailDebit cmd -> cmd.Data.AccountId
      | RefundDebit cmd -> cmd.Data.AccountId
      | MaintenanceFee cmd -> cmd.Data.AccountId
      | SkipMaintenanceFee cmd -> cmd.Data.AccountId
      | InternalTransfer cmd -> cmd.Data.Sender.AccountId
      | ScheduleInternalTransferBetweenOrgs cmd ->
         cmd.Data.TransferInput.Sender.AccountId
      | InternalTransferBetweenOrgs cmd -> cmd.Data.Sender.AccountId
      | SettleInternalTransferBetweenOrgs cmd ->
         cmd.Data.BaseInfo.Sender.AccountId
      | FailInternalTransferBetweenOrgs cmd ->
         cmd.Data.BaseInfo.Sender.AccountId
      | DepositTransferWithinOrg cmd -> cmd.Data.BaseInfo.Recipient.AccountId
      | DepositTransferBetweenOrgs cmd -> cmd.Data.BaseInfo.Recipient.AccountId
      | ScheduleDomesticTransfer cmd ->
         cmd.Data.TransferInput.Originator.AccountId
      | DomesticTransfer cmd -> cmd.Data.Originator.AccountId
      | UpdateDomesticTransferProgress cmd ->
         cmd.Data.BaseInfo.Originator.AccountId
      | SettleDomesticTransfer cmd -> cmd.Data.BaseInfo.Originator.AccountId
      | FailDomesticTransfer cmd -> cmd.Data.BaseInfo.Originator.AccountId
      | RequestPayment cmd -> cmd.Data.SharedDetails.Payee.AccountId
      | CancelPaymentRequest cmd -> cmd.Data.SharedDetails.Payee.AccountId
      | DeclinePaymentRequest cmd -> cmd.Data.SharedDetails.Payee.AccountId
      | CloseAccount cmd -> cmd.Data.AccountId
      | ConfigureAutoTransferRule cmd -> cmd.Data.AccountId
      | DeleteAutoTransferRule cmd -> cmd.Data.AccountId
      | InternalAutoTransfer cmd -> cmd.Data.Transfer.Sender.AccountId
      | DepositInternalAutoTransfer cmd -> cmd.Data.BaseInfo.Recipient.AccountId
      // Commands pertaining to the parent account do not have
      // an AccountId, only a ParentAccountId.
      | ParentAccount _ -> AccountId Guid.Empty

[<RequireQualifiedAccess>]
type ParentAccountEvent =
   | RegisteredCounterparty of BankEvent<RegisteredCounterparty>
   | EditedCounterparty of BankEvent<EditedCounterparty>
   | NicknamedCounterparty of BankEvent<NicknamedCounterparty>

type AccountEvent =
   | InitializedPrimaryCheckingAccount of
      BankEvent<InitializedPrimaryCheckingAccount>
   | CreatedVirtualAccount of BankEvent<CreatedVirtualAccount>
   | DepositedCash of BankEvent<DepositedCash>
   | DebitPending of BankEvent<DebitPending>
   | DebitSettled of BankEvent<DebitSettled>
   | DebitRefunded of BankEvent<DebitRefunded>
   | DebitFailed of BankEvent<DebitFailed>
   | MaintenanceFeeDebited of BankEvent<MaintenanceFeeDebited>
   | MaintenanceFeeSkipped of BankEvent<MaintenanceFeeSkipped>
   | InternalTransferWithinOrgDeducted of
      BankEvent<InternalTransferWithinOrgDeducted>
   | InternalTransferWithinOrgDeposited of
      BankEvent<InternalTransferWithinOrgDeposited>
   | InternalTransferBetweenOrgsScheduled of
      BankEvent<InternalTransferBetweenOrgsScheduled>
   | InternalTransferBetweenOrgsPending of
      BankEvent<InternalTransferBetweenOrgsPending>
   | InternalTransferBetweenOrgsFailed of
      BankEvent<InternalTransferBetweenOrgsFailed>
   | InternalTransferBetweenOrgsDeposited of
      BankEvent<InternalTransferBetweenOrgsDeposited>
   | InternalTransferBetweenOrgsSettled of
      BankEvent<InternalTransferBetweenOrgsSettled>
   | DomesticTransferScheduled of BankEvent<DomesticTransferScheduled>
   | DomesticTransferPending of BankEvent<DomesticTransferPending>
   | DomesticTransferProgress of BankEvent<DomesticTransferProgressUpdated>
   | DomesticTransferSettled of BankEvent<DomesticTransferSettled>
   | DomesticTransferFailed of BankEvent<DomesticTransferFailed>
   | PaymentRequested of BankEvent<PaymentRequested>
   | PaymentRequestCancelled of BankEvent<PaymentRequestCancelled>
   | PaymentRequestDeclined of BankEvent<PaymentRequestDeclined>
   | AccountClosed of BankEvent<AccountClosed>
   | AutoTransferRuleConfigured of BankEvent<AutomaticTransferRuleConfigured>
   | AutoTransferRuleDeleted of BankEvent<AutomaticTransferRuleDeleted>
   | InternalAutomatedTransferDeducted of
      BankEvent<InternalAutomatedTransferDeducted>
   | InternalAutomatedTransferDeposited of
      BankEvent<InternalAutomatedTransferDeposited>
   | ParentAccount of ParentAccountEvent

   member x.AccountId =
      match x with
      | InitializedPrimaryCheckingAccount evt ->
         evt.Data.PrimaryChecking.AccountId
      | CreatedVirtualAccount evt -> evt.Data.AccountId
      | DepositedCash evt -> evt.Data.AccountId
      | DebitPending evt -> evt.Data.AccountId
      | DebitSettled evt -> evt.Data.AccountId
      | DebitRefunded evt -> evt.Data.AccountId
      | DebitFailed evt -> evt.Data.AccountId
      | MaintenanceFeeDebited evt -> evt.Data.AccountId
      | MaintenanceFeeSkipped evt -> evt.Data.AccountId
      | InternalTransferWithinOrgDeducted evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | InternalTransferWithinOrgDeposited evt ->
         evt.Data.BaseInfo.Recipient.AccountId
      | InternalTransferBetweenOrgsScheduled evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | InternalTransferBetweenOrgsPending evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | InternalTransferBetweenOrgsDeposited evt ->
         evt.Data.BaseInfo.Recipient.AccountId
      | InternalTransferBetweenOrgsSettled evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | InternalTransferBetweenOrgsFailed evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | DomesticTransferScheduled evt -> evt.Data.BaseInfo.Originator.AccountId
      | DomesticTransferPending evt -> evt.Data.BaseInfo.Originator.AccountId
      | DomesticTransferProgress evt -> evt.Data.BaseInfo.Originator.AccountId
      | DomesticTransferSettled evt -> evt.Data.BaseInfo.Originator.AccountId
      | DomesticTransferFailed evt -> evt.Data.BaseInfo.Originator.AccountId
      | PaymentRequested evt -> evt.Data.SharedDetails.Payee.AccountId
      | PaymentRequestCancelled evt -> evt.Data.SharedDetails.Payee.AccountId
      | PaymentRequestDeclined evt -> evt.Data.SharedDetails.Payee.AccountId
      | AccountClosed evt -> evt.Data.AccountId
      | AutoTransferRuleConfigured evt -> evt.Data.AccountId
      | AutoTransferRuleDeleted evt -> evt.Data.AccountId
      | InternalAutomatedTransferDeducted evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | InternalAutomatedTransferDeposited evt ->
         evt.Data.BaseInfo.Recipient.AccountId
      | ParentAccount _ -> AccountId Guid.Empty

module AccountEvent =
   let moneyTransaction =
      function
      | AccountEvent.DepositedCash evt ->
         Some evt.Data.Amount, Some MoneyFlow.In, Some evt.Data.Origin
      | AccountEvent.DebitPending evt ->
         Some evt.Data.Amount, Some MoneyFlow.Out, Some evt.Data.Merchant.Value
      | AccountEvent.DebitFailed evt ->
         Some evt.Data.Amount, None, Some evt.Data.Merchant.Value
      | AccountEvent.DebitSettled evt ->
         let clearing = evt.Data.Clearing.ClearedAmount

         Some clearing.Amount, Some clearing.Flow, Some evt.Data.Merchant.Value
      | AccountEvent.DebitRefunded evt ->
         Some evt.Data.Amount, Some MoneyFlow.In, Some evt.Data.Merchant.Value
      | AccountEvent.InternalTransferWithinOrgDeducted evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.Out,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.InternalTransferWithinOrgDeposited evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.In,
         Some evt.Data.BaseInfo.Sender.Name
      | AccountEvent.InternalAutomatedTransferDeducted evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.Out,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.InternalAutomatedTransferDeposited evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.In,
         Some evt.Data.BaseInfo.Sender.Name
      | AccountEvent.InternalTransferBetweenOrgsScheduled evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.Out,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.InternalTransferBetweenOrgsPending evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.Out,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.InternalTransferBetweenOrgsDeposited evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.In,
         Some evt.Data.BaseInfo.Sender.Name
      | AccountEvent.InternalTransferBetweenOrgsSettled evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.Out,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.InternalTransferBetweenOrgsFailed evt ->
         Some evt.Data.BaseInfo.Amount,
         None,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.DomesticTransferScheduled evt ->
         Some evt.Data.BaseInfo.Amount,
         Some evt.Data.BaseInfo.MoneyFlow,
         Some evt.Data.BaseInfo.Counterparty.Name
      | AccountEvent.DomesticTransferPending evt ->
         Some evt.Data.BaseInfo.Amount,
         Some evt.Data.BaseInfo.MoneyFlow,
         Some evt.Data.BaseInfo.Counterparty.Name
      | AccountEvent.DomesticTransferSettled evt ->
         Some evt.Data.BaseInfo.Amount,
         Some evt.Data.BaseInfo.MoneyFlow,
         Some evt.Data.BaseInfo.Counterparty.Name
      | AccountEvent.DomesticTransferFailed evt ->
         Some evt.Data.BaseInfo.Amount,
         None,
         Some evt.Data.BaseInfo.Counterparty.Name
      | AccountEvent.MaintenanceFeeDebited evt ->
         Some evt.Data.Amount, Some MoneyFlow.Out, Some "Maintenance Fee"
      | AccountEvent.PaymentRequested evt ->
         let p = evt.Data.SharedDetails
         Some p.Amount, None, Some p.Payee.OrgName
      | AccountEvent.PaymentRequestCancelled evt ->
         let p = evt.Data.SharedDetails
         Some p.Amount, None, Some p.Payee.OrgName
      | AccountEvent.PaymentRequestDeclined evt ->
         let p = evt.Data.SharedDetails
         Some p.Amount, None, Some p.Payee.OrgName
      | _ -> None, None, None

type OpenEventEnvelope = AccountEvent * Envelope

[<RequireQualifiedAccess>]
module AccountEnvelope =
   let get (evt: BankEvent<'E>) : Envelope = {
      Id = evt.Id
      EntityId = evt.EntityId
      OrgId = evt.OrgId
      CorrelationId = evt.CorrelationId
      InitiatedBy = evt.InitiatedBy
      Timestamp = evt.Timestamp
      EventName = evt.EventName
   }

   let wrap (o: BankEvent<_>) : AccountEvent =
      match box o with
      | :? BankEvent<InitializedPrimaryCheckingAccount> as evt ->
         InitializedPrimaryCheckingAccount evt
      | :? BankEvent<CreatedVirtualAccount> as evt -> CreatedVirtualAccount evt
      | :? BankEvent<DepositedCash> as evt -> DepositedCash evt
      | :? BankEvent<DebitPending> as evt -> DebitPending evt
      | :? BankEvent<DebitSettled> as evt -> DebitSettled evt
      | :? BankEvent<DebitRefunded> as evt -> DebitRefunded evt
      | :? BankEvent<DebitFailed> as evt -> DebitFailed evt
      | :? BankEvent<MaintenanceFeeDebited> as evt -> MaintenanceFeeDebited evt
      | :? BankEvent<MaintenanceFeeSkipped> as evt -> MaintenanceFeeSkipped evt
      | :? BankEvent<InternalTransferWithinOrgDeducted> as evt ->
         InternalTransferWithinOrgDeducted evt
      | :? BankEvent<InternalTransferWithinOrgDeposited> as evt ->
         InternalTransferWithinOrgDeposited evt
      | :? BankEvent<InternalTransferBetweenOrgsScheduled> as evt ->
         InternalTransferBetweenOrgsScheduled evt
      | :? BankEvent<InternalTransferBetweenOrgsPending> as evt ->
         InternalTransferBetweenOrgsPending evt
      | :? BankEvent<InternalTransferBetweenOrgsSettled> as evt ->
         InternalTransferBetweenOrgsSettled evt
      | :? BankEvent<InternalTransferBetweenOrgsFailed> as evt ->
         InternalTransferBetweenOrgsFailed evt
      | :? BankEvent<InternalTransferBetweenOrgsDeposited> as evt ->
         InternalTransferBetweenOrgsDeposited evt
      | :? BankEvent<DomesticTransferScheduled> as evt ->
         DomesticTransferScheduled evt
      | :? BankEvent<DomesticTransferPending> as evt ->
         DomesticTransferPending evt
      | :? BankEvent<DomesticTransferProgressUpdated> as evt ->
         DomesticTransferProgress evt
      | :? BankEvent<DomesticTransferSettled> as evt ->
         DomesticTransferSettled evt
      | :? BankEvent<DomesticTransferFailed> as evt ->
         DomesticTransferFailed evt
      | :? BankEvent<PaymentRequested> as evt -> PaymentRequested evt
      | :? BankEvent<PaymentRequestCancelled> as evt ->
         PaymentRequestCancelled evt
      | :? BankEvent<PaymentRequestDeclined> as evt ->
         PaymentRequestDeclined evt
      | :? BankEvent<AccountClosed> as evt -> AccountClosed evt
      | :? BankEvent<AutomaticTransferRuleConfigured> as evt ->
         AutoTransferRuleConfigured evt
      | :? BankEvent<AutomaticTransferRuleDeleted> as evt ->
         AutoTransferRuleDeleted evt
      | :? BankEvent<InternalAutomatedTransferDeducted> as evt ->
         InternalAutomatedTransferDeducted evt
      | :? BankEvent<InternalAutomatedTransferDeposited> as evt ->
         InternalAutomatedTransferDeposited evt
      | :? BankEvent<RegisteredCounterparty> as evt ->
         evt
         |> ParentAccountEvent.RegisteredCounterparty
         |> AccountEvent.ParentAccount
      | :? BankEvent<EditedCounterparty> as evt ->
         evt
         |> ParentAccountEvent.EditedCounterparty
         |> AccountEvent.ParentAccount
      | :? BankEvent<NicknamedCounterparty> as evt ->
         evt
         |> ParentAccountEvent.NicknamedCounterparty
         |> AccountEvent.ParentAccount
      | _ -> failwith "Missing definition for AccountEvent message"

   let unwrap (o: AccountEvent) : OpenEventEnvelope =
      match o with
      | InitializedPrimaryCheckingAccount evt -> wrap evt, get evt
      | CreatedVirtualAccount evt -> wrap evt, get evt
      | DepositedCash evt -> wrap evt, get evt
      | DebitPending evt -> wrap evt, get evt
      | DebitFailed evt -> wrap evt, get evt
      | DebitSettled evt -> wrap evt, get evt
      | DebitRefunded evt -> wrap evt, get evt
      | MaintenanceFeeDebited evt -> wrap evt, get evt
      | MaintenanceFeeSkipped evt -> wrap evt, get evt
      | InternalTransferWithinOrgDeducted evt -> wrap evt, get evt
      | InternalTransferWithinOrgDeposited evt -> wrap evt, get evt
      | InternalTransferBetweenOrgsScheduled evt -> wrap evt, get evt
      | InternalTransferBetweenOrgsPending evt -> wrap evt, get evt
      | InternalTransferBetweenOrgsDeposited evt -> wrap evt, get evt
      | InternalTransferBetweenOrgsSettled evt -> wrap evt, get evt
      | InternalTransferBetweenOrgsFailed evt -> wrap evt, get evt
      | DomesticTransferScheduled evt -> wrap evt, get evt
      | DomesticTransferPending evt -> wrap evt, get evt
      | DomesticTransferProgress evt -> wrap evt, get evt
      | DomesticTransferSettled evt -> wrap evt, get evt
      | DomesticTransferFailed evt -> wrap evt, get evt
      | PaymentRequested evt -> wrap evt, get evt
      | PaymentRequestCancelled evt -> wrap evt, get evt
      | PaymentRequestDeclined evt -> wrap evt, get evt
      | AccountClosed evt -> wrap evt, get evt
      | AutoTransferRuleConfigured evt -> wrap evt, get evt
      | AutoTransferRuleDeleted evt -> wrap evt, get evt
      | InternalAutomatedTransferDeducted evt -> wrap evt, get evt
      | InternalAutomatedTransferDeposited evt -> wrap evt, get evt
      | ParentAccount evt ->
         match evt with
         | ParentAccountEvent.RegisteredCounterparty evt -> wrap evt, get evt
         | ParentAccountEvent.EditedCounterparty evt -> wrap evt, get evt
         | ParentAccountEvent.NicknamedCounterparty evt -> wrap evt, get evt

type Account = {
   AccountId: AccountId
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   Name: string
   Depository: AccountDepository
   Currency: Currency
   Status: AccountStatus
   Balance: decimal
   PendingFunds: PendingFunds
   AccountNumber: AccountNumber
   RoutingNumber: RoutingNumber
   AutoTransferRule: AutomaticTransferConfig option
} with

   member x.CompositeId = x.AccountId, x.ParentAccountId, x.OrgId

   member x.FullName = $"{x.Name} **{x.AccountNumber.Last4}"

   member x.AvailableBalance =
      let pendingTally = PendingFunds.amount x.PendingFunds
      x.Balance - pendingTally.Out

   member private x.autoTransferManagement
      (computedTransferFromRule:
         AutomaticTransferRule
            -> decimal
            -> AutoTransferDerivedFromRule list option)
      : AutoTransferDerivedFromRule list
      =
      x.AutoTransferRule
      |> Option.bind (fun conf ->
         computedTransferFromRule conf.Info x.AvailableBalance)
      |> Option.defaultValue []

   member x.AutoTransfersPerTransaction =
      x.autoTransferManagement
         AutomaticTransfer.requiresPerTransactionBalanceManagement

   member x.AutoTransfersDaily =
      x.autoTransferManagement AutomaticTransfer.requiresDailyBalanceManagement

   member x.AutoTransfersTwiceMonthly =
      x.autoTransferManagement
         AutomaticTransfer.requiresTwiceMonthlyBalanceManagement

   member x.AutoTransfers =
      x.autoTransferManagement AutomaticTransfer.computeTransfer

   static member empty: Account = {
      AccountId = AccountId Guid.Empty
      OrgId = OrgId Guid.Empty
      ParentAccountId = ParentAccountId Guid.Empty
      Name = ""
      Depository = AccountDepository.Checking
      Currency = Currency.USD
      Status = AccountStatus.InitialEmptyState
      Balance = 0m
      PendingFunds = PendingFunds.zero
      AccountNumber = AccountNumber.Empty
      RoutingNumber = RoutingNumber.Empty
      AutoTransferRule = None
   }

/// If the account actor receives a message with an ID equivalent
/// to an event we already persisted then we ignore it.  This ignores
/// messages that may be duplicated due to AtLeastOnceDelivery.
/// However, we need to have slightly different behavior if the
/// message comes from a saga actor since it has retry capabilities.
/// If the saga actor has an in-progress activity which has reached its
/// InactivityTimeout (it did not complete within the expected time),
/// then it will resend the message to the account actor using
/// the same ID. So, in that case the saga actor wants to receive an
/// ACK that the account actor indeed processed the message so it can
/// complete it's activity.
/// Scenario:
///    1. Account event persisted
///    2. message sent from account actor to saga actor
///    3. message lost in translation instead of delivered
///    (probably will be a rare situation since I am delivering most
///     messages to saga actors via RabbitMQ but better safe than sorry)
/// Reconciliation:
///    1. saga actor receives EvaluateRemainingWork message a few
///       seconds/minutes into the future due to having an in-progress activity
///       with an expired InactivityTimeout
///    2. saga actor reattempts delivery of the message to the account actor
///    3. account actor receives message and checks Outbox to see
///       if it has already associated an outbox message with the same ID
///    4. since an outgoing saga message exists in the Outbox, it will
///       resend that saga message
///    5. saga actor will receive the message, complete the activity, and resume
///       processing next steps
type AccountOutboxMessage =
   // NOTE:
   // AppSaga.AppSagaMessage in Saga/Domain directory depends
   // on Account/Domain directory so can not reference it here.
   // Instead will use obj here and interpret it in the account actor.
   // Good enough for now.
   | Saga of obj

type ParentAccountSnapshot = {
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   PartnerBankLink: PartnerBankInternalAccountLink
   PrimaryVirtualAccountId: AccountId
   VirtualAccounts: Map<AccountId, Account>
   LastBillingCycleDate: DateTime option
   MaintenanceFeeCriteria: MaintenanceFeeCriteria
   Status: ParentAccountStatus
   Counterparties: Map<CounterpartyId, Counterparty>
   Events: AccountEvent list
   ProcessedCommands: Map<EventId, DateTime>
   Outbox: Map<EventId, DateTime * AccountOutboxMessage>
} with

   static member empty: ParentAccountSnapshot = {
      OrgId = OrgId Guid.Empty
      ParentAccountId = ParentAccountId Guid.Empty
      PartnerBankLink = {
         AccountNumber = PartnerBankAccountNumber AccountNumber.Empty
         RoutingNumber = PartnerBankRoutingNumber RoutingNumber.Empty
         PartnerBankAccountId = PartnerBankAccountId ""
         PartnerBankLegalEntityId = PartnerBankLegalEntityId ""
      }
      PrimaryVirtualAccountId = AccountId Guid.Empty
      VirtualAccounts = Map.empty
      LastBillingCycleDate = None
      Status = ParentAccountStatus.InitialEmptyState
      MaintenanceFeeCriteria = {
         QualifyingDepositFound = false
         DailyBalanceThreshold = false
      }
      Counterparties = Map.empty
      Events = []
      ProcessedCommands = Map.empty
      Outbox = Map.empty
   }

   member x.eventsForAccount(accountId: AccountId) =
      x.Events |> List.filter (fun evt -> evt.AccountId = accountId)

   member x.Balance = x.VirtualAccounts.Values |> Seq.sumBy _.Balance

   member x.PrimaryVirtualAccountCompositeId =
      x.PrimaryVirtualAccountId, x.ParentAccountId, x.OrgId

   /// External funding source we can pull money from or transfer money to.
   /// (We own these entities so can directly link our bank data via Plaid)
   /// Ex: We link our Chase bank account
   member x.ExternalFundingSources =
      x.Counterparties
      |> Map.filter (fun _ counterparty ->
         counterparty.Kind = CounterpartyType.FundingSource)

   /// External trading partners we can send money to or request money from.
   /// (These accounts belong to entities outside our control so we need to
   /// email invite these entities to contribute their bank data via Plaid)
   member x.ExternalTradingPartners =
      x.Counterparties
      |> Map.filter (fun _ counterparty ->
         counterparty.Kind = CounterpartyType.TradingPartner)

type AccountMetrics = {
   DailyInternalTransferWithinOrg: decimal
   DailyInternalTransferBetweenOrgs: decimal
   DailyDomesticTransfer: decimal
   DailyPurchase: decimal
   MonthlyInternalTransferWithinOrg: decimal
   MonthlyInternalTransferBetweenOrgs: decimal
   MonthlyDomesticTransfer: decimal
   MonthlyPurchase: decimal
}

module AccountMetrics =
   let empty: AccountMetrics = {
      DailyInternalTransferWithinOrg = 0m
      DailyInternalTransferBetweenOrgs = 0m
      DailyDomesticTransfer = 0m
      DailyPurchase = 0m
      MonthlyInternalTransferWithinOrg = 0m
      MonthlyInternalTransferBetweenOrgs = 0m
      MonthlyDomesticTransfer = 0m
      MonthlyPurchase = 0m
   }

type AccountProfile = {
   Account: Account
   Metrics: AccountMetrics
}

[<RequireQualifiedAccess>]
type AccountMessage =
   | GetAccount
   | GetVirtualAccount of AccountId
   | StateChange of AccountCommand
   | AutoTransferCompute of AutomaticTransfer.Frequency * AccountId
   | ProcessBillingStatement of CorrelationId * BillingPeriod
   | DomesticTransfersRetryableUponRecipientEdit of
      Result<DomesticTransfer list option, Err>
   | Delete
   | PruneIdempotencyChecker
   | PruneOutbox

[<RequireQualifiedAccess>]
type AccountClosureMessage =
   | Register of Account
   | ScheduleDeleteAll
   | DeleteAll of AccountId list
   | GetRegisteredAccounts

[<RequireQualifiedAccess>]
type AccountSeederMessage =
   | SeedAccounts
   | VerifyAccountsCreated
   | OverwriteDomesticTransferTimestamps

module AccountLoadTestTypes =
   type ProgressCheck = {
      RemainingAccountTests: int
      NumberOfProgressChecks: int
   }

   type LoadTestEventPersisted = {
      OrgId: OrgId
      AccountId: AccountId
      AccountBalance: decimal
      Event: AccountEvent
   }

   type AccountLoadTestMessage =
      | StartLoadTest
      | CheckProgress of ProgressCheck
      | Finish
      | Teardown
      | Lookup
      | AccountEventPersisted of LoadTestEventPersisted
