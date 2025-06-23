namespace Bank.Account.Domain

open System

open Lib.SharedTypes
open Bank.Transfer.Domain
open MaintenanceFee
open AutomaticTransfer

/// Commands which pertain to the parent account rather
/// than one of the subacounts.
[<RequireQualifiedAccess>]
type ParentAccountCommand =
   | RegisterDomesticTransferRecipient of
      RegisterDomesticTransferRecipientCommand
   | EditDomesticTransferRecipient of EditDomesticTransferRecipientCommand
   | NicknameDomesticTransferRecipient of
      NicknameDomesticTransferRecipientCommand

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
   | RequestPlatformPayment of RequestPlatformPaymentCommand
   | CancelPlatformPayment of CancelPlatformPaymentCommand
   | DeclinePlatformPayment of DeclinePlatformPaymentCommand
   | PlatformPayment of PlatformPaymentCommand
   | DepositPlatformPayment of DepositPlatformPaymentCommand
   | FailPlatformPayment of FailPlatformPaymentCommand
   | SettlePlatformPayment of SettlePlatformPaymentCommand
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
      | RequestPlatformPayment cmd -> Command.envelope cmd
      | CancelPlatformPayment cmd -> Command.envelope cmd
      | DeclinePlatformPayment cmd -> Command.envelope cmd
      | PlatformPayment cmd -> Command.envelope cmd
      | DepositPlatformPayment cmd -> Command.envelope cmd
      | FailPlatformPayment cmd -> Command.envelope cmd
      | SettlePlatformPayment cmd -> Command.envelope cmd
      | CloseAccount cmd -> Command.envelope cmd
      | ConfigureAutoTransferRule cmd -> Command.envelope cmd
      | DeleteAutoTransferRule cmd -> Command.envelope cmd
      | InternalAutoTransfer cmd -> Command.envelope cmd
      | DepositInternalAutoTransfer cmd -> Command.envelope cmd
      | ParentAccount cmd ->
         match cmd with
         | ParentAccountCommand.RegisterDomesticTransferRecipient cmd ->
            Command.envelope cmd
         | ParentAccountCommand.EditDomesticTransferRecipient cmd ->
            Command.envelope cmd
         | ParentAccountCommand.NicknameDomesticTransferRecipient cmd ->
            Command.envelope cmd

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
      | ScheduleDomesticTransfer cmd -> cmd.Data.TransferInput.Sender.AccountId
      | DomesticTransfer cmd -> cmd.Data.Sender.AccountId
      | UpdateDomesticTransferProgress cmd -> cmd.Data.BaseInfo.Sender.AccountId
      | SettleDomesticTransfer cmd -> cmd.Data.BaseInfo.Sender.AccountId
      | FailDomesticTransfer cmd -> cmd.Data.BaseInfo.Sender.AccountId
      | RequestPlatformPayment cmd -> cmd.Data.BaseInfo.Payee.AccountId
      | CancelPlatformPayment cmd ->
         cmd.Data.RequestedPayment.BaseInfo.Payee.AccountId
      | DeclinePlatformPayment cmd ->
         cmd.Data.RequestedPayment.BaseInfo.Payee.AccountId
      | PlatformPayment cmd ->
         match cmd.Data.PaymentMethod with
         | PaymentMethod.Platform accountId -> accountId
         | PaymentMethod.ThirdParty _ -> AccountId Guid.Empty
      | DepositPlatformPayment cmd -> cmd.Data.BaseInfo.Payee.AccountId
      | SettlePlatformPayment cmd -> cmd.Data.BaseInfo.Payee.AccountId
      | FailPlatformPayment cmd ->
         match cmd.Data.PaymentMethod with
         | PaymentMethod.Platform accountId -> accountId
         | PaymentMethod.ThirdParty _ -> AccountId Guid.Empty
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
   | RegisteredDomesticTransferRecipient of
      BankEvent<RegisteredDomesticTransferRecipient>
   | EditedDomesticTransferRecipient of
      BankEvent<EditedDomesticTransferRecipient>
   | NicknamedDomesticTransferRecipient of
      BankEvent<NicknamedDomesticTransferRecipient>

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
   | PlatformPaymentRequested of BankEvent<PlatformPaymentRequested>
   | PlatformPaymentCancelled of BankEvent<PlatformPaymentCancelled>
   | PlatformPaymentDeclined of BankEvent<PlatformPaymentDeclined>
   | PlatformPaymentPending of BankEvent<PlatformPaymentPending>
   | PlatformPaymentDeposited of BankEvent<PlatformPaymentDeposited>
   | PlatformPaymentSettled of BankEvent<PlatformPaymentSettled>
   | PlatformPaymentFailed of BankEvent<PlatformPaymentFailed>
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
      | DomesticTransferScheduled evt -> evt.Data.BaseInfo.Sender.AccountId
      | DomesticTransferPending evt -> evt.Data.BaseInfo.Sender.AccountId
      | DomesticTransferProgress evt -> evt.Data.BaseInfo.Sender.AccountId
      | DomesticTransferSettled evt -> evt.Data.BaseInfo.Sender.AccountId
      | DomesticTransferFailed evt -> evt.Data.BaseInfo.Sender.AccountId
      | PlatformPaymentRequested evt -> evt.Data.BaseInfo.Payee.AccountId
      | PlatformPaymentCancelled evt -> evt.Data.BaseInfo.Payee.AccountId
      | PlatformPaymentDeclined evt -> evt.Data.BaseInfo.Payee.AccountId
      | PlatformPaymentPending evt ->
         match evt.Data.PaymentMethod with
         | PaymentMethod.Platform accountId -> accountId
         | PaymentMethod.ThirdParty _ -> AccountId Guid.Empty
      | PlatformPaymentDeposited evt -> evt.Data.BaseInfo.Payee.AccountId
      | PlatformPaymentSettled evt -> evt.Data.BaseInfo.Payee.AccountId
      | PlatformPaymentFailed evt ->
         match evt.Data.PaymentMethod with
         | PaymentMethod.Platform accountId -> accountId
         | PaymentMethod.ThirdParty _ -> AccountId Guid.Empty
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
         Some evt.Data.Amount, None, Some evt.Data.Merchant
      | AccountEvent.DebitFailed evt ->
         Some evt.Data.Amount, None, Some evt.Data.Merchant
      | AccountEvent.DebitSettled evt ->
         Some evt.Data.Amount, Some MoneyFlow.Out, Some evt.Data.Merchant
      | AccountEvent.DebitRefunded evt ->
         Some evt.Data.Amount, Some MoneyFlow.In, Some evt.Data.Merchant
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
         None,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.InternalTransferBetweenOrgsPending evt ->
         Some evt.Data.BaseInfo.Amount,
         None,
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
         None,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.DomesticTransferPending evt ->
         Some evt.Data.BaseInfo.Amount,
         None,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.DomesticTransferSettled evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.Out,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.DomesticTransferFailed evt ->
         Some evt.Data.BaseInfo.Amount,
         None,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.MaintenanceFeeDebited evt ->
         Some evt.Data.Amount, Some MoneyFlow.Out, Some "Maintenance Fee"
      | AccountEvent.PlatformPaymentRequested evt ->
         let p = evt.Data.BaseInfo
         Some p.Amount, None, Some p.Payee.OrgName
      | AccountEvent.PlatformPaymentCancelled evt ->
         let p = evt.Data.BaseInfo
         Some p.Amount, None, Some p.Payee.OrgName
      | AccountEvent.PlatformPaymentDeclined evt ->
         let p = evt.Data.BaseInfo
         Some p.Amount, None, Some p.Payer.OrgName
      | AccountEvent.PlatformPaymentPending evt ->
         let p = evt.Data.BaseInfo
         Some p.Amount, None, Some p.Payee.OrgName
      | AccountEvent.PlatformPaymentDeposited evt ->
         let p = evt.Data.BaseInfo
         Some p.Amount, Some MoneyFlow.In, Some p.Payer.OrgName
      | AccountEvent.PlatformPaymentSettled evt ->
         let p = evt.Data.BaseInfo
         Some p.Amount, Some MoneyFlow.Out, Some p.Payee.OrgName
      | AccountEvent.PlatformPaymentFailed evt ->
         let p = evt.Data.BaseInfo
         Some p.Amount, Some MoneyFlow.In, Some p.Payee.OrgName
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
      | :? BankEvent<PlatformPaymentRequested> as evt ->
         PlatformPaymentRequested evt
      | :? BankEvent<PlatformPaymentCancelled> as evt ->
         PlatformPaymentCancelled evt
      | :? BankEvent<PlatformPaymentDeclined> as evt ->
         PlatformPaymentDeclined evt
      | :? BankEvent<PlatformPaymentPending> as evt ->
         PlatformPaymentPending evt
      | :? BankEvent<PlatformPaymentDeposited> as evt ->
         PlatformPaymentDeposited evt
      | :? BankEvent<PlatformPaymentSettled> as evt ->
         PlatformPaymentSettled evt
      | :? BankEvent<PlatformPaymentFailed> as evt -> PlatformPaymentFailed evt
      | :? BankEvent<AccountClosed> as evt -> AccountClosed evt
      | :? BankEvent<AutomaticTransferRuleConfigured> as evt ->
         AutoTransferRuleConfigured evt
      | :? BankEvent<AutomaticTransferRuleDeleted> as evt ->
         AutoTransferRuleDeleted evt
      | :? BankEvent<InternalAutomatedTransferDeducted> as evt ->
         InternalAutomatedTransferDeducted evt
      | :? BankEvent<InternalAutomatedTransferDeposited> as evt ->
         InternalAutomatedTransferDeposited evt
      | :? BankEvent<RegisteredDomesticTransferRecipient> as evt ->
         evt
         |> ParentAccountEvent.RegisteredDomesticTransferRecipient
         |> AccountEvent.ParentAccount
      | :? BankEvent<EditedDomesticTransferRecipient> as evt ->
         evt
         |> ParentAccountEvent.EditedDomesticTransferRecipient
         |> AccountEvent.ParentAccount
      | :? BankEvent<NicknamedDomesticTransferRecipient> as evt ->
         evt
         |> ParentAccountEvent.NicknamedDomesticTransferRecipient
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
      | PlatformPaymentRequested evt -> wrap evt, get evt
      | PlatformPaymentCancelled evt -> wrap evt, get evt
      | PlatformPaymentDeclined evt -> wrap evt, get evt
      | PlatformPaymentPending evt -> wrap evt, get evt
      | PlatformPaymentDeposited evt -> wrap evt, get evt
      | PlatformPaymentFailed evt -> wrap evt, get evt
      | PlatformPaymentSettled evt -> wrap evt, get evt
      | AccountClosed evt -> wrap evt, get evt
      | AutoTransferRuleConfigured evt -> wrap evt, get evt
      | AutoTransferRuleDeleted evt -> wrap evt, get evt
      | InternalAutomatedTransferDeducted evt -> wrap evt, get evt
      | InternalAutomatedTransferDeposited evt -> wrap evt, get evt
      | ParentAccount evt ->
         match evt with
         | ParentAccountEvent.RegisteredDomesticTransferRecipient evt ->
            wrap evt, get evt
         | ParentAccountEvent.EditedDomesticTransferRecipient evt ->
            wrap evt, get evt
         | ParentAccountEvent.NicknamedDomesticTransferRecipient evt ->
            wrap evt, get evt

type Account = {
   AccountId: AccountId
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   Name: string
   Depository: AccountDepository
   Currency: Currency
   Status: AccountStatus
   Balance: decimal
   PendingDeductions: decimal
   AccountNumber: AccountNumber
   RoutingNumber: RoutingNumber
   AutoTransferRule: AutomaticTransferConfig option
} with

   member x.CompositeId = x.AccountId, x.ParentAccountId, x.OrgId

   member x.FullName = $"{x.Name} **{x.AccountNumber.Last4}"

   member private x.autoTransferManagement
      (computedTransferFromRule:
         AutomaticTransferRule
            -> decimal
            -> AutoTransferDerivedFromRule list option)
      : AutoTransferDerivedFromRule list =
      x.AutoTransferRule
      |> Option.bind (fun conf -> computedTransferFromRule conf.Info x.Balance)
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
      PendingDeductions = 0m
      AccountNumber = AccountNumber.Empty
      RoutingNumber = RoutingNumber.Empty
      AutoTransferRule = None
   }

type ParentAccountSnapshot = {
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   AccountNumber: ParentAccountNumber
   RoutingNumber: ParentRoutingNumber
   PrimaryVirtualAccountId: AccountId
   VirtualAccounts: Map<AccountId, Account>
   LastBillingCycleDate: DateTime option
   MaintenanceFeeCriteria: MaintenanceFeeCriteria
   Status: ParentAccountStatus
   DomesticTransferRecipients: Map<AccountId, DomesticTransferRecipient>
   Events: AccountEvent list
} with

   static member empty: ParentAccountSnapshot = {
      OrgId = OrgId Guid.Empty
      ParentAccountId = ParentAccountId Guid.Empty
      AccountNumber = ParentAccountNumber AccountNumber.Empty
      RoutingNumber = ParentRoutingNumber RoutingNumber.Empty
      PrimaryVirtualAccountId = AccountId Guid.Empty
      VirtualAccounts = Map.empty
      LastBillingCycleDate = None
      Status = ParentAccountStatus.InitialEmptyState
      MaintenanceFeeCriteria = {
         QualifyingDepositFound = false
         DailyBalanceThreshold = false
      }
      DomesticTransferRecipients = Map.empty
      Events = []
   }

   member x.eventsForAccount(accountId: AccountId) =
      x.Events |> List.filter (fun evt -> evt.AccountId = accountId)

   member x.Balance = x.VirtualAccounts.Values |> Seq.sumBy _.Balance

   member x.PendingDeductions =
      x.VirtualAccounts.Values |> Seq.sumBy _.PendingDeductions

   member x.PrimaryVirtualAccountCompositeId =
      x.PrimaryVirtualAccountId, x.ParentAccountId, x.OrgId

type AccountMetrics = {
   DailyInternalTransferWithinOrg: decimal
   DailyInternalTransferBetweenOrgs: decimal
   DailyDomesticTransfer: decimal
   DailyPaymentPaid: decimal
   DailyPurchase: decimal
   MonthlyInternalTransferWithinOrg: decimal
   MonthlyInternalTransferBetweenOrgs: decimal
   MonthlyDomesticTransfer: decimal
   MonthlyPurchase: decimal
   MonthlyPaymentPaid: decimal
}

module AccountMetrics =
   let empty: AccountMetrics = {
      DailyInternalTransferWithinOrg = 0m
      DailyInternalTransferBetweenOrgs = 0m
      DailyDomesticTransfer = 0m
      DailyPaymentPaid = 0m
      DailyPurchase = 0m
      MonthlyInternalTransferWithinOrg = 0m
      MonthlyInternalTransferBetweenOrgs = 0m
      MonthlyDomesticTransfer = 0m
      MonthlyPaymentPaid = 0m
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
