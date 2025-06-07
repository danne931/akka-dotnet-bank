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
   | FailDomesticTransferRecipient of FailDomesticTransferRecipientCommand
   | DomesticTransferRetryConfirmsRecipient of
      DomesticTransferRetryConfirmsRecipientCommand

[<RequireQualifiedAccess>]
type AccountCommand =
   | InitializePrimaryCheckingAccount of InitializePrimaryCheckingAccountCommand
   | CreateVirtualAccount of CreateVirtualAccountCommand
   | DepositCash of DepositCashCommand
   | Debit of DebitCommand
   | RefundDebit of RefundDebitCommand
   | MaintenanceFee of MaintenanceFeeCommand
   | SkipMaintenanceFee of SkipMaintenanceFeeCommand
   | InternalTransfer of InternalTransferWithinOrgCommand
   | FailInternalTransfer of FailInternalTransferWithinOrgCommand
   | ScheduleInternalTransferBetweenOrgs of
      ScheduleInternalTransferBetweenOrgsCommand
   | InternalTransferBetweenOrgs of InternalTransferBetweenOrgsCommand
   | FailInternalTransferBetweenOrgs of FailInternalTransferBetweenOrgsCommand
   | DepositTransferWithinOrg of DepositInternalTransferWithinOrgCommand
   | DepositTransferBetweenOrgs of DepositInternalTransferBetweenOrgsCommand
   | ScheduleDomesticTransfer of ScheduleDomesticTransferCommand
   | DomesticTransfer of DomesticTransferCommand
   | UpdateDomesticTransferProgress of UpdateDomesticTransferProgressCommand
   | CompleteDomesticTransfer of CompleteDomesticTransferCommand
   | FailDomesticTransfer of FailDomesticTransferCommand
   | RequestPlatformPayment of RequestPlatformPaymentCommand
   | CancelPlatformPayment of CancelPlatformPaymentCommand
   | DeclinePlatformPayment of DeclinePlatformPaymentCommand
   | FulfillPlatformPayment of FulfillPlatformPaymentCommand
   | DepositPlatformPayment of DepositPlatformPaymentCommand
   | RefundPlatformPayment of RefundPlatformPaymentCommand
   | CloseAccount of CloseAccountCommand
   | ConfigureAutoTransferRule of ConfigureAutoTransferRuleCommand
   | DeleteAutoTransferRule of DeleteAutoTransferRuleCommand
   | InternalAutoTransfer of InternalAutoTransferCommand
   | FailInternalAutoTransfer of FailInternalAutoTransferCommand
   | DepositInternalAutoTransfer of DepositInternalAutoTransferCommand
   | ParentAccount of ParentAccountCommand

   member x.Envelope: Envelope =
      match x with
      | InitializePrimaryCheckingAccount cmd -> Command.envelope cmd
      | CreateVirtualAccount cmd -> Command.envelope cmd
      | DepositCash cmd -> Command.envelope cmd
      | Debit cmd -> Command.envelope cmd
      | RefundDebit cmd -> Command.envelope cmd
      | MaintenanceFee cmd -> Command.envelope cmd
      | SkipMaintenanceFee cmd -> Command.envelope cmd
      | InternalTransfer cmd -> Command.envelope cmd
      | FailInternalTransfer cmd -> Command.envelope cmd
      | ScheduleInternalTransferBetweenOrgs cmd -> Command.envelope cmd
      | InternalTransferBetweenOrgs cmd -> Command.envelope cmd
      | FailInternalTransferBetweenOrgs cmd -> Command.envelope cmd
      | DepositTransferWithinOrg cmd -> Command.envelope cmd
      | DepositTransferBetweenOrgs cmd -> Command.envelope cmd
      | ScheduleDomesticTransfer cmd -> Command.envelope cmd
      | DomesticTransfer cmd -> Command.envelope cmd
      | UpdateDomesticTransferProgress cmd -> Command.envelope cmd
      | CompleteDomesticTransfer cmd -> Command.envelope cmd
      | FailDomesticTransfer cmd -> Command.envelope cmd
      | RequestPlatformPayment cmd -> Command.envelope cmd
      | CancelPlatformPayment cmd -> Command.envelope cmd
      | DeclinePlatformPayment cmd -> Command.envelope cmd
      | FulfillPlatformPayment cmd -> Command.envelope cmd
      | DepositPlatformPayment cmd -> Command.envelope cmd
      | RefundPlatformPayment cmd -> Command.envelope cmd
      | CloseAccount cmd -> Command.envelope cmd
      | ConfigureAutoTransferRule cmd -> Command.envelope cmd
      | DeleteAutoTransferRule cmd -> Command.envelope cmd
      | InternalAutoTransfer cmd -> Command.envelope cmd
      | FailInternalAutoTransfer cmd -> Command.envelope cmd
      | DepositInternalAutoTransfer cmd -> Command.envelope cmd
      | ParentAccount cmd ->
         match cmd with
         | ParentAccountCommand.RegisterDomesticTransferRecipient cmd ->
            Command.envelope cmd
         | ParentAccountCommand.EditDomesticTransferRecipient cmd ->
            Command.envelope cmd
         | ParentAccountCommand.NicknameDomesticTransferRecipient cmd ->
            Command.envelope cmd
         | ParentAccountCommand.FailDomesticTransferRecipient cmd ->
            Command.envelope cmd
         | ParentAccountCommand.DomesticTransferRetryConfirmsRecipient cmd ->
            Command.envelope cmd

   member x.AccountId =
      match x with
      | InitializePrimaryCheckingAccount _ -> AccountId Guid.Empty
      | CreateVirtualAccount cmd -> cmd.Data.AccountId
      | DepositCash cmd -> cmd.Data.AccountId
      | Debit cmd -> cmd.Data.AccountId
      | RefundDebit cmd -> cmd.Data.AccountId
      | MaintenanceFee cmd -> cmd.Data.AccountId
      | SkipMaintenanceFee cmd -> cmd.Data.AccountId
      | InternalTransfer cmd -> cmd.Data.Sender.AccountId
      | FailInternalTransfer cmd -> cmd.Data.BaseInfo.Sender.AccountId
      | ScheduleInternalTransferBetweenOrgs cmd ->
         cmd.Data.TransferInput.Sender.AccountId
      | InternalTransferBetweenOrgs cmd -> cmd.Data.Sender.AccountId
      | FailInternalTransferBetweenOrgs cmd ->
         cmd.Data.BaseInfo.Sender.AccountId
      | DepositTransferWithinOrg cmd -> cmd.Data.BaseInfo.Recipient.AccountId
      | DepositTransferBetweenOrgs cmd -> cmd.Data.BaseInfo.Recipient.AccountId
      | ScheduleDomesticTransfer cmd -> cmd.Data.TransferInput.Sender.AccountId
      | DomesticTransfer cmd -> cmd.Data.Sender.AccountId
      | UpdateDomesticTransferProgress cmd -> cmd.Data.BaseInfo.Sender.AccountId
      | CompleteDomesticTransfer cmd -> cmd.Data.BaseInfo.Sender.AccountId
      | FailDomesticTransfer cmd -> cmd.Data.BaseInfo.Sender.AccountId
      | RequestPlatformPayment cmd -> cmd.Data.BaseInfo.Payee.AccountId
      | CancelPlatformPayment cmd ->
         cmd.Data.RequestedPayment.BaseInfo.Payee.AccountId
      | DeclinePlatformPayment cmd ->
         cmd.Data.RequestedPayment.BaseInfo.Payee.AccountId
      | FulfillPlatformPayment cmd ->
         match cmd.Data.PaymentMethod with
         | PaymentMethod.Platform accountId -> accountId
         | PaymentMethod.ThirdParty _ -> AccountId Guid.Empty
      | DepositPlatformPayment cmd -> cmd.Data.BaseInfo.Payee.AccountId
      | RefundPlatformPayment cmd ->
         match cmd.Data.PaymentMethod with
         | PaymentMethod.Platform accountId -> accountId
         | PaymentMethod.ThirdParty _ -> AccountId Guid.Empty
      | CloseAccount cmd -> cmd.Data.AccountId
      | ConfigureAutoTransferRule cmd -> cmd.Data.AccountId
      | DeleteAutoTransferRule cmd -> cmd.Data.AccountId
      | InternalAutoTransfer cmd -> cmd.Data.Transfer.Sender.AccountId
      | FailInternalAutoTransfer cmd -> cmd.Data.BaseInfo.Sender.AccountId
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
   | DomesticTransferRecipientFailed of
      BankEvent<DomesticTransferRecipientFailed>
   | DomesticTransferRetryConfirmsRecipient of
      BankEvent<DomesticTransferRetryConfirmsRecipient>

type AccountEvent =
   | InitializedPrimaryCheckingAccount of
      BankEvent<InitializedPrimaryCheckingAccount>
   | CreatedVirtualAccount of BankEvent<CreatedVirtualAccount>
   | DepositedCash of BankEvent<DepositedCash>
   | DebitedAccount of BankEvent<DebitedAccount>
   | RefundedDebit of BankEvent<RefundedDebit>
   | MaintenanceFeeDebited of BankEvent<MaintenanceFeeDebited>
   | MaintenanceFeeSkipped of BankEvent<MaintenanceFeeSkipped>
   | InternalTransferWithinOrgPending of
      BankEvent<InternalTransferWithinOrgPending>
   | InternalTransferWithinOrgFailed of
      BankEvent<InternalTransferWithinOrgFailed>
   | InternalTransferBetweenOrgsScheduled of
      BankEvent<InternalTransferBetweenOrgsScheduled>
   | InternalTransferBetweenOrgsPending of
      BankEvent<InternalTransferBetweenOrgsPending>
   | InternalTransferBetweenOrgsFailed of
      BankEvent<InternalTransferBetweenOrgsFailed>
   | InternalTransferWithinOrgDeposited of
      BankEvent<InternalTransferWithinOrgDeposited>
   | InternalTransferBetweenOrgsDeposited of
      BankEvent<InternalTransferBetweenOrgsDeposited>
   | DomesticTransferScheduled of BankEvent<DomesticTransferScheduled>
   | DomesticTransferPending of BankEvent<DomesticTransferPending>
   | DomesticTransferProgress of BankEvent<DomesticTransferProgressUpdate>
   | DomesticTransferCompleted of BankEvent<DomesticTransferCompleted>
   | DomesticTransferFailed of BankEvent<DomesticTransferFailed>
   | PlatformPaymentRequested of BankEvent<PlatformPaymentRequested>
   | PlatformPaymentCancelled of BankEvent<PlatformPaymentCancelled>
   | PlatformPaymentDeclined of BankEvent<PlatformPaymentDeclined>
   | PlatformPaymentPaid of BankEvent<PlatformPaymentPaid>
   | PlatformPaymentDeposited of BankEvent<PlatformPaymentDeposited>
   | PlatformPaymentRefunded of BankEvent<PlatformPaymentRefunded>
   | AccountClosed of BankEvent<AccountClosed>
   | AutoTransferRuleConfigured of BankEvent<AutomaticTransferRuleConfigured>
   | AutoTransferRuleDeleted of BankEvent<AutomaticTransferRuleDeleted>
   | InternalAutomatedTransferPending of
      BankEvent<InternalAutomatedTransferPending>
   | InternalAutomatedTransferFailed of
      BankEvent<InternalAutomatedTransferFailed>
   | InternalAutomatedTransferDeposited of
      BankEvent<InternalAutomatedTransferDeposited>
   | ParentAccount of ParentAccountEvent

   member x.AccountId =
      match x with
      | InitializedPrimaryCheckingAccount evt ->
         evt.Data.PrimaryChecking.AccountId
      | CreatedVirtualAccount evt -> evt.Data.AccountId
      | DepositedCash evt -> evt.Data.AccountId
      | DebitedAccount evt -> evt.Data.AccountId
      | RefundedDebit evt -> evt.Data.AccountId
      | MaintenanceFeeDebited evt -> evt.Data.AccountId
      | MaintenanceFeeSkipped evt -> evt.Data.AccountId
      | InternalTransferWithinOrgPending evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | InternalTransferWithinOrgFailed evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | InternalTransferBetweenOrgsScheduled evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | InternalTransferBetweenOrgsPending evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | InternalTransferBetweenOrgsFailed evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | DomesticTransferScheduled evt -> evt.Data.BaseInfo.Sender.AccountId
      | DomesticTransferPending evt -> evt.Data.BaseInfo.Sender.AccountId
      | DomesticTransferProgress evt -> evt.Data.BaseInfo.Sender.AccountId
      | DomesticTransferCompleted evt -> evt.Data.BaseInfo.Sender.AccountId
      | DomesticTransferFailed evt -> evt.Data.BaseInfo.Sender.AccountId
      | InternalTransferWithinOrgDeposited evt ->
         evt.Data.BaseInfo.Recipient.AccountId
      | InternalTransferBetweenOrgsDeposited evt ->
         evt.Data.BaseInfo.Recipient.AccountId
      | PlatformPaymentRequested evt -> evt.Data.BaseInfo.Payee.AccountId
      | PlatformPaymentCancelled evt -> evt.Data.BaseInfo.Payee.AccountId
      | PlatformPaymentDeclined evt -> evt.Data.BaseInfo.Payee.AccountId
      | PlatformPaymentPaid evt ->
         match evt.Data.PaymentMethod with
         | PaymentMethod.Platform accountId -> accountId
         | PaymentMethod.ThirdParty _ -> AccountId Guid.Empty
      | PlatformPaymentDeposited evt -> evt.Data.BaseInfo.Payee.AccountId
      | PlatformPaymentRefunded evt ->
         match evt.Data.PaymentMethod with
         | PaymentMethod.Platform accountId -> accountId
         | PaymentMethod.ThirdParty _ -> AccountId Guid.Empty
      | AccountClosed evt -> evt.Data.AccountId
      | AutoTransferRuleConfigured evt -> evt.Data.AccountId
      | AutoTransferRuleDeleted evt -> evt.Data.AccountId
      | InternalAutomatedTransferPending evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | InternalAutomatedTransferFailed evt ->
         evt.Data.BaseInfo.Sender.AccountId
      | InternalAutomatedTransferDeposited evt ->
         evt.Data.BaseInfo.Recipient.AccountId
      | ParentAccount _ -> AccountId Guid.Empty

module AccountEvent =
   let moneyTransaction =
      function
      | AccountEvent.DepositedCash evt ->
         Some evt.Data.Amount, Some MoneyFlow.In, Some evt.Data.Origin
      | AccountEvent.DebitedAccount evt ->
         Some evt.Data.Amount, Some MoneyFlow.Out, Some evt.Data.Merchant
      | AccountEvent.RefundedDebit evt ->
         Some evt.Data.Amount, Some MoneyFlow.In, Some evt.Data.Merchant
      | AccountEvent.InternalTransferWithinOrgPending evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.Out,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.InternalTransferWithinOrgFailed evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.In,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.InternalTransferWithinOrgDeposited evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.In,
         Some evt.Data.BaseInfo.Sender.Name
      | AccountEvent.InternalAutomatedTransferPending evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.Out,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.InternalAutomatedTransferFailed evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.In,
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
         Some MoneyFlow.Out,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.InternalTransferBetweenOrgsFailed evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.In,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.InternalTransferBetweenOrgsDeposited evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.In,
         Some evt.Data.BaseInfo.Sender.Name
      | AccountEvent.DomesticTransferScheduled evt ->
         Some evt.Data.BaseInfo.Amount,
         None,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.DomesticTransferPending evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.Out,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.DomesticTransferFailed evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.In,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.MaintenanceFeeDebited evt ->
         Some evt.Data.Amount, Some MoneyFlow.Out, Some "Maintenance Fee"
      | AccountEvent.PlatformPaymentPaid evt ->
         let p = evt.Data.BaseInfo
         Some p.Amount, Some MoneyFlow.Out, Some p.Payee.OrgName
      | AccountEvent.PlatformPaymentDeposited evt ->
         let p = evt.Data.BaseInfo
         Some p.Amount, Some MoneyFlow.In, Some p.Payer.OrgName
      | AccountEvent.PlatformPaymentRefunded evt ->
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
      | :? BankEvent<DebitedAccount> as evt -> DebitedAccount evt
      | :? BankEvent<RefundedDebit> as evt -> RefundedDebit evt
      | :? BankEvent<MaintenanceFeeDebited> as evt -> MaintenanceFeeDebited evt
      | :? BankEvent<MaintenanceFeeSkipped> as evt -> MaintenanceFeeSkipped evt
      | :? BankEvent<InternalTransferWithinOrgPending> as evt ->
         InternalTransferWithinOrgPending evt
      | :? BankEvent<InternalTransferWithinOrgFailed> as evt ->
         InternalTransferWithinOrgFailed evt
      | :? BankEvent<InternalTransferBetweenOrgsScheduled> as evt ->
         InternalTransferBetweenOrgsScheduled evt
      | :? BankEvent<InternalTransferBetweenOrgsPending> as evt ->
         InternalTransferBetweenOrgsPending evt
      | :? BankEvent<InternalTransferBetweenOrgsFailed> as evt ->
         InternalTransferBetweenOrgsFailed evt
      | :? BankEvent<InternalTransferWithinOrgDeposited> as evt ->
         InternalTransferWithinOrgDeposited evt
      | :? BankEvent<InternalTransferBetweenOrgsDeposited> as evt ->
         InternalTransferBetweenOrgsDeposited evt
      | :? BankEvent<DomesticTransferScheduled> as evt ->
         DomesticTransferScheduled evt
      | :? BankEvent<DomesticTransferPending> as evt ->
         DomesticTransferPending evt
      | :? BankEvent<DomesticTransferProgressUpdate> as evt ->
         DomesticTransferProgress evt
      | :? BankEvent<DomesticTransferCompleted> as evt ->
         DomesticTransferCompleted evt
      | :? BankEvent<DomesticTransferFailed> as evt ->
         DomesticTransferFailed evt
      | :? BankEvent<PlatformPaymentRequested> as evt ->
         PlatformPaymentRequested evt
      | :? BankEvent<PlatformPaymentCancelled> as evt ->
         PlatformPaymentCancelled evt
      | :? BankEvent<PlatformPaymentDeclined> as evt ->
         PlatformPaymentDeclined evt
      | :? BankEvent<PlatformPaymentPaid> as evt -> PlatformPaymentPaid evt
      | :? BankEvent<PlatformPaymentDeposited> as evt ->
         PlatformPaymentDeposited evt
      | :? BankEvent<PlatformPaymentRefunded> as evt ->
         PlatformPaymentRefunded evt
      | :? BankEvent<AccountClosed> as evt -> AccountClosed evt
      | :? BankEvent<AutomaticTransferRuleConfigured> as evt ->
         AutoTransferRuleConfigured evt
      | :? BankEvent<AutomaticTransferRuleDeleted> as evt ->
         AutoTransferRuleDeleted evt
      | :? BankEvent<InternalAutomatedTransferPending> as evt ->
         InternalAutomatedTransferPending evt
      | :? BankEvent<InternalAutomatedTransferFailed> as evt ->
         InternalAutomatedTransferFailed evt
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
      | :? BankEvent<DomesticTransferRecipientFailed> as evt ->
         evt
         |> ParentAccountEvent.DomesticTransferRecipientFailed
         |> AccountEvent.ParentAccount
      | :? BankEvent<DomesticTransferRetryConfirmsRecipient> as evt ->
         evt
         |> ParentAccountEvent.DomesticTransferRetryConfirmsRecipient
         |> AccountEvent.ParentAccount
      | _ -> failwith "Missing definition for AccountEvent message"

   let unwrap (o: AccountEvent) : OpenEventEnvelope =
      match o with
      | InitializedPrimaryCheckingAccount evt -> wrap evt, get evt
      | CreatedVirtualAccount evt -> wrap evt, get evt
      | DepositedCash evt -> wrap evt, get evt
      | DebitedAccount evt -> wrap evt, get evt
      | RefundedDebit evt -> wrap evt, get evt
      | MaintenanceFeeDebited evt -> wrap evt, get evt
      | MaintenanceFeeSkipped evt -> wrap evt, get evt
      | InternalTransferWithinOrgPending evt -> wrap evt, get evt
      | InternalTransferWithinOrgFailed evt -> wrap evt, get evt
      | InternalTransferBetweenOrgsScheduled evt -> wrap evt, get evt
      | InternalTransferBetweenOrgsPending evt -> wrap evt, get evt
      | InternalTransferBetweenOrgsFailed evt -> wrap evt, get evt
      | DomesticTransferScheduled evt -> wrap evt, get evt
      | DomesticTransferPending evt -> wrap evt, get evt
      | DomesticTransferProgress evt -> wrap evt, get evt
      | DomesticTransferCompleted evt -> wrap evt, get evt
      | DomesticTransferFailed evt -> wrap evt, get evt
      | InternalTransferWithinOrgDeposited evt -> wrap evt, get evt
      | InternalTransferBetweenOrgsDeposited evt -> wrap evt, get evt
      | PlatformPaymentRequested evt -> wrap evt, get evt
      | PlatformPaymentCancelled evt -> wrap evt, get evt
      | PlatformPaymentDeclined evt -> wrap evt, get evt
      | PlatformPaymentPaid evt -> wrap evt, get evt
      | PlatformPaymentDeposited evt -> wrap evt, get evt
      | PlatformPaymentRefunded evt -> wrap evt, get evt
      | AccountClosed evt -> wrap evt, get evt
      | AutoTransferRuleConfigured evt -> wrap evt, get evt
      | AutoTransferRuleDeleted evt -> wrap evt, get evt
      | InternalAutomatedTransferPending evt -> wrap evt, get evt
      | InternalAutomatedTransferFailed evt -> wrap evt, get evt
      | InternalAutomatedTransferDeposited evt -> wrap evt, get evt
      | ParentAccount evt ->
         match evt with
         | ParentAccountEvent.RegisteredDomesticTransferRecipient evt ->
            wrap evt, get evt
         | ParentAccountEvent.EditedDomesticTransferRecipient evt ->
            wrap evt, get evt
         | ParentAccountEvent.NicknamedDomesticTransferRecipient evt ->
            wrap evt, get evt
         | ParentAccountEvent.DomesticTransferRetryConfirmsRecipient evt ->
            wrap evt, get evt
         | ParentAccountEvent.DomesticTransferRecipientFailed evt ->
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
      AccountNumber = AccountNumber <| Int64.Parse "123456789123456"
      RoutingNumber = RoutingNumber 123456789
      AutoTransferRule = None
   }

type ParentAccountSnapshot = {
   OrgId: OrgId
   ParentAccountId: ParentAccountId
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
   | Event of AccountEvent
   | AutoTransferCompute of AutomaticTransfer.Frequency * AccountId
   | ProcessBillingStatement of CorrelationId * BillingPeriod
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
