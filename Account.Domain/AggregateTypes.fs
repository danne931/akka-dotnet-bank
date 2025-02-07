namespace Bank.Account.Domain

open System

open Lib.SharedTypes
open Bank.Transfer.Domain
open MaintenanceFee
open AutomaticTransfer

[<RequireQualifiedAccess>]
type AccountCommand =
   | CreateAccount of CreateAccountCommand
   | DepositCash of DepositCashCommand
   | Debit of DebitCommand
   | MaintenanceFee of MaintenanceFeeCommand
   | SkipMaintenanceFee of SkipMaintenanceFeeCommand
   | InternalTransfer of InternalTransferWithinOrgCommand
   | CompleteInternalTransfer of CompleteInternalTransferWithinOrgCommand
   | FailInternalTransfer of FailInternalTransferWithinOrgCommand
   | ScheduleInternalTransferBetweenOrgs of
      ScheduleInternalTransferBetweenOrgsCommand
   | InternalTransferBetweenOrgs of InternalTransferBetweenOrgsCommand
   | CompleteInternalTransferBetweenOrgs of
      CompleteInternalTransferBetweenOrgsCommand
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
   | CloseAccount of CloseAccountCommand
   | StartBillingCycle of StartBillingCycleCommand
   | ConfigureAutoTransferRule of ConfigureAutoTransferRuleCommand
   | DeleteAutoTransferRule of DeleteAutoTransferRuleCommand
   | InternalAutoTransfer of InternalAutoTransferCommand
   | CompleteInternalAutoTransfer of CompleteInternalAutoTransferCommand
   | FailInternalAutoTransfer of FailInternalAutoTransferCommand
   | DepositInternalAutoTransfer of DepositInternalAutoTransferCommand

type AccountEvent =
   | CreatedAccount of BankEvent<CreatedAccount>
   | DepositedCash of BankEvent<DepositedCash>
   | DebitedAccount of BankEvent<DebitedAccount>
   | MaintenanceFeeDebited of BankEvent<MaintenanceFeeDebited>
   | MaintenanceFeeSkipped of BankEvent<MaintenanceFeeSkipped>
   | InternalTransferWithinOrgPending of
      BankEvent<InternalTransferWithinOrgPending>
   | InternalTransferWithinOrgCompleted of
      BankEvent<InternalTransferWithinOrgCompleted>
   | InternalTransferWithinOrgFailed of
      BankEvent<InternalTransferWithinOrgFailed>
   | InternalTransferBetweenOrgsScheduled of
      BankEvent<InternalTransferBetweenOrgsScheduled>
   | InternalTransferBetweenOrgsPending of
      BankEvent<InternalTransferBetweenOrgsPending>
   | InternalTransferBetweenOrgsCompleted of
      BankEvent<InternalTransferBetweenOrgsCompleted>
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
   | AccountClosed of BankEvent<AccountClosed>
   | BillingCycleStarted of BankEvent<BillingCycleStarted>
   | AutoTransferRuleConfigured of BankEvent<AutomaticTransferRuleConfigured>
   | AutoTransferRuleDeleted of BankEvent<AutomaticTransferRuleDeleted>
   | InternalAutomatedTransferPending of
      BankEvent<InternalAutomatedTransferPending>
   | InternalAutomatedTransferCompleted of
      BankEvent<InternalAutomatedTransferCompleted>
   | InternalAutomatedTransferFailed of
      BankEvent<InternalAutomatedTransferFailed>
   | InternalAutomatedTransferDeposited of
      BankEvent<InternalAutomatedTransferDeposited>

module AccountEvent =
   let moneyTransaction =
      function
      | AccountEvent.DepositedCash evt ->
         Some evt.Data.Amount, Some MoneyFlow.In, Some evt.Data.Origin
      | AccountEvent.DebitedAccount evt ->
         Some evt.Data.Amount, Some MoneyFlow.Out, Some evt.Data.Merchant
      | AccountEvent.InternalTransferWithinOrgPending evt ->
         Some evt.Data.BaseInfo.Amount,
         Some MoneyFlow.Out,
         Some evt.Data.BaseInfo.Recipient.Name
      | AccountEvent.InternalTransferWithinOrgCompleted evt ->
         Some evt.Data.BaseInfo.Amount,
         None,
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
      | AccountEvent.InternalAutomatedTransferCompleted evt ->
         Some evt.Data.BaseInfo.Amount,
         None,
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
      | AccountEvent.InternalTransferBetweenOrgsCompleted evt ->
         Some evt.Data.BaseInfo.Amount,
         None,
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
      | _ -> None, None, None

type OpenEventEnvelope = AccountEvent * Envelope

[<RequireQualifiedAccess>]
module AccountEnvelope =
   let get (evt: BankEvent<'E>) : Envelope = {
      Id = evt.Id
      EntityId = evt.EntityId
      OrgId = evt.OrgId
      CorrelationId = evt.CorrelationId
      InitiatedById = evt.InitiatedById
      Timestamp = evt.Timestamp
      EventName = evt.EventName
   }

   let wrap (o: BankEvent<_>) : AccountEvent =
      match box o with
      | :? BankEvent<CreatedAccount> as evt -> CreatedAccount evt
      | :? BankEvent<DepositedCash> as evt -> DepositedCash evt
      | :? BankEvent<DebitedAccount> as evt -> DebitedAccount evt
      | :? BankEvent<MaintenanceFeeDebited> as evt -> MaintenanceFeeDebited evt
      | :? BankEvent<MaintenanceFeeSkipped> as evt -> MaintenanceFeeSkipped evt
      | :? BankEvent<InternalTransferWithinOrgPending> as evt ->
         InternalTransferWithinOrgPending evt
      | :? BankEvent<InternalTransferWithinOrgCompleted> as evt ->
         InternalTransferWithinOrgCompleted evt
      | :? BankEvent<InternalTransferWithinOrgFailed> as evt ->
         InternalTransferWithinOrgFailed evt
      | :? BankEvent<InternalTransferBetweenOrgsScheduled> as evt ->
         InternalTransferBetweenOrgsScheduled evt
      | :? BankEvent<InternalTransferBetweenOrgsPending> as evt ->
         InternalTransferBetweenOrgsPending evt
      | :? BankEvent<InternalTransferBetweenOrgsCompleted> as evt ->
         InternalTransferBetweenOrgsCompleted evt
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
      | :? BankEvent<AccountClosed> as evt -> AccountClosed evt
      | :? BankEvent<BillingCycleStarted> as evt -> BillingCycleStarted evt
      | :? BankEvent<AutomaticTransferRuleConfigured> as evt ->
         AutoTransferRuleConfigured evt
      | :? BankEvent<AutomaticTransferRuleDeleted> as evt ->
         AutoTransferRuleDeleted evt
      | :? BankEvent<InternalAutomatedTransferPending> as evt ->
         InternalAutomatedTransferPending evt
      | :? BankEvent<InternalAutomatedTransferCompleted> as evt ->
         InternalAutomatedTransferCompleted evt
      | :? BankEvent<InternalAutomatedTransferFailed> as evt ->
         InternalAutomatedTransferFailed evt
      | :? BankEvent<InternalAutomatedTransferDeposited> as evt ->
         InternalAutomatedTransferDeposited evt
      | _ -> failwith "Missing definition for AccountEvent message"

   let unwrap (o: AccountEvent) : OpenEventEnvelope =
      match o with
      | CreatedAccount evt -> wrap evt, get evt
      | DepositedCash evt -> wrap evt, get evt
      | DebitedAccount evt -> wrap evt, get evt
      | MaintenanceFeeDebited evt -> wrap evt, get evt
      | MaintenanceFeeSkipped evt -> wrap evt, get evt
      | InternalTransferWithinOrgPending evt -> wrap evt, get evt
      | InternalTransferWithinOrgCompleted evt -> wrap evt, get evt
      | InternalTransferWithinOrgFailed evt -> wrap evt, get evt
      | InternalTransferBetweenOrgsScheduled evt -> wrap evt, get evt
      | InternalTransferBetweenOrgsPending evt -> wrap evt, get evt
      | InternalTransferBetweenOrgsCompleted evt -> wrap evt, get evt
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
      | AccountClosed evt -> wrap evt, get evt
      | BillingCycleStarted evt -> wrap evt, get evt
      | AutoTransferRuleConfigured evt -> wrap evt, get evt
      | AutoTransferRuleDeleted evt -> wrap evt, get evt
      | InternalAutomatedTransferPending evt -> wrap evt, get evt
      | InternalAutomatedTransferCompleted evt -> wrap evt, get evt
      | InternalAutomatedTransferFailed evt -> wrap evt, get evt
      | InternalAutomatedTransferDeposited evt -> wrap evt, get evt

type Account = {
   AccountId: AccountId
   OrgId: OrgId
   Name: string
   Depository: AccountDepository
   Currency: Currency
   Status: AccountStatus
   Balance: decimal
   LastBillingCycleDate: DateTime option
   MaintenanceFeeCriteria: MaintenanceFeeCriteria
   AccountNumber: AccountNumber
   RoutingNumber: RoutingNumber
   AutoTransferRule: AutomaticTransferConfig option
} with

   member x.CompositeId = x.AccountId, x.OrgId

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
      AccountId = AccountId System.Guid.Empty
      OrgId = OrgId System.Guid.Empty
      Name = ""
      Depository = AccountDepository.Checking
      Currency = Currency.USD
      Status = AccountStatus.InitialEmptyState
      Balance = 0m
      LastBillingCycleDate = None
      MaintenanceFeeCriteria = {
         QualifyingDepositFound = false
         DailyBalanceThreshold = false
      }
      AccountNumber = AccountNumber <| System.Int64.Parse "123456789123456"
      RoutingNumber = RoutingNumber 123456789
      AutoTransferRule = None
   }

type AccountSnapshot = {
   Info: Account
   Events: AccountEvent list
   // TODO: Add Scheduled transfer fields & probably change these
   //       in-progress/failed fields to just contain the Id
   InProgressInternalTransfers: Map<TransferId, InProgressInternalTransfer>
   InProgressDomesticTransfers: Map<TransferId, DomesticTransfer>
   FailedDomesticTransfers: Map<TransferId, DomesticTransfer>
}

module AccountSnapshot =
   let empty: AccountSnapshot = {
      Info = Account.empty
      Events = []
      InProgressInternalTransfers = Map.empty
      InProgressDomesticTransfers = Map.empty
      FailedDomesticTransfers = Map.empty
   }

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
   | StateChange of AccountCommand
   | Event of AccountEvent
   | AutoTransferCompute of AutomaticTransfer.Frequency
   | Delete

type AccountEventPersistedConfirmation = {
   EventPersisted: AccountEvent
   Account: Account
   Date: DateTime
}

type AccountEventRejected = {
   AccountId: AccountId
   Error: Err
   Date: DateTime
}

type AccountBroadcast = {
   accountEventPersisted: AccountEvent -> Account -> unit
   accountEventValidationFail: AccountId -> Err -> unit
   accountEventPersistenceFail: AccountId -> Err -> unit
   circuitBreaker: CircuitBreakerEvent -> unit
}

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

type TransactionWithAncillaryInfo = {
   Id: EventId
   Event: AccountEvent
   Category: TransactionCategory option
   Note: string option
}
