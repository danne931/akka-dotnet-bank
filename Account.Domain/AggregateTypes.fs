namespace Bank.Account.Domain

open System

open Lib.SharedTypes
open Bank.Transfer.Domain
open MaintenanceFee

type AccountCommand =
   | CreateAccount of CreateAccountCommand
   | DepositCash of DepositCashCommand
   | Debit of DebitCommand
   | MaintenanceFee of MaintenanceFeeCommand
   | SkipMaintenanceFee of SkipMaintenanceFeeCommand
   | InternalTransfer of InternalTransferCommand
   | ApproveInternalTransfer of ApproveInternalTransferCommand
   | RejectInternalTransfer of RejectInternalTransferCommand
   | DepositTransfer of DepositTransferCommand
   | RegisterInternalTransferRecipient of
      RegisterInternalTransferRecipientCommand
   | RegisterInternalSender of RegisterInternalSenderCommand
   | DeactivateInternalRecipient of DeactivateInternalRecipientCommand
   | RegisterDomesticTransferRecipient of
      RegisterDomesticTransferRecipientCommand
   | EditDomesticTransferRecipient of EditDomesticTransferRecipientCommand
   | DomesticTransfer of DomesticTransferCommand
   | UpdateDomesticTransferProgress of UpdateDomesticTransferProgressCommand
   | ApproveDomesticTransfer of ApproveDomesticTransferCommand
   | RejectDomesticTransfer of RejectDomesticTransferCommand
   | NicknameRecipient of NicknameRecipientCommand
   | CloseAccount of CloseAccountCommand
   | StartBillingCycle of StartBillingCycleCommand

type AccountEvent =
   | CreatedAccount of BankEvent<CreatedAccount>
   | DepositedCash of BankEvent<DepositedCash>
   | DebitedAccount of BankEvent<DebitedAccount>
   | MaintenanceFeeDebited of BankEvent<MaintenanceFeeDebited>
   | MaintenanceFeeSkipped of BankEvent<MaintenanceFeeSkipped>
   | InternalTransferRecipient of BankEvent<RegisteredInternalTransferRecipient>
   | InternalTransferPending of BankEvent<InternalTransferPending>
   | InternalTransferApproved of BankEvent<InternalTransferApproved>
   | InternalTransferRejected of BankEvent<InternalTransferRejected>
   | InternalSenderRegistered of BankEvent<InternalSenderRegistered>
   | InternalRecipientDeactivated of BankEvent<InternalRecipientDeactivated>
   | TransferDeposited of BankEvent<TransferDeposited>
   | DomesticTransferRecipient of BankEvent<RegisteredDomesticTransferRecipient>
   | EditedDomesticTransferRecipient of
      BankEvent<EditedDomesticTransferRecipient>
   | DomesticTransferPending of BankEvent<DomesticTransferPending>
   | DomesticTransferProgress of BankEvent<DomesticTransferProgressUpdate>
   | DomesticTransferApproved of BankEvent<DomesticTransferApproved>
   | DomesticTransferRejected of BankEvent<DomesticTransferRejected>
   | RecipientNicknamed of BankEvent<RecipientNicknamed>
   | AccountClosed of BankEvent<AccountClosed>
   | BillingCycleStarted of BankEvent<BillingCycleStarted>

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
      | :? BankEvent<RegisteredInternalTransferRecipient> as evt ->
         InternalTransferRecipient evt
      | :? BankEvent<RegisteredDomesticTransferRecipient> as evt ->
         DomesticTransferRecipient evt
      | :? BankEvent<EditedDomesticTransferRecipient> as evt ->
         EditedDomesticTransferRecipient evt
      | :? BankEvent<InternalRecipientDeactivated> as evt ->
         InternalRecipientDeactivated evt
      | :? BankEvent<RecipientNicknamed> as evt ->
         AccountEvent.RecipientNicknamed evt
      | :? BankEvent<InternalSenderRegistered> as evt ->
         InternalSenderRegistered evt
      | :? BankEvent<InternalTransferPending> as evt ->
         InternalTransferPending evt
      | :? BankEvent<InternalTransferApproved> as evt ->
         InternalTransferApproved evt
      | :? BankEvent<InternalTransferRejected> as evt ->
         InternalTransferRejected evt
      | :? BankEvent<TransferDeposited> as evt -> TransferDeposited evt
      | :? BankEvent<DomesticTransferPending> as evt ->
         DomesticTransferPending evt
      | :? BankEvent<DomesticTransferProgressUpdate> as evt ->
         DomesticTransferProgress evt
      | :? BankEvent<DomesticTransferApproved> as evt ->
         DomesticTransferApproved evt
      | :? BankEvent<DomesticTransferRejected> as evt ->
         DomesticTransferRejected evt
      | :? BankEvent<AccountClosed> as evt -> AccountClosed evt
      | :? BankEvent<BillingCycleStarted> as evt -> BillingCycleStarted evt
      | _ -> failwith "Missing definition for AccountEvent message"

   let unwrap (o: AccountEvent) : OpenEventEnvelope =
      match o with
      | CreatedAccount evt -> wrap evt, get evt
      | DepositedCash evt -> wrap evt, get evt
      | DebitedAccount evt -> wrap evt, get evt
      | MaintenanceFeeDebited evt -> wrap evt, get evt
      | MaintenanceFeeSkipped evt -> wrap evt, get evt
      | InternalTransferRecipient evt -> wrap evt, get evt
      | InternalRecipientDeactivated evt -> wrap evt, get evt
      | InternalSenderRegistered evt -> wrap evt, get evt
      | InternalTransferPending evt -> wrap evt, get evt
      | InternalTransferApproved evt -> wrap evt, get evt
      | InternalTransferRejected evt -> wrap evt, get evt
      | DomesticTransferRecipient evt -> wrap evt, get evt
      | EditedDomesticTransferRecipient evt -> wrap evt, get evt
      | DomesticTransferPending evt -> wrap evt, get evt
      | DomesticTransferProgress evt -> wrap evt, get evt
      | DomesticTransferApproved evt -> wrap evt, get evt
      | DomesticTransferRejected evt -> wrap evt, get evt
      | RecipientNicknamed evt -> wrap evt, get evt
      | TransferDeposited evt -> wrap evt, get evt
      | AccountClosed evt -> wrap evt, get evt
      | BillingCycleStarted evt -> wrap evt, get evt

type Account = {
   AccountId: AccountId
   OrgId: OrgId
   Name: string
   Depository: AccountDepository
   Currency: Currency
   Status: AccountStatus
   Balance: decimal
   LastBillingCycleDate: DateTime option
   InternalTransferSenders: Map<AccountId, InternalTransferSender>
   InternalTransferRecipients: Map<AccountId, InternalTransferRecipient>
   DomesticTransferRecipients: Map<AccountId, DomesticTransferRecipient>
   InProgressInternalTransfers:
      Map<CorrelationId, BankEvent<InternalTransferPending>>
   InProgressDomesticTransfers: Map<CorrelationId, DomesticTransfer>
   FailedDomesticTransfers: Map<CorrelationId, DomesticTransfer>
   MaintenanceFeeCriteria: MaintenanceFeeCriteria
   Events: AccountEvent list
   AccountNumber: AccountNumber
   RoutingNumber: RoutingNumber
} with

   member x.CompositeId = x.AccountId, x.OrgId

   member x.TransferRecipients =
      Map.fold
      <| fun acc key value -> Map.add key value acc
      <| Map.map
            (fun _ o -> TransferRecipient.Internal o)
            x.InternalTransferRecipients
      <| Map.map
            (fun _ o -> TransferRecipient.Domestic o)
            x.DomesticTransferRecipients

type AccountProfile = {
   AccountId: AccountId
   OrgId: OrgId
   Name: string
   Depository: AccountDepository
   AccountNumber: AccountNumber
   RoutingNumber: RoutingNumber
   Balance: decimal
   DailyInternalTransferAccrued: decimal
   DailyDomesticTransferAccrued: decimal
} with

   member x.CompositeId = x.AccountId, x.OrgId

module AccountProfile =
   let fromAccount (account: Account) : AccountProfile = {
      AccountId = account.AccountId
      OrgId = account.OrgId
      Name = account.Name
      Depository = account.Depository
      AccountNumber = account.AccountNumber
      RoutingNumber = account.RoutingNumber
      Balance = account.Balance
      DailyInternalTransferAccrued = 0m
      DailyDomesticTransferAccrued = 0m
   }

type OrgWithAccountProfiles = {
   Org: Org
   AccountProfiles: Map<AccountId, AccountProfile>
   Balance: decimal
}

type AccountMessage =
   | GetAccount
   | StateChange of AccountCommand
   | Event of AccountEvent
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
   | Register of Account * InitiatedById
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
