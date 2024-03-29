module BankTypes

open System
open System.Threading.Tasks
open Akka.Routing

open Lib.Types
open Bank.Account.Domain
open Bank.Transfer.Domain

type AccountEvent =
   | CreatedAccount of BankEvent<CreatedAccount>
   | DepositedCash of BankEvent<DepositedCash>
   | DebitedAccount of BankEvent<DebitedAccount>
   | MaintenanceFeeDebited of BankEvent<MaintenanceFeeDebited>
   | MaintenanceFeeSkipped of BankEvent<MaintenanceFeeSkipped>
   | DailyDebitLimitUpdated of BankEvent<DailyDebitLimitUpdated>
   | LockedCard of BankEvent<LockedCard>
   | UnlockedCard of BankEvent<UnlockedCard>
   | TransferPending of BankEvent<TransferPending>
   | TransferApproved of BankEvent<TransferApproved>
   | TransferRejected of BankEvent<TransferRejected>
   | InternalTransferRecipient of BankEvent<RegisteredInternalTransferRecipient>
   | DomesticTransferRecipient of BankEvent<RegisteredDomesticTransferRecipient>
   | InternationalTransferRecipient of
      BankEvent<RegisteredInternationalTransferRecipient>

type OpenEventEnvelope = AccountEvent * Envelope

[<RequireQualifiedAccess>]
module Envelope =
   let private get (evt: BankEvent<'E>) = {
      EntityId = evt.EntityId
      Timestamp = evt.Timestamp
      EventName = evt.EventName
      CorrelationId = evt.CorrelationId
   }

   let bind (transformer: obj -> 't) (evt: AccountEvent) =
      match evt with
      | CreatedAccount evt -> transformer evt
      | DepositedCash evt -> transformer evt
      | DebitedAccount evt -> transformer evt
      | MaintenanceFeeDebited evt -> transformer evt
      | MaintenanceFeeSkipped evt -> transformer evt
      | DailyDebitLimitUpdated evt -> transformer evt
      | LockedCard evt -> transformer evt
      | UnlockedCard evt -> transformer evt
      | InternalTransferRecipient evt -> transformer evt
      | DomesticTransferRecipient evt -> transformer evt
      | InternationalTransferRecipient evt -> transformer evt
      | TransferPending evt -> transformer evt
      | TransferApproved evt -> transformer evt
      | TransferRejected evt -> transformer evt

   let wrap (o: obj) : AccountEvent =
      match o with
      | :? BankEvent<CreatedAccount> as evt -> evt |> CreatedAccount
      | :? BankEvent<DepositedCash> as evt -> evt |> DepositedCash
      | :? BankEvent<DebitedAccount> as evt -> evt |> DebitedAccount
      | :? BankEvent<MaintenanceFeeDebited> as evt ->
         evt |> MaintenanceFeeDebited
      | :? BankEvent<MaintenanceFeeSkipped> as evt ->
         evt |> MaintenanceFeeSkipped
      | :? BankEvent<DailyDebitLimitUpdated> as evt ->
         evt |> DailyDebitLimitUpdated
      | :? BankEvent<LockedCard> as evt -> evt |> LockedCard
      | :? BankEvent<UnlockedCard> as evt -> evt |> UnlockedCard
      | :? BankEvent<RegisteredInternalTransferRecipient> as evt ->
         evt |> InternalTransferRecipient
      | :? BankEvent<RegisteredDomesticTransferRecipient> as evt ->
         evt |> DomesticTransferRecipient
      | :? BankEvent<RegisteredInternationalTransferRecipient> as evt ->
         evt |> InternationalTransferRecipient
      | :? BankEvent<TransferPending> as evt -> evt |> TransferPending
      | :? BankEvent<TransferApproved> as evt -> evt |> TransferApproved
      | :? BankEvent<TransferRejected> as evt -> evt |> TransferRejected

   let unwrap (o: AccountEvent) : OpenEventEnvelope =
      match o with
      | CreatedAccount evt -> (wrap evt, get evt)
      | DepositedCash evt -> (wrap evt, get evt)
      | DebitedAccount evt -> (wrap evt, get evt)
      | MaintenanceFeeDebited evt -> (wrap evt, get evt)
      | MaintenanceFeeSkipped evt -> (wrap evt, get evt)
      | DailyDebitLimitUpdated evt -> (wrap evt, get evt)
      | LockedCard evt -> (wrap evt, get evt)
      | UnlockedCard evt -> (wrap evt, get evt)
      | InternalTransferRecipient evt -> (wrap evt, get evt)
      | DomesticTransferRecipient evt -> (wrap evt, get evt)
      | InternationalTransferRecipient evt -> (wrap evt, get evt)
      | TransferPending evt -> (wrap evt, get evt)
      | TransferApproved evt -> (wrap evt, get evt)
      | TransferRejected evt -> (wrap evt, get evt)

type AccountStatus =
   | Active
   | ActiveWithLockedCard
   | Closed

type AccountState = {
   EntityId: Guid
   FirstName: string
   LastName: string
   Currency: Currency
   Status: AccountStatus
   Balance: decimal
   AllowedOverdraft: decimal
   DailyDebitLimit: decimal
   DailyDebitAccrued: decimal
   LastDebitDate: DateTime option
   TransferRecipients: Map<string, TransferRecipient>
   MaintenanceFeeCriteria: MaintenanceFeeCriteria
} with

   member x.FullName = $"{x.FirstName} {x.LastName}"

type AccountCoordinatorMessage =
   | InitAccount of AccountState
   | StateChange of Command
   | Delete of Guid

   member x.consistentHash() =
      match x with
      | InitAccount account ->
         ConsistentHashableEnvelope(InitAccount account, account.EntityId)
      | StateChange cmd ->
         ConsistentHashableEnvelope(StateChange cmd, cmd.EntityId)
      | Delete id -> ConsistentHashableEnvelope(Delete id, id)

type AccountMessage =
   | StartChildren
   | Lookup
   | StateChange of Command

type AccountPersistence = {
   loadAccountEvents: Guid -> AccountEvent list option Task
   loadAccount: Guid -> AccountState option Task
   save: OpenEventEnvelope -> unit Task
}

type Service = | DomesticTransfer

type CircuitBreakerStatus =
   | Closed
   | HalfOpen
   | Open

type CircuitBreakerMessage = {
   Service: Service
   Status: CircuitBreakerStatus
}

type AccountBroadcast = {
   broadcast: AccountEvent * AccountState -> Task
   broadcastError: string -> Task
   broadcastCircuitBreaker: CircuitBreakerMessage -> Task
}
