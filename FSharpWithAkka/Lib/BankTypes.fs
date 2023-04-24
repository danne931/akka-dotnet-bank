module BankTypes

open Lib.Types
open Bank.Account.Domain
open Bank.Transfer.Domain

type AccountCommand =
   | DepositCashCommand
   | DebitCommand
   | LimitDailyDebitsCommand
   | LockCardCommand
   | UnlockCardCommand

type AccountEvent =
   | CreatedAccount of BankEvent<CreatedAccount>
   | DepositedCash of BankEvent<DepositedCash>
   | DebitedAccount of BankEvent<DebitedAccount>
   | DailyDebitLimitUpdated of BankEvent<DailyDebitLimitUpdated>
   | LockedCard of BankEvent<LockedCard>
   | UnlockedCard of BankEvent<UnlockedCard>
   | DebitedTransfer of BankEvent<DebitedTransfer>
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
   }

   let bind (transformer: obj -> 't) (evt: AccountEvent) =
      match evt with
      | CreatedAccount(evt) -> transformer evt
      | DepositedCash(evt) -> transformer evt
      | DebitedAccount(evt) -> transformer evt
      | DailyDebitLimitUpdated(evt) -> transformer evt
      | LockedCard(evt) -> transformer evt
      | UnlockedCard(evt) -> transformer evt
      | InternalTransferRecipient(evt) -> transformer evt
      | DomesticTransferRecipient(evt) -> transformer evt
      | InternationalTransferRecipient(evt) -> transformer evt
      | DebitedTransfer(evt) -> transformer evt

   let unwrap (o: obj) : OpenEventEnvelope =
      match o with
      | :? BankEvent<CreatedAccount> as evt -> (evt |> CreatedAccount, get evt)
      | :? BankEvent<DepositedCash> as evt -> (evt |> DepositedCash, get evt)
      | :? BankEvent<DebitedAccount> as evt -> (evt |> DebitedAccount, get evt)
      | :? BankEvent<DailyDebitLimitUpdated> as evt ->
         (evt |> DailyDebitLimitUpdated, get evt)
      | :? BankEvent<LockedCard> as evt -> (evt |> LockedCard, get evt)
      | :? BankEvent<UnlockedCard> as evt -> (evt |> UnlockedCard, get evt)
      | :? BankEvent<RegisteredInternalTransferRecipient> as evt ->
         (evt |> InternalTransferRecipient, get evt)
      | :? BankEvent<RegisteredDomesticTransferRecipient> as evt ->
         (evt |> DomesticTransferRecipient, get evt)
      | :? BankEvent<RegisteredInternationalTransferRecipient> as evt ->
         (evt |> InternationalTransferRecipient, get evt)
      | :? BankEvent<DebitedTransfer> as evt ->
         (evt |> DebitedTransfer, get evt)
