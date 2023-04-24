namespace Bank.Account.Domain

open System

open Lib.Types

type CreatedAccount = {
   FirstName: string
   LastName: string
   Balance: decimal
   Currency: string
}

module CreatedAccountEvent =
   let create (cmd: CreateAccountCommand) = {
      EntityId = cmd.EntityId
      Timestamp = cmd.Timestamp
      Data = {
         FirstName = cmd.FirstName
         LastName = cmd.LastName
         Balance = cmd.Balance
         Currency = cmd.Currency.Value
      }
   }

type LockedCard = { Reference: string }

module LockedCardEvent =
   let create (cmd: LockCardCommand) = {
      EntityId = cmd.EntityId
      Timestamp = cmd.Timestamp
      Data = { Reference = cmd.Reference }
   }

type UnlockedCard = { Reference: string }

module UnlockedCardEvent =
   let create (cmd: UnlockCardCommand) = {
      EntityId = cmd.EntityId
      Timestamp = cmd.Timestamp
      Data = { Reference = cmd.Reference }
   }

type DepositedCash = {
   DepositedAmount: decimal
   Origin: string
}

module DepositedCashEvent =
   let create (cmd: DepositCashCommand) = {
      EntityId = cmd.EntityId
      Timestamp = cmd.Timestamp
      Data = {
         DepositedAmount = cmd.Amount
         Origin = cmd.Origin.Value
      }
   }

type DebitedAccount = {
   Date: DateTime
   DebitedAmount: decimal
   Origin: string
   Reference: string
}

module DebitedAccountEvent =
   let create (cmd: DebitCommand) = {
      EntityId = cmd.EntityId
      Timestamp = cmd.Timestamp
      Data = {
         DebitedAmount = cmd.Amount
         Origin = cmd.Origin
         Date = cmd.Date
         Reference = cmd.Reference.Value
      }
   }

type DailyDebitLimitUpdated = { DebitLimit: decimal }

module DailyDebitLimitUpdatedEvent =
   let create
      (cmd: LimitDailyDebitsCommand)
      : BankEvent<DailyDebitLimitUpdated>
      =
      {
         EntityId = cmd.EntityId
         Timestamp = cmd.Timestamp
         Data = { DebitLimit = cmd.DebitLimit }
      }
