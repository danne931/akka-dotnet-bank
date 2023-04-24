namespace Bank.Account.Domain

open System
open Microsoft.FSharp.Core.Option
open Lib.Types

type CreateAccountCommand = {
   Currency: string option
   Balance: decimal
   FirstName: string
   LastName: string
   EntityId: Guid
   Timestamp: DateTime
}

module CreateAccountCommand =
   let create (cmd: CreateAccountCommand) = {
      cmd with
         Timestamp = DateTime.UtcNow
         Currency = if isSome cmd.Currency then cmd.Currency else Some "USD"
   }

type DepositCashCommand = {
   Amount: decimal
   Origin: string option
   EntityId: Guid
   Timestamp: DateTime
}

module DepositCashCommand =
   let create (cmd: DepositCashCommand) = {
      cmd with
         Origin = if isSome cmd.Origin then cmd.Origin else Some "ATM"
   }

type DebitCommand = {
   EntityId: Guid
   Date: DateTime
   Amount: decimal
   Origin: string
   Reference: string option
   Timestamp: DateTime
}

type LimitDailyDebitsCommand = {
   EntityId: Guid
   Timestamp: DateTime
   DebitLimit: decimal
}

type LockCardCommand = {
   EntityId: Guid
   Timestamp: DateTime
   Reference: string
}

type UnlockCardCommand = {
   EntityId: Guid
   Timestamp: DateTime
   Reference: string
}
