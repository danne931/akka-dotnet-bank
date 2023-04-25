namespace Bank.Account.Domain

open System
open Lib.Types

type CreateAccountCommand
   (
      entityId,
      balance: decimal,
      firstName: string,
      lastName: string,
      currency: string
   ) =
   inherit Command(entityId)
   member x.Currency = if isNull currency then "USD" else currency
   member x.Balance = balance
   member x.FirstName = firstName
   member x.LastName = lastName

type DepositCashCommand(entityId, amount: decimal, origin: string) =
   inherit Command(entityId)
   member x.Amount = amount
   member x.Origin = if isNull origin then "ATM" else origin

type DebitCommand
   (entityId, date: DateTime, amount: decimal, origin: string, reference: string)
   =
   inherit Command(entityId)
   member x.Date = date
   member x.Amount = amount
   member x.Origin = origin
   member x.Reference = reference

type LimitDailyDebitsCommand(entityId, debitLimit: decimal) =
   inherit Command(entityId)
   member x.DebitLimit = debitLimit

type LockCardCommand(entityId, reference: string) =
   inherit Command(entityId)
   member x.Reference = reference

type UnlockCardCommand(entityId, reference: string) =
   inherit Command(entityId)
   member x.Reference = reference
