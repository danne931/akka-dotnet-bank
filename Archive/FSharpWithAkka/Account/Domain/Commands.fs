namespace Bank.Account.Domain

open System
open Lib.Types

type CreateAccountCommand
   (
      entityId,
      balance: decimal,
      firstName: string,
      lastName: string,
      currency: Currency,
      correlationId
   ) =
   inherit Command(entityId, correlationId)
   member x.Currency = currency
   member x.Balance = balance
   member x.FirstName = firstName
   member x.LastName = lastName

type DepositCashCommand
   (entityId, amount: decimal, origin: string, correlationId) =
   inherit Command(entityId, correlationId)
   member x.Amount = amount
   member x.Origin = if isNull origin then "ATM" else origin

type DebitCommand
   (
      entityId,
      date: DateTime,
      amount: decimal,
      origin: string,
      reference: string,
      correlationId
   ) =
   inherit Command(entityId, correlationId)
   member x.Date = date
   member x.Amount = amount
   member x.Origin = origin
   member x.Reference = reference

type LimitDailyDebitsCommand(entityId, debitLimit: decimal, correlationId) =
   inherit Command(entityId, correlationId)
   member x.DebitLimit = debitLimit

type LockCardCommand(entityId, reference: string, correlationId) =
   inherit Command(entityId, correlationId)
   member x.Reference = reference

type UnlockCardCommand(entityId, reference: string, correlationId) =
   inherit Command(entityId, correlationId)
   member x.Reference = reference

type MaintenanceFeeCommand(entityId) =
   inherit Command(entityId, correlationId = Guid.Empty)
   member x.Amount = 5m

type SkipMaintenanceFeeCommand(entityId, reason: MaintenanceFeeCriteria) =
   inherit Command(entityId, correlationId = Guid.Empty)
   member x.Reason = reason
