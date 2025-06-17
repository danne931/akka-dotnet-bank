module BillingStatement

open System

open Lib.SharedTypes
open Bank.Account.Domain

type BillingTransaction = private BillingTransaction of AccountEvent

module BillingTransaction =
   let create
      (period: BillingPeriod)
      (evt: AccountEvent)
      : BillingTransaction option
      =
      let cutoffStart = DateTime(period.Year, period.Month, 1).ToUniversalTime()
      let cutoffEnd = cutoffStart.AddMonths(1)
      let _, env = AccountEnvelope.unwrap evt

      if env.Timestamp < cutoffStart || env.Timestamp >= cutoffEnd then
         None
      else
         match evt with
         | DepositedCash _
         | DebitSettled _
         | DebitRefunded _
         | MaintenanceFeeDebited _
         | InternalTransferWithinOrgDeducted _
         | InternalTransferWithinOrgDeposited _
         | InternalTransferBetweenOrgsSettled _
         | InternalTransferBetweenOrgsDeposited _
         | InternalAutomatedTransferDeducted _
         | InternalAutomatedTransferDeposited _
         | PlatformPaymentSettled _
         | PlatformPaymentDeposited _
         | PlatformPaymentRefunded _
         | DomesticTransferSettled _ -> Some(BillingTransaction evt)
         | _ -> None

   let value (BillingTransaction evt) = evt

let billingTransactions (period: BillingPeriod) (evts: AccountEvent list) =
   List.choose (BillingTransaction.create period) evts

/// Contains all money in/out pertaining to a virtual account.
/// Corresponds directly to what is stored in the billing_statement table.
type BillingStatement = {
   Transactions: BillingTransaction list
   Month: int
   Year: int
   Balance: decimal
   AccountName: string
   AccountId: AccountId
   ParentAccountId: ParentAccountId
   OrgId: OrgId
}

/// Compute a BillingStatement for a given billing period for a virtual account
let virtualAccountBillingStatement
   (account: Account)
   (events: AccountEvent list)
   (period: BillingPeriod)
   : BillingStatement
   =
   {
      Transactions = billingTransactions period events
      Month = period.Month
      Year = period.Year
      Balance = account.Balance
      AccountName = account.FullName
      AccountId = account.AccountId
      ParentAccountId = account.ParentAccountId
      OrgId = account.OrgId
   }

/// Compute BillingStatements for a given billing period for each
/// of a parent account's subaccounts.
let parentAccountBillingStatements
   (state: ParentAccountSnapshot)
   (period: BillingPeriod)
   =
   state.VirtualAccounts.Values
   |> Seq.filter (fun a -> a.Status = AccountStatus.Active)
   |> Seq.map (fun account ->
      virtualAccountBillingStatement
         account
         (state.eventsForAccount account.AccountId)
         period)
   |> Seq.toList

/// Contains BillingStatements for all virtual accounts of a parent account.
/// Also contains the ParentAccountSnapshot corresponding to the actor snapshot.
///
/// Sent to the BillingStatementActor once a month to persist billing statements
/// and parent account snapshots for all organizations.
type BillingPersistable = {
   CorrelationId: CorrelationId
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   ParentAccountSnapshot: byte[]
   LastPersistedEventSequenceNumber: Int64
   Statements: BillingStatement list
}

[<RequireQualifiedAccess>]
type BillingCycleMessage =
   | BillingCycleFanout
   | BillingCycleFinished

[<RequireQualifiedAccess>]
type BillingStatementMessage =
   | BulkPersist of BillingPersistable
   | GetFailedWrites
