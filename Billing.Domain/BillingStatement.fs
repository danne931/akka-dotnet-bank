module BillingStatement

open System

open Lib.SharedTypes
open Bank.Account.Domain

type BillingTransaction = private BillingTransaction of AccountEvent

type BillingPeriod = { Month: int; Year: int }

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
         | CreatedVirtualAccount _
         | DepositedCash _
         | DebitedAccount _
         | MaintenanceFeeDebited _
         | InternalTransferWithinOrgPending _
         | InternalTransferWithinOrgFailed _
         | InternalTransferWithinOrgDeposited _
         | InternalTransferBetweenOrgsPending _
         | InternalTransferBetweenOrgsFailed _
         | InternalTransferBetweenOrgsDeposited _
         | DomesticTransferPending _
         | DomesticTransferFailed _ -> Some(BillingTransaction evt)
         | _ -> None

   let value (BillingTransaction evt) = evt

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

type BillingPersistable = {
   ParentAccountId: ParentAccountId
   ParentAccountSnapshot: byte[]
   LastPersistedEventSequenceNumber: Int64
   Statements: BillingStatement list
}

type BillingCycleMessage =
   | BillingCycleFanout
   | BillingCycleFinished

type BillingStatementMessage =
   | RegisterBillingStatement of BillingPersistable
   | GetFailedWrites

let billingTransactions (period: BillingPeriod) (evts: AccountEvent list) =
   List.choose (BillingTransaction.create period) evts

let billingStatement
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
