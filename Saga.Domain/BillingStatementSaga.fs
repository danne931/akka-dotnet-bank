module BillingSaga

open System

open Lib.SharedTypes
open Bank.Account.Domain
open MaintenanceFee
open Lib.Saga

[<RequireQualifiedAccess>]
type BillingSagaStatus =
   | InProgress
   | Completed
   | Failed

type BillingSagaStartEvent = {
   BillingPeriod: BillingPeriod
   BillingCycleDate: DateTime
   CorrelationId: CorrelationId
   ParentAccountId: ParentAccountId
   OrgId: OrgId
}

type ProcessingBillingStatement = {
   PrimaryCheckingAccountId: AccountId
   MaintenanceFeeCriteria: MaintenanceFeeCriteria
}

[<RequireQualifiedAccess>]
type BillingSagaEvent =
   | BillingStatementProcessing of ProcessingBillingStatement
   | BillingStatementPersisted
   | MaintenanceFeeProcessed
   | BillingEmailSent
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess>]
type Activity =
   | StartBilling
   | ProcessBillingStatement
   | WaitForBillingStatementPersisted
   | ProcessMaintenanceFee
   | SendBillingEmail

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | StartBilling
         | WaitForBillingStatementPersisted -> 1
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | StartBilling
         | WaitForBillingStatementPersisted -> None
         | ProcessBillingStatement -> Some(TimeSpan.FromSeconds 10.)
         | ProcessMaintenanceFee -> Some(TimeSpan.FromSeconds 10.)
         | SendBillingEmail -> Some(TimeSpan.FromMinutes 4.)

type BillingSaga = {
   CorrelationId: CorrelationId
   ParentAccountId: ParentAccountId
   OrgId: OrgId
   BillingPeriod: BillingPeriod
   BillingCycleDate: DateTime
   ProcessingBillingStatement: ProcessingBillingStatement option
   StartEvent: BillingSagaStartEvent
   Events: BillingSagaEvent list
   Status: BillingSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
}
