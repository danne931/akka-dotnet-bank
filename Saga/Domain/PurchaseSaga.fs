module PurchaseSaga

open System

open Bank.Purchase.Domain
open Lib.Saga

[<RequireQualifiedAccess>]
type PurchaseSagaStatus =
   | InProgress
   | Completed
   | Failed of PurchaseFailReason

[<RequireQualifiedAccess>]
type PurchaseSagaStartEvent = PurchaseIntent of PurchaseInfo

[<RequireQualifiedAccess>]
type PurchaseSagaEvent =
   | AccountReservedFunds
   | CardReservedFunds
   | CardIssuerUpdatedPurchaseProgress of CardIssuerPurchaseProgress
   | PurchaseRejected of PurchaseFailReason
   | PurchaseFailureAcknowledgedByCard
   | PurchaseFailureAcknowledgedByAccount
   | PurchaseSettledWithAccount of PurchaseClearing
   | PurchaseSettledWithCard of PurchaseClearing
   | PurchaseNotificationSent
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess; CustomEquality; NoComparison>]
type Activity =
   | ReserveEmployeeCardFunds
   | ReserveAccountFunds
   | SettlePurchaseWithAccount of PurchaseClearing
   | SettlePurchaseWithCard of PurchaseClearing
   | SendPurchaseNotification
   | AcquireCardFailureAcknowledgement
   | AcquireAccountFailureAcknowledgement
   | WaitForCardNetworkResolution

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | ReserveEmployeeCardFunds -> 1
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | ReserveEmployeeCardFunds
         | WaitForCardNetworkResolution -> None
         | SendPurchaseNotification -> Some(TimeSpan.FromMinutes 4.)
         | ReserveAccountFunds
         | AcquireCardFailureAcknowledgement
         | AcquireAccountFailureAcknowledgement
         | SettlePurchaseWithAccount _
         | SettlePurchaseWithCard _ -> Some(TimeSpan.FromSeconds 4.)

   override x.Equals compareTo =
      match compareTo with
      | :? Activity as compareTo -> x.GetHashCode() = compareTo.GetHashCode()
      | _ -> false

   override x.GetHashCode() =
      match x with
      | SettlePurchaseWithCard clearing ->
         hash $"SettlePurchaseWithCard-{clearing.PurchaseClearedId}"
      | SettlePurchaseWithAccount clearing ->
         hash $"SettlePurchaseWithAccount-{clearing.PurchaseClearedId}"
      | _ -> hash (string x)

type PurchaseSaga = {
   PurchaseInfo: PurchaseInfo
   InitialPurchaseIntentAmount: decimal
   CardIssuerPurchaseEvents: PurchaseEvent list
   CardIssuerProgressAmounts: PurchaseAmounts
   StartEvent: PurchaseSagaStartEvent
   StartedAt: DateTime
   Events: PurchaseSagaEvent list
   Status: PurchaseSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
   FailReason: PurchaseFailReason option
} with

   member x.ReservedEmployeeCardFunds =
      x.LifeCycle.Completed |> List.exists _.Activity.IsReserveEmployeeCardFunds

   member x.ReservedAccountFunds =
      x.LifeCycle.Completed |> List.exists _.Activity.IsReserveAccountFunds

   member x.OutgoingSettlementWithAccount =
      x.LifeCycle.InProgress
      |> List.tryPick (fun o ->
         match o.Activity with
         | Activity.SettlePurchaseWithAccount clearing -> Some clearing
         | _ -> None)

   member x.SettledWithAccount =
      x.LifeCycle.Completed
      |> List.exists _.Activity.IsSettlePurchaseWithAccount

   member x.SettledWithCard =
      x.LifeCycle.Completed |> List.exists _.Activity.IsSettlePurchaseWithCard

   member x.RequiresCardFailureAcknowledgement =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsAcquireCardFailureAcknowledgement

   member x.RequiresAccountFailureAcknowledgement =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsAcquireAccountFailureAcknowledgement

   member x.PurchaseNotificationSent =
      x.LifeCycle.Completed |> List.exists _.Activity.IsSendPurchaseNotification
