module PurchaseSaga

open System

open Bank.Purchase.Domain
open Lib.Saga
open Lib.SharedTypes

[<RequireQualifiedAccess>]
type PurchaseSagaStatus =
   | InProgress
   | Completed
   | Failed of PurchaseFailReason

[<RequireQualifiedAccess>]
type PurchaseSagaStartEvent =
   /// Initiate progress workflow from purchase authorization received by
   /// card issuer purchase auth webhook.
   | PurchaseIntent of PurchaseInfo
   /// It is sometimes possible to receive a purchase progress update from Lithic without
   /// first receiving the purchase intent at the authorization stream access
   /// webhook.  This poses some risk in that they may allow a purchase to settle
   /// for an amount above the cardholder's balance, without affording the user
   /// the opportunity to decline.  Such situations may be subject to chargeback.
   /// See Purchase/Domain/PurchaseLifecycleEvent.fs cases 16-20.
   | PurchaseProgress of PurchaseInfo * CardIssuerPurchaseProgress

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
   | ReserveEmployeeCardFundsBypassingAuth
   | ReserveAccountFundsBypassingAuth
   | BufferCardIssuerPurchaseProgress of CardIssuerPurchaseProgress
   | SettlePurchaseWithAccount of PurchaseClearing
   | SettlePurchaseWithCard of PurchaseClearing
   | SendPurchaseNotification
   | AcquireCardFailureAcknowledgement
   | AcquireAccountFailureAcknowledgement
   | WaitForCardNetworkResolution

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForCardNetworkResolution
         | ReserveEmployeeCardFunds
         | ReserveAccountFunds -> 1
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | WaitForCardNetworkResolution -> None
         | ReserveEmployeeCardFundsBypassingAuth
         | ReserveAccountFundsBypassingAuth
         | BufferCardIssuerPurchaseProgress _ -> Some(TimeSpan.FromMinutes 1.)
         | SendPurchaseNotification -> Some(TimeSpan.FromMinutes 4.)
         | ReserveEmployeeCardFunds
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
      | BufferCardIssuerPurchaseProgress _ ->
         hash "BufferCardIssuerPurchaseProgress"
      | _ -> hash (string x)

type OutgoingCommandIdempotencyKeys = {
   ReserveEmployeeCardFunds: EventId
   ReserveAccountFunds: EventId
   AcquireCardFailureAcknowledgement: EventId
   AcquireAccountFailureAcknowledgement: EventId
}

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
   OutgoingCommandIdempotencyKeys: OutgoingCommandIdempotencyKeys
} with

   member x.ReservedEmployeeCardFunds =
      x.LifeCycle.Completed |> List.exists _.Activity.IsReserveEmployeeCardFunds
      || x.LifeCycle.Completed
         |> List.exists _.Activity.IsReserveEmployeeCardFundsBypassingAuth

   member x.ReservedAccountFunds =
      x.LifeCycle.Completed |> List.exists _.Activity.IsReserveAccountFunds
      || x.LifeCycle.Completed
         |> List.exists _.Activity.IsReserveAccountFundsBypassingAuth

   member x.ReservedFunds =
      x.ReservedAccountFunds && x.ReservedEmployeeCardFunds

   /// Should immediately designate funds as settled after fund reservation
   member x.OriginatedFromForcePost =
      match x.StartEvent with
      | PurchaseSagaStartEvent.PurchaseProgress(_, progress) ->
         let origin = progress.OriginatingEvent

         if origin.Type = PurchaseEventType.Clearing then
            let clearing = {
               ClearedAmount = origin.Money
               PurchaseClearedId = PurchaseClearedId origin.EventId
            }

            Some clearing
         else
            None
      | _ -> None

   member x.OriginatedFromPendingAuthAdvice =
      match x.StartEvent with
      | PurchaseSagaStartEvent.PurchaseProgress(_, progress) ->
         let origin = progress.OriginatingEvent
         origin.Type.IsAuthAdvice && progress.Status.IsPending
      | _ -> false

   member x.OutgoingSettlementWithAccount =
      x.LifeCycle.InProgress
      |> List.tryPick (fun o ->
         match o.Activity with
         | Activity.SettlePurchaseWithAccount clearing -> Some clearing
         | _ -> None)

   member x.BufferedCardIssuerPurchaseProgress =
      x.LifeCycle.InProgress
      |> List.tryPick (fun o ->
         match o.Activity with
         | Activity.BufferCardIssuerPurchaseProgress progress -> Some progress
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
