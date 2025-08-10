module PurchaseSaga

open System

open Lib.SharedTypes
open Bank.Account.Domain
open Lib.Saga
open PartnerBank.Service.Domain

[<RequireQualifiedAccess>]
type PurchaseSagaStatus =
   | InProgress
   | Completed
   | Failed of PurchaseFailReason

[<RequireQualifiedAccess>]
type PurchaseSagaStartEvent =
   | PurchaseIntent of PurchaseInfo
   | PurchaseRejectedByCard of PurchaseInfo * PurchaseCardFailReason

[<RequireQualifiedAccess>]
type PurchaseSagaEvent =
   | PurchaseFailureAcknowledgedByCard
   | AccountReservedFunds of PartnerBankAccountLink
   | PurchaseRejectedByAccount of PurchaseAccountFailReason
   | PurchaseFailureAcknowledgedByAccount
   | PurchaseRejectedCardNetworkResponse of
      PurchaseFailReason *
      Result<string, string>
   | CardNetworkResponse of Result<string, string>
   | PartnerBankSyncResponse of Result<SettlementId, string>
   | PurchaseSettledWithAccount
   | PurchaseSettledWithCard
   | PurchaseNotificationSent
   | SupportTeamResolvedPartnerBankSync
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess>]
type Activity =
   | ReserveEmployeeCardFunds
   | ReserveAccountFunds
   | NotifyCardNetworkOfRejectedPurchase
   | NotifyCardNetworkOfConfirmedPurchase
   | SyncToPartnerBank
   | SettlePurchaseWithAccount
   | SettlePurchaseWithCard
   | SendPurchaseNotification
   | AcquireCardFailureAcknowledgement
   | AcquireAccountFailureAcknowledgement
   | WaitForSupportTeamToResolvePartnerBankSync

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForSupportTeamToResolvePartnerBankSync -> 0
         | ReserveEmployeeCardFunds -> 1
         | NotifyCardNetworkOfConfirmedPurchase -> 2
         | SyncToPartnerBank -> 4
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | ReserveEmployeeCardFunds
         | NotifyCardNetworkOfRejectedPurchase
         | WaitForSupportTeamToResolvePartnerBankSync -> None
         | SendPurchaseNotification
         | SyncToPartnerBank -> Some(TimeSpan.FromMinutes 4.)
         | NotifyCardNetworkOfConfirmedPurchase
         | ReserveAccountFunds
         | AcquireCardFailureAcknowledgement
         | AcquireAccountFailureAcknowledgement
         | SettlePurchaseWithAccount
         | SettlePurchaseWithCard -> Some(TimeSpan.FromSeconds 4.)

type PurchaseSaga = {
   PurchaseInfo: PurchaseInfo
   StartEvent: PurchaseSagaStartEvent
   Events: PurchaseSagaEvent list
   Status: PurchaseSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
   FailReason: PurchaseFailReason option
   PartnerBankAccountLink: PartnerBankAccountLink option
} with

   member x.ReservedEmployeeCardFunds =
      x.LifeCycle.Completed |> List.exists _.Activity.IsReserveEmployeeCardFunds

   member x.ReservedAccountFunds =
      x.LifeCycle.Completed |> List.exists _.Activity.IsReserveAccountFunds

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

   member x.SyncedToPartnerBank =
      x.Events
      |> List.tryPick (function
         | PurchaseSagaEvent.PartnerBankSyncResponse(Ok settlementId) ->
            Some settlementId
         | _ -> None)

   member x.RequiresManualSupportFixForPartnerBankSync =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsWaitForSupportTeamToResolvePartnerBankSync
