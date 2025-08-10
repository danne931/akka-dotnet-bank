module PlatformTransferSaga

open System

open Lib.SharedTypes
open Lib.Saga
open Bank.Transfer.Domain
open PartnerBank.Service.Domain

[<RequireQualifiedAccess>]
type PlatformTransferSagaStatus =
   | Scheduled
   | InProgress
   | Completed
   | Failed of InternalTransferFailReason

[<RequireQualifiedAccess>]
type PlatformTransferSagaStartEvent =
   | SenderReservedFunds of
      BankEvent<InternalTransferBetweenOrgsPending> *
      PartnerBankAccountLink
   | ScheduleTransferRequest of BankEvent<InternalTransferBetweenOrgsScheduled>

[<RequireQualifiedAccess>]
type PlatformTransferSagaEvent =
   | ScheduledTransferActivated
   | SenderReservedFunds of PartnerBankAccountLink
   | SenderUnableToReserveFunds of InternalTransferFailReason
   | RecipientDepositedFunds of PartnerBankAccountLink
   | RecipientUnableToDepositFunds of InternalTransferFailReason
   | TransferNotificationSent
   | TransferDepositNotificationSent
   | PartnerBankSyncResponse of Result<SettlementId, string>
   | SupportTeamResolvedPartnerBankSync
   | SenderReleasedReservedFunds
   | RecipientDepositUndo
   | TransferSettled
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess; CustomEquality; NoComparison>]
type Activity =
   | WaitForScheduledTransferActivation of TimeSpan
   | ReserveSenderFunds
   | DepositToRecipientAccount
   | SyncToPartnerBank
   | SettleTransfer
   | SendTransferNotification
   | SendTransferDepositNotification
   | ReleaseSenderFunds
   | UndoRecipientDeposit
   | WaitForSupportTeamToResolvePartnerBankSync

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForScheduledTransferActivation _
         | WaitForSupportTeamToResolvePartnerBankSync -> 0
         | SyncToPartnerBank -> 4
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | SendTransferNotification
         | SendTransferDepositNotification
         | SyncToPartnerBank -> Some(TimeSpan.FromMinutes 4.)
         | ReserveSenderFunds
         | DepositToRecipientAccount
         | SettleTransfer
         | ReleaseSenderFunds
         | UndoRecipientDeposit -> Some(TimeSpan.FromSeconds 5.)
         | WaitForScheduledTransferActivation time -> Some time
         | WaitForSupportTeamToResolvePartnerBankSync -> None

   // Custom equality check so we can, for example, check for completeness
   // of WaitForScheduledTransferActivation without comparing the inner value.
   // Ex: activityIsDone (Activity.WaitForScheduledTransferActivation TimeSpan.Zero)
   override x.Equals compareTo =
      match compareTo with
      | :? Activity as compareTo -> x.GetHashCode() = compareTo.GetHashCode()
      | _ -> false

   override x.GetHashCode() =
      match x with
      | WaitForScheduledTransferActivation _ ->
         hash "WaitForScheduledTransferActivation"
      | _ -> hash (string x)

type PlatformTransferSaga = {
   StartEvent: PlatformTransferSagaStartEvent
   Events: PlatformTransferSagaEvent list
   Status: PlatformTransferSagaStatus
   TransferInfo: BaseInternalTransferBetweenOrgsInfo
   LifeCycle: SagaLifeCycle<Activity>
   PartnerBankSenderAccountLink: PartnerBankAccountLink option
   PartnerBankRecipientAccountLink: PartnerBankAccountLink option
} with

   member x.SyncedToPartnerBank =
      x.LifeCycle.Completed |> List.exists _.Activity.IsSyncToPartnerBank

   member x.IsSettled =
      x.LifeCycle.Completed |> List.exists _.Activity.IsSettleTransfer

   member x.SettlementId =
      x.Events
      |> List.tryPick (function
         | PlatformTransferSagaEvent.PartnerBankSyncResponse(Ok settlementId) ->
            Some settlementId
         | _ -> None)

   member x.TransferNotificationSent =
      x.LifeCycle.Completed |> List.exists _.Activity.IsSendTransferNotification

   member x.TransferDepositNotificationSent =
      x.LifeCycle.Completed
      |> List.exists _.Activity.IsSendTransferDepositNotification

   member x.RequiresReleaseSenderFunds =
      x.LifeCycle.InProgress |> List.exists _.Activity.IsReleaseSenderFunds

   member x.IsTransferSchedulingAwaitingActivation =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsWaitForScheduledTransferActivation
