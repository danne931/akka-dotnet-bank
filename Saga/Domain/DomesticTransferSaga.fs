module DomesticTransferSaga

open System

open Lib.SharedTypes
open Bank.Transfer.Domain
open Lib.Saga
open Bank.Account.Domain

[<RequireQualifiedAccess>]
type DomesticTransferSagaStartEvent =
   | SenderReservedFunds of
      BankEvent<DomesticTransferPending> *
      PartnerBankInternalAccountLink
   | ScheduleTransferRequest of
      BankEvent<DomesticTransferScheduled> *
      PartnerBankInternalAccountLink

[<RequireQualifiedAccess>]
type DomesticTransferSagaEvent =
   | ScheduledTransferActivated
   | SenderReservedFunds
   | SenderReleasedReservedFunds
   | TransferProcessorProgressUpdate of DomesticTransferThirdPartyUpdate
   | SenderDeductedFunds
   | TransferInitiatedNotificationSent
   | RetryTransferServiceRequest of
      updatedRecipient: DomesticTransferRecipient option
   | SenderUnableToReserveFunds of DomesticTransferFailReason
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess; CustomEquality; NoComparison>]
type Activity =
   | WaitForScheduledTransferActivation of TimeSpan
   | ReserveSenderFunds
   | ReleaseSenderReservedFunds
   | TransferServiceAck
   | WaitForTransferServiceComplete
   | DeductSenderFunds
   | SendTransferInitiatedNotification
   | WaitForDevelopmentTeamFix

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForScheduledTransferActivation _
         | WaitForDevelopmentTeamFix -> 0
         // Check every 4 hours, 6 times a day for 6 days.
         | WaitForTransferServiceComplete -> 36
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | WaitForScheduledTransferActivation time -> Some time
         | WaitForDevelopmentTeamFix -> None
         | WaitForTransferServiceComplete ->
            Some(
               if Env.isProd then
                  TimeSpan.FromHours 4.
               else
                  TimeSpan.FromMinutes 1.
            )
         | TransferServiceAck
         | SendTransferInitiatedNotification -> Some(TimeSpan.FromMinutes 4.)
         | ReserveSenderFunds
         | ReleaseSenderReservedFunds
         | DeductSenderFunds -> Some(TimeSpan.FromSeconds 5.)

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

type DomesticTransferSaga = {
   StartEvent: DomesticTransferSagaStartEvent
   Events: DomesticTransferSagaEvent list
   Status: DomesticTransferProgress
   TransferInfo: BaseDomesticTransferInfo
   PartnerBankAccountLink: PartnerBankInternalAccountLink
   ExpectedSettlementDate: DateTime
   LifeCycle: SagaLifeCycle<Activity>
   ReasonForRetryServiceAck: DomesticTransferFailReason option
} with

   member x.OriginatedFromSchedule = x.StartEvent.IsScheduleTransferRequest

   member x.SenderDeductedFunds =
      x.LifeCycle.Completed |> List.exists _.Activity.IsDeductSenderFunds

   member x.TransferInitiatedNotificationSent =
      x.LifeCycle.Completed
      |> List.exists _.Activity.IsSendTransferInitiatedNotification

   member x.RequiresAccountRefund =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsReleaseSenderReservedFunds

   member x.RequiresTransferServiceDevelopmentFix =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsWaitForDevelopmentTeamFix

   member x.IsTransferSchedulingAwaitingActivation =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsWaitForScheduledTransferActivation
