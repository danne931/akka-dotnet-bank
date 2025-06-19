module PurchaseSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Employee.Domain
open Email
open Lib.Saga
open PartnerBank.Service.Domain

[<RequireQualifiedAccess>]
type PurchaseSagaStatus =
   | InProgress
   | Completed
   | Failed of PurchaseFailReason

[<RequireQualifiedAccess>]
type PurchaseSagaStartEvent =
   | DeductedCardFunds of PurchaseInfo
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
   | PurchaseSettled
   | PurchaseNotificationSent
   | SupportTeamResolvedPartnerBankSync
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess>]
type Activity =
   | DeductFromEmployeeCard
   | ReserveAccountFunds
   | NotifyCardNetworkOfRejectedPurchase
   | NotifyCardNetworkOfConfirmedPurchase
   | SyncToPartnerBank
   | SettlePurchase
   | SendPurchaseNotification
   | AcquireCardFailureAcknowledgement
   | AcquireAccountFailureAcknowledgement
   | WaitForSupportTeamToResolvePartnerBankSync

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForSupportTeamToResolvePartnerBankSync -> 0
         | NotifyCardNetworkOfConfirmedPurchase -> 2
         | SyncToPartnerBank -> 4
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | NotifyCardNetworkOfRejectedPurchase
         | WaitForSupportTeamToResolvePartnerBankSync -> None
         | SendPurchaseNotification
         | SyncToPartnerBank -> Some(TimeSpan.FromMinutes 4)
         | NotifyCardNetworkOfConfirmedPurchase
         | DeductFromEmployeeCard
         | ReserveAccountFunds
         | AcquireCardFailureAcknowledgement
         | AcquireAccountFailureAcknowledgement
         | SettlePurchase -> Some(TimeSpan.FromSeconds 4)

type PurchaseSaga = {
   PurchaseInfo: PurchaseInfo
   StartEvent: PurchaseSagaStartEvent
   Events: PurchaseSagaEvent list
   Status: PurchaseSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
   FailReason: PurchaseFailReason option
   PartnerBankAccountLink: PartnerBankAccountLink option
} with

   member x.DeductedFromEmployeeCard =
      x.LifeCycle.Completed |> List.exists _.Activity.IsDeductFromEmployeeCard

   member x.DeductedFromAccount =
      x.LifeCycle.Completed |> List.exists _.Activity.IsReserveAccountFunds

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

let applyStartEvent
   (start: PurchaseSagaStartEvent)
   (timestamp: DateTime)
   : PurchaseSaga
   =
   match start with
   | PurchaseSagaStartEvent.DeductedCardFunds info -> {
      PurchaseInfo = info
      StartEvent = start
      Events = []
      Status = PurchaseSagaStatus.InProgress
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init timestamp Activity.ReserveAccountFunds
            ]
            Completed = [
               {
                  Start = timestamp
                  End = Some timestamp
                  Activity = Activity.DeductFromEmployeeCard
                  MaxAttempts =
                     (Activity.DeductFromEmployeeCard :> IActivity).MaxAttempts
                  Attempts = 1
               }
            ]
      }
      FailReason = None
      PartnerBankAccountLink = None
     }
   | PurchaseSagaStartEvent.PurchaseRejectedByCard(info, reason) -> {
      PurchaseInfo = info
      StartEvent = start
      Events = []
      Status = PurchaseSagaStatus.Failed(PurchaseFailReason.Card reason)
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init
                  timestamp
                  Activity.NotifyCardNetworkOfRejectedPurchase
            ]
      }
      FailReason = None
      PartnerBankAccountLink = None
     }

let applyEvent
   (saga: PurchaseSaga)
   (evt: PurchaseSagaEvent)
   (timestamp: DateTime)
   : PurchaseSaga
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp
   let failActivity = SagaLifeCycle.failActivity timestamp
   let retryActivity = SagaLifeCycle.retryActivity timestamp

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | PurchaseSagaEvent.AccountReservedFunds link -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.ReserveAccountFunds
            |> addActivity Activity.NotifyCardNetworkOfConfirmedPurchase
         PartnerBankAccountLink = Some link
     }
   | PurchaseSagaEvent.PurchaseRejectedCardNetworkResponse(_,
                                                           cardNetworkResponse) ->
      match cardNetworkResponse with
      | Error _ -> {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> failActivity Activity.NotifyCardNetworkOfRejectedPurchase
        }
      | Ok _ -> {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.NotifyCardNetworkOfRejectedPurchase
        }
   | PurchaseSagaEvent.CardNetworkResponse res ->
      match res with
      | Error err ->
         let activity = Activity.NotifyCardNetworkOfConfirmedPurchase

         if saga.LifeCycle.ActivityHasRemainingAttempts activity then
            {
               saga with
                  LifeCycle = retryActivity activity saga.LifeCycle
            }
         else
            let life = failActivity activity saga.LifeCycle

            let life =
               if saga.DeductedFromEmployeeCard then
                  addActivity Activity.AcquireCardFailureAcknowledgement life
               else
                  life

            let life =
               if saga.DeductedFromAccount then
                  addActivity Activity.AcquireAccountFailureAcknowledgement life
               else
                  life

            let err = PurchaseFailReason.CardNetwork err

            {
               saga with
                  LifeCycle = life
                  Status = PurchaseSagaStatus.Failed err
                  FailReason = Some err
            }
      | Ok _ -> {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.NotifyCardNetworkOfConfirmedPurchase
               |> addActivity Activity.SyncToPartnerBank
        }
   | PurchaseSagaEvent.PartnerBankSyncResponse res ->
      let activity = Activity.SyncToPartnerBank

      match res with
      | Error err ->
         if saga.LifeCycle.ActivityHasRemainingAttempts activity then
            {
               saga with
                  LifeCycle = retryActivity activity saga.LifeCycle
            }
         else
            {
               saga with
                  Status =
                     PurchaseFailReason.PartnerBankSync err
                     |> PurchaseSagaStatus.Failed
                  LifeCycle =
                     saga.LifeCycle
                     |> failActivity activity
                     |> addActivity
                           Activity.WaitForSupportTeamToResolvePartnerBankSync
            }
      | Ok _ -> {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> finishActivity activity
               |> addActivity Activity.SettlePurchase
        }
   | PurchaseSagaEvent.PurchaseSettled -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.SettlePurchase
            |> addActivity Activity.SendPurchaseNotification
     }
   | PurchaseSagaEvent.PurchaseNotificationSent -> {
      saga with
         Status = PurchaseSagaStatus.Completed
         LifeCycle =
            finishActivity Activity.SendPurchaseNotification saga.LifeCycle
     }
   | PurchaseSagaEvent.PurchaseRejectedByAccount reason -> {
      saga with
         Status = PurchaseFailReason.Account reason |> PurchaseSagaStatus.Failed
         LifeCycle =
            saga.LifeCycle
            |> failActivity Activity.ReserveAccountFunds
            |> addActivity Activity.AcquireCardFailureAcknowledgement
         FailReason = Some(PurchaseFailReason.Account reason)
     }
   | PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard -> {
      saga with
         LifeCycle =
            finishActivity
               Activity.AcquireCardFailureAcknowledgement
               saga.LifeCycle
     }
   | PurchaseSagaEvent.PurchaseFailureAcknowledgedByAccount -> {
      saga with
         LifeCycle =
            finishActivity
               Activity.AcquireAccountFailureAcknowledgement
               saga.LifeCycle
     }
   | PurchaseSagaEvent.SupportTeamResolvedPartnerBankSync -> {
      saga with
         Status = PurchaseSagaStatus.InProgress
         LifeCycle =
            saga.LifeCycle
            |> finishActivity
                  Activity.WaitForSupportTeamToResolvePartnerBankSync
            |> addActivity Activity.SyncToPartnerBank
     }
   | PurchaseSagaEvent.EvaluateRemainingWork -> {
      saga with
         LifeCycle =
            SagaLifeCycle.retryActivitiesAfterInactivity
               timestamp
               saga.LifeCycle
     }
   | PurchaseSagaEvent.ResetInProgressActivityAttempts -> {
      saga with
         LifeCycle = SagaLifeCycle.resetInProgressActivities saga.LifeCycle
     }

let stateTransitionStart
   (start: PurchaseSagaStartEvent)
   (timestamp: DateTime)
   : Result<PurchaseSaga, SagaStateTransitionError>
   =
   Ok(applyStartEvent start timestamp)

let stateTransition
   (saga: PurchaseSaga)
   (evt: PurchaseSagaEvent)
   (timestamp: DateTime)
   : Result<PurchaseSaga, SagaStateTransitionError>
   =
   let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

   let invalidStepProgression =
      match evt with
      | PurchaseSagaEvent.EvaluateRemainingWork
      | PurchaseSagaEvent.ResetInProgressActivityAttempts -> false
      | PurchaseSagaEvent.SupportTeamResolvedPartnerBankSync ->
         activityIsDone Activity.WaitForSupportTeamToResolvePartnerBankSync
      | PurchaseSagaEvent.PurchaseNotificationSent ->
         activityIsDone Activity.SendPurchaseNotification
      | PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard ->
         activityIsDone Activity.AcquireCardFailureAcknowledgement
      | PurchaseSagaEvent.PurchaseFailureAcknowledgedByAccount ->
         activityIsDone Activity.AcquireAccountFailureAcknowledgement
      | PurchaseSagaEvent.PurchaseRejectedByAccount _
      | PurchaseSagaEvent.AccountReservedFunds _ ->
         activityIsDone Activity.ReserveAccountFunds
      | PurchaseSagaEvent.PurchaseRejectedCardNetworkResponse _ ->
         activityIsDone Activity.NotifyCardNetworkOfRejectedPurchase
      | PurchaseSagaEvent.CardNetworkResponse _ ->
         activityIsDone Activity.NotifyCardNetworkOfConfirmedPurchase
      | PurchaseSagaEvent.PartnerBankSyncResponse _ ->
         activityIsDone Activity.SyncToPartnerBank
      | PurchaseSagaEvent.PurchaseSettled ->
         activityIsDone Activity.SettlePurchase

   if saga.Status = PurchaseSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

type PersistenceHandlerDependencies = {
   getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>
   getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   getPartnerBankServiceRef: unit -> IActorRef<PartnerBankServiceMessage>
   cardNetworkConfirmPurchase: PurchaseInfo -> Async<PurchaseSagaEvent>
   cardNetworkRejectPurchase:
      PurchaseInfo -> PurchaseFailReason -> Async<PurchaseSagaEvent>
   sendMessageToSelf: PurchaseInfo -> Async<PurchaseSagaEvent> -> unit
}

// Purchase Saga is started by either a PurchaseRejectedByCard event
// or a DeductedCardFunds coming from the Employee actor.
let onStartEventPersisted
   (dep: PersistenceHandlerDependencies)
   (evt: PurchaseSagaStartEvent)
   =
   match evt with
   | PurchaseSagaStartEvent.PurchaseRejectedByCard(purchaseInfo, reason) ->
      let reason = PurchaseFailReason.Card reason

      let msg = {
         OrgId = purchaseInfo.OrgId
         CorrelationId = purchaseInfo.CorrelationId
         Info =
            EmailInfo.PurchaseFailed {
               Email = purchaseInfo.EmployeeEmail
               Reason = reason
            }
      }

      dep.getEmailRef () <! msg

      dep.sendMessageToSelf
         purchaseInfo
         (dep.cardNetworkRejectPurchase purchaseInfo reason)
   | PurchaseSagaStartEvent.DeductedCardFunds info ->
      let msg =
         DebitCommand.fromPurchase info
         |> AccountCommand.Debit
         |> AccountMessage.StateChange

      // Notify associated company account actor of
      // debit request and wait for approval before
      // sending a response to issuing card network.
      dep.getAccountRef info.ParentAccountId <! msg

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
   (previousState: PurchaseSaga)
   (updatedState: PurchaseSaga)
   (evt: PurchaseSagaEvent)
   =
   let purchaseInfo = updatedState.PurchaseInfo

   let acquireCardFailureAcknowledgement reason =
      let msg =
         FailPurchaseCommand.create
            (purchaseInfo.EmployeeId, purchaseInfo.OrgId)
            { Reason = reason; Info = purchaseInfo }
         |> EmployeeCommand.FailPurchase
         |> EmployeeMessage.StateChange

      dep.getEmployeeRef purchaseInfo.EmployeeId <! msg

   let acquireAccountFailureAcknowledgement reason =
      let msg =
         FailDebitCommand.fromPurchase purchaseInfo reason
         |> AccountCommand.FailDebit
         |> AccountMessage.StateChange

      dep.getAccountRef purchaseInfo.ParentAccountId <! msg

   let sendPurchaseEmail () =
      let emailMsg = {
         OrgId = purchaseInfo.OrgId
         CorrelationId = purchaseInfo.CorrelationId
         Info =
            EmailInfo.Purchase {
               Email = purchaseInfo.EmployeeEmail
               Amount = purchaseInfo.Amount
               CardNumberLast4 = purchaseInfo.CardNumberLast4
               Merchant = purchaseInfo.Merchant
            }
      }

      dep.getEmailRef () <! emailMsg

   let sendPurchaseFailedEmail reason =
      let emailMsg = {
         OrgId = purchaseInfo.OrgId
         CorrelationId = purchaseInfo.CorrelationId
         Info =
            EmailInfo.PurchaseFailed(
               {
                  Email = purchaseInfo.EmployeeEmail
                  Reason = reason
               }
            )
      }

      dep.getEmailRef () <! emailMsg

   let cardNetworkRejectPurchase reason =
      dep.sendMessageToSelf
         purchaseInfo
         (dep.cardNetworkRejectPurchase purchaseInfo reason)

   let cardNetworkConfirmPurchase () =
      dep.sendMessageToSelf
         purchaseInfo
         (dep.cardNetworkConfirmPurchase purchaseInfo)

   let syncToPartnerBank () =
      updatedState.PartnerBankAccountLink
      |> Option.iter (fun link ->
         let msg =
            PartnerBankServiceMessage.Purchase {
               Amount = purchaseInfo.Amount
               Account = link
               Metadata = {
                  OrgId = purchaseInfo.OrgId
                  CorrelationId = purchaseInfo.CorrelationId
               }
            }

         dep.getPartnerBankServiceRef () <! msg)

   let settlePurchase settlementId =
      let msg =
         SettleDebitCommand.fromPurchase purchaseInfo settlementId
         |> AccountCommand.SettleDebit
         |> AccountMessage.StateChange

      dep.getAccountRef purchaseInfo.ParentAccountId <! msg

   match evt with
   | PurchaseSagaEvent.AccountReservedFunds _ -> cardNetworkConfirmPurchase ()
   | PurchaseSagaEvent.PurchaseRejectedCardNetworkResponse _ -> ()
   | PurchaseSagaEvent.CardNetworkResponse res ->
      match res with
      | Ok _ -> syncToPartnerBank ()
      | Error err ->
         if
            previousState.LifeCycle.ActivityHasRemainingAttempts
               Activity.NotifyCardNetworkOfConfirmedPurchase
         then
            cardNetworkConfirmPurchase ()
         else
            let reason = PurchaseFailReason.CardNetwork err

            if updatedState.RequiresCardFailureAcknowledgement then
               acquireCardFailureAcknowledgement reason

            if updatedState.RequiresAccountFailureAcknowledgement then
               acquireAccountFailureAcknowledgement reason
   | PurchaseSagaEvent.PartnerBankSyncResponse res ->
      match res with
      | Ok settlementId -> settlePurchase settlementId
      | Error reason ->
         if
            previousState.LifeCycle.ActivityHasRemainingAttempts
               Activity.SyncToPartnerBank
         then
            syncToPartnerBank ()
         else
            dep.getEmailRef ()
            <! {
                  OrgId = purchaseInfo.OrgId
                  CorrelationId = purchaseInfo.CorrelationId
                  Info = EmailInfo.ApplicationErrorRequiresSupport reason
               }
   | PurchaseSagaEvent.PurchaseRejectedByAccount reason ->
      let reason = PurchaseFailReason.Account reason
      acquireCardFailureAcknowledgement reason
      sendPurchaseFailedEmail reason
      cardNetworkRejectPurchase reason
   | PurchaseSagaEvent.SupportTeamResolvedPartnerBankSync ->
      // Support team resolved dispute with partner bank so
      // reattempt syncing transaction to partner bank.
      syncToPartnerBank ()
   | PurchaseSagaEvent.PurchaseNotificationSent
   | PurchaseSagaEvent.ResetInProgressActivityAttempts
   | PurchaseSagaEvent.PurchaseFailureAcknowledgedByAccount
   | PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard -> ()
   | PurchaseSagaEvent.PurchaseSettled -> sendPurchaseEmail ()
   | PurchaseSagaEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.NotifyCardNetworkOfRejectedPurchase
         | Activity.WaitForSupportTeamToResolvePartnerBankSync
         | Activity.DeductFromEmployeeCard -> ()
         | Activity.ReserveAccountFunds ->
            let msg =
               DebitCommand.fromPurchase purchaseInfo
               |> AccountCommand.Debit
               |> AccountMessage.StateChange

            dep.getAccountRef purchaseInfo.ParentAccountId <! msg
         | Activity.NotifyCardNetworkOfConfirmedPurchase ->
            cardNetworkConfirmPurchase ()
         | Activity.SyncToPartnerBank -> syncToPartnerBank ()
         | Activity.SettlePurchase ->
            updatedState.SyncedToPartnerBank |> Option.iter settlePurchase
         | Activity.SendPurchaseNotification -> sendPurchaseEmail ()
         | Activity.AcquireCardFailureAcknowledgement ->
            updatedState.FailReason
            |> Option.iter acquireCardFailureAcknowledgement
         | Activity.AcquireAccountFailureAcknowledgement ->
            updatedState.FailReason
            |> Option.iter acquireAccountFailureAcknowledgement

// TODO: Notify card network which issued the debit request to our bank.
let cardNetworkConfirmPurchase (info: PurchaseInfo) = async {
   if false then
      do! Async.Sleep(System.TimeSpan.FromMinutes(12))

   // TODO: HTTP to card network

   if false then
      return PurchaseSagaEvent.CardNetworkResponse(Error "")
   else
      return PurchaseSagaEvent.CardNetworkResponse(Ok "some res")
}

let cardNetworkRejectPurchase
   (info: PurchaseInfo)
   (reason: PurchaseFailReason)
   =
   async {
      // HTTP to card network
      do! Async.Sleep(1000)

      let networkResponse = Ok ""

      return
         PurchaseSagaEvent.PurchaseRejectedCardNetworkResponse(
            reason,
            networkResponse
         )
   }
