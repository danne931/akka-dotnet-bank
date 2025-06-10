module PurchaseSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Employee.Domain
open Email
open Lib.Saga

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
   | PurchaseRefundedToCard
   | PurchaseConfirmedByAccount
   | PurchaseRejectedByAccount of PurchaseAccountFailReason
   | PurchaseRefundedToAccount
   | PurchaseRejectedCardNetworkResponse of
      PurchaseFailReason *
      Result<string, string>
   | CardNetworkResponse of Result<string, string>
   | PartnerBankSyncResponse of Result<string, string>
   | PurchaseNotificationSent
   | SupportTeamResolvedPartnerBankSync
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess>]
type Activity =
   | DeductFromEmployeeCard
   | DeductFromAccount
   | NotifyCardNetworkOfRejectedPurchase
   | NotifyCardNetworkOfConfirmedPurchase
   | SyncToPartnerBank
   | SendPurchaseNotification
   | RefundCard
   | RefundAccount
   | WaitForSupportTeamToResolvePartnerBankSync

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForSupportTeamToResolvePartnerBankSync -> 1
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
         | DeductFromAccount
         | RefundCard
         | RefundAccount -> Some(TimeSpan.FromSeconds 4)

type PurchaseSaga = {
   PurchaseInfo: PurchaseInfo
   StartEvent: PurchaseSagaStartEvent
   Events: PurchaseSagaEvent list
   Status: PurchaseSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
   RefundReason: PurchaseRefundReason option
} with

   member x.DeductedFromEmployeeCard =
      x.LifeCycle.Completed
      |> List.exists (fun w -> w.Activity = Activity.DeductFromEmployeeCard)

   member x.DeductedFromAccount =
      x.LifeCycle.Completed
      |> List.exists (fun w -> w.Activity = Activity.DeductFromAccount)

   member x.RequiresCardRefund =
      x.LifeCycle.InProgress
      |> List.exists (fun w -> w.Activity = Activity.RefundCard)

   member x.RequiresAccountRefund =
      x.LifeCycle.InProgress
      |> List.exists (fun w -> w.Activity = Activity.RefundAccount)

   member x.RequiresManualSupportFixForPartnerBankSync =
      x.LifeCycle.InProgress
      |> List.exists (fun w ->
         w.Activity = Activity.WaitForSupportTeamToResolvePartnerBankSync)

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
               ActivityLifeCycle.init timestamp Activity.DeductFromAccount
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
      RefundReason = None
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
      RefundReason = None
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
   | PurchaseSagaEvent.PurchaseConfirmedByAccount -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.DeductFromAccount
            |> addActivity Activity.NotifyCardNetworkOfConfirmedPurchase
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
                  addActivity Activity.RefundCard life
               else
                  life

            let life =
               if saga.DeductedFromAccount then
                  addActivity Activity.RefundAccount life
               else
                  life

            {
               saga with
                  LifeCycle = life
                  Status =
                     PurchaseFailReason.CardNetwork err
                     |> PurchaseSagaStatus.Failed
                  RefundReason = Some(PurchaseRefundReason.CardNetworkError err)
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
            |> failActivity Activity.DeductFromAccount
            |> addActivity Activity.RefundCard
         RefundReason = Some(PurchaseRefundReason.AccountStateInvalid reason)
     }
   | PurchaseSagaEvent.PurchaseRefundedToCard -> {
      saga with
         LifeCycle = finishActivity Activity.RefundCard saga.LifeCycle
     }
   | PurchaseSagaEvent.PurchaseRefundedToAccount -> {
      saga with
         LifeCycle = finishActivity Activity.RefundAccount saga.LifeCycle
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
      | PurchaseSagaEvent.PurchaseRefundedToCard ->
         activityIsDone Activity.RefundCard
      | PurchaseSagaEvent.PurchaseRefundedToAccount ->
         activityIsDone Activity.RefundAccount
      | PurchaseSagaEvent.PurchaseRejectedByAccount _
      | PurchaseSagaEvent.PurchaseConfirmedByAccount ->
         activityIsDone Activity.DeductFromAccount
      | PurchaseSagaEvent.PurchaseRejectedCardNetworkResponse _ ->
         activityIsDone Activity.NotifyCardNetworkOfRejectedPurchase
      | PurchaseSagaEvent.CardNetworkResponse _ ->
         activityIsDone Activity.NotifyCardNetworkOfConfirmedPurchase
      | PurchaseSagaEvent.PartnerBankSyncResponse _ ->
         activityIsDone Activity.SyncToPartnerBank

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
   cardNetworkConfirmPurchase: PurchaseInfo -> Async<PurchaseSagaEvent>
   cardNetworkRejectPurchase:
      PurchaseInfo -> PurchaseFailReason -> Async<PurchaseSagaEvent>
   syncPurchaseToPartnerBank: PurchaseInfo -> Async<PurchaseSagaEvent>
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

   let refundCard reason =
      let msg =
         RefundPurchaseCommand.create
            (purchaseInfo.EmployeeId, purchaseInfo.OrgId)
            { Reason = reason; Info = purchaseInfo }
         |> EmployeeCommand.RefundPurchase
         |> EmployeeMessage.StateChange

      dep.getEmployeeRef purchaseInfo.EmployeeId <! msg

   let refundAccount reason =
      let msg =
         RefundDebitCommand.fromPurchase purchaseInfo reason
         |> AccountCommand.RefundDebit
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
      dep.sendMessageToSelf
         purchaseInfo
         (dep.syncPurchaseToPartnerBank purchaseInfo)

   match evt with
   | PurchaseSagaEvent.PurchaseConfirmedByAccount ->
      cardNetworkConfirmPurchase ()
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
            let reason = PurchaseRefundReason.CardNetworkError err

            if updatedState.RequiresCardRefund then
               refundCard reason

            if updatedState.RequiresAccountRefund then
               refundAccount reason
   | PurchaseSagaEvent.PartnerBankSyncResponse res ->
      match res with
      | Ok _ -> sendPurchaseEmail ()
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
      refundCard (PurchaseRefundReason.AccountStateInvalid reason)
      let reason = PurchaseFailReason.Account reason
      sendPurchaseFailedEmail reason
      cardNetworkRejectPurchase reason
   | PurchaseSagaEvent.SupportTeamResolvedPartnerBankSync ->
      // Support team resolved dispute with partner bank so
      // reattempt syncing transaction to partner bank.
      syncToPartnerBank ()
   | PurchaseSagaEvent.PurchaseNotificationSent
   | PurchaseSagaEvent.ResetInProgressActivityAttempts
   | PurchaseSagaEvent.PurchaseRefundedToAccount
   | PurchaseSagaEvent.PurchaseRefundedToCard -> ()
   | PurchaseSagaEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.NotifyCardNetworkOfRejectedPurchase
         | Activity.WaitForSupportTeamToResolvePartnerBankSync
         | Activity.DeductFromEmployeeCard -> ()
         | Activity.DeductFromAccount ->
            let msg =
               DebitCommand.fromPurchase purchaseInfo
               |> AccountCommand.Debit
               |> AccountMessage.StateChange

            dep.getAccountRef purchaseInfo.ParentAccountId <! msg
         | Activity.NotifyCardNetworkOfConfirmedPurchase ->
            cardNetworkConfirmPurchase ()
         | Activity.SyncToPartnerBank -> syncToPartnerBank ()
         | Activity.SendPurchaseNotification -> sendPurchaseEmail ()
         | Activity.RefundCard ->
            updatedState.RefundReason |> Option.iter refundCard
         | Activity.RefundAccount ->
            updatedState.RefundReason |> Option.iter refundAccount

let mutable failCnt = 6

let syncPurchaseToPartnerBank (info: PurchaseInfo) = async {
   do! Async.Sleep(3000)

   // TODO: HTTP to partner bank

   if failCnt < 3 then
      failCnt <- failCnt + 1
      return PurchaseSagaEvent.PartnerBankSyncResponse(Error "")
   else
      return PurchaseSagaEvent.PartnerBankSyncResponse(Ok "some response")
}

let mutable cardFailCnt = 0
let mutable cardSleepCnt = 0

// TODO: Notify card network which issued the debit request to our bank.
let cardNetworkConfirmPurchase (info: PurchaseInfo) = async {
   if cardSleepCnt < 2 then
      cardSleepCnt <- cardSleepCnt + 1
      do! Async.Sleep(System.TimeSpan.FromMinutes(1.2))

   // TODO: HTTP to card network

   if false then //cardFailCnt < 6 then
      cardFailCnt <- cardFailCnt + 1
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
