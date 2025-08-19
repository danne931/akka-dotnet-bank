module PurchaseSaga

open System
open Akkling

open Bank.Account.Domain
open Bank.Employee.Domain
open EmailMessage
open Lib.Saga
open PartnerBank.Service.Domain
open PurchaseSaga
open BankActorRegistry

let applyStartEvent
   (start: PurchaseSagaStartEvent)
   (timestamp: DateTime)
   : PurchaseSaga
   =
   match start with
   | PurchaseSagaStartEvent.PurchaseIntent info -> {
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
                  Activity = Activity.ReserveEmployeeCardFunds
                  MaxAttempts =
                     (Activity.ReserveEmployeeCardFunds :> IActivity)
                        .MaxAttempts
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
               if saga.ReservedEmployeeCardFunds then
                  addActivity Activity.AcquireCardFailureAcknowledgement life
               else
                  life

            let life =
               if saga.ReservedAccountFunds then
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
               |> addActivity Activity.SettlePurchaseWithAccount
        }
   | PurchaseSagaEvent.PurchaseSettledWithAccount -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.SettlePurchaseWithAccount
            |> addActivity Activity.SettlePurchaseWithCard
     }
   | PurchaseSagaEvent.PurchaseSettledWithCard -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.SettlePurchaseWithCard
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
      | PurchaseSagaEvent.PurchaseSettledWithAccount ->
         activityIsDone Activity.SettlePurchaseWithAccount
      | PurchaseSagaEvent.PurchaseSettledWithCard ->
         activityIsDone Activity.SettlePurchaseWithCard

   if saga.Status = PurchaseSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

type PersistenceStartHandlerDependencies = {
   cardNetworkRejectPurchase:
      PurchaseInfo -> PurchaseFailReason -> Async<PurchaseSagaEvent>
   sendMessageToSelf: PurchaseInfo -> Async<PurchaseSagaEvent> -> unit
}
// Purchase Saga is started by either a PurchaseRejectedByCard event,
// or a PurchasePending event coming from the Employee actor.
let onStartEventPersisted
   (registry: #IAccountActor & #IEmailActor)
   (operationEnv: PersistenceStartHandlerDependencies)
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
               Reason = reason.Display
            }
      }

      registry.EmailActor() <! msg

      operationEnv.sendMessageToSelf
         purchaseInfo
         (operationEnv.cardNetworkRejectPurchase purchaseInfo reason)
   | PurchaseSagaStartEvent.PurchaseIntent info ->
      let msg =
         DebitCommand.fromPurchase info
         |> AccountCommand.Debit
         |> AccountMessage.StateChange

      // Notify associated company account actor of
      // debit request and wait for approval before
      // sending a response to issuing card network.
      registry.AccountActor info.ParentAccountId <! msg

type OperationEnv = {
   cardNetworkConfirmPurchase: PurchaseInfo -> Async<PurchaseSagaEvent>
   cardNetworkRejectPurchase:
      PurchaseInfo -> PurchaseFailReason -> Async<PurchaseSagaEvent>
   sendMessageToSelf: PurchaseInfo -> Async<PurchaseSagaEvent> -> unit
}

let onEventPersisted
   (registry:
      #IEmployeeActor & #IAccountActor & #IEmailActor & #IPartnerBankServiceActor)
   (operationEnv: OperationEnv)
   (previousState: PurchaseSaga)
   (updatedState: PurchaseSaga)
   (evt: PurchaseSagaEvent)
   =
   let purchaseInfo = updatedState.PurchaseInfo

   let acquireCardFailureAcknowledgement reason =
      let msg =
         FailPurchaseCommand.create { Info = purchaseInfo; Reason = reason }
         |> EmployeeCommand.FailPurchase
         |> EmployeeMessage.StateChange

      registry.EmployeeActor purchaseInfo.EmployeeId <! msg

   let acquireAccountFailureAcknowledgement reason =
      let msg =
         FailDebitCommand.fromPurchase purchaseInfo reason
         |> AccountCommand.FailDebit
         |> AccountMessage.StateChange

      registry.AccountActor purchaseInfo.ParentAccountId <! msg

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

      registry.EmailActor() <! emailMsg

   let sendPurchaseFailedEmail (reason: PurchaseFailReason) =
      let emailMsg = {
         OrgId = purchaseInfo.OrgId
         CorrelationId = purchaseInfo.CorrelationId
         Info =
            EmailInfo.PurchaseFailed {
               Email = purchaseInfo.EmployeeEmail
               Reason = reason.Display
            }
      }

      registry.EmailActor() <! emailMsg

   let cardNetworkRejectPurchase reason =
      operationEnv.sendMessageToSelf
         purchaseInfo
         (operationEnv.cardNetworkRejectPurchase purchaseInfo reason)

   let cardNetworkConfirmPurchase () =
      operationEnv.sendMessageToSelf
         purchaseInfo
         (operationEnv.cardNetworkConfirmPurchase purchaseInfo)

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

         registry.PartnerBankServiceActor() <! msg)

   let settlePurchaseWithAccount settlementId =
      let msg =
         SettleDebitCommand.fromPurchase purchaseInfo settlementId
         |> AccountCommand.SettleDebit
         |> AccountMessage.StateChange

      registry.AccountActor purchaseInfo.ParentAccountId <! msg

   let settlePurchaseWithCard settlementId =
      let msg =
         SettlePurchaseWithCardCommand.create {
            Info = purchaseInfo
            SettlementId = settlementId
         }
         |> EmployeeCommand.SettlePurchase
         |> EmployeeMessage.StateChange

      registry.EmployeeActor purchaseInfo.EmployeeId <! msg

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
      | Ok settlementId -> settlePurchaseWithAccount settlementId
      | Error reason ->
         if
            previousState.LifeCycle.ActivityHasRemainingAttempts
               Activity.SyncToPartnerBank
         then
            syncToPartnerBank ()
         else
            registry.EmailActor()
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
   | PurchaseSagaEvent.PurchaseSettledWithAccount ->
      updatedState.SyncedToPartnerBank |> Option.iter settlePurchaseWithCard
   | PurchaseSagaEvent.PurchaseSettledWithCard -> sendPurchaseEmail ()
   | PurchaseSagaEvent.PurchaseNotificationSent
   | PurchaseSagaEvent.ResetInProgressActivityAttempts
   | PurchaseSagaEvent.PurchaseFailureAcknowledgedByAccount
   | PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard -> ()
   | PurchaseSagaEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.ReserveEmployeeCardFunds
         | Activity.NotifyCardNetworkOfRejectedPurchase
         | Activity.WaitForSupportTeamToResolvePartnerBankSync -> ()
         | Activity.ReserveAccountFunds ->
            let msg =
               DebitCommand.fromPurchase purchaseInfo
               |> AccountCommand.Debit
               |> AccountMessage.StateChange

            registry.AccountActor purchaseInfo.ParentAccountId <! msg
         | Activity.SettlePurchaseWithCard ->
            updatedState.SyncedToPartnerBank
            |> Option.iter settlePurchaseWithCard
         | Activity.NotifyCardNetworkOfConfirmedPurchase ->
            cardNetworkConfirmPurchase ()
         | Activity.SyncToPartnerBank -> syncToPartnerBank ()
         | Activity.SettlePurchaseWithAccount ->
            updatedState.SyncedToPartnerBank
            |> Option.iter settlePurchaseWithAccount
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
      do! Async.Sleep(TimeSpan.FromMinutes(12.))

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
