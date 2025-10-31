module PurchaseSaga

open System
open Akkling

open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Purchase.Domain
open EmailMessage
open Lib.Saga
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
      InitialPurchaseIntentAmount = info.Amount
      CardIssuerPurchaseEvents = []
      CardIssuerProgressAmounts = PurchaseAmounts.Empty
      StartEvent = start
      StartedAt = timestamp
      Events = []
      Status = PurchaseSagaStatus.InProgress
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init timestamp Activity.ReserveAccountFunds
               ActivityLifeCycle.init
                  timestamp
                  Activity.ReserveEmployeeCardFunds
            ]
      }
      FailReason = None
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
   let abortActivity = SagaLifeCycle.abortActivity timestamp

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | PurchaseSagaEvent.AccountReservedFunds -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.ReserveAccountFunds
     }
   | PurchaseSagaEvent.CardReservedFunds -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.ReserveEmployeeCardFunds
            |> addActivity Activity.WaitForCardNetworkResolution
     }
   | PurchaseSagaEvent.CardIssuerUpdatedPurchaseProgress progress ->
      let amount =
         match progress.Status with
         | PurchaseStatus.Pending -> progress.Amounts.Hold.Amount
         | PurchaseStatus.Settled -> progress.Amounts.Settlement.Amount
         | PurchaseStatus.Declined
         | PurchaseStatus.Voided
         | PurchaseStatus.Expired -> saga.PurchaseInfo.Amount

      let priorState = saga

      let saga = {
         saga with
            PurchaseInfo = {
               saga.PurchaseInfo with
                  Amount = amount
            }
            CardIssuerProgressAmounts = progress.Amounts
            CardIssuerPurchaseEvents =
               saga.CardIssuerPurchaseEvents @ progress.Events
               |> List.sortBy _.CreatedAt
      }

      let failSaga reason = {
         saga with
            Status =
               PurchaseFailReason.CardNetwork reason
               |> PurchaseSagaStatus.Failed
            PurchaseInfo = {
               saga.PurchaseInfo with
                  Amount = saga.InitialPurchaseIntentAmount
            }
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.WaitForCardNetworkResolution
               |> addActivity Activity.AcquireAccountFailureAcknowledgement
               |> addActivity Activity.AcquireCardFailureAcknowledgement
               |> abortActivity Activity.SendPurchaseNotification
      }

      match progress.Status with
      | PurchaseStatus.Pending ->
         if saga.PurchaseNotificationSent then
            saga
         else
            // Send purchase notification upon receiving initial Auth confirmation event
            {
               saga with
                  LifeCycle =
                     saga.LifeCycle
                     |> addActivity Activity.SendPurchaseNotification
            }
      | PurchaseStatus.Settled ->
         // NOTE:
         // PurchaseEventType.AuthExpiry & AuthReversal occurring after
         // a clearing will not alter the settled amount.
         //
         // NOTE:
         // It is not certain from the docs whether a PARTIAL AuthExpiry or
         // AuthReversal occurring before the first clearing can occur.
         // If it does, the behavior of this program is to record the fact in
         // sagaState.CardIssuerPurchaseEvents without interacting with
         // account & employee actors.  As such, no funds reserved from the
         // initial purchase intent will be released from the card or account entities
         // until the first clearing or COMPLETE expiry/reversal of funds.
         //
         // TODO:
         // If this situation can occur and we observe a large enough gap in time
         // between the AuthExpiry/AuthRelease and the Clearing then we may want
         // to interact with the account/employee actors to release those funds
         // instead of waiting for the first clearing or complete
         // expiry/reversal.
         let amount =
            progress.Events
            |> List.sumBy (fun o ->
               match o.Type with
               | PurchaseEventType.Clearing
               | PurchaseEventType.Return ->
                  match o.Flow with
                  | MoneyFlow.Out -> -o.Amount
                  | MoneyFlow.In -> o.Amount
               | _ -> 0m)

         if amount = 0m then
            saga
         elif
            priorState.Status = PurchaseSagaStatus.Completed
            && priorState.PurchaseInfo.Amount = progress.Amounts.Settlement.Amount
         then
            saga
         else
            let clearedAmount = {
               ClearedAmount = {
                  Flow = if amount < 0m then MoneyFlow.Out else MoneyFlow.In
                  Amount = abs amount
               }
               // Deterministically set the ClearedId by consuming the Lithic
               // EventId of the oldest event in a CardIssuerPurchaseProgress.
               PurchaseClearedId =
                  progress.Events
                  |> List.sortBy _.CreatedAt
                  |> List.head
                  |> _.EventId
                  |> PurchaseClearedId
            }

            {
               saga with
                  LifeCycle =
                     saga.LifeCycle
                     |> finishActivity Activity.WaitForCardNetworkResolution
                     |> addActivity (
                        Activity.SettlePurchaseWithAccount clearedAmount
                     )
                     |> if saga.PurchaseNotificationSent then
                           id
                        else
                           addActivity Activity.SendPurchaseNotification
                  Status =
                     // If a clearing event was previously received from Lithic
                     // and the workflow completed we would have marked the saga
                     // with Completed status.  It is still possible to receive
                     // follow up clearing events indicating the need to
                     // apply additional settled funds to the account.
                     // See Purchase/Domain/PurchaseLifecycleEvent.fs
                     // Multiple Completion section, specifically cases
                     // 9-12.
                     //
                     // NOTE:
                     // A purchase may continue receiving updates from Lithic
                     // for 7 days, but sometimes 30 days for some txns such as
                     // auto rentals and hotel reservations.
                     //
                     // TODO:
                     // Determine whether resetting the saga status to
                     // InProgress, as we do here, is ideal or if it may be
                     // preferable to keep the saga status as InProgress after
                     // the initial clearing and only set it to Completed after
                     // the 7 or 30 day window (no news is good news).
                     if saga.Status = PurchaseSagaStatus.Completed then
                        PurchaseSagaStatus.InProgress
                     else
                        saga.Status
            }
      | PurchaseStatus.Expired -> failSaga CardNetworkFailReason.Expired
      | PurchaseStatus.Voided -> failSaga CardNetworkFailReason.Voided
      | PurchaseStatus.Declined -> failSaga CardNetworkFailReason.Declined
   | PurchaseSagaEvent.PurchaseSettledWithAccount clearing -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity (Activity.SettlePurchaseWithAccount clearing)
            |> addActivity (Activity.SettlePurchaseWithCard clearing)
     }
   | PurchaseSagaEvent.PurchaseSettledWithCard clearing -> {
      saga with
         Status =
            if saga.PurchaseNotificationSent then
               PurchaseSagaStatus.Completed
            else
               saga.Status
         LifeCycle =
            saga.LifeCycle
            |> finishActivity (Activity.SettlePurchaseWithCard clearing)
     }
   | PurchaseSagaEvent.PurchaseNotificationSent -> {
      saga with
         Status =
            if saga.SettledWithCard && saga.SettledWithAccount then
               PurchaseSagaStatus.Completed
            else
               saga.Status
         LifeCycle =
            finishActivity Activity.SendPurchaseNotification saga.LifeCycle
     }
   | PurchaseSagaEvent.PurchaseRejected reason ->
      let life = saga.LifeCycle

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

      let life =
         life
         |> failActivity Activity.ReserveAccountFunds
         |> failActivity Activity.ReserveEmployeeCardFunds

      {
         saga with
            Status = PurchaseSagaStatus.Failed reason
            FailReason = Some reason
            LifeCycle = life
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
      | PurchaseSagaEvent.PurchaseRejected _
      | PurchaseSagaEvent.EvaluateRemainingWork
      | PurchaseSagaEvent.ResetInProgressActivityAttempts -> false
      // Continue to consume purchase progress from Lithic
      // even after the initial clearing of funds.
      | PurchaseSagaEvent.CardIssuerUpdatedPurchaseProgress _ -> false
      | PurchaseSagaEvent.PurchaseNotificationSent ->
         activityIsDone Activity.SendPurchaseNotification
      | PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard ->
         activityIsDone Activity.AcquireCardFailureAcknowledgement
      | PurchaseSagaEvent.PurchaseFailureAcknowledgedByAccount ->
         activityIsDone Activity.AcquireAccountFailureAcknowledgement
      | PurchaseSagaEvent.AccountReservedFunds ->
         activityIsDone Activity.ReserveAccountFunds
      | PurchaseSagaEvent.CardReservedFunds ->
         activityIsDone Activity.ReserveEmployeeCardFunds
      | PurchaseSagaEvent.PurchaseSettledWithAccount clearing ->
         activityIsDone (Activity.SettlePurchaseWithAccount clearing)
      | PurchaseSagaEvent.PurchaseSettledWithCard clearing ->
         activityIsDone (Activity.SettlePurchaseWithCard clearing)

   if invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

// Purchase Saga is started by PurchaseIntent event coming from the Employee actor.
let onStartEventPersisted (evt: PurchaseSagaStartEvent) =
   match evt with
   | PurchaseSagaStartEvent.PurchaseIntent _ -> ()

let onEventPersisted
   (broadcaster: SignalRBroadcast.SignalRBroadcast)
   (registry:
      #IEmployeeActor & #IAccountActor & #IEmailActor & #IPartnerBankServiceActor)
   (previousState: PurchaseSaga)
   (updatedState: PurchaseSaga)
   (evt: PurchaseSagaEvent)
   =
   broadcaster.sagaUpdated (AppSaga.Saga.Purchase updatedState).AsDTO

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

   let settlePurchaseWithAccount clearing =
      let msg =
         SettleDebitCommand.create purchaseInfo clearing
         |> AccountCommand.SettleDebit
         |> AccountMessage.StateChange

      registry.AccountActor purchaseInfo.ParentAccountId <! msg

   let settlePurchaseWithCard clearing =
      let msg =
         SettlePurchaseWithCardCommand.create {
            Info = purchaseInfo
            Clearing = clearing
         }
         |> EmployeeCommand.SettlePurchase
         |> EmployeeMessage.StateChange

      registry.EmployeeActor purchaseInfo.EmployeeId <! msg

   match evt with
   | PurchaseSagaEvent.CardReservedFunds
   | PurchaseSagaEvent.AccountReservedFunds -> ()
   | PurchaseSagaEvent.PurchaseRejected reason ->
      if updatedState.ReservedEmployeeCardFunds then
         acquireCardFailureAcknowledgement reason

      if updatedState.ReservedAccountFunds then
         acquireAccountFailureAcknowledgement reason

      sendPurchaseFailedEmail reason
   | PurchaseSagaEvent.CardIssuerUpdatedPurchaseProgress progress ->
      let fail reason =
         let reason = PurchaseFailReason.CardNetwork reason
         acquireCardFailureAcknowledgement reason
         acquireAccountFailureAcknowledgement reason
         sendPurchaseFailedEmail reason

      let sendPurchaseEmailMaybe () =
         if not updatedState.PurchaseNotificationSent then
            sendPurchaseEmail ()

      match progress.Status with
      | PurchaseStatus.Pending -> sendPurchaseEmailMaybe ()
      | PurchaseStatus.Settled ->
         sendPurchaseEmailMaybe ()

         match updatedState.OutgoingSettlementWithAccount with
         | Some clearing -> settlePurchaseWithAccount clearing
         | None -> ()
      | PurchaseStatus.Declined -> fail CardNetworkFailReason.Declined
      | PurchaseStatus.Expired -> fail CardNetworkFailReason.Expired
      | PurchaseStatus.Voided -> fail CardNetworkFailReason.Voided
   | PurchaseSagaEvent.PurchaseSettledWithAccount clearing ->
      settlePurchaseWithCard clearing
   | PurchaseSagaEvent.PurchaseSettledWithCard _
   | PurchaseSagaEvent.PurchaseNotificationSent
   | PurchaseSagaEvent.ResetInProgressActivityAttempts
   | PurchaseSagaEvent.PurchaseFailureAcknowledgedByAccount
   | PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard -> ()
   | PurchaseSagaEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.WaitForCardNetworkResolution
         | Activity.ReserveEmployeeCardFunds
         | Activity.ReserveAccountFunds -> ()
         | Activity.SettlePurchaseWithCard clearing ->
            settlePurchaseWithCard clearing
         | Activity.SettlePurchaseWithAccount clearing ->
            settlePurchaseWithAccount clearing
         | Activity.SendPurchaseNotification -> sendPurchaseEmail ()
         | Activity.AcquireCardFailureAcknowledgement ->
            updatedState.FailReason
            |> Option.iter acquireCardFailureAcknowledgement
         | Activity.AcquireAccountFailureAcknowledgement ->
            updatedState.FailReason
            |> Option.iter acquireAccountFailureAcknowledgement
