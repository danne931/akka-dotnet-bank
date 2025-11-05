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
open Lib.SharedTypes

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
   // NOTE:
   // It is sometimes possible to receive a purchase progress update from Lithic without
   // first receiving the purchase intent at the authorization stream access
   // webhook.  This poses some risk in that they may allow a purchase to settle
   // for an amount above the cardholder's balance, without affording the user
   // the opportunity to decline.  Such situations may be subject to chargeback.
   // See Purchase/Domain/PurchaseLifecycleEvent.fs cases 16-20.
   | PurchaseSagaStartEvent.PurchaseProgress(purchaseInfo, progress) ->
      let origin = progress.OriginatingEvent

      let saga = {
         PurchaseInfo = purchaseInfo
         InitialPurchaseIntentAmount = origin.Money.Amount
         CardIssuerPurchaseEvents = []
         CardIssuerProgressAmounts = progress.Amounts
         StartEvent = start
         StartedAt = timestamp
         Events = []
         Status = PurchaseSagaStatus.InProgress
         LifeCycle = SagaLifeCycle.empty
         FailReason = None
      }

      if
         saga.OriginatedFromForcePost.IsSome
         || saga.OriginatedFromPendingAuthAdvice
      then
         {
            saga with
               LifeCycle.InProgress = [
                  ActivityLifeCycle.init
                     timestamp
                     Activity.ReserveAccountFundsBypassingAuth

                  ActivityLifeCycle.init
                     timestamp
                     Activity.ReserveEmployeeCardFundsBypassingAuth

                  ActivityLifeCycle.init
                     timestamp
                     (Activity.BufferCardIssuerPurchaseProgress progress)
               ]
         }
      else
         // Ignore purchase progress start event
         {
            saga with
               Status = PurchaseSagaStatus.Completed
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

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | PurchaseSagaEvent.AccountReservedFunds ->
      let activity =
         match saga.PurchaseInfo.AuthorizationType with
         | PurchaseAuthType.BypassAuth ->
            Activity.ReserveAccountFundsBypassingAuth
         | _ -> Activity.ReserveAccountFunds

      {
         saga with
            LifeCycle = saga.LifeCycle |> finishActivity activity
      }
   | PurchaseSagaEvent.CardReservedFunds ->
      let activity =
         match saga.PurchaseInfo.AuthorizationType with
         | PurchaseAuthType.BypassAuth ->
            Activity.ReserveEmployeeCardFundsBypassingAuth
         | _ -> Activity.ReserveEmployeeCardFunds

      {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> finishActivity activity
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
               saga.CardIssuerPurchaseEvents
               |> List.append (NonEmptyList.toList progress.Events)
               |> List.sortBy _.CreatedAt
            LifeCycle =
               let life =
                  saga.LifeCycle
                  |> finishActivity Activity.WaitForCardNetworkResolution

               match saga.BufferedCardIssuerPurchaseProgress with
               | Some buffer when buffer = progress ->
                  life
                  |> finishActivity (
                     Activity.BufferCardIssuerPurchaseProgress buffer
                  )
               | _ -> life
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
               |> SagaLifeCycle.abortActivities timestamp
               |> addActivity Activity.AcquireAccountFailureAcknowledgement
               |> addActivity Activity.AcquireCardFailureAcknowledgement
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
         // AuthReversal can occur before the first clearing.
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
            |> NonEmptyList.toList
            |> List.sumBy (fun o ->
               match o.Type with
               | PurchaseEventType.Clearing
               | PurchaseEventType.Return
               // Financial Auths are SMS requests, wherein auth
               // & clearing are combined into a single message.
               // See Purchase/Domain/PurchaseLifecycleEvent.fs case 13.
               | PurchaseEventType.FinancialAuth -> Money.amountSigned o.Money
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
               ClearedAmount = Money.create amount
               // Deterministically set the ClearedId by consuming the Lithic
               // EventId of the oldest event in a CardIssuerPurchaseProgress.
               PurchaseClearedId =
                  PurchaseClearedId progress.OriginatingEvent.EventId
            }

            {
               saga with
                  LifeCycle =
                     saga.LifeCycle
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
         && activityIsDone Activity.ReserveAccountFundsBypassingAuth
      | PurchaseSagaEvent.CardReservedFunds ->
         activityIsDone Activity.ReserveEmployeeCardFunds
         && activityIsDone Activity.ReserveEmployeeCardFundsBypassingAuth
      | PurchaseSagaEvent.PurchaseSettledWithAccount clearing ->
         activityIsDone (Activity.SettlePurchaseWithAccount clearing)
      | PurchaseSagaEvent.PurchaseSettledWithCard clearing ->
         activityIsDone (Activity.SettlePurchaseWithCard clearing)

   if invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

let private sendPurchaseEmail
   (registry: #IEmailActor)
   (purchase: PurchaseInfo)
   =
   let emailMsg = {
      OrgId = purchase.OrgId
      CorrelationId = purchase.CorrelationId
      Info =
         EmailInfo.Purchase {
            Email = purchase.EmployeeEmail
            Amount = purchase.Amount
            CardNumberLast4 = purchase.CardNumberLast4
            Merchant = string purchase.Merchant
         }
   }

   registry.EmailActor() <! emailMsg

let private reserveCardFunds (registry: #IEmployeeActor) purchase =
   let msg =
      PurchaseIntentCommand.create purchase
      |> EmployeeCommand.PurchaseIntent
      |> EmployeeMessage.StateChange

   registry.EmployeeActor purchase.EmployeeId <! msg


let private reserveAccountFunds (registry: #IAccountActor) purchase =
   let msg =
      purchase
      |> DebitCommand.fromPurchase
      |> AccountCommand.Debit
      |> AccountMessage.StateChange

   registry.AccountActor purchase.ParentAccountId <! msg

// Purchase Saga is started by PurchaseIntent event coming from the Employee actor.
let onStartEventPersisted
   registry
   (saga: PurchaseSaga)
   (evt: PurchaseSagaStartEvent)
   =
   match evt with
   | PurchaseSagaStartEvent.PurchaseIntent _ -> ()
   | PurchaseSagaStartEvent.PurchaseProgress(purchase, _) ->
      if saga.Status.IsInProgress then
         reserveCardFunds registry purchase
         reserveAccountFunds registry purchase

let onEventPersisted
   (broadcaster: SignalRBroadcast.SignalRBroadcast)
   (sendEventToSelf: PurchaseSagaEvent -> unit)
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

   let sendProgressFromBufferMaybe () =
      match
         updatedState.ReservedAccountFunds,
         updatedState.ReservedEmployeeCardFunds,
         updatedState.BufferedCardIssuerPurchaseProgress
      with
      | true, true, Some progress ->
         PurchaseSagaEvent.CardIssuerUpdatedPurchaseProgress progress
         |> sendEventToSelf
      | _ -> ()

   match evt with
   | PurchaseSagaEvent.CardReservedFunds
   | PurchaseSagaEvent.AccountReservedFunds -> sendProgressFromBufferMaybe ()
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
            sendPurchaseEmail registry purchaseInfo

      match progress.Status with
      | PurchaseStatus.Pending -> sendPurchaseEmailMaybe ()
      | PurchaseStatus.Settled ->
         sendPurchaseEmailMaybe ()

         updatedState.OutgoingSettlementWithAccount
         |> Option.iter settlePurchaseWithAccount
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
         | Activity.ReserveEmployeeCardFundsBypassingAuth ->
            reserveCardFunds registry purchaseInfo
         | Activity.ReserveAccountFundsBypassingAuth ->
            reserveAccountFunds registry purchaseInfo
         | Activity.SettlePurchaseWithCard clearing ->
            settlePurchaseWithCard clearing
         | Activity.SettlePurchaseWithAccount clearing ->
            settlePurchaseWithAccount clearing
         | Activity.SendPurchaseNotification ->
            sendPurchaseEmail registry purchaseInfo
         | Activity.AcquireCardFailureAcknowledgement ->
            updatedState.FailReason
            |> Option.iter acquireCardFailureAcknowledgement
         | Activity.AcquireAccountFailureAcknowledgement ->
            updatedState.FailReason
            |> Option.iter acquireAccountFailureAcknowledgement
         | Activity.BufferCardIssuerPurchaseProgress _ ->
            sendProgressFromBufferMaybe ()
