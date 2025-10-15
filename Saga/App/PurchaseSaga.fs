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
      CardIssuerPurchaseEvents = []
      StartEvent = start
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
   let abortActivity = SagaLifeCycle.abortActivity timestamp

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | PurchaseSagaEvent.AccountReservedFunds link -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.ReserveAccountFunds
         PartnerBankAccountLink = Some link
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

      let saga = {
         saga with
            PurchaseInfo = {
               saga.PurchaseInfo with
                  Amount = amount
            }
            CardIssuerPurchaseEvents =
               saga.CardIssuerPurchaseEvents @ progress.Events
               |> List.sortBy _.CreatedAt
      }

      let failSaga reason = {
         saga with
            Status =
               PurchaseFailReason.CardNetwork reason
               |> PurchaseSagaStatus.Failed
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
      | PurchaseStatus.Settled -> {
         saga with
            LifeCycle =
               saga.LifeCycle
               |> finishActivity Activity.WaitForCardNetworkResolution
               |> addActivity Activity.SettlePurchaseWithAccount
               |> if saga.PurchaseNotificationSent then
                     id
                  else
                     addActivity Activity.SendPurchaseNotification
        }
      | PurchaseStatus.Expired -> failSaga CardNetworkFailReason.Expired
      | PurchaseStatus.Voided -> failSaga CardNetworkFailReason.Voided
      | PurchaseStatus.Declined -> failSaga CardNetworkFailReason.Declined
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
            |> addActivity Activity.SyncToPartnerBank
     }
   | PurchaseSagaEvent.PurchaseNotificationSent -> {
      saga with
         Status =
            if saga.SyncedToPartnerBank.IsSome then
               PurchaseSagaStatus.Completed
            else
               saga.Status
         LifeCycle =
            finishActivity Activity.SendPurchaseNotification saga.LifeCycle
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
            Status =
               if saga.PurchaseNotificationSent then
                  PurchaseSagaStatus.Completed
               else
                  saga.Status
            LifeCycle = saga.LifeCycle |> finishActivity activity
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
      | PurchaseSagaEvent.PurchaseRejected _
      | PurchaseSagaEvent.EvaluateRemainingWork
      | PurchaseSagaEvent.ResetInProgressActivityAttempts -> false
      | PurchaseSagaEvent.CardIssuerUpdatedPurchaseProgress _ ->
         activityIsDone Activity.WaitForCardNetworkResolution
      | PurchaseSagaEvent.SupportTeamResolvedPartnerBankSync ->
         activityIsDone Activity.WaitForSupportTeamToResolvePartnerBankSync
      | PurchaseSagaEvent.PurchaseNotificationSent ->
         activityIsDone Activity.SendPurchaseNotification
      | PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard ->
         activityIsDone Activity.AcquireCardFailureAcknowledgement
      | PurchaseSagaEvent.PurchaseFailureAcknowledgedByAccount ->
         activityIsDone Activity.AcquireAccountFailureAcknowledgement
      | PurchaseSagaEvent.AccountReservedFunds _ ->
         activityIsDone Activity.ReserveAccountFunds
      | PurchaseSagaEvent.CardReservedFunds ->
         activityIsDone Activity.ReserveEmployeeCardFunds
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

// Purchase Saga is started by PurchaseIntent event coming from the Employee actor.
let onStartEventPersisted (evt: PurchaseSagaStartEvent) =
   match evt with
   | PurchaseSagaStartEvent.PurchaseIntent _ -> ()

let onEventPersisted
   (registry:
      #IEmployeeActor & #IAccountActor & #IEmailActor & #IPartnerBankServiceActor)
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

   let syncToPartnerBank () =
      updatedState.PartnerBankAccountLink
      |> Option.iter (fun link ->
         let msg =
            PartnerBankServiceMessage.Purchase {
               Amount = purchaseInfo.Amount
               Account = link
               SagaMetadata = {
                  OrgId = purchaseInfo.OrgId
                  CorrelationId = purchaseInfo.CorrelationId
               }
            }

         registry.PartnerBankServiceActor() <! msg)

   let settlePurchaseWithAccount () =
      let msg =
         SettleDebitCommand.fromPurchase purchaseInfo
         |> AccountCommand.SettleDebit
         |> AccountMessage.StateChange

      registry.AccountActor purchaseInfo.ParentAccountId <! msg

   let settlePurchaseWithCard () =
      let msg =
         SettlePurchaseWithCardCommand.create { Info = purchaseInfo }
         |> EmployeeCommand.SettlePurchase
         |> EmployeeMessage.StateChange

      registry.EmployeeActor purchaseInfo.EmployeeId <! msg

   match evt with
   | PurchaseSagaEvent.CardReservedFunds -> ()
   | PurchaseSagaEvent.AccountReservedFunds _ -> ()
   | PurchaseSagaEvent.PartnerBankSyncResponse res ->
      match res with
      | Ok _ -> ()
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
   | PurchaseSagaEvent.PurchaseRejected reason ->
      if updatedState.ReservedEmployeeCardFunds then
         acquireCardFailureAcknowledgement reason

      if updatedState.ReservedAccountFunds then
         acquireAccountFailureAcknowledgement reason

      sendPurchaseFailedEmail reason
   | PurchaseSagaEvent.CardIssuerUpdatedPurchaseProgress progress ->
      match progress.Status with
      | PurchaseStatus.Settled
      | PurchaseStatus.Pending ->
         if not updatedState.PurchaseNotificationSent then
            sendPurchaseEmail ()
      | _ -> ()

      match progress.Status with
      | PurchaseStatus.Declined -> Some CardNetworkFailReason.Declined
      | PurchaseStatus.Expired -> Some CardNetworkFailReason.Expired
      | PurchaseStatus.Voided -> Some CardNetworkFailReason.Voided
      | _ -> None
      |> Option.map PurchaseFailReason.CardNetwork
      |> Option.iter (fun reason ->
         acquireCardFailureAcknowledgement reason
         acquireAccountFailureAcknowledgement reason
         sendPurchaseFailedEmail reason)
   | PurchaseSagaEvent.SupportTeamResolvedPartnerBankSync ->
      // Support team resolved dispute with partner bank so
      // reattempt syncing transaction to partner bank.
      syncToPartnerBank ()
   | PurchaseSagaEvent.PurchaseSettledWithAccount -> settlePurchaseWithCard ()
   | PurchaseSagaEvent.PurchaseSettledWithCard -> syncToPartnerBank ()
   | PurchaseSagaEvent.PurchaseNotificationSent
   | PurchaseSagaEvent.ResetInProgressActivityAttempts
   | PurchaseSagaEvent.PurchaseFailureAcknowledgedByAccount
   | PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard -> ()
   | PurchaseSagaEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.WaitForCardNetworkResolution
         | Activity.ReserveEmployeeCardFunds
         | Activity.ReserveAccountFunds
         | Activity.WaitForSupportTeamToResolvePartnerBankSync -> ()
         | Activity.SettlePurchaseWithCard -> settlePurchaseWithCard ()
         | Activity.SyncToPartnerBank -> syncToPartnerBank ()
         | Activity.SettlePurchaseWithAccount -> settlePurchaseWithAccount ()
         | Activity.SendPurchaseNotification -> sendPurchaseEmail ()
         | Activity.AcquireCardFailureAcknowledgement ->
            updatedState.FailReason
            |> Option.iter acquireCardFailureAcknowledgement
         | Activity.AcquireAccountFailureAcknowledgement ->
            updatedState.FailReason
            |> Option.iter acquireAccountFailureAcknowledgement
