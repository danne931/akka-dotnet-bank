module PlatformPaymentSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Saga
open Bank.Account.Domain
open Bank.Transfer.Domain
open Email
open PartnerBank.Service.Domain

[<RequireQualifiedAccess>]
type PlatformPaymentSagaStartEvent =
   | PaymentRequested of BankEvent<PlatformPaymentRequested>

[<RequireQualifiedAccess>]
type PlatformPaymentSagaEvent =
   | PaymentRequestCancelled
   | PaymentRequestDeclined
   | PaymentFulfilled of PaymentFulfilled
   | PaymentFailed of TransferId * PlatformPaymentFailReason
   | PaymentRequestNotificationSentToPayer
   | PaymentDeclinedNotificationSentToPayee
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

type private StartEvent = PlatformPaymentSagaStartEvent
type private Event = PlatformPaymentSagaEvent

[<RequireQualifiedAccess>]
type PlatformPaymentSagaStatus =
   | InProgress of PaymentRequestStatus
   | Completed
   | Failed of PlatformPaymentFailReason

[<RequireQualifiedAccess>]
type Activity =
   | NotifyPayerOfRequest
   | NotifyPayeeOfDecline
   | WaitForPayment

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForPayment -> 0
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | WaitForPayment -> None
         | NotifyPayerOfRequest
         | NotifyPayeeOfDecline -> Some(TimeSpan.FromMinutes 4.)

type PlatformPaymentSaga = {
   StartEvent: PlatformPaymentSagaStartEvent
   Events: PlatformPaymentSagaEvent list
   Status: PlatformPaymentSagaStatus
   PaymentInfo: PlatformPaymentBaseInfo
   LifeCycle: SagaLifeCycle<Activity>
   InitiatedBy: Initiator
}

let applyStartEvent (e: PlatformPaymentSagaStartEvent) (timestamp: DateTime) =
   match e with
   | StartEvent.PaymentRequested evt -> {
      StartEvent = e
      Events = []
      Status =
         PlatformPaymentSagaStatus.InProgress PaymentRequestStatus.Requested
      PaymentInfo = evt.Data.BaseInfo
      InitiatedBy = evt.InitiatedBy
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init timestamp Activity.NotifyPayerOfRequest
               ActivityLifeCycle.init timestamp Activity.WaitForPayment
            ]
      }
     }

let applyEvent
   (saga: PlatformPaymentSaga)
   (evt: PlatformPaymentSagaEvent)
   (timestamp: DateTime)
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp
   let failActivity = SagaLifeCycle.failActivity timestamp

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | Event.PaymentRequestCancelled -> {
      saga with
         Status = PlatformPaymentSagaStatus.Completed
         LifeCycle = saga.LifeCycle |> finishActivity Activity.WaitForPayment
     }
   | Event.PaymentRequestDeclined -> {
      saga with
         Status =
            PlatformPaymentSagaStatus.InProgress PaymentRequestStatus.Declined
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.WaitForPayment
            |> addActivity Activity.NotifyPayeeOfDecline
     }
   | Event.PaymentFulfilled _ -> {
      saga with
         LifeCycle = saga.LifeCycle |> finishActivity Activity.WaitForPayment
         Status = PlatformPaymentSagaStatus.Completed
     }
   | Event.PaymentFailed(_, reason) -> {
      saga with
         Status = PlatformPaymentSagaStatus.Failed reason
         LifeCycle = saga.LifeCycle |> failActivity Activity.WaitForPayment
     }
   | Event.PaymentRequestNotificationSentToPayer -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.NotifyPayerOfRequest
     }
   | Event.PaymentDeclinedNotificationSentToPayee -> {
      saga with
         LifeCycle =
            saga.LifeCycle |> finishActivity Activity.NotifyPayeeOfDecline
         Status = PlatformPaymentSagaStatus.Completed
     }
   | Event.EvaluateRemainingWork -> {
      saga with
         LifeCycle =
            SagaLifeCycle.retryActivitiesAfterInactivity
               timestamp
               saga.LifeCycle
     }
   | Event.ResetInProgressActivityAttempts -> {
      saga with
         LifeCycle = SagaLifeCycle.resetInProgressActivities saga.LifeCycle
     }

let stateTransitionStart
   (evt: PlatformPaymentSagaStartEvent)
   (timestamp: DateTime)
   : Result<PlatformPaymentSaga, SagaStateTransitionError>
   =
   Ok(applyStartEvent evt timestamp)

let stateTransition
   (saga: PlatformPaymentSaga)
   (evt: PlatformPaymentSagaEvent)
   (timestamp: DateTime)
   : Result<PlatformPaymentSaga, SagaStateTransitionError>
   =
   let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

   let invalidStepProgression =
      match evt with
      | PlatformPaymentSagaEvent.EvaluateRemainingWork
      | PlatformPaymentSagaEvent.ResetInProgressActivityAttempts -> false
      | PlatformPaymentSagaEvent.PaymentRequestDeclined
      | PlatformPaymentSagaEvent.PaymentRequestCancelled
      | PlatformPaymentSagaEvent.PaymentFailed _
      | PlatformPaymentSagaEvent.PaymentFulfilled _ ->
         activityIsDone Activity.WaitForPayment
      | PlatformPaymentSagaEvent.PaymentRequestNotificationSentToPayer ->
         activityIsDone Activity.NotifyPayerOfRequest
      | PlatformPaymentSagaEvent.PaymentDeclinedNotificationSentToPayee ->
         activityIsDone Activity.NotifyPayeeOfDecline

   if saga.Status = PlatformPaymentSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

let notifyPayerOfPaymentRequest emailRef (payment: PlatformPaymentBaseInfo) =
   let msg =
      EmailMessage.create
         payment.Payer.OrgId
         (PaymentId.toCorrelationId payment.Id)
         (EmailInfo.PlatformPaymentRequested {
            Amount = payment.Amount
            PayeeBusinessName = payment.Payee.OrgName
            PayerBusinessName = payment.Payer.OrgName
         })

   emailRef <! msg

let onStartEventPersisted
   (getEmailRef: unit -> IActorRef<EmailMessage>)
   (evt: StartEvent)
   =
   match evt with
   | StartEvent.PaymentRequested e ->
      notifyPayerOfPaymentRequest (getEmailRef ()) e.Data.BaseInfo

type PersistenceHandlerDependencies = {
   getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   getPartnerBankServiceRef: unit -> IActorRef<PartnerBankServiceMessage>
   sendMessageToSelf: PlatformPaymentBaseInfo -> Async<Event> -> unit
}

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
   (previousState: PlatformPaymentSaga)
   (state: PlatformPaymentSaga)
   (evt: Event)
   =
   let payment = state.PaymentInfo
   let correlationId = PaymentId.toCorrelationId payment.Id
   let emailRef = dep.getEmailRef ()

   let notifyPayeeOfPaymentDecline () =
      let msg =
         EmailMessage.create
            payment.Payee.OrgId
            correlationId
            (EmailInfo.PlatformPaymentDeclined {
               PayerBusinessName = payment.Payer.OrgName
               PayeeBusinessName = payment.Payee.OrgName
               Amount = payment.Amount
            })

      emailRef <! msg

   match evt with
   | Event.PaymentRequestCancelled -> ()
   | Event.PaymentRequestDeclined -> notifyPayeeOfPaymentDecline ()
   | Event.PaymentFulfilled _ -> ()
   | Event.PaymentFailed _ -> ()
   | Event.ResetInProgressActivityAttempts
   | Event.PaymentDeclinedNotificationSentToPayee
   | Event.PaymentRequestNotificationSentToPayer
   | Event.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.NotifyPayerOfRequest ->
            notifyPayerOfPaymentRequest emailRef payment
         | Activity.NotifyPayeeOfDecline -> notifyPayeeOfPaymentDecline ()
         | Activity.WaitForPayment -> ()
