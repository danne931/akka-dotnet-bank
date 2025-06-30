module CardSetupSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Bank.Employee.Domain
open Email
open Lib.Saga
open CardIssuer.Service.Domain

[<RequireQualifiedAccess>]
type CardSetupFailureReason = | CardProviderCardCreateFail

[<RequireQualifiedAccess>]
type CardSetupSagaStatus =
   | InProgress
   | Completed
   | Failed of CardSetupFailureReason

[<RequireQualifiedAccess>]
type CardSetupSagaStartEvent = {
   EmployeeName: string
   EmployeeEmail: Email
   Event: BankEvent<CreatedCard>
}

[<RequireQualifiedAccess>]
type CardSetupSagaEvent =
   | CardSetupSuccessNotificationSent
   | CardSetupFailNotificationSent
   | CardCreateResponse of Result<ThirdPartyProviderCardId, string>
   | ProviderCardIdLinked
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess>]
type Activity =
   | InitializeCard
   | SendCardSetupSuccessNotification
   | SendCardSetupFailNotification
   | CreateCardViaThirdPartyProvider
   | LinkProviderCardId

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | InitializeCard -> 1
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | InitializeCard -> None
         | CreateCardViaThirdPartyProvider -> Some(TimeSpan.FromMinutes 2.)
         | SendCardSetupSuccessNotification
         | SendCardSetupFailNotification -> Some(TimeSpan.FromMinutes 4.)
         | LinkProviderCardId -> Some(TimeSpan.FromSeconds 5.)

type CardSetupSaga = {
   CardId: CardId
   CardNumberLast4: string
   EmployeeId: EmployeeId
   OrgId: OrgId
   CorrelationId: CorrelationId
   InitiatedBy: Initiator
   EmployeeName: string
   EmployeeEmail: Email
   CardType: CardType
   StartEvent: CardSetupSagaStartEvent
   Events: CardSetupSagaEvent list
   Status: CardSetupSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
   ProviderCardId: ThirdPartyProviderCardId option
}

let applyStartEvent
   (start: CardSetupSagaStartEvent)
   (timestamp: DateTime)
   : CardSetupSaga
   =
   let evt = start.Event

   {
      Status = CardSetupSagaStatus.InProgress
      StartEvent = start
      Events = []
      OrgId = evt.OrgId
      CardId = evt.Data.Card.CardId
      CardNumberLast4 = evt.Data.Card.CardNumberLast4
      CorrelationId = evt.CorrelationId
      InitiatedBy = evt.InitiatedBy
      EmployeeId = EmployeeId.fromEntityId evt.EntityId
      EmployeeName = start.EmployeeName
      EmployeeEmail = start.EmployeeEmail
      CardType = evt.Data.Card.CardType
      ProviderCardId = None
      LifeCycle = {
         SagaLifeCycle.empty with
            InProgress = [
               ActivityLifeCycle.init
                  timestamp
                  Activity.CreateCardViaThirdPartyProvider
            ]
            Completed = [
               {
                  Start = timestamp
                  End = Some timestamp
                  Activity = Activity.InitializeCard
                  MaxAttempts =
                     (Activity.InitializeCard :> IActivity).MaxAttempts
                  Attempts = 1
               }
            ]
      }
   }

let applyEvent
   (saga: CardSetupSaga)
   (evt: CardSetupSagaEvent)
   (timestamp: DateTime)
   : CardSetupSaga
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp
   let failActivity = SagaLifeCycle.failActivity timestamp

   let saga = {
      saga with
         Events = evt :: saga.Events
   }

   match evt with
   | CardSetupSagaEvent.CardSetupSuccessNotificationSent -> {
      saga with
         Status = CardSetupSagaStatus.Completed
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.SendCardSetupSuccessNotification
     }
   | CardSetupSagaEvent.CardSetupFailNotificationSent -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.SendCardSetupFailNotification
     }
   | CardSetupSagaEvent.CardCreateResponse res ->
      let activity = Activity.CreateCardViaThirdPartyProvider

      match res with
      | Ok providerCardId -> {
         saga with
            ProviderCardId = Some providerCardId
            LifeCycle =
               saga.LifeCycle
               |> finishActivity activity
               |> addActivity Activity.LinkProviderCardId
        }
      | Error _ -> {
         saga with
            Status =
               CardSetupFailureReason.CardProviderCardCreateFail
               |> CardSetupSagaStatus.Failed
            LifeCycle =
               saga.LifeCycle
               |> failActivity activity
               |> addActivity Activity.SendCardSetupFailNotification
        }
   | CardSetupSagaEvent.ProviderCardIdLinked -> {
      saga with
         LifeCycle =
            saga.LifeCycle
            |> finishActivity Activity.LinkProviderCardId
            |> addActivity Activity.SendCardSetupSuccessNotification
     }
   | CardSetupSagaEvent.EvaluateRemainingWork -> {
      saga with
         LifeCycle =
            SagaLifeCycle.retryActivitiesAfterInactivity
               timestamp
               saga.LifeCycle
     }
   | CardSetupSagaEvent.ResetInProgressActivityAttempts -> {
      saga with
         LifeCycle = SagaLifeCycle.resetInProgressActivities saga.LifeCycle
     }

let stateTransitionStart
   (evt: CardSetupSagaStartEvent)
   (timestamp: DateTime)
   : Result<CardSetupSaga, SagaStateTransitionError>
   =
   Ok(applyStartEvent evt timestamp)

let stateTransition
   (saga: CardSetupSaga)
   (evt: CardSetupSagaEvent)
   (timestamp: DateTime)
   : Result<CardSetupSaga, SagaStateTransitionError>
   =
   let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

   let invalidStepProgression =
      match evt with
      | CardSetupSagaEvent.EvaluateRemainingWork
      | CardSetupSagaEvent.ResetInProgressActivityAttempts -> false
      | CardSetupSagaEvent.CardSetupSuccessNotificationSent ->
         activityIsDone Activity.SendCardSetupSuccessNotification
      | CardSetupSagaEvent.CardSetupFailNotificationSent ->
         activityIsDone Activity.SendCardSetupFailNotification
      | CardSetupSagaEvent.CardCreateResponse _ ->
         activityIsDone Activity.CreateCardViaThirdPartyProvider
      | CardSetupSagaEvent.ProviderCardIdLinked ->
         activityIsDone Activity.LinkProviderCardId

   if saga.Status = CardSetupSagaStatus.Completed then
      Error SagaStateTransitionError.HasAlreadyCompleted
   elif invalidStepProgression then
      Error SagaStateTransitionError.InvalidStepProgression
   else
      Ok(applyEvent saga evt timestamp)

let onStartEventPersisted
   (getCardIssuerServiceRef: unit -> IActorRef<CardIssuerMessage>)
   (evt: CardSetupSagaStartEvent)
   =
   let request = {
      CardHolderName = evt.EmployeeName
      CardType = evt.Event.Data.Card.CardType
      Metadata = {
         OrgId = evt.Event.OrgId
         CorrelationId = evt.Event.CorrelationId
      }
      ReplyTo = SagaReplyTo.CardSetup
   }

   getCardIssuerServiceRef () <! CardIssuerMessage.CreateCard request

type PersistenceHandlerDependencies = {
   getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   getCardIssuerServiceRef: unit -> IActorRef<CardIssuerMessage>
}

let onEventPersisted
   (dep: PersistenceHandlerDependencies)
   (previousState: CardSetupSaga)
   (updatedState: CardSetupSaga)
   (evt: CardSetupSagaEvent)
   =
   let orgId = updatedState.OrgId
   let corrId = updatedState.CorrelationId

   let sendCardSetupSuccessEmail () =
      let emailMsg =
         EmailInfo.CardSetupSuccess {
            EmployeeName = updatedState.EmployeeName
            EmployeeEmail = updatedState.EmployeeEmail
         }
         |> EmailMessage.create orgId corrId

      dep.getEmailRef () <! emailMsg

   let sendCardSetupFailEmail (reason: CardSetupFailureReason) =
      let emailMsg =
         EmailInfo.CardSetupFail {
            EmployeeName = updatedState.EmployeeName
            Reason = string reason
         }
         |> EmailMessage.create orgId corrId

      dep.getEmailRef () <! emailMsg

   let linkProviderCardId (providerCardId: ThirdPartyProviderCardId) =
      let msg =
         LinkThirdPartyProviderCardCommand.create {
            CardId = updatedState.CardId
            ProviderCardId = providerCardId
            CardNumberLast4 = updatedState.CardNumberLast4
            OrgId = orgId
            EmployeeId = updatedState.EmployeeId
            CorrelationId = corrId
            InitiatedBy = updatedState.InitiatedBy
         }
         |> EmployeeCommand.LinkThirdPartyProviderCard
         |> EmployeeMessage.StateChange

      dep.getEmployeeRef updatedState.EmployeeId <! msg

   match evt with
   | CardSetupSagaEvent.CardCreateResponse res ->
      match res with
      | Ok providerCardId -> linkProviderCardId providerCardId
      | Error _ ->
         sendCardSetupFailEmail
            CardSetupFailureReason.CardProviderCardCreateFail
   | CardSetupSagaEvent.ProviderCardIdLinked -> sendCardSetupSuccessEmail ()
   | CardSetupSagaEvent.CardSetupSuccessNotificationSent
   | CardSetupSagaEvent.CardSetupFailNotificationSent
   | CardSetupSagaEvent.ResetInProgressActivityAttempts -> ()
   | CardSetupSagaEvent.EvaluateRemainingWork ->
      for activity in previousState.LifeCycle.ActivitiesRetryableAfterInactivity do
         match activity.Activity with
         | Activity.InitializeCard -> ()
         | Activity.SendCardSetupSuccessNotification ->
            sendCardSetupSuccessEmail ()
         | Activity.SendCardSetupFailNotification ->
            match updatedState.Status with
            | CardSetupSagaStatus.Failed fail -> sendCardSetupFailEmail fail
            | _ -> ()
         | Activity.CreateCardViaThirdPartyProvider ->
            let request = {
               CardHolderName = updatedState.EmployeeName
               CardType = updatedState.CardType
               Metadata = {
                  OrgId = orgId
                  CorrelationId = corrId
               }
               ReplyTo = SagaReplyTo.CardSetup
            }

            dep.getCardIssuerServiceRef ()
            <! CardIssuerMessage.CreateCard request
         | Activity.LinkProviderCardId ->
            updatedState.ProviderCardId |> Option.iter linkProviderCardId
