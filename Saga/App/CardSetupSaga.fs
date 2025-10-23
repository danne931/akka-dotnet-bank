module CardSetupSaga

open System
open Akkling

open Lib.SharedTypes
open Bank.Employee.Domain
open EmailMessage
open Lib.Saga
open CardIssuer.Service.Domain
open CardSetupSaga
open EmployeeOnboardingSaga
open BankActorRegistry

let applyStartEvent
   (start: CardSetupSagaStartEvent)
   (timestamp: DateTime)
   : CardSetupSaga
   =
   let evt = start.Event
   let card = evt.Data.Card

   {
      Status = CardSetupSagaStatus.InProgress
      StartEvent = start
      StartedAt = timestamp
      Events = []
      OrgId = evt.OrgId
      CardId = card.CardId
      Expiration = card.Expiration
      CardNickname = card.CardNickname
      CorrelationId = evt.CorrelationId
      InitiatedBy = evt.InitiatedBy
      EmployeeId = EmployeeId.fromEntityId evt.EntityId
      EmployeeName = start.EmployeeName
      EmployeeEmail = start.EmployeeEmail
      OriginatedFromEmployeeOnboarding =
         start.Event.Data.OriginatedFromEmployeeOnboarding
      CardType = card.CardType
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
      | Ok res -> {
         saga with
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
   (registry: #ICardIssuerServiceActor)
   (evt: CardSetupSagaStartEvent)
   =
   let card = evt.Event.Data.Card

   let request = {
      CardNickname = card.CardNickname
      CardType = card.CardType
      Expiration = card.Expiration
      Metadata = {
         OrgId = evt.Event.OrgId
         CorrelationId = evt.Event.CorrelationId
      }
   }

   registry.CardIssuerServiceActor() <! CardIssuerMessage.CreateCard request

let onEventPersisted
   (registry:
      #ICardIssuerServiceActor & #IEmailActor & #IEmployeeActor & #ISagaGuaranteedDeliveryActor)
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

      registry.EmailActor() <! emailMsg

   let sendCardSetupFailEmail (reason: CardSetupFailureReason) =
      let emailMsg =
         EmailInfo.CardSetupFail {
            EmployeeName = updatedState.EmployeeName
            Reason = string reason
         }
         |> EmailMessage.create orgId corrId

      registry.EmailActor() <! emailMsg

   let linkProviderCardId (res: CardCreateResponse) =
      let msg =
         LinkCardCommand.create {
            Link = {
               CardIssuerName = res.CardIssuerName
               CardIssuerCardId = res.CardIssuerCardId
               CardId = updatedState.CardId
            }
            CardNumberLast4 = res.CardNumberLast4
            OrgId = orgId
            EmployeeId = updatedState.EmployeeId
            CorrelationId = corrId
            InitiatedBy = updatedState.InitiatedBy
         }
         |> EmployeeCommand.LinkCard
         |> EmployeeMessage.StateChange

      registry.EmployeeActor updatedState.EmployeeId <! msg

   match evt with
   | CardSetupSagaEvent.CardCreateResponse res ->
      match res with
      | Ok res -> linkProviderCardId res
      | Error _ ->
         sendCardSetupFailEmail
            CardSetupFailureReason.CardProviderCardCreateFail
   | CardSetupSagaEvent.ProviderCardIdLinked -> sendCardSetupSuccessEmail ()
   | CardSetupSagaEvent.CardSetupSuccessNotificationSent ->
      match updatedState.OriginatedFromEmployeeOnboarding with
      | Some employeeOnboardingSagaId ->
         let msg =
            EmployeeOnboardingSagaEvent.CardSetupSagaCompleted(
               CardSetupSagaId corrId
            )
            |> AppSaga.Message.employeeOnboard orgId employeeOnboardingSagaId
            |> GuaranteedDelivery.message employeeOnboardingSagaId.Value

         registry.SagaGuaranteedDeliveryActor() <! msg
      | None -> ()
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
               CardNickname = updatedState.CardNickname
               CardType = updatedState.CardType
               Expiration = updatedState.Expiration
               Metadata = {
                  OrgId = orgId
                  CorrelationId = corrId
               }
            }

            registry.CardIssuerServiceActor()
            <! CardIssuerMessage.CreateCard request
         | Activity.LinkProviderCardId ->
            updatedState.CardCreateResponse |> Option.iter linkProviderCardId
