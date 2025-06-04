module CardSetupSaga

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Bank.Employee.Domain
open Email
open Lib.Saga

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
   | Start of CardSetupSagaStartEvent
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
         | CreateCardViaThirdPartyProvider -> Some(TimeSpan.FromMinutes 2)
         | SendCardSetupSuccessNotification
         | SendCardSetupFailNotification -> Some(TimeSpan.FromMinutes 4)
         | LinkProviderCardId -> Some(TimeSpan.FromSeconds 5)

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
   Events: CardSetupSagaEvent list
   Status: CardSetupSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
   ProviderCardId: ThirdPartyProviderCardId option
}

let applyEvent
   (state: CardSetupSaga option)
   (e: CardSetupSagaEvent)
   (timestamp: DateTime)
   : CardSetupSaga option
   =
   let addActivity = SagaLifeCycle.addActivity timestamp
   let finishActivity = SagaLifeCycle.finishActivity timestamp
   let failActivity = SagaLifeCycle.failActivity timestamp

   match state with
   | None ->
      match e with
      | CardSetupSagaEvent.Start start ->
         let evt = start.Event

         Some {
            Status = CardSetupSagaStatus.InProgress
            Events = [ e ]
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
      | _ -> state
   | Some state ->
      let state =
         match e with
         | CardSetupSagaEvent.Start _ -> state
         | CardSetupSagaEvent.CardSetupSuccessNotificationSent -> {
            state with
               Status = CardSetupSagaStatus.Completed
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.SendCardSetupSuccessNotification
           }
         | CardSetupSagaEvent.CardSetupFailNotificationSent -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.SendCardSetupFailNotification
           }
         | CardSetupSagaEvent.CardCreateResponse res ->
            let activity = Activity.CreateCardViaThirdPartyProvider

            match res with
            | Ok providerCardId -> {
               state with
                  ProviderCardId = Some providerCardId
                  LifeCycle =
                     state.LifeCycle
                     |> finishActivity activity
                     |> addActivity Activity.LinkProviderCardId
              }
            | Error _ -> {
               state with
                  Status =
                     CardSetupFailureReason.CardProviderCardCreateFail
                     |> CardSetupSagaStatus.Failed
                  LifeCycle =
                     state.LifeCycle
                     |> failActivity activity
                     |> addActivity Activity.SendCardSetupFailNotification
              }
         | CardSetupSagaEvent.ProviderCardIdLinked -> {
            state with
               LifeCycle =
                  state.LifeCycle
                  |> finishActivity Activity.LinkProviderCardId
                  |> addActivity Activity.SendCardSetupSuccessNotification
           }
         | CardSetupSagaEvent.EvaluateRemainingWork -> {
            state with
               LifeCycle =
                  SagaLifeCycle.retryActivitiesAfterInactivity
                     timestamp
                     state.LifeCycle
           }
         | CardSetupSagaEvent.ResetInProgressActivityAttempts -> {
            state with
               LifeCycle =
                  SagaLifeCycle.resetInProgressActivities state.LifeCycle
           }

      Some {
         state with
            Events = e :: state.Events
      }

let stateTransition
   (state: CardSetupSaga option)
   (evt: CardSetupSagaEvent)
   (timestamp: DateTime)
   : Result<CardSetupSaga option, SagaStateTransitionError>
   =
   match state with
   | None ->
      match evt with
      | CardSetupSagaEvent.Start _ -> Ok(applyEvent state evt timestamp)
      | _ -> Error SagaStateTransitionError.HasNotStarted
   | Some saga ->
      let eventIsStartEvent =
         match evt with
         | CardSetupSagaEvent.Start _ -> true
         | _ -> false

      let activityIsDone = saga.LifeCycle.ActivityIsInProgress >> not

      let invalidStepProgression =
         match evt with
         | CardSetupSagaEvent.Start _
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
      elif eventIsStartEvent then
         Error SagaStateTransitionError.HasAlreadyStarted
      elif invalidStepProgression then
         Error SagaStateTransitionError.InvalidStepProgression
      else
         Ok(applyEvent state evt timestamp)

type CardProviderRequest = {
   CardHolderName: string
   CardType: string
}

type PersistenceHandlerDependencies = {
   getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>
   getEmailRef: unit -> IActorRef<EmailMessage>
   createCardViaThirdPartyProvider:
      CardProviderRequest -> Async<CardSetupSagaEvent>
   sendMessageToSelf:
      OrgId -> CorrelationId -> Async<CardSetupSagaEvent> -> unit
}

let onStartEventPersisted
   (dep: PersistenceHandlerDependencies)
   (evt: CardSetupSagaStartEvent)
   =
   let request = {
      CardHolderName = evt.EmployeeName
      CardType = string evt.Event.Data.Card.CardType
   }

   dep.sendMessageToSelf
      evt.Event.OrgId
      evt.Event.CorrelationId
      (dep.createCardViaThirdPartyProvider request)

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
   | CardSetupSagaEvent.Start _
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
               CardType = string updatedState.CardType
            }

            dep.sendMessageToSelf
               orgId
               corrId
               (dep.createCardViaThirdPartyProvider request)
         | Activity.LinkProviderCardId ->
            updatedState.ProviderCardId |> Option.iter linkProviderCardId

let createCardViaThirdPartyProvider (_: CardProviderRequest) = async {
   do! Async.Sleep(1300)

   let response = Guid.NewGuid() |> ThirdPartyProviderCardId |> Ok

   return CardSetupSagaEvent.CardCreateResponse response
}
