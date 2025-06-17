[<RequireQualifiedAccess>]
module EmployeeActor

open System
open Akka.Actor
open Akka.Persistence
open Akka.Persistence.Extras
open Akkling
open Akkling.Persistence
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Types
open ActorUtil
open Bank.Account.Domain
open Bank.Employee.Domain
open SignalRBroadcast
open PurchaseSaga
open EmployeeOnboardingSaga
open CardSetupSaga

let private handleValidationError
   (broadcaster: SignalRBroadcast)
   mailbox
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   (employee: Employee)
   (cmd: EmployeeCommand)
   (err: Err)
   =
   logWarning
      mailbox
      $"Validation fail %s{string err} for command %s{cmd.GetType().Name}"

   broadcaster.employeeEventError
      employee.OrgId
      employee.EmployeeId
      cmd.Envelope.CorrelationId
      err

   let hasPurchaseFail =
      match cmd, err with
      | EmployeeCommand.Purchase cmd, EmployeeStateTransitionError e ->
         match e with
         | CardNotFound -> Some(cmd.Data, PurchaseCardFailReason.CardNotFound)
         | CardExpired -> Some(cmd.Data, PurchaseCardFailReason.CardExpired)
         | CardLocked -> Some(cmd.Data, PurchaseCardFailReason.CardLocked)
         | ExceededDailyDebit(limit, accrued) ->
            Some(
               cmd.Data,
               PurchaseCardFailReason.ExceededDailyCardLimit(limit, accrued)
            )
         | ExceededMonthlyDebit(limit, accrued) ->
            Some(
               cmd.Data,
               PurchaseCardFailReason.ExceededMonthlyCardLimit(limit, accrued)
            )
         | _ -> None
      | _ -> None

   match hasPurchaseFail with
   | Some(purchaseInfo, reason) ->
      let msg =
         (purchaseInfo, reason)
         |> PurchaseSagaStartEvent.PurchaseRejectedByCard
         |> AppSaga.Message.purchaseStart
               purchaseInfo.OrgId
               purchaseInfo.CorrelationId

      getSagaRef purchaseInfo.CorrelationId <! msg
   | None -> ()

let private onPersist
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   (mailbox: Eventsourced<obj>)
   (employee: Employee)
   evt
   =
   match evt with
   | EmployeeEvent.CreatedAccountOwner e ->
      let msg =
         EmployeeOnboardingSagaStartEvent.AccountOwnerCreated e
         |> AppSaga.Message.employeeOnboardStart e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | EmployeeEvent.CreatedEmployee e ->
      let msg =
         EmployeeOnboardingSagaStartEvent.EmployeeCreated e
         |> AppSaga.Message.employeeOnboardStart e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | EmployeeEvent.AccessApproved e ->
      let msg =
         EmployeeOnboardingSagaEvent.AccessApproved
         |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | EmployeeEvent.AccessRestored e ->
      let msg =
         EmployeeOnboardingSagaStartEvent.EmployeeAccessRestored {|
            Event = e
            EmployeeEmail = employee.Email
            EmployeeName = employee.Name
            InviteToken = e.Data.InviteToken
         |}
         |> AppSaga.Message.employeeOnboardStart e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | EmployeeEvent.InvitationTokenRefreshed e ->
      let msg =
         e.Data.InviteToken
         |> EmployeeOnboardingSagaEvent.InviteTokenRefreshed
         |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | EmployeeEvent.InvitationCancelled e ->
      let msg =
         EmployeeOnboardingSagaEvent.InviteCancelled e.Data.Reason
         |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | EmployeeEvent.InvitationConfirmed e ->
      let msg =
         EmployeeOnboardingSagaEvent.InviteConfirmed
         |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | EmployeeEvent.CreatedCard e ->
      match e.Data.Card.Status with
      | CardStatus.Pending ->
         let msg =
            AppSaga.Message.cardSetupStart e.OrgId e.CorrelationId {
               Event = e
               EmployeeName = employee.Name
               EmployeeEmail = employee.Email
            }

         getSagaRef e.CorrelationId <! msg
      | CardStatus.Active _ ->
         let msg =
            EmployeeOnboardingSagaEvent.CardAssociatedWithEmployee
            |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId

         getSagaRef e.CorrelationId <! msg
      | _ -> ()
   | EmployeeEvent.UpdatedRole e ->
      match e.Data.CardInfo with
      | Some info ->
         let msg =
            CreateCardCommand.create {
               AccountId = info.LinkedAccountId
               DailyPurchaseLimit = Some info.DailyPurchaseLimit
               MonthlyPurchaseLimit = Some info.MonthlyPurchaseLimit
               PersonName = employee.Name
               CardNickname = None
               OrgId = e.OrgId
               EmployeeId = employee.EmployeeId
               CardId = CardId <| Guid.NewGuid()
               ProviderCardId = None
               Virtual = true
               CardType = CardType.Debit
               InitiatedBy = e.InitiatedBy
            }
            |> EmployeeCommand.CreateCard
            |> EmployeeMessage.StateChange

         mailbox.Parent() <! msg
      | None -> ()
   | EmployeeEvent.ThirdPartyProviderCardLinked e ->
      let msg =
         CardSetupSagaEvent.ProviderCardIdLinked
         |> AppSaga.Message.cardSetup e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | EmployeeEvent.PurchaseApplied e ->
      let msg =
         PurchaseSagaStartEvent.DeductedCardFunds e.Data.Info
         |> AppSaga.Message.purchaseStart e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | EmployeeEvent.PurchaseFailed e ->
      let msg =
         PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard
         |> AppSaga.Message.purchase e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | _ -> ()

let actorProps
   (broadcaster: SignalRBroadcast)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   =
   let handler (mailbox: Eventsourced<obj>) =
      let logError = logError mailbox

      let rec loop (stateOpt: EmployeeSnapshot option) = actor {
         let! msg = mailbox.Receive()

         let state =
            stateOpt
            |> Option.defaultValue { Info = Employee.empty; Events = [] }

         let employee = state.Info

         let handleValidationError =
            handleValidationError broadcaster mailbox getSagaRef employee

         match box msg with
         | Persisted mailbox e ->
            let (EmployeeMessage.Event evt) = unbox e
            let state = Employee.applyEvent state evt
            let employee = state.Info

            broadcaster.employeeEventPersisted evt employee

            onPersist getSagaRef mailbox employee evt

            return! loop <| Some state
         | :? SnapshotOffer as o -> return! loop <| Some(unbox o.Snapshot)
         | :? ConfirmableMessageEnvelope as envelope ->
            match envelope.Message with
            | :? EmployeeMessage as msg ->
               match msg with
               | EmployeeMessage.StateChange cmd ->
                  let validation = Employee.stateTransition state cmd

                  match validation with
                  | Ok(evt, _) ->
                     return!
                        confirmPersist
                           mailbox
                           envelope.ConfirmationId
                           (EmployeeMessage.Event evt)
                  | Error err -> handleValidationError cmd err
               | msg ->
                  logError
                     $"Unknown message in ConfirmableMessageEnvelope - {msg}"

                  unhandled ()
            | msg ->
               logError $"Unknown message in ConfirmableMessageEnvelope - {msg}"
               return unhandled ()
         | :? EmployeeMessage as msg ->
            match msg with
            | EmployeeMessage.GetEmployee ->
               mailbox.Sender() <! (stateOpt |> Option.map _.Info)
            | EmployeeMessage.Delete ->
               let newState = {
                  state with
                     Info.Status = EmployeeStatus.ReadyForDelete
               }

               return! loop (Some newState) <@> DeleteMessages Int64.MaxValue
         // Event replay on actor start
         | :? EmployeeEvent as e when mailbox.IsRecovering() ->
            return! loop <| Some(Employee.applyEvent state e)
         | msg ->
            PersistentActorEventHandler.handleEvent
               {
                  PersistentActorEventHandler.init with
                     DeleteMessagesSuccess =
                        fun _ ->
                           if
                              employee.Status = EmployeeStatus.ReadyForDelete
                           then
                              logDebug mailbox "<Passivate Employee Actor>"
                              passivate ()
                           else
                              ignored ()
               }
               mailbox
               msg
      }

      loop None

   propsPersist handler

let get
   (sys: ActorSystem)
   (employeeId: EmployeeId)
   : IEntityRef<EmployeeMessage>
   =
   getEntityRef
      sys
      ClusterMetadata.employeeShardRegion
      (EmployeeId.get employeeId)

let isPersistableMessage (msg: obj) =
   match msg with
   | :? EmployeeMessage as msg ->
      match msg with
      | EmployeeMessage.StateChange _ -> true
      | _ -> false
   | _ -> false

let initProps
   (supervisorOpts: PersistenceSupervisorOptions)
   (persistenceId: string)
   (broadcaster: SignalRBroadcast)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   =
   persistenceSupervisor
      supervisorOpts
      isPersistableMessage
      (actorProps broadcaster getSagaRef)
      persistenceId
