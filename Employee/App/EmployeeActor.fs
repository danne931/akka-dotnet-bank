[<RequireQualifiedAccess>]
module EmployeeActor

open System
open Akka.Persistence
open Akka.Persistence.Extras
open Akka.Delivery
open Akkling
open Akkling.Persistence
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Types
open ActorUtil
open Bank.Account.Domain
open Bank.Employee.Domain
open CardIssuer.Service.Domain
open SignalRBroadcast
open PurchaseSaga
open EmployeeOnboardingSaga
open CardSetupSaga
open BankActorRegistry

let private handleValidationError
   (broadcaster: SignalRBroadcast)
   mailbox
   (registry: #ISagaGuaranteedDeliveryActor)
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
      | EmployeeCommand.PurchaseIntent cmd, EmployeeStateTransitionError e ->
         match e with
         | CardNotFound -> Some PurchaseCardFailReason.CardNotFound
         | CardExpired -> Some PurchaseCardFailReason.CardExpired
         | CardLocked -> Some PurchaseCardFailReason.CardLocked
         | ExceededDailyDebit(limit, accrued) ->
            PurchaseCardFailReason.ExceededDailyCardLimit(limit, accrued)
            |> Some
         | ExceededMonthlyDebit(limit, accrued) ->
            PurchaseCardFailReason.ExceededMonthlyCardLimit(limit, accrued)
            |> Some
         | _ -> None
         |> Option.map (fun err -> cmd.Data, err)
      | _ -> None

   match hasPurchaseFail with
   | Some(purchaseInfo, reason) ->
      let msg =
         (purchaseInfo, reason)
         |> PurchaseSagaStartEvent.PurchaseRejectedByCard
         |> AppSaga.Message.purchaseStart
               purchaseInfo.OrgId
               purchaseInfo.CorrelationId

      registry.SagaGuaranteedDeliveryActor() <! msg
   | None -> ()

let private closeEmployeeCards
   (registry: #ICardIssuerServiceActor)
   (cards: Card seq)
   orgId
   corrId
   =
   cards
   |> Seq.choose _.ThirdPartyProviderCardId
   |> Seq.iter (fun providerCardId ->
      registry.CardIssuerServiceActor()
      <! CardIssuerMessage.CloseCard {
         ProviderCardId = providerCardId
         Metadata = {
            OrgId = orgId
            CorrelationId = corrId
         }
      })

let private onPersist
   (registry: #ISagaActor & #ISagaGuaranteedDeliveryActor)
   (mailbox: Eventsourced<obj>)
   (employee: Employee)
   evt
   =
   match evt with
   | EmployeeEvent.CreatedAccountOwner e ->
      let msg =
         EmployeeOnboardingSagaStartEvent.AccountOwnerCreated e
         |> AppSaga.Message.employeeOnboardStart e.OrgId e.CorrelationId

      registry.SagaGuaranteedDeliveryActor() <! msg
   | EmployeeEvent.CreatedEmployee e ->
      let msg =
         EmployeeOnboardingSagaStartEvent.EmployeeCreated e
         |> AppSaga.Message.employeeOnboardStart e.OrgId e.CorrelationId

      registry.SagaGuaranteedDeliveryActor() <! msg
   | EmployeeEvent.AccessApproved e ->
      let msg =
         EmployeeOnboardingSagaEvent.AccessApproved
         |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId

      registry.SagaActor e.CorrelationId <! msg
   | EmployeeEvent.AccessRestored e ->
      let msg =
         EmployeeOnboardingSagaStartEvent.EmployeeAccessRestored {|
            Event = e
            EmployeeEmail = employee.Email
            EmployeeName = employee.Name
            InviteToken = e.Data.InviteToken
         |}
         |> AppSaga.Message.employeeOnboardStart e.OrgId e.CorrelationId

      registry.SagaGuaranteedDeliveryActor() <! msg
   | EmployeeEvent.InvitationTokenRefreshed e ->
      let msg =
         e.Data.InviteToken
         |> EmployeeOnboardingSagaEvent.InviteTokenRefreshed
         |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId

      registry.SagaActor e.CorrelationId <! msg
   | EmployeeEvent.InvitationCancelled e ->
      let msg =
         EmployeeOnboardingSagaEvent.InviteCancelled e.Data.Reason
         |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId

      registry.SagaActor e.CorrelationId <! msg
   | EmployeeEvent.InvitationConfirmed e ->
      let msg =
         EmployeeOnboardingSagaEvent.InviteConfirmed
         |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId

      registry.SagaActor e.CorrelationId <! msg
   | EmployeeEvent.CreatedCard e ->
      let msg =
         AppSaga.Message.cardSetupStart e.OrgId e.CorrelationId {
            Event = e
            EmployeeName = employee.Name
            EmployeeEmail = employee.Email
         }

      registry.SagaGuaranteedDeliveryActor() <! msg
   | EmployeeEvent.UpdatedRole e ->
      match e.Data.Role, e.Data.CardInfo with
      | Role.Scholar, _ ->
         closeEmployeeCards
            registry
            employee.Cards.Values
            e.OrgId
            e.CorrelationId
      | _, Some info ->
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
               Virtual = true
               CardType = CardType.Debit
               InitiatedBy = e.InitiatedBy
               OriginatedFromEmployeeOnboarding = None
            }
            |> EmployeeCommand.CreateCard
            |> EmployeeMessage.StateChange

         mailbox.Parent() <! msg
      | _ -> ()
   | EmployeeEvent.ThirdPartyProviderCardLinked e ->
      let msg =
         CardSetupSagaEvent.ProviderCardIdLinked
         |> AppSaga.Message.cardSetup e.OrgId e.CorrelationId

      registry.SagaActor e.CorrelationId <! msg
   | EmployeeEvent.PurchasePending e ->
      let msg =
         PurchaseSagaStartEvent.PurchaseIntent e.Data.Info
         |> AppSaga.Message.purchaseStart e.OrgId e.CorrelationId

      registry.SagaGuaranteedDeliveryActor() <! msg
   | EmployeeEvent.PurchaseSettled e ->
      let msg =
         PurchaseSagaEvent.PurchaseSettledWithCard
         |> AppSaga.Message.purchase e.OrgId e.CorrelationId

      registry.SagaActor e.CorrelationId <! msg
   | EmployeeEvent.PurchaseFailed e ->
      let msg =
         PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard
         |> AppSaga.Message.purchase e.OrgId e.CorrelationId

      registry.SagaActor e.CorrelationId <! msg
   | _ -> ()

let actorProps
   registry
   (broadcaster: SignalRBroadcast)
   (guaranteedDeliveryConsumerControllerRef:
      IActorRef<ConsumerController.IConsumerCommand<EmployeeMessage>>)
   =
   let handler (mailbox: Eventsourced<obj>) =
      let logError = logError mailbox

      let rec loop (stateOpt: EmployeeSnapshot option) = actor {
         let! msg = mailbox.Receive()

         let state = stateOpt |> Option.defaultValue EmployeeSnapshot.Empty

         let employee = state.Info

         let handleValidationError =
            handleValidationError broadcaster mailbox registry employee

         match box msg with
         | Deferred mailbox (:? EmployeeEvent as evt)
         | Persisted mailbox (:? EmployeeEvent as evt) ->
            let state = Employee.applyEvent state evt
            let employee = state.Info

            broadcaster.employeeEventPersisted evt employee

            onPersist registry mailbox employee evt

            return! loop <| Some state
         | :? SnapshotOffer as o -> return! loop <| Some(unbox o.Snapshot)
         | :? ConsumerController.Delivery<EmployeeMessage> as msg ->
            GuaranteedDelivery.ack msg

            // Send message to parent actor (Persistence Supervisor)
            // for message command to confirmed event persistence.
            mailbox.Parent() <! msg.Message

            return ignored ()
         | :? ConfirmableMessageEnvelope as envelope ->
            match envelope.Message with
            | :? EmployeeMessage as msg ->
               match msg with
               | EmployeeMessage.StateChange cmd ->
                  let validation = Employee.stateTransition state cmd

                  match validation with
                  | Ok(evt, _) ->
                     return!
                        PersistenceSupervisor.confirmPersist
                           mailbox
                           envelope.ConfirmationId
                           evt
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
            // NOTE: Perceived optimization:
            // Persisting PurchaseSettled is essential but probably no need to
            // persist the PurchasePending event as long as SaveSnapshot,
            // invoked below, is called in case the actor restarts. Without
            // SaveSnapshot in the LifecyclePostStop hook below, the actor would
            // restart and potentially lose the latest value for
            // EmployeeSnapshot.PendingPurchaseDeductions since it would not be
            // able to derive the latest value from the persisted events during
            // event reply on actor start.
            //
            // With PersistentEffect.Defer, PurchaseIntent command will invoke
            // the persistence handler to start the PurchaseSaga and record the
            // PendingPurchaseDeductions on EmployeeSnapshot. It will do so
            // without saving it as an event in the event journal.
            | EmployeeMessage.StateChange(EmployeeCommand.PurchaseIntent _ as cmd) ->
               match Employee.stateTransition state cmd with
               | Ok(evt, _) -> return! PersistentEffect.Defer [ evt ]
               | Error err -> handleValidationError cmd err
            // Some messages are sent through traditional AtMostOnceDelivery via
            // a reference to the cluster sharded entity ref rather than Akka.Delivery
            // AtLeastOnceDelivery producer ref so will not hit the
            // ConsumerController.Delivery match case above so need to send message
            // to parent actor (Persistence Supervisor) so the command gets wrapped in a
            // ConfirmableMessageEnvelope for Akka.Persistence.Extras.Confirmation
            | EmployeeMessage.StateChange _ ->
               mailbox.Parent() <! msg
               return ignored ()
         // Event replay on actor start
         | :? EmployeeEvent as e when mailbox.IsRecovering() ->
            return! loop <| Some(Employee.applyEvent state e)
         | msg ->
            PersistentActorEventHandler.handleEvent
               {
                  PersistentActorEventHandler.init with
                     LifecyclePreStart =
                        fun _ ->
                           logDebug mailbox $"EMPLOYEE PRESTART"

                           // Start Guaranteed Delivery Consumer Controller
                           guaranteedDeliveryConsumerControllerRef
                           <! new ConsumerController.Start<EmployeeMessage>(
                              untyped mailbox.Self
                           )

                           ignored ()
                     LifecyclePostStop =
                        fun _ ->
                           logDebug mailbox $"EMPLOYEE POSTSTOP"
                           SaveSnapshot state
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

let initProps
   registry
   (broadcaster: SignalRBroadcast)
   (supervisorEnvConfig: PersistenceSupervisorEnvConfig)
   (persistenceId: string)
   (consumerControllerRef:
      IActorRef<ConsumerController.IConsumerCommand<EmployeeMessage>>)
   =
   let childProps = actorProps registry broadcaster consumerControllerRef

   PersistenceSupervisor.create {
      EnvConfig = supervisorEnvConfig
      ChildProps = childProps.ToProps()
      PersistenceId = persistenceId
      CompatibleWithGuaranteedDelivery = true
      IsPersistableMessage =
         function
         | :? EmployeeMessage as msg ->
            match msg with
            // PurchaseIntent will invoke persistence handler without persisting
            // the event, via PersistentEffect.Defer.
            | EmployeeMessage.StateChange(EmployeeCommand.PurchaseIntent _) ->
               false
            | EmployeeMessage.StateChange _ -> true
            | _ -> false
         | _ -> false
   }
