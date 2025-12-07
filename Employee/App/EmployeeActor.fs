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
open Bank.Purchase.Domain
open CardIssuer.Service.Domain
open SignalRBroadcast
open PurchaseSaga
open EmployeeOnboardingSaga
open CardSetupSaga
open BankActorRegistry

type LookbackHours = { Outbox: int; ProcessedCommands: int }

module private PurchaseInfo =
   let fromAuth
      (auth: PurchaseAuthorization)
      (employee: Employee)
      (card: Card)
      : PurchaseInfo
      =
      {
         Amount = auth.Amount
         Date = auth.CreatedAt
         Merchant = auth.MerchantName
         Reference = None
         OrgId = employee.OrgId
         EmployeeId = employee.EmployeeId
         ParentAccountId = employee.ParentAccountId
         AccountId = card.AccountId
         CorrelationId = Guid.NewGuid() |> CorrelationId
         CardId = card.CardId
         CardNickname = card.CardNickname
         CardIssuerCardId = auth.CardIssuerCardId
         CardIssuerTransactionId = auth.CardIssuerTransactionId
         CardNumberLast4 = card.CardNumberLast4
         CurrencyMerchant = auth.CurrencyMerchant
         CurrencyCardHolder = auth.CurrencyCardHolder
         InitiatedBy = {
            Id = InitiatedById employee.EmployeeId
            Name = employee.Name
         }
         EmployeeName = employee.Name
         EmployeeEmail = employee.Email
         AuthorizationType = auth.Type
      }

   let fromProgressWithAuthBypass
      (progress: CardIssuerPurchaseProgress)
      (employee: Employee)
      (card: Card)
      : PurchaseInfo
      =
      let first = progress.OriginatingEvent

      {
         Amount = first.Money.Amount
         Date = first.CreatedAt
         Merchant = progress.MerchantName
         Reference = None
         OrgId = employee.OrgId
         EmployeeId = employee.EmployeeId
         ParentAccountId = employee.ParentAccountId
         AccountId = card.AccountId
         CorrelationId = Guid.NewGuid() |> CorrelationId
         CardId = card.CardId
         CardNickname = card.CardNickname
         CardIssuerCardId = progress.CardIssuerCardId
         CardIssuerTransactionId = progress.PurchaseId
         CardNumberLast4 = card.CardNumberLast4
         CurrencyMerchant = progress.Amounts.Merchant.Currency
         CurrencyCardHolder = progress.Amounts.Cardholder.Currency
         InitiatedBy = {
            Id = InitiatedById employee.EmployeeId
            Name = employee.Name
         }
         EmployeeName = employee.Name
         EmployeeEmail = employee.Email
         AuthorizationType = PurchaseAuthType.BypassAuth
      }

let private hasPurchaseFail (cmd: EmployeeCommand) (err: Err) =
   match cmd, err with
   | EmployeeCommand.PurchaseIntent cmd, EmployeeStateTransitionError e ->
      match e with
      | CardNotFound -> Some PurchaseCardFailReason.CardNotFound
      | CardExpired -> Some PurchaseCardFailReason.CardExpired
      | CardLocked -> Some PurchaseCardFailReason.CardLocked
      | ExceededDailyDebit(limit, accrued) ->
         PurchaseCardFailReason.ExceededDailyCardLimit(limit, accrued) |> Some
      | ExceededMonthlyDebit(limit, accrued) ->
         PurchaseCardFailReason.ExceededMonthlyCardLimit(limit, accrued) |> Some
      | _ -> None
      |> Option.map (fun reason -> cmd.Data, reason)
   | _ -> None

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

   match hasPurchaseFail cmd err with
   | Some(purchaseInfo, reason) ->
      let evt =
         reason |> PurchaseFailReason.Card |> PurchaseSagaEvent.PurchaseRejected

      let msg =
         AppSaga.Message.purchase
            purchaseInfo.OrgId
            purchaseInfo.CorrelationId
            evt
         |> GuaranteedDelivery.message purchaseInfo.CorrelationId.Value

      registry.SagaGuaranteedDeliveryActor() <! msg

      if not purchaseInfo.AuthorizationType.IsBypassAuth then
         mailbox.Sender()
         <! PurchaseAuthorizationStatus.fromCardFailReason reason
   | None -> ()

let private closeEmployeeCards
   (registry: #ICardIssuerServiceActor)
   (employee: EmployeeSnapshot)
   corrId
   =
   for link in employee.CardIssuerLinks.Values do
      registry.CardIssuerServiceActor()
      <! CardIssuerMessage.CloseCard {
         CardIssuerCardId = link.CardIssuerCardId
         Metadata = {
            OrgId = employee.Info.OrgId
            CorrelationId = corrId
         }
      }

let private computeOutboxMessage
   (employee: Employee)
   (evt: EmployeeEvent)
   : EmployeeOutboxMessage option
   =
   match evt with
   | EmployeeEvent.CreatedAccountOwner e ->
      let msg =
         EmployeeOnboardingSagaStartEvent.AccountOwnerCreated e
         |> AppSaga.Message.employeeOnboardStart e.OrgId e.CorrelationId

      Some(EmployeeOutboxMessage.Saga msg)
   | EmployeeEvent.CreatedEmployee e ->
      let msg =
         EmployeeOnboardingSagaStartEvent.EmployeeCreated e
         |> AppSaga.Message.employeeOnboardStart e.OrgId e.CorrelationId

      Some(EmployeeOutboxMessage.Saga msg)
   | EmployeeEvent.AccessApproved e ->
      let msg =
         EmployeeOnboardingSagaEvent.AccessApproved
         |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(EmployeeOutboxMessage.Saga msg)
   | EmployeeEvent.AccessRestored e ->
      let msg =
         EmployeeOnboardingSagaStartEvent.EmployeeAccessRestored {|
            Event = e
            EmployeeEmail = employee.Email
            EmployeeName = employee.Name
            InviteToken = e.Data.InviteToken
         |}
         |> AppSaga.Message.employeeOnboardStart e.OrgId e.CorrelationId

      Some(EmployeeOutboxMessage.Saga msg)
   | EmployeeEvent.InvitationTokenRefreshed e ->
      let msg =
         e.Data.InviteToken
         |> EmployeeOnboardingSagaEvent.InviteTokenRefreshed
         |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(EmployeeOutboxMessage.Saga msg)
   | EmployeeEvent.InvitationCancelled e ->
      let msg =
         EmployeeOnboardingSagaEvent.InviteCancelled e.Data.Reason
         |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(EmployeeOutboxMessage.Saga msg)
   | EmployeeEvent.InvitationConfirmed e ->
      let msg =
         EmployeeOnboardingSagaEvent.InviteConfirmed
         |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(EmployeeOutboxMessage.Saga msg)
   | EmployeeEvent.CreatedCard e ->
      let msg =
         AppSaga.Message.cardSetupStart e.OrgId e.CorrelationId {
            Event = e
            EmployeeName = employee.Name
            EmployeeEmail = employee.Email
         }

      Some(EmployeeOutboxMessage.Saga msg)
   | EmployeeEvent.CardLinked e ->
      let msg =
         CardSetupSagaEvent.ProviderCardIdLinked
         |> AppSaga.Message.cardSetup e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(EmployeeOutboxMessage.Saga msg)
   | EmployeeEvent.PurchasePending e when
      e.Data.Info.AuthorizationType.IsBypassAuth
      ->
      let msg =
         AppSaga.Message.purchase
            e.OrgId
            e.CorrelationId
            PurchaseSagaEvent.CardReservedFunds
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(EmployeeOutboxMessage.Saga msg)
   | EmployeeEvent.PurchaseSettled e ->
      let msg =
         PurchaseSagaEvent.PurchaseSettledWithCard e.Data.Clearing
         |> AppSaga.Message.purchase e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(EmployeeOutboxMessage.Saga msg)
   | EmployeeEvent.PurchaseFailed e ->
      let msg =
         PurchaseSagaEvent.PurchaseFailureAcknowledgedByCard
         |> AppSaga.Message.purchase e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(EmployeeOutboxMessage.Saga msg)
   | _ -> None

let applyComputedOutboxToState (state: EmployeeSnapshot) (evt: EmployeeEvent) =
   let computed = computeOutboxMessage state.Info evt
   let _, envelope = EmployeeEnvelope.unwrap evt

   match computed with
   | Some msg -> {
      state with
         Outbox = state.Outbox |> Map.add envelope.Id (envelope.Timestamp, msg)
     }
   | None -> state

let private forwardMessagesAfterEventPersistence
   (registry: #ISagaGuaranteedDeliveryActor & #ISagaActor)
   (mailbox: Eventsourced<obj>)
   (state: EmployeeSnapshot)
   (evt: EmployeeEvent)
   =
   let employee = state.Info
   let _, envelope = EmployeeEnvelope.unwrap evt

   match evt with
   | EmployeeEvent.UpdatedRole e ->
      match e.Data.Role, e.Data.CardInfo with
      | Role.Scholar, _ -> closeEmployeeCards registry state e.CorrelationId
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
   | _ ->
      match computeOutboxMessage employee evt with
      | Some(EmployeeOutboxMessage.Saga msg) ->
         match msg with
         | :? AppSaga.AppSagaMessage as msg ->
            registry.SagaActor envelope.CorrelationId <! msg
         | :? GuaranteedDelivery.Message<AppSaga.AppSagaMessage> as msg ->
            registry.SagaGuaranteedDeliveryActor() <! msg
         | msg -> logError mailbox $"Unknown outbox message {msg}"
      | None -> ()

type private PurchaseAuthTimeout() = class end

let actorProps
   (registry: #IAccountActor & #ISagaActor & #ISagaGuaranteedDeliveryActor)
   (broadcaster: SignalRBroadcast)
   (lookbackHours: LookbackHours)
   (onLifeCycleEvent: Eventsourced<obj> -> Employee -> obj -> Effect<obj>)
   =
   let handler (mailbox: Eventsourced<obj>) =
      let logError = logError mailbox

      let rec waitForPurchaseAuthResponseByAccountActor
         replyTo
         state
         (evt: EmployeeEvent)
         =
         let authTimeout =
            mailbox.Schedule
               (TimeSpan.FromSeconds 4.5)
               mailbox.Self
               (PurchaseAuthTimeout())

         actor {
            let! msg = mailbox.Receive()

            match box msg with
            | Persisted mailbox (:? EmployeeEvent as evt) ->
               replyTo <! PurchaseAuthorizationStatus.Approved
               mailbox.UnstashAll()

               let state = Employee.applyEvent state evt
               return! operating (Some state)
            | :? PurchaseAuthorizationStatus as authStatusFromAccount ->
               authTimeout.Cancel()

               match authStatusFromAccount with
               | PurchaseAuthorizationStatus.Approved ->
                  return! Persist(box evt)
               | _ ->
                  replyTo <! authStatusFromAccount

                  mailbox.UnstashAll()

                  return! operating (Some state)
            | :? ConfirmableMessageEnvelope
            | :? EmployeeMessage -> mailbox.Stash()
            | :? PurchaseAuthTimeout ->
               logWarning
                  mailbox
                  "Purchase Auth Timeout waiting for Account Actor response"

               replyTo <! PurchaseAuthorizationStatus.VelocityExceeded

               return! operating (Some state)
            | other -> return onLifeCycleEvent mailbox state.Info other
         }

      and operating (stateOpt: EmployeeSnapshot option) = actor {
         let! msg = mailbox.Receive()

         let state = stateOpt |> Option.defaultValue EmployeeSnapshot.Empty

         let employee = state.Info

         let handleValidationError =
            handleValidationError broadcaster mailbox registry employee

         match box msg with
         | Persisted mailbox (:? EmployeeEvent as evt) ->
            let state = Employee.applyEvent state evt
            let employee = state.Info

            broadcaster.employeeEventPersisted evt employee

            forwardMessagesAfterEventPersistence registry mailbox state evt

            let state = applyComputedOutboxToState state evt
            return! operating (Some state)
         | :? SnapshotOffer as o -> return! operating <| Some(unbox o.Snapshot)
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
               // Redeliver messages in outbox to saga actor.
               // See EmployeeOutbox type for more information.
               | EmployeeMessage.StateChange cmd when
                  state.Outbox.ContainsKey cmd.Envelope.Id
                  ->
                  let corrId = cmd.Envelope.CorrelationId

                  match state.Outbox |> Map.tryFind cmd.Envelope.Id with
                  | Some(timestamp, EmployeeOutboxMessage.Saga msg) ->
                     let logOutboxRedeliver () =
                        logWarning
                           mailbox
                           $"Associated message exists in outbox {msg} {timestamp}"

                     match msg with
                     | :? AppSaga.AppSagaMessage as msg ->
                        logOutboxRedeliver ()

                        registry.SagaActor corrId
                        <! AppSaga.Message.redelivered msg

                        return ignored ()
                     | :? GuaranteedDelivery.Message<AppSaga.AppSagaMessage> as msg ->
                        logOutboxRedeliver ()

                        let msg =
                           AppSaga.Message.guaranteedDelivery
                              corrId
                              (AppSaga.Message.redelivered msg.Message)

                        registry.SagaGuaranteedDeliveryActor() <! msg
                        return ignored ()
                     | other ->
                        logError
                           $"Unknown message in outbox {other} {timestamp}"
                  | None -> return ignored ()
               // Handle potential for receiving the same command multiple times.
               | EmployeeMessage.StateChange cmd when
                  state.ProcessedCommands.ContainsKey cmd.Envelope.Id
                  ->
                  logDebug mailbox $"Received duplicate command {cmd}"
                  return ignored ()
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

               return!
                  operating (Some newState) <@> DeleteMessages Int64.MaxValue
            | EmployeeMessage.AuthorizePurchase auth ->
               match state.Info.Cards.TryFind auth.CardId with
               | None ->
                  let err = Error PurchaseCardFailReason.CardNotFound
                  mailbox.Sender() <! err
                  return ignored ()
               | Some card ->
                  let msg =
                     PurchaseInfo.fromAuth auth employee card
                     |> PurchaseIntentCommand.create
                     |> EmployeeCommand.PurchaseIntent
                     |> EmployeeMessage.StateChange

                  mailbox.Self <<! msg
                  return ignored ()
            | EmployeeMessage.PurchaseProgress(progress, cardId) ->
               match
                  state.PendingPurchases.TryFind progress.PurchaseId,
                  state.Info.Cards.TryFind cardId
               with
               | _, None ->
                  logError
                     $"Received PurchaseUpdate but no card found {progress.PurchaseId}"
               | None, Some card ->
                  let purchase =
                     PurchaseInfo.fromProgressWithAuthBypass
                        progress
                        employee
                        card

                  let evt =
                     PurchaseSagaStartEvent.PurchaseProgress(purchase, progress)

                  let msg =
                     AppSaga.Message.purchaseStart
                        employee.OrgId
                        purchase.CorrelationId
                        evt
                     |> GuaranteedDelivery.message purchase.CorrelationId.Value

                  registry.SagaGuaranteedDeliveryActor() <! msg
               | Some purchase, _ ->
                  let evt =
                     PurchaseSagaEvent.CardIssuerUpdatedPurchaseProgress
                        progress

                  let info = purchase.Info

                  let msg =
                     AppSaga.Message.purchase info.OrgId info.CorrelationId evt
                     |> GuaranteedDelivery.message info.CorrelationId.Value

                  registry.SagaGuaranteedDeliveryActor() <! msg

               return ignored ()
            | EmployeeMessage.StateChange(EmployeeCommand.PurchaseIntent intent as cmd) when
               not intent.Data.AuthorizationType.IsBypassAuth
               ->
               let purchase = intent.Data

               let msg =
                  AppSaga.Message.purchaseStart
                     intent.OrgId
                     intent.CorrelationId
                     (PurchaseSagaStartEvent.PurchaseIntent purchase)
                  |> GuaranteedDelivery.message intent.CorrelationId.Value

               registry.SagaGuaranteedDeliveryActor() <! msg

               match Employee.stateTransition state cmd with
               | Ok(evt, _) ->
                  let accountRef =
                     registry.AccountActor purchase.ParentAccountId

                  let msg =
                     purchase
                     |> DebitCommand.fromPurchase
                     |> AccountCommand.Debit
                     |> AccountMessage.StateChange

                  accountRef <! msg

                  return
                     waitForPurchaseAuthResponseByAccountActor
                        (mailbox.Sender())
                        state
                        evt
               | Error err ->
                  handleValidationError cmd err
                  return ignored ()
            // Some messages are sent through traditional AtMostOnceDelivery via
            // a reference to the cluster sharded entity ref rather than Akka.Delivery
            // AtLeastOnceDelivery producer ref so will not hit the
            // ConsumerController.Delivery match case above so need to send message
            // to parent actor (Persistence Supervisor) so the command gets wrapped in a
            // ConfirmableMessageEnvelope for Akka.Persistence.Extras.Confirmation
            | EmployeeMessage.StateChange _ ->
               mailbox.Parent() <! msg
               return ignored ()
            | EmployeeMessage.PruneIdempotencyChecker ->
               let cutoff =
                  DateTime.UtcNow.AddHours -lookbackHours.ProcessedCommands

               let stateOpt =
                  stateOpt
                  |> Option.map (fun state -> {
                     state with
                        ProcessedCommands =
                           state.ProcessedCommands
                           |> Map.filter (fun _ date -> date > cutoff)
                  })

               return! operating stateOpt
            | EmployeeMessage.PruneOutbox ->
               let cutoff = DateTime.UtcNow.AddHours -lookbackHours.Outbox

               let stateOpt =
                  stateOpt
                  |> Option.map (fun state -> {
                     state with
                        Outbox =
                           state.Outbox
                           |> Map.filter (fun _ (date, _) -> date > cutoff)
                  })

               return! operating stateOpt
         // Event replay on actor start
         | :? EmployeeEvent as e when mailbox.IsRecovering() ->
            let state = Employee.applyEvent state e
            let state = applyComputedOutboxToState state e
            return! operating (Some state)
         | msg -> return onLifeCycleEvent mailbox employee msg
      }

      operating None

   propsPersist handler

let private handleLifeCycleEvent
   (guaranteedDeliveryConsumerControllerRef:
      IActorRef<ConsumerController.IConsumerCommand<EmployeeMessage>>)
   (mailbox: Eventsourced<obj>)
   (employee: Employee)
   (msg: obj)
   =
   let logDebug = logDebug mailbox

   PersistentActorEventHandler.handleEvent
      {
         PersistentActorEventHandler.init with
            LifecyclePreStart =
               fun _ ->
                  logDebug "<PreStart Employee Actor>"

                  // Start Guaranteed Delivery Consumer Controller
                  guaranteedDeliveryConsumerControllerRef
                  <! new ConsumerController.Start<EmployeeMessage>(
                     untyped mailbox.Self
                  )

                  mailbox.ScheduleRepeatedly
                     TimeSpan.Zero
                     (TimeSpan.FromHours 4)
                     mailbox.Self
                     EmployeeMessage.PruneIdempotencyChecker
                  |> ignore

                  mailbox.ScheduleRepeatedly
                     TimeSpan.Zero
                     (TimeSpan.FromHours 4)
                     mailbox.Self
                     EmployeeMessage.PruneOutbox
                  |> ignore

                  ignored ()
            LifecyclePostStop =
               fun _ ->
                  logDebug "<PostStop Employee Actor>"
                  ignored ()
            DeleteMessagesSuccess =
               fun _ ->
                  if employee.Status = EmployeeStatus.ReadyForDelete then
                     logDebug "<Passivate Employee Actor>"
                     passivate ()
                  else
                     ignored ()
      }
      mailbox
      msg

let initProps
   registry
   (broadcaster: SignalRBroadcast)
   (supervisorEnvConfig: PersistenceSupervisorEnvConfig)
   (persistenceId: string)
   (consumerControllerRef:
      IActorRef<ConsumerController.IConsumerCommand<EmployeeMessage>>)
   =
   let lookbackHours = { Outbox = 24; ProcessedCommands = 24 }

   let childProps =
      actorProps
         registry
         broadcaster
         lookbackHours
         (handleLifeCycleEvent consumerControllerRef)

   PersistenceSupervisor.create {
      EnvConfig = supervisorEnvConfig
      ChildProps = childProps.ToProps()
      PersistenceId = persistenceId
      CompatibleWithGuaranteedDelivery = true
      IsPersistableMessage =
         function
         | :? EmployeeMessage as msg ->
            match msg with
            | EmployeeMessage.StateChange(EmployeeCommand.PurchaseIntent intent) ->
               intent.Data.AuthorizationType.IsBypassAuth
            | EmployeeMessage.StateChange _ -> true
            | _ -> false
         | _ -> false
   }
