[<RequireQualifiedAccess>]
module AccountActor

open System
open Akka.Actor
open Akka.Persistence
open Akka.Persistence.Extras
open Akkling
open Akkling.Persistence
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Types
open Lib.Saga
open ActorUtil
open Bank.Account.Domain
open Bank.Transfer.Domain
open BillingStatement
open AutomaticTransfer
open SignalRBroadcast
open Email
open PurchaseSaga
open DomesticTransferSaga
open PlatformTransferSaga
open PlatformPaymentSaga

type private InternalTransferMsg =
   InternalTransferRecipientActor.InternalTransferMessage

// Pass monthly billing statement to BillingStatementActor.
// Conditionally apply monthly maintenance fee.
// Email account owner to notify of billing statement availability.
let private billingCycle
   (getBillingStatementActor: ActorSystem -> IActorRef<BillingStatementMessage>)
   (getEmailActor: ActorSystem -> IActorRef<EmailMessage>)
   (mailbox: Eventsourced<obj>)
   (state: AccountSnapshot)
   (evt: BankEvent<BillingCycleStarted>)
   =
   let account = state.Info

   let billingPeriod = {
      Month = evt.Data.Month
      Year = evt.Data.Year
   }

   let billing =
      BillingStatement.billingStatement state billingPeriod
      <| mailbox.LastSequenceNr()

   getBillingStatementActor mailbox.System <! RegisterBillingStatement billing

   let criteria = account.MaintenanceFeeCriteria

   if criteria.CanSkipFee then
      let msg =
         SkipMaintenanceFeeCommand.create account.CompositeId {
            Reason = criteria
         }
         |> AccountCommand.SkipMaintenanceFee
         |> AccountMessage.StateChange

      mailbox.Parent() <! msg
   else
      let msg =
         MaintenanceFeeCommand.create account.CompositeId
         |> AccountCommand.MaintenanceFee
         |> AccountMessage.StateChange

      mailbox.Parent() <! msg

   let msg =
      EmailMessage.create
         account.OrgId
         evt.CorrelationId
         (EmailInfo.BillingStatement account.FullName)

   getEmailActor mailbox.System <! msg

// Account events with an in/out money flow can produce an
// automatic transfer.  Automated transfer account events have
// money flow but they can not generate an auto transfer.
let private canProduceAutoTransfer =
   function
   | AccountEvent.InternalAutomatedTransferPending _
   | AccountEvent.InternalAutomatedTransferCompleted _
   | AccountEvent.InternalAutomatedTransferFailed _
   | AccountEvent.InternalAutomatedTransferDeposited _ -> false
   | e ->
      let _, flow, _ = AccountEvent.moneyTransaction e
      flow.IsSome

let private onValidationError
   (broadcaster: SignalRBroadcast)
   mailbox
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
   (account: Account)
   (cmd: AccountCommand)
   (err: Err)
   =
   logWarning
      mailbox
      $"Validation fail %s{string err} for command %s{cmd.GetType().Name}"

   let isNoop, fail =
      match err with
      | AccountStateTransitionError e ->
         match e with
         | TransferProgressNoChange
         | TransferAlreadyProgressedToCompletedOrFailed
         | AccountNotReadyToActivate -> true, None
         | AccountNotActive ->
            match cmd with
            | AccountCommand.Debit cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  cmd.OrgId,
                  PurchaseAccountFailReason.AccountNotActive account.FullName
                  |> PurchaseSagaEvent.PurchaseRejectedByAccount
                  |> AppSaga.Event.Purchase
               )
            | AccountCommand.DomesticTransfer cmd when
               cmd.Data.OriginatedFromSchedule
               ->
               false,
               Some(
                  cmd.CorrelationId,
                  cmd.OrgId,
                  DomesticTransferFailReason.SenderAccountNotActive
                  |> DomesticTransferSagaEvent.SenderAccountUnableToDeductFunds
                  |> AppSaga.Event.DomesticTransfer
               )
            | AccountCommand.InternalTransferBetweenOrgs cmd when
               cmd.Data.OriginatedFromSchedule
               ->
               false,
               Some(
                  cmd.CorrelationId,
                  cmd.OrgId,
                  InternalTransferFailReason.AccountClosed
                  |> PlatformTransferSagaEvent.SenderAccountUnableToDeductFunds
                  |> AppSaga.Event.PlatformTransfer
               )
            | AccountCommand.DepositTransferBetweenOrgs cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  cmd.OrgId,
                  InternalTransferFailReason.AccountClosed
                  |> PlatformTransferSagaEvent.RecipientAccountUnableToDepositFunds
                  |> AppSaga.Event.PlatformTransfer
               )
            | AccountCommand.FulfillPlatformPayment cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  cmd.OrgId,
                  PlatformPaymentFailReason.AccountClosed
                  |> PlatformPaymentSagaEvent.PayerAccountUnableToDeductFunds
                  |> AppSaga.Event.PlatformPayment
               )
            | AccountCommand.DepositPlatformPayment cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  cmd.OrgId,
                  (PlatformPaymentFailReason.AccountClosed,
                   cmd.Data.PaymentMethod)
                  |> PlatformPaymentSagaEvent.PayeeAccountUnableToDepositFunds
                  |> AppSaga.Event.PlatformPayment
               )
            | _ -> false, None
         | InsufficientBalance _ ->
            match cmd with
            | AccountCommand.Debit cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  cmd.OrgId,
                  PurchaseAccountFailReason.InsufficientAccountFunds(
                     account.Balance,
                     account.FullName
                  )
                  |> PurchaseSagaEvent.PurchaseRejectedByAccount
                  |> AppSaga.Event.Purchase
               )
            | AccountCommand.DomesticTransfer cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  cmd.OrgId,
                  DomesticTransferFailReason.SenderAccountInsufficientFunds
                  |> DomesticTransferSagaEvent.SenderAccountUnableToDeductFunds
                  |> AppSaga.Event.DomesticTransfer
               )
            | _ -> false, None
         | _ -> false, None
      | _ -> false, None

   if isNoop then
      logDebug mailbox $"AccountTransferActor NOOP msg {err}"
   else
      broadcaster.accountEventError
         account.OrgId
         account.AccountId
         cmd.Envelope.CorrelationId
         err

      match fail with
      | Some(correlationId, orgId, evt) ->
         let msg = SagaEvent.create orgId correlationId evt |> SagaMessage.Event

         getSagaRef correlationId <! msg
      | None -> ()

let onPersisted
   (getOrStartInternalTransferRef: Actor<_> -> IActorRef<InternalTransferMsg>)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (getAccountClosureRef: ActorSystem -> IActorRef<AccountClosureMessage>)
   (getBillingStatementRef: ActorSystem -> IActorRef<BillingStatementMessage>)
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
   (mailbox: Eventsourced<obj>)
   (state: AccountSnapshot)
   (evt: AccountEvent)
   =
   let account = state.Info

   let onPlatformPaymentEvent evt corrId orgId =
      let msg =
         AppSaga.Event.PlatformPayment evt
         |> SagaEvent.create orgId corrId
         |> SagaMessage.Event

      getSagaRef corrId <! msg

   let onPlatformTransferEvent evt corrId orgId =
      let msg =
         AppSaga.Event.PlatformTransfer evt
         |> SagaEvent.create orgId corrId
         |> SagaMessage.Event

      getSagaRef corrId <! msg

   let onDomesticTransferEvent evt corrId orgId =
      let msg =
         AppSaga.Event.DomesticTransfer evt
         |> SagaEvent.create orgId corrId
         |> SagaMessage.Event

      getSagaRef corrId <! msg

   match evt with
   | CreatedAccount evt ->
      let msg =
         EmailMessage.create
            account.OrgId
            evt.CorrelationId
            (EmailInfo.AccountOpen account.FullName)

      getEmailRef mailbox.System <! msg
   | AccountEvent.AccountClosed _ ->
      getAccountClosureRef mailbox.System
      <! AccountClosureMessage.Register account
   | DebitedAccount e ->
      let msg =
         PurchaseSagaEvent.PurchaseConfirmedByAccount
         |> AppSaga.Event.Purchase
         |> SagaEvent.create e.OrgId e.CorrelationId
         |> SagaMessage.Event

      getSagaRef e.CorrelationId <! msg
   | RefundedDebit e ->
      let msg =
         PurchaseSagaEvent.PurchaseRefundedToAccount
         |> AppSaga.Event.Purchase
         |> SagaEvent.create e.OrgId e.CorrelationId
         |> SagaMessage.Event

      getSagaRef e.CorrelationId <! msg
   | InternalTransferWithinOrgPending e ->
      getOrStartInternalTransferRef mailbox
      <! InternalTransferMsg.TransferRequestWithinOrg e
   | InternalAutomatedTransferPending e ->
      getOrStartInternalTransferRef mailbox
      <! InternalTransferMsg.AutomatedTransferRequest e
   | BillingCycleStarted e ->
      billingCycle getBillingStatementRef getEmailRef mailbox state e
   | PlatformPaymentRequested e ->
      onPlatformPaymentEvent
         (PlatformPaymentSagaStartEvent.PaymentRequested e
          |> PlatformPaymentSagaEvent.Start)
         e.CorrelationId
         e.OrgId
   | PlatformPaymentPaid e ->
      onPlatformPaymentEvent
         (PlatformPaymentSagaEvent.PayerAccountDeductedFunds e)
         e.CorrelationId
         e.Data.BaseInfo.Payee.OrgId
   | PlatformPaymentDeposited e ->
      onPlatformPaymentEvent
         PlatformPaymentSagaEvent.PayeeAccountDepositedFunds
         e.CorrelationId
         e.OrgId
   | PlatformPaymentDeclined e ->
      onPlatformPaymentEvent
         PlatformPaymentSagaEvent.PaymentRequestDeclined
         e.CorrelationId
         e.Data.BaseInfo.Payee.OrgId
   | PlatformPaymentCancelled e ->
      onPlatformPaymentEvent
         PlatformPaymentSagaEvent.PaymentRequestCancelled
         e.CorrelationId
         e.OrgId
   | PlatformPaymentRefunded e ->
      onPlatformPaymentEvent
         PlatformPaymentSagaEvent.PayerAccountRefunded
         e.CorrelationId
         e.OrgId
   (*
   | ThirdPartyPaymentRequested e ->
      // TODO: Send email requesting payment
   *)
   | InternalTransferBetweenOrgsPending e ->
      if e.Data.FromSchedule then
         onPlatformTransferEvent
            PlatformTransferSagaEvent.SenderAccountDeductedFunds
            e.CorrelationId
            e.OrgId
      else
         let evt =
            e
            |> PlatformTransferSagaStartEvent.SenderAccountDeductedFunds
            |> PlatformTransferSagaEvent.Start

         onPlatformTransferEvent evt e.CorrelationId e.OrgId
   | InternalTransferBetweenOrgsScheduled e ->
      let evt =
         PlatformTransferSagaStartEvent.ScheduleTransferRequest e
         |> PlatformTransferSagaEvent.Start

      onPlatformTransferEvent evt e.CorrelationId e.OrgId
   | InternalTransferBetweenOrgsDeposited e ->
      onPlatformTransferEvent
         PlatformTransferSagaEvent.RecipientAccountDepositedFunds
         e.CorrelationId
         e.OrgId
   | InternalTransferBetweenOrgsCompleted e ->
      onPlatformTransferEvent
         PlatformTransferSagaEvent.TransferMarkedAsSettled
         e.CorrelationId
         e.OrgId
   | DomesticTransferScheduled e ->
      let evt =
         DomesticTransferSagaStartEvent.ScheduleTransferRequest e
         |> DomesticTransferSagaEvent.Start

      onDomesticTransferEvent evt e.CorrelationId e.OrgId
   | DomesticTransferPending e ->
      if e.Data.FromSchedule then
         onDomesticTransferEvent
            DomesticTransferSagaEvent.SenderAccountDeductedFunds
            e.CorrelationId
            e.OrgId
      else
         let evt =
            DomesticTransferSagaStartEvent.SenderAccountDeductedFunds e
            |> DomesticTransferSagaEvent.Start

         onDomesticTransferEvent evt e.CorrelationId e.OrgId
   | DomesticTransferFailed e ->
      onDomesticTransferEvent
         DomesticTransferSagaEvent.SenderAccountRefunded
         e.CorrelationId
         e.OrgId
   | DomesticTransferCompleted e ->
      onDomesticTransferEvent
         DomesticTransferSagaEvent.TransferMarkedAsSettled
         e.CorrelationId
         e.OrgId
   | _ -> ()

   if
      canProduceAutoTransfer evt
      && not account.AutoTransfersPerTransaction.IsEmpty
   then
      mailbox.Self
      <! AccountMessage.AutoTransferCompute Frequency.PerTransaction

let actorProps
   (broadcaster: SignalRBroadcast)
   (getOrStartInternalTransferRef: Actor<_> -> IActorRef<InternalTransferMsg>)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (getAccountClosureRef: ActorSystem -> IActorRef<AccountClosureMessage>)
   (getBillingStatementRef: ActorSystem -> IActorRef<BillingStatementMessage>)
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   =
   let handler (mailbox: Eventsourced<obj>) =
      let logError = logError mailbox

      let rec loop (stateOpt: AccountSnapshot option) = actor {
         let! msg = mailbox.Receive()

         let state = stateOpt |> Option.defaultValue AccountSnapshot.empty

         let account = state.Info

         let onValidationError =
            onValidationError broadcaster mailbox getSagaRef account

         match msg with
         | Persisted mailbox e ->
            let (AccountMessage.Event evt) = unbox e
            let state = Account.applyEvent state evt
            let account = state.Info

            broadcaster.accountEventPersisted evt account

            onPersisted
               getOrStartInternalTransferRef
               getEmailRef
               getAccountClosureRef
               getBillingStatementRef
               getSagaRef
               mailbox
               state
               evt

            return! loop <| Some state
         | :? SnapshotOffer as o -> return! loop <| Some(unbox o.Snapshot)
         | :? ConfirmableMessageEnvelope as envelope ->
            let unknownMsg msg =
               logError $"Unknown message in ConfirmableMessageEnvelope - {msg}"
               unhandled ()

            match envelope.Message with
            | :? AccountMessage as msg ->
               match msg with
               | AccountMessage.StateChange cmd ->
                  let validation = Account.stateTransition state cmd

                  match validation with
                  | Ok(evt, _) ->
                     return!
                        confirmPersist
                           mailbox
                           (AccountMessage.Event evt)
                           envelope.ConfirmationId
                  | Error err -> onValidationError cmd err
               | msg -> return unknownMsg msg
            | msg -> return unknownMsg msg
         | :? AccountMessage as msg ->
            match msg with
            | AccountMessage.GetAccount ->
               mailbox.Sender() <! (stateOpt |> Option.map _.Info)
            | AccountMessage.Delete ->
               let state =
                  Some {
                     state with
                        Info.Status = AccountStatus.ReadyForDelete
                  }

               return! loop state <@> DeleteMessages Int64.MaxValue
            | AccountMessage.AutoTransferCompute frequency ->
               let transfers =
                  match frequency with
                  | Frequency.PerTransaction ->
                     account.AutoTransfersPerTransaction
                  | Frequency.Schedule CronSchedule.Daily ->
                     account.AutoTransfersDaily
                  | Frequency.Schedule CronSchedule.TwiceMonthly ->
                     account.AutoTransfersTwiceMonthly

               let transfersOut, transfersIn =
                  transfers
                  |> List.partition (fun t ->
                     t.Transfer.Sender.AccountId = account.AccountId)

               // NOTE: Transfers-in
               // Computed transfers which are generated from a
               // TargetBalanceRule.  When the target balance is lower
               // than desired, the ManagingPartnerAccount is designated
               // as the sender in order to restore funds to the target.
               for t in transfersIn do
                  let msg =
                     InternalAutoTransferCommand.create t
                     |> AccountCommand.InternalAutoTransfer
                     |> AccountMessage.StateChange

                  (getAccountRef t.Transfer.Sender.AccountId) <! msg

               // NOTE: Transfers-out
               // Outgoing auto transfers are computed, applied against the
               // aggregate state, and persisted in one go.
               //
               // If instead they were computed and then sent as individual
               // StateChange InternalAutoTransfer messages then you would
               // run the risk of other StateChange messages being processed
               // before the computed InternalAutoTransfer message leading to
               // InsufficientBalance validation errors in busy workloads.
               match transfersOut with
               | [] -> return ignored ()
               | transfers ->
                  let validations =
                     transfers
                     |> List.map (
                        InternalAutoTransferCommand.create
                        >> AccountCommand.InternalAutoTransfer
                     )
                     |> List.fold
                           (fun acc cmd ->
                              match acc with
                              | Ok(accountState, events) ->
                                 Account.stateTransition accountState cmd
                                 |> Result.map (fun (evt, newState) ->
                                    newState, evt :: events)
                                 |> Result.mapError (fun err -> cmd, err)
                              | Error err -> Error err)
                           (Ok(state, []))

                  match validations with
                  | Ok(_, evts) ->
                     let evts = List.map (AccountMessage.Event >> box) evts
                     return! PersistAll evts
                  | Error(cmd, err) -> onValidationError cmd err
         // Event replay on actor start
         | :? AccountEvent as e when mailbox.IsRecovering() ->
            return! loop <| Some(Account.applyEvent state e)
         | msg ->
            PersistentActorEventHandler.handleEvent
               {
                  PersistentActorEventHandler.init with
                     DeleteMessagesSuccess =
                        fun _ ->
                           if account.Status = AccountStatus.ReadyForDelete then
                              logDebug mailbox "<Passivate>"
                              passivate ()
                           else
                              ignored ()
                     LifecyclePostStop =
                        fun _ ->
                           logInfo
                              mailbox
                              $"ACCOUNT POSTSTOP {account.FullName} {account.AccountId}"

                           ignored ()
               }
               mailbox
               msg
      }

      loop None

   propsPersist handler

let get (sys: ActorSystem) (accountId: AccountId) : IEntityRef<AccountMessage> =
   getEntityRef sys ClusterMetadata.accountShardRegion (AccountId.get accountId)

let isPersistableMessage (msg: obj) =
   match msg with
   | :? AccountMessage as msg ->
      match msg with
      | AccountMessage.StateChange _ -> true
      | _ -> false
   | _ -> false

let initProps
   (broadcaster: SignalRBroadcast)
   (system: ActorSystem)
   (supervisorOpts: PersistenceSupervisorOptions)
   (persistenceId: string)
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
   (getEmailActor: ActorSystem -> IActorRef<EmailMessage>)
   (getAccountClosureActor: ActorSystem -> IActorRef<AccountClosureMessage>)
   (getBillingStatementActor: ActorSystem -> IActorRef<BillingStatementMessage>)
   =
   let getOrStartInternalTransferActor mailbox =
      InternalTransferRecipientActor.getOrStart mailbox (get system)

   let childProps =
      actorProps
         broadcaster
         getOrStartInternalTransferActor
         getEmailActor
         getAccountClosureActor
         getBillingStatementActor
         getSagaRef
         (get system)

   persistenceSupervisor
      supervisorOpts
      isPersistableMessage
      childProps
      persistenceId
