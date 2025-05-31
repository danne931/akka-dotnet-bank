[<RequireQualifiedAccess>]
module AccountActor

open Akka.Actor
open Akka.Persistence
open Akka.Persistence.Extras
open Akkling
open Akkling.Persistence
open Akkling.Cluster.Sharding
open FsToolkit.ErrorHandling
open System.Text.Json
open System.Threading.Tasks

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
open OrgOnboardingSaga
open PurchaseSaga
open DomesticTransferSaga
open PlatformTransferSaga
open PlatformPaymentSaga

// Child actor handles retrying domestic transfers which failed
// due to the mock third party transfer service regarding the
// Recipient of the Transfer as having invalid account info.
module private RetryDomesticTransfers =
   let private actorProps
      (getRetryableTransfers:
         AccountId -> Task<Result<DomesticTransfer list option, Err>>)
      : Props<AccountId>
      =
      let handler (ctx: Actor<_>) (recipientId: AccountId) = actor {
         let! res = getRetryableTransfers recipientId

         match res with
         | Error err ->
            logError ctx $"Error getting retryable transfers {err}"
            return unhandled ()
         | Ok None -> return ignored ()
         | Ok(Some transfers) ->
            for (transfer: DomesticTransfer) in transfers do
               let msg =
                  DomesticTransferCommand.create
                     (transfer.TransferId |> TransferId.get |> CorrelationId)
                     transfer.InitiatedBy
                     {
                        Amount = transfer.Amount
                        Sender = transfer.Sender
                        Recipient = transfer.Recipient
                        Memo = transfer.Memo
                        ScheduledDateSeedOverride = None
                        OriginatedFromSchedule = false
                     }
                  |> AccountCommand.DomesticTransfer
                  |> AccountMessage.StateChange

               ctx.Parent() <! msg

            return ignored ()
      }

      props (actorOf2 handler)

   let getOrStart
      (getRetryableTransfers:
         AccountId -> Task<Result<DomesticTransfer list option, Err>>)
      (mailbox: Actor<_>)
      =
      let name = "retry-domestic-transfers"

      getChildActorRef<_, AccountId> mailbox name
      |> Option.defaultWith (fun _ ->
         spawn mailbox name <| actorProps getRetryableTransfers)

// Pass monthly billing statement to BillingStatementActor.
// Conditionally apply monthly maintenance fee.
// Email account owner to notify of billing statement availability.

let private billingCycle
   (getBillingStatementActor: ActorSystem -> IActorRef<BillingStatementMessage>)
   (getEmailActor: ActorSystem -> IActorRef<EmailMessage>)
   (mailbox: Eventsourced<obj>)
   (state: ParentAccountSnapshot)
   (evt: BankEvent<BillingCycleStarted>)
   =
   let billingPeriod = {
      Month = evt.Data.Month
      Year = evt.Data.Year
   }

   let billing: BillingPersistable = {
      ParentAccountId = state.ParentAccountId
      ParentAccountSnapshot =
         JsonSerializer.SerializeToUtf8Bytes(state, Serialization.jsonOptions)
      LastPersistedEventSequenceNumber = mailbox.LastSequenceNr()
      Statements =
         state.VirtualAccounts.Values
         |> Seq.filter (fun a -> a.Status = AccountStatus.Active)
         |> Seq.map (fun account ->
            BillingStatement.billingStatement
               account
               (state.eventsForAccount account.AccountId)
               billingPeriod)
         |> Seq.toList
   }

   getBillingStatementActor mailbox.System <! RegisterBillingStatement billing

   let criteria = state.MaintenanceFeeCriteria
   let compositeId = state.PrimaryVirtualAccountCompositeId

   if criteria.CanSkipFee then
      let msg =
         SkipMaintenanceFeeCommand.create compositeId criteria
         |> AccountCommand.SkipMaintenanceFee
         |> AccountMessage.StateChange

      mailbox.Parent() <! msg
   else
      let msg =
         MaintenanceFeeCommand.create compositeId
         |> AccountCommand.MaintenanceFee
         |> AccountMessage.StateChange

      mailbox.Parent() <! msg

   let msg =
      EmailMessage.create
         state.OrgId
         evt.CorrelationId
         EmailInfo.BillingStatement

   getEmailActor mailbox.System <! msg

// Account events with an in/out money flow can produce an
// automatic transfer.  Automated transfer account events have
// money flow but they can not generate an auto transfer.
let private canProduceAutoTransfer =
   function
   | AccountEvent.InternalAutomatedTransferPending _
   | AccountEvent.InternalAutomatedTransferFailed _
   | AccountEvent.InternalAutomatedTransferDeposited _ -> false
   | e ->
      let _, flow, _ = AccountEvent.moneyTransaction e
      flow.IsSome

let private onValidationError
   (broadcaster: SignalRBroadcast)
   mailbox
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
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
         | AccountNotReadyToActivate -> true, None
         | AccountNotActive accountName ->
            match cmd with
            | AccountCommand.Debit cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  cmd.OrgId,
                  PurchaseAccountFailReason.AccountNotActive accountName
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
         | InsufficientBalance(balance, accountName) ->
            match cmd with
            | AccountCommand.Debit cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  cmd.OrgId,
                  PurchaseAccountFailReason.InsufficientAccountFunds(
                     balance,
                     accountName
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
         cmd.Envelope.OrgId
         cmd.AccountId
         cmd.Envelope.CorrelationId
         err

      match fail with
      | Some(corrId, orgId, evt) ->
         let msg = AppSaga.sagaMessage orgId corrId evt
         getSagaRef corrId <! msg
      | None -> ()

let onPersisted
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (getAccountClosureRef: ActorSystem -> IActorRef<AccountClosureMessage>)
   (getBillingStatementRef: ActorSystem -> IActorRef<BillingStatementMessage>)
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
   (getRetryableTransfers:
      AccountId -> Task<Result<DomesticTransfer list option, Err>>)
   (mailbox: Eventsourced<obj>)
   (state: ParentAccountSnapshot)
   (evt: AccountEvent)
   =
   let onPlatformPaymentEvent evt corrId orgId =
      let msg =
         AppSaga.Event.PlatformPayment evt |> AppSaga.sagaMessage orgId corrId

      getSagaRef corrId <! msg

   let onPlatformTransferEvent evt corrId orgId =
      let msg =
         AppSaga.Event.PlatformTransfer evt |> AppSaga.sagaMessage orgId corrId

      getSagaRef corrId <! msg

   let onDomesticTransferEvent evt corrId orgId =
      let msg =
         AppSaga.Event.DomesticTransfer evt |> AppSaga.sagaMessage orgId corrId

      getSagaRef corrId <! msg

   match evt with
   | AccountEvent.InitializedPrimaryCheckingAccount e ->
      let msg =
         OrgOnboardingSagaEvent.InitializedPrimaryVirtualAccount
         |> AppSaga.Event.OrgOnboarding
         |> AppSaga.sagaMessage e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.CreatedAccount e ->
      let msg =
         EmailMessage.create
            state.OrgId
            e.CorrelationId
            (EmailInfo.AccountOpen e.Data.Name)

      getEmailRef mailbox.System <! msg
   | AccountEvent.AccountClosed _ ->
      (*
      getAccountClosureRef mailbox.System
      <! AccountClosureMessage.Register account
      *)
      ()
   | AccountEvent.DebitedAccount e ->
      let msg =
         PurchaseSagaEvent.PurchaseConfirmedByAccount
         |> AppSaga.Event.Purchase
         |> AppSaga.sagaMessage e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.RefundedDebit e ->
      let msg =
         PurchaseSagaEvent.PurchaseRefundedToAccount
         |> AppSaga.Event.Purchase
         |> AppSaga.sagaMessage e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.ParentAccount(ParentAccountEvent.BillingCycleStarted e) ->
      billingCycle getBillingStatementRef getEmailRef mailbox state e
   | AccountEvent.PlatformPaymentRequested e ->
      onPlatformPaymentEvent
         (PlatformPaymentSagaStartEvent.PaymentRequested e
          |> PlatformPaymentSagaEvent.Start)
         e.CorrelationId
         e.OrgId
   | AccountEvent.PlatformPaymentPaid e ->
      onPlatformPaymentEvent
         (PlatformPaymentSagaEvent.PayerAccountDeductedFunds e)
         e.CorrelationId
         e.Data.BaseInfo.Payee.OrgId
   | AccountEvent.PlatformPaymentDeposited e ->
      onPlatformPaymentEvent
         PlatformPaymentSagaEvent.PayeeAccountDepositedFunds
         e.CorrelationId
         e.OrgId
   | AccountEvent.PlatformPaymentDeclined e ->
      onPlatformPaymentEvent
         PlatformPaymentSagaEvent.PaymentRequestDeclined
         e.CorrelationId
         e.Data.BaseInfo.Payee.OrgId
   | AccountEvent.PlatformPaymentCancelled e ->
      onPlatformPaymentEvent
         PlatformPaymentSagaEvent.PaymentRequestCancelled
         e.CorrelationId
         e.OrgId
   | AccountEvent.PlatformPaymentRefunded e ->
      onPlatformPaymentEvent
         PlatformPaymentSagaEvent.PayerAccountRefunded
         e.CorrelationId
         e.OrgId
   (*
   | AccountEvent.ThirdPartyPaymentRequested e ->
      // TODO: Send email requesting payment
   *)
   | AccountEvent.InternalTransferBetweenOrgsPending e ->
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
   | AccountEvent.InternalTransferBetweenOrgsScheduled e ->
      let evt =
         PlatformTransferSagaStartEvent.ScheduleTransferRequest e
         |> PlatformTransferSagaEvent.Start

      onPlatformTransferEvent evt e.CorrelationId e.OrgId
   | AccountEvent.InternalTransferBetweenOrgsDeposited e ->
      onPlatformTransferEvent
         PlatformTransferSagaEvent.RecipientAccountDepositedFunds
         e.CorrelationId
         e.OrgId
   | AccountEvent.DomesticTransferScheduled e ->
      let evt =
         DomesticTransferSagaStartEvent.ScheduleTransferRequest e
         |> DomesticTransferSagaEvent.Start

      onDomesticTransferEvent evt e.CorrelationId e.OrgId
   | AccountEvent.DomesticTransferPending e ->
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
   | AccountEvent.DomesticTransferFailed e ->
      onDomesticTransferEvent
         DomesticTransferSagaEvent.SenderAccountRefunded
         e.CorrelationId
         e.OrgId
   | AccountEvent.DomesticTransferCompleted e ->
      onDomesticTransferEvent
         DomesticTransferSagaEvent.TransferMarkedAsSettled
         e.CorrelationId
         e.OrgId
   | AccountEvent.ParentAccount(ParentAccountEvent.EditedDomesticTransferRecipient e) ->
      let aref = RetryDomesticTransfers.getOrStart getRetryableTransfers mailbox

      aref <! e.Data.Recipient.RecipientAccountId
   | _ -> ()

let persistInternalTransferWithinOrgEvent
   (logError: string -> unit)
   (confirmPersist: AccountMessage -> PersistentEffect<obj>)
   (transfer: BankEvent<InternalTransferWithinOrgPending>)
   (state: ParentAccountSnapshot)
   : Effect<obj>
   =
   let transferPendingEvt =
      AccountEvent.InternalTransferWithinOrgPending transfer

   let correspondingTransferDeposit =
      DepositInternalTransferWithinOrgCommand.fromPending transfer
      |> AccountCommand.DepositTransferWithinOrg
      |> ParentAccount.stateTransition state

   match correspondingTransferDeposit with
   | Error err ->
      logError
         $"Not able to deposit transfer into recipient account.
         Will not deduct transfer from sender account. {err}"

      ignored ()
   | Ok(transferDepositEvt, state) ->
      let autoTransferStateTransitions =
         ParentAccount.computeAutoTransferStateTransitions
            Frequency.PerTransaction
            transfer.Data.BaseInfo.Sender.AccountId
            state
         |> Option.sequenceResult
         |> ResultOption.bind (fun (state, evts) ->
            ParentAccount.computeAutoTransferStateTransitions
               Frequency.PerTransaction
               transfer.Data.BaseInfo.Recipient.AccountId
               state
            |> Option.sequenceResult
            |> ResultOption.map (fun (state, evts2) -> state, evts @ evts2))

      let toConfirm = AccountMessage.Event transferPendingEvt

      match autoTransferStateTransitions with
      | Ok None ->
         confirmPersist toConfirm
         <@> Persist(AccountMessage.Event transferDepositEvt)
      | Error(_, err) ->
         logError
            $"Will not persist auto transfers due to state transition error: {err}"

         confirmPersist toConfirm
         <@> Persist(AccountMessage.Event transferDepositEvt)
      | Ok(Some(_, autoTransferEvts)) ->
         let evts =
            List.map
               (AccountMessage.Event >> box)
               (transferDepositEvt :: autoTransferEvts)

         confirmPersist toConfirm <@> PersistAll evts

let persistEvent
   (logError: string -> unit)
   (confirmPersist: AccountMessage -> PersistentEffect<obj>)
   (evt: AccountEvent)
   (state: ParentAccountSnapshot)
   : Effect<obj>
   =
   let autoTransferStateTransitions =
      if canProduceAutoTransfer evt then
         ParentAccount.computeAutoTransferStateTransitions
            Frequency.PerTransaction
            evt.AccountId
            state
      else
         None

   let toConfirm = AccountMessage.Event evt

   match autoTransferStateTransitions with
   | None -> confirmPersist toConfirm
   | Some(Error(_, err)) ->
      logError
         $"Will not persist auto transfers due to state transition error: {err}"

      confirmPersist toConfirm
   | Some(Ok(_, autoTransferEvts)) ->
      let autoTransferEvts =
         List.map (AccountMessage.Event >> box) autoTransferEvts

      confirmPersist toConfirm <@> PersistAll autoTransferEvts

let actorProps
   (broadcaster: SignalRBroadcast)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (getAccountClosureRef: ActorSystem -> IActorRef<AccountClosureMessage>)
   (getBillingStatementRef: ActorSystem -> IActorRef<BillingStatementMessage>)
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
   (getDomesticTransfersRetryableUponRecipientEdit:
      AccountId -> Task<Result<DomesticTransfer list option, Err>>)
   =
   let handler (mailbox: Eventsourced<obj>) =
      let logError = logError mailbox

      let rec loop (stateOpt: ParentAccountSnapshot option) = actor {
         let! msg = mailbox.Receive()

         let state = stateOpt |> Option.defaultValue ParentAccountSnapshot.empty

         let onValidationError =
            onValidationError broadcaster mailbox getSagaRef

         match msg with
         | Persisted mailbox e ->
            let (AccountMessage.Event evt) = unbox e

            let state = ParentAccount.applyEvent state evt

            match evt with
            | AccountEvent.ParentAccount evt ->
               broadcaster.parentAccountEventPersisted evt
            | _ ->
               state.VirtualAccounts.TryFind evt.AccountId
               |> Option.iter (broadcaster.accountEventPersisted evt)

            onPersisted
               getEmailRef
               getAccountClosureRef
               getBillingStatementRef
               getSagaRef
               getDomesticTransfersRetryableUponRecipientEdit
               mailbox
               state
               evt

            return! loop <| Some state
         | :? SnapshotOffer as o -> return! loop <| Some(unbox o.Snapshot)
         | :? ConfirmableMessageEnvelope as envelope ->
            let unknownMsg msg =
               logError $"Unknown message in ConfirmableMessageEnvelope - {msg}"
               unhandled ()

            let confirmPersist = confirmPersist mailbox envelope.ConfirmationId

            match envelope.Message with
            | :? AccountMessage as msg ->
               match msg with
               | AccountMessage.StateChange cmd ->
                  let validation = ParentAccount.stateTransition state cmd

                  match validation with
                  | Error err -> onValidationError cmd err
                  | Ok(InternalTransferWithinOrgPending transfer, state) ->
                     return!
                        persistInternalTransferWithinOrgEvent
                           logError
                           confirmPersist
                           transfer
                           state
                  | Ok(evt, state) ->
                     return! persistEvent logError confirmPersist evt state
               | msg -> return unknownMsg msg
            | msg -> return unknownMsg msg
         | :? AccountMessage as msg ->
            match msg with
            | AccountMessage.GetAccount -> mailbox.Sender() <! stateOpt
            | AccountMessage.GetVirtualAccount accountId ->
               let account =
                  stateOpt |> Option.bind (_.VirtualAccounts.TryFind(accountId))

               mailbox.Sender() <! account
            | AccountMessage.AutoTransferCompute(frequency, accountId) ->
               let autoTransfers =
                  ParentAccount.computeAutoTransferStateTransitions
                     frequency
                     accountId
                     state

               match autoTransfers with
               | None ->
                  logDebug
                     mailbox
                     $"Received AutoTransferCompute {frequency} {accountId} but nothing computed."
               | Some(Error(_, err)) ->
                  logError
                     $"Will not persist auto transfers due to state transition error: {err}"
               | Some(Ok(_, autoTransferEvts)) ->
                  let evts =
                     List.map (AccountMessage.Event >> box) autoTransferEvts

                  return! PersistAll evts
            | _ -> unhandled ()
         (*
            | AccountMessage.Delete ->
               let state =
                  Some {
                     state with
                        Info.Status = AccountStatus.ReadyForDelete
                  }

               return! loop state <@> DeleteMessages Int64.MaxValue
            *)
         // Event replay on actor start
         | :? AccountEvent as e when mailbox.IsRecovering() ->
            return! loop <| Some(ParentAccount.applyEvent state e)
         | msg ->
            PersistentActorEventHandler.handleEvent
               {
                  PersistentActorEventHandler.init with
                     (*
                     DeleteMessagesSuccess =
                        fun _ ->
                           if account.Status = AccountStatus.ReadyForDelete then
                              logDebug mailbox "<Passivate>"
                              passivate ()
                           else
                              ignored ()
                     *)
                     LifecyclePostStop =
                        fun _ ->
                           logInfo mailbox $"ACCOUNT POSTSTOP {mailbox.Pid}"

                           ignored ()
               }
               mailbox
               msg
      }

      loop None

   propsPersist handler

let get
   (sys: ActorSystem)
   (parentAccountId: ParentAccountId)
   : IEntityRef<AccountMessage>
   =
   getEntityRef
      sys
      ClusterMetadata.accountShardRegion
      (ParentAccountId.get parentAccountId)

let isPersistableMessage (msg: obj) =
   match msg with
   | :? AccountMessage as msg ->
      match msg with
      | AccountMessage.StateChange _ -> true
      | _ -> false
   | _ -> false

let initProps
   (broadcaster: SignalRBroadcast)
   (supervisorOpts: PersistenceSupervisorOptions)
   (persistenceId: string)
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
   (getEmailActor: ActorSystem -> IActorRef<EmailMessage>)
   (getAccountClosureActor: ActorSystem -> IActorRef<AccountClosureMessage>)
   (getBillingStatementActor: ActorSystem -> IActorRef<BillingStatementMessage>)
   (getDomesticTransfersRetryableUponRecipientEdit:
      AccountId -> Task<Result<DomesticTransfer list option, Err>>)
   =
   let childProps =
      actorProps
         broadcaster
         getEmailActor
         getAccountClosureActor
         getBillingStatementActor
         getSagaRef
         getDomesticTransfersRetryableUponRecipientEdit

   persistenceSupervisor
      supervisorOpts
      isPersistableMessage
      childProps
      persistenceId
