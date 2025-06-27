[<RequireQualifiedAccess>]
module AccountActor

open Akka.Actor
open Akka.Persistence
open Akka.Persistence.Extras
open Akka.Delivery
open Akkling
open Akkling.Persistence
open Akkling.Cluster.Sharding
open FsToolkit.ErrorHandling
open System.Text.Json
open System.Threading.Tasks

open Lib.SharedTypes
open Lib.Types
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
open BillingSaga

// Account events with an in/out money flow can produce an
// automatic transfer.  Automated transfer account events have
// money flow but they can not generate an auto transfer.
let private canProduceAutoTransfer =
   function
   | AccountEvent.InternalAutomatedTransferDeducted _
   | AccountEvent.InternalAutomatedTransferDeposited _ -> false
   | e ->
      let _, flow, _ = AccountEvent.moneyTransaction e
      flow.IsSome

let private onValidationError
   (broadcaster: SignalRBroadcast)
   mailbox
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   (cmd: AccountCommand)
   (err: Err)
   =
   logWarning
      mailbox
      $"Validation fail %s{string err} for command %s{cmd.GetType().Name}"

   let orgId = cmd.Envelope.OrgId
   let corrId = cmd.Envelope.CorrelationId

   let isNoop, fail =
      match err with
      | AccountStateTransitionError e ->
         match e with
         | AccountNotReadyToActivate -> true, None
         | AccountNotActive accountName ->
            match cmd with
            | AccountCommand.Debit _ ->
               false,
               Some(
                  PurchaseAccountFailReason.AccountNotActive accountName
                  |> PurchaseSagaEvent.PurchaseRejectedByAccount
                  |> AppSaga.Message.purchase orgId corrId
               )
            | AccountCommand.DomesticTransfer cmd when
               cmd.Data.OriginatedFromSchedule
               ->
               false,
               Some(
                  DomesticTransferFailReason.SenderAccountNotActive
                  |> DomesticTransferSagaEvent.SenderUnableToReserveFunds
                  |> AppSaga.Message.domesticTransfer orgId corrId
               )
            | AccountCommand.InternalTransferBetweenOrgs cmd when
               cmd.Data.OriginatedFromSchedule
               ->
               false,
               Some(
                  InternalTransferFailReason.AccountClosed
                  |> PlatformTransferSagaEvent.SenderUnableToReserveFunds
                  |> AppSaga.Message.platformTransfer orgId corrId
               )
            | AccountCommand.DepositTransferBetweenOrgs _ ->
               false,
               Some(
                  InternalTransferFailReason.AccountClosed
                  |> PlatformTransferSagaEvent.RecipientUnableToDepositFunds
                  |> AppSaga.Message.platformTransfer orgId corrId
               )
            | AccountCommand.PlatformPayment _ ->
               false,
               Some(
                  PlatformPaymentFailReason.AccountClosed
                  |> PlatformPaymentSagaEvent.PayerAccountUnableToReserveFunds
                  |> AppSaga.Message.platformPayment orgId corrId
               )
            | AccountCommand.DepositPlatformPayment cmd ->
               false,
               Some(
                  (PlatformPaymentFailReason.AccountClosed,
                   cmd.Data.PaymentMethod)
                  |> PlatformPaymentSagaEvent.PayeeAccountUnableToDepositFunds
                  |> AppSaga.Message.platformPayment orgId corrId
               )
            | _ -> false, None
         | InsufficientBalance(balance, accountName) ->
            match cmd with
            | AccountCommand.Debit _ ->
               false,
               Some(
                  PurchaseAccountFailReason.InsufficientAccountFunds(
                     balance,
                     accountName
                  )
                  |> PurchaseSagaEvent.PurchaseRejectedByAccount
                  |> AppSaga.Message.purchase orgId corrId
               )
            | AccountCommand.DomesticTransfer _ ->
               false,
               Some(
                  DomesticTransferFailReason.SenderAccountInsufficientFunds
                  |> DomesticTransferSagaEvent.SenderUnableToReserveFunds
                  |> AppSaga.Message.domesticTransfer orgId corrId
               )
            | _ -> false, None
         | _ -> false, None
      | _ -> false, None

   if isNoop then
      logDebug mailbox $"AccountTransferActor NOOP msg {err}"
   else
      broadcaster.accountEventError orgId cmd.AccountId corrId err

      fail |> Option.iter (fun msg -> getSagaRef corrId <! msg)

let onPersisted
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (getAccountClosureRef: ActorSystem -> IActorRef<AccountClosureMessage>)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   (getSagaGuaranteedDeliveryRef:
      unit -> IActorRef<GuaranteedDelivery.Message<AppSaga.AppSagaMessage>>)
   (getRetryableTransfers:
      AccountId -> Task<Result<DomesticTransfer list option, Err>>)
   (mailbox: Eventsourced<obj>)
   (state: ParentAccountSnapshot)
   (evt: AccountEvent)
   =
   let partnerBankAccountLink: PartnerBank.Service.Domain.PartnerBankAccountLink = {
      AccountNumber = state.AccountNumber
      RoutingNumber = state.RoutingNumber
   }

   match evt with
   | AccountEvent.InitializedPrimaryCheckingAccount e ->
      let msg =
         OrgOnboardingSagaEvent.InitializedPrimaryVirtualAccount
         |> AppSaga.Message.orgOnboard e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.CreatedVirtualAccount e ->
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
   | AccountEvent.DebitPending e ->
      let msg =
         partnerBankAccountLink
         |> PurchaseSagaEvent.AccountReservedFunds
         |> AppSaga.Message.purchase e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.DebitSettled e ->
      let msg =
         PurchaseSagaEvent.PurchaseSettledWithAccount
         |> AppSaga.Message.purchase e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.DebitFailed e ->
      let msg =
         PurchaseSagaEvent.PurchaseFailureAcknowledgedByAccount
         |> AppSaga.Message.purchase e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.MaintenanceFeeDebited e ->
      let msg =
         BillingSagaEvent.MaintenanceFeeProcessed
         |> AppSaga.Message.billing e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.MaintenanceFeeSkipped e ->
      let msg =
         BillingSagaEvent.MaintenanceFeeProcessed
         |> AppSaga.Message.billing e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.PlatformPaymentRequested e ->
      let msg =
         PlatformPaymentSagaStartEvent.PaymentRequested e
         |> AppSaga.Message.platformPaymentStart e.OrgId e.CorrelationId

      getSagaGuaranteedDeliveryRef () <! msg
   | AccountEvent.PlatformPaymentPending e ->
      let msg =
         PlatformPaymentSagaEvent.PayerAccountReservedFunds(
            e,
            partnerBankAccountLink
         )
         |> AppSaga.Message.platformPayment e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.PlatformPaymentDeposited e ->
      let msg =
         partnerBankAccountLink
         |> PlatformPaymentSagaEvent.PayeeAccountDepositedFunds
         |> AppSaga.Message.platformPayment e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.PlatformPaymentDeclined e ->
      let msg =
         PlatformPaymentSagaEvent.PaymentRequestDeclined
         |> AppSaga.Message.platformPayment e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.PlatformPaymentCancelled e ->
      let msg =
         PlatformPaymentSagaEvent.PaymentRequestCancelled
         |> AppSaga.Message.platformPayment e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.PlatformPaymentSettled e ->
      let msg =
         PlatformPaymentSagaEvent.PaymentSettled
         |> AppSaga.Message.platformPayment e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.PlatformPaymentFailed e ->
      let msg =
         PlatformPaymentSagaEvent.PayerAccountReleasedReservedFunds
         |> AppSaga.Message.platformPayment e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   (*
   | AccountEvent.ThirdPartyPaymentRequested e ->
      // TODO: Send email requesting payment
   *)
   | AccountEvent.InternalTransferBetweenOrgsPending e ->
      if e.Data.FromSchedule then
         let msg =
            partnerBankAccountLink
            |> PlatformTransferSagaEvent.SenderReservedFunds
            |> AppSaga.Message.platformTransfer e.OrgId e.CorrelationId

         getSagaRef e.CorrelationId <! msg
      else
         let msg =
            PlatformTransferSagaStartEvent.SenderReservedFunds(
               e,
               partnerBankAccountLink
            )
            |> AppSaga.Message.platformTransferStart e.OrgId e.CorrelationId

         getSagaGuaranteedDeliveryRef () <! msg
   | AccountEvent.InternalTransferBetweenOrgsScheduled e ->
      let msg =
         PlatformTransferSagaStartEvent.ScheduleTransferRequest e
         |> AppSaga.Message.platformTransferStart e.OrgId e.CorrelationId

      getSagaGuaranteedDeliveryRef () <! msg
   | AccountEvent.InternalTransferBetweenOrgsDeposited e ->
      let msg =
         partnerBankAccountLink
         |> PlatformTransferSagaEvent.RecipientDepositedFunds
         |> AppSaga.Message.platformTransfer e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.InternalTransferBetweenOrgsSettled e ->
      let msg =
         PlatformTransferSagaEvent.TransferSettled
         |> AppSaga.Message.platformTransfer e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.DomesticTransferScheduled e ->
      let msg =
         DomesticTransferSagaStartEvent.ScheduleTransferRequest e
         |> AppSaga.Message.domesticTransferStart e.OrgId e.CorrelationId

      getSagaGuaranteedDeliveryRef () <! msg
   | AccountEvent.DomesticTransferPending e ->
      if e.Data.FromSchedule then
         let msg =
            DomesticTransferSagaEvent.SenderReservedFunds
            |> AppSaga.Message.domesticTransfer e.OrgId e.CorrelationId

         getSagaRef e.CorrelationId <! msg
      else
         let msg =
            DomesticTransferSagaStartEvent.SenderReservedFunds e
            |> AppSaga.Message.domesticTransferStart e.OrgId e.CorrelationId

         getSagaGuaranteedDeliveryRef () <! msg
   | AccountEvent.DomesticTransferFailed e ->
      let msg =
         DomesticTransferSagaEvent.SenderReleasedReservedFunds
         |> AppSaga.Message.domesticTransfer e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.DomesticTransferSettled e ->
      let msg =
         DomesticTransferSagaEvent.SenderDeductedFunds
         |> AppSaga.Message.domesticTransfer e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.ParentAccount(ParentAccountEvent.EditedDomesticTransferRecipient e) ->
      // Retries domestic transfers which failed due to the mock third party
      // transfer service regarding the Recipient of the Transfer as having
      // invalid account info.
      let retryable =
         getRetryableTransfers e.Data.Recipient.RecipientAccountId
         |> Async.AwaitTask
         |> Async.map AccountMessage.DomesticTransfersRetryableUponRecipientEdit

      retype mailbox.Self <!| retryable
   | _ -> ()

let persistInternalTransferWithinOrgEvent
   (logError: string -> unit)
   (confirmPersistAll: AccountEvent seq -> PersistentEffect<obj>)
   (transfer: BankEvent<InternalTransferWithinOrgDeducted>)
   (state: ParentAccountSnapshot)
   : Effect<obj>
   =
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

      let toConfirm = AccountEvent.InternalTransferWithinOrgDeducted transfer

      match autoTransferStateTransitions with
      | Ok None -> confirmPersistAll [ toConfirm; transferDepositEvt ]
      | Error(_, err) ->
         logError
            $"Will not persist auto transfers due to state transition error: {err}"

         confirmPersistAll [ toConfirm; transferDepositEvt ]
      | Ok(Some(_, autoTransferEvts)) ->
         let evts = [ toConfirm; transferDepositEvt ] @ autoTransferEvts
         confirmPersistAll evts

let persistEvent
   (logError: string -> unit)
   (confirmPersistAll: AccountEvent seq -> PersistentEffect<obj>)
   (evt: AccountEvent)
   (state: ParentAccountSnapshot)
   : PersistentEffect<obj>
   =
   let autoTransferStateTransitions =
      if canProduceAutoTransfer evt then
         ParentAccount.computeAutoTransferStateTransitions
            Frequency.PerTransaction
            evt.AccountId
            state
      else
         None

   match autoTransferStateTransitions with
   | None -> confirmPersistAll [ evt ]
   | Some(Error(_, err)) ->
      logError
         $"Will not persist auto transfers due to state transition error: {err}"

      confirmPersistAll [ evt ]
   | Some(Ok(_, autoTransferEvts)) ->
      confirmPersistAll (evt :: autoTransferEvts)

let actorProps
   (broadcaster: SignalRBroadcast)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (getAccountClosureRef: ActorSystem -> IActorRef<AccountClosureMessage>)
   (getBillingStatementActor: ActorSystem -> IActorRef<BillingStatementMessage>)
   (getDomesticTransfersRetryableUponRecipientEdit:
      AccountId -> Task<Result<DomesticTransfer list option, Err>>)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   (getSagaGuaranteedDeliveryRef:
      unit -> IActorRef<GuaranteedDelivery.Message<AppSaga.AppSagaMessage>>)
   (guaranteedDeliveryConsumerControllerRef:
      IActorRef<ConsumerController.IConsumerCommand<AccountMessage>>)
   =
   let handler (mailbox: Eventsourced<obj>) =
      let logError = logError mailbox

      let rec loop (stateOpt: ParentAccountSnapshot option) = actor {
         let! msg = mailbox.Receive()

         let state = stateOpt |> Option.defaultValue ParentAccountSnapshot.empty

         let onValidationError =
            onValidationError broadcaster mailbox getSagaRef

         match msg with
         | Deferred mailbox (:? AccountEvent as evt)
         | Persisted mailbox (:? AccountEvent as evt) ->
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
               getSagaRef
               getSagaGuaranteedDeliveryRef
               getDomesticTransfersRetryableUponRecipientEdit
               mailbox
               state
               evt

            return! loop <| Some state
         | :? SnapshotOffer as o -> return! loop <| Some(unbox o.Snapshot)
         | :? ConsumerController.Delivery<AccountMessage> as msg ->
            GuaranteedDelivery.ack msg

            // Send message to parent actor (Persistence Supervisor)
            // for message command to confirmed event persistence.
            mailbox.Parent() <! msg.Message

            return ignored ()
         | :? ConfirmableMessageEnvelope as envelope ->
            let unhandledMsg msg =
               logError
                  $"Unhandled message in ConfirmableMessageEnvelope - {msg}"

               unhandled ()

            let confirmPersistAll (evts: AccountEvent seq) =
               evts
               |> Seq.map box
               |> PersistenceSupervisor.confirmPersistAll
                     mailbox
                     envelope.ConfirmationId

            match envelope.Message with
            | :? AccountMessage as msg ->
               match msg with
               | AccountMessage.StateChange cmd ->
                  let validation = ParentAccount.stateTransition state cmd

                  match validation with
                  | Error err -> onValidationError cmd err
                  | Ok(InternalTransferWithinOrgDeducted transfer, state) ->
                     return!
                        persistInternalTransferWithinOrgEvent
                           logError
                           confirmPersistAll
                           transfer
                           state
                  | Ok(evt, state) ->
                     return! persistEvent logError confirmPersistAll evt state
               | msg -> return unhandledMsg msg
            | msg -> return unhandledMsg msg
         | :? AccountMessage as msg ->
            match msg with
            | AccountMessage.GetAccount ->
               let stateOpt =
                  stateOpt
                  |> Option.map (fun pa -> {
                     pa with
                        Events = pa.Events |> List.truncate 5
                  })

               mailbox.Sender() <! stateOpt
            | AccountMessage.GetVirtualAccount accountId ->
               let account =
                  stateOpt |> Option.bind _.VirtualAccounts.TryFind(accountId)

               mailbox.Sender() <! account
            | AccountMessage.ProcessBillingStatement(corrId, billingPeriod) ->
               if state.Status <> ParentAccountStatus.Active then
                  logError
                     $"Attempt to process billing statement for inactive account {state.Status}"
               else
                  let msg =
                     BillingSagaEvent.BillingStatementProcessing {
                        MaintenanceFeeCriteria = state.MaintenanceFeeCriteria
                        PrimaryCheckingAccountId = state.PrimaryVirtualAccountId
                     }
                     |> AppSaga.Message.billing state.OrgId corrId

                  getSagaRef corrId <! msg

                  let billing = {
                     OrgId = state.OrgId
                     ParentAccountId = state.ParentAccountId
                     CorrelationId = corrId
                     ParentAccountSnapshot =
                        JsonSerializer.SerializeToUtf8Bytes(
                           state,
                           Serialization.jsonOptions
                        )
                     LastPersistedEventSequenceNumber = mailbox.LastSequenceNr()
                     Statements =
                        parentAccountBillingStatements state billingPeriod
                  }

                  getBillingStatementActor mailbox.System
                  <! BillingStatementMessage.BulkPersist billing
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
                  let evts = List.map box autoTransferEvts
                  return! PersistAll evts
            | AccountMessage.DomesticTransfersRetryableUponRecipientEdit res ->
               match res with
               | Error err ->
                  logError $"Error getting retryable transfers {err}"
               | Ok None ->
                  logDebug mailbox "No retryable transfers upon recipient edit."
               | Ok(Some transfers) ->
                  logInfo
                     mailbox
                     $"Retrying {transfers.Length} transfers upon recipient edit"

                  for transfer: DomesticTransfer in transfers do
                     let corrId = TransferId.toCorrelationId transfer.TransferId

                     let evt =
                        Some transfer.Recipient
                        |> DomesticTransferSagaEvent.RetryTransferServiceRequest

                     let msg =
                        AppSaga.Message.domesticTransfer
                           transfer.Sender.OrgId
                           corrId
                           evt

                     getSagaRef corrId <! msg

               return ignored ()
            (*
            | AccountMessage.Delete ->
               let state =
                  Some {
                     state with
                        Info.Status = AccountStatus.ReadyForDelete
                  }

               return! loop state <@> DeleteMessages Int64.MaxValue
            *)
            // NOTE: Perceived optimization:
            // Persisting DebitSettled is essential but probably no need to
            // persist the DebitPending event as long as SaveSnapshot,
            // invoked below, is called in case the actor restarts. Without
            // SaveSnapshot in the LifecyclePostStop hook below, the actor would
            // restart and potentially lose the latest value for
            // ParentAccountSnapshot.PendingDeductions since it would not be
            // able to derive the latest value from the persisted events during
            // event reply on actor start.
            //
            // With PersistentEffect.Defer, Debit command will invoke the
            // persistence handler to record the PendingDeductions on
            // ParentAccountSnapshot and interact with PurchaseSaga. It will
            // do so without saving it as an event in the event journal.
            | AccountMessage.StateChange(AccountCommand.Debit _ as cmd) ->
               match ParentAccount.stateTransition state cmd with
               | Ok(evt, _) -> return! PersistentEffect.Defer [ evt ]
               | Error err -> onValidationError cmd err
            // Some messages are sent through traditional AtMostOnceDelivery via
            // a reference to the cluster sharded entity ref rather than Akka.Delivery
            // AtLeastOnceDelivery producer ref so will not hit the
            // ConsumerController.Delivery match case above. Need to send message
            // to parent actor (Persistence Supervisor) so the command gets wrapped in a
            // ConfirmableMessageEnvelope for Akka.Persistence.Extras.Confirmation
            | AccountMessage.StateChange _ ->
               mailbox.Parent() <! msg
               return ignored ()
            | _ -> return unhandled ()
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
                     LifecyclePreStart =
                        fun _ ->
                           logDebug mailbox $"ACCOUNT PRESTART"

                           // Start Guaranteed Delivery Consumer Controller
                           guaranteedDeliveryConsumerControllerRef
                           <! new ConsumerController.Start<AccountMessage>(
                              untyped mailbox.Self
                           )

                           ignored ()
                     LifecyclePostStop =
                        fun _ ->
                           logDebug mailbox $"ACCOUNT POSTSTOP"
                           SaveSnapshot state
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

let getGuaranteedDeliveryProducerRef
   (system: ActorSystem)
   : IActorRef<GuaranteedDelivery.Message<AccountMessage>>
   =
   typed
   <| Akka.Hosting.ActorRegistry
      .For(system)
      .Get<ActorUtil.ActorMetadata.AccountGuaranteedDeliveryProducerMarker>()

let initProps
   (broadcaster: SignalRBroadcast)
   (supervisorEnvConfig: PersistenceSupervisorEnvConfig)
   (persistenceId: string)
   (getBillingStatementActor: ActorSystem -> IActorRef<BillingStatementMessage>)
   (getDomesticTransfersRetryableUponRecipientEdit:
      AccountId -> Task<Result<DomesticTransfer list option, Err>>)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   (getSagaGuaranteedDeliveryRef:
      unit -> IActorRef<GuaranteedDelivery.Message<AppSaga.AppSagaMessage>>)
   (getEmailActor: ActorSystem -> IActorRef<EmailMessage>)
   (getAccountClosureActor: ActorSystem -> IActorRef<AccountClosureMessage>)
   (guaranteedDeliveryConsumerControllerRef:
      IActorRef<ConsumerController.IConsumerCommand<AccountMessage>>)
   =
   let childProps =
      actorProps
         broadcaster
         getEmailActor
         getAccountClosureActor
         getBillingStatementActor
         getDomesticTransfersRetryableUponRecipientEdit
         getSagaRef
         getSagaGuaranteedDeliveryRef
         guaranteedDeliveryConsumerControllerRef

   PersistenceSupervisor.create {
      EnvConfig = supervisorEnvConfig
      ChildProps = childProps.ToProps()
      PersistenceId = persistenceId
      CompatibleWithGuaranteedDelivery = true
      IsPersistableMessage =
         function
         | :? AccountMessage as msg ->
            match msg with
            // Debit intent will invoke persistence handler without persisting
            // the event, via PersistentEffect.Defer.
            | AccountMessage.StateChange(AccountCommand.Debit _) -> false
            | AccountMessage.StateChange _ -> true
            | _ -> false
         | _ -> false
   }
