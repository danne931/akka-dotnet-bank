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
                  PurchaseAccountFailReason.AccountNotActive accountName
                  |> PurchaseSagaEvent.PurchaseRejectedByAccount
                  |> AppSaga.Message.purchase cmd.OrgId cmd.CorrelationId
               )
            | AccountCommand.DomesticTransfer cmd when
               cmd.Data.OriginatedFromSchedule
               ->
               false,
               Some(
                  cmd.CorrelationId,
                  DomesticTransferFailReason.SenderAccountNotActive
                  |> DomesticTransferSagaEvent.SenderAccountUnableToDeductFunds
                  |> AppSaga.Message.domesticTransfer
                        cmd.OrgId
                        cmd.CorrelationId
               )
            | AccountCommand.InternalTransferBetweenOrgs cmd when
               cmd.Data.OriginatedFromSchedule
               ->
               false,
               Some(
                  cmd.CorrelationId,
                  InternalTransferFailReason.AccountClosed
                  |> PlatformTransferSagaEvent.SenderAccountUnableToDeductFunds
                  |> AppSaga.Message.platformTransfer
                        cmd.OrgId
                        cmd.CorrelationId
               )
            | AccountCommand.DepositTransferBetweenOrgs cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  InternalTransferFailReason.AccountClosed
                  |> PlatformTransferSagaEvent.RecipientAccountUnableToDepositFunds
                  |> AppSaga.Message.platformTransfer
                        cmd.OrgId
                        cmd.CorrelationId
               )
            | AccountCommand.PlatformPayment cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  PlatformPaymentFailReason.AccountClosed
                  |> PlatformPaymentSagaEvent.PayerAccountUnableToDeductFunds
                  |> AppSaga.Message.platformPayment cmd.OrgId cmd.CorrelationId
               )
            | AccountCommand.DepositPlatformPayment cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  (PlatformPaymentFailReason.AccountClosed,
                   cmd.Data.PaymentMethod)
                  |> PlatformPaymentSagaEvent.PayeeAccountUnableToDepositFunds
                  |> AppSaga.Message.platformPayment cmd.OrgId cmd.CorrelationId
               )
            | _ -> false, None
         | InsufficientBalance(balance, accountName) ->
            match cmd with
            | AccountCommand.Debit cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  PurchaseAccountFailReason.InsufficientAccountFunds(
                     balance,
                     accountName
                  )
                  |> PurchaseSagaEvent.PurchaseRejectedByAccount
                  |> AppSaga.Message.purchase cmd.OrgId cmd.CorrelationId
               )
            | AccountCommand.DomesticTransfer cmd ->
               false,
               Some(
                  cmd.CorrelationId,
                  DomesticTransferFailReason.SenderAccountInsufficientFunds
                  |> DomesticTransferSagaEvent.SenderAccountUnableToDeductFunds
                  |> AppSaga.Message.domesticTransfer
                        cmd.OrgId
                        cmd.CorrelationId
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

      fail |> Option.iter (fun (corrId, msg) -> getSagaRef corrId <! msg)


let onPersisted
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (getAccountClosureRef: ActorSystem -> IActorRef<AccountClosureMessage>)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
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
         |> PurchaseSagaEvent.PurchaseConfirmedByAccount
         |> AppSaga.Message.purchase e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.DebitSettled e ->
      let msg =
         PurchaseSagaEvent.PurchaseSettled
         |> AppSaga.Message.purchase e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.DebitRefunded e ->
      let msg =
         PurchaseSagaEvent.PurchaseRefundedToAccount
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

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.PlatformPaymentPending e ->
      let msg =
         PlatformPaymentSagaEvent.PayerAccountDeductedFunds(
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
   | AccountEvent.PlatformPaymentRefunded e ->
      let msg =
         PlatformPaymentSagaEvent.PayerAccountRefunded
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
            |> PlatformTransferSagaEvent.SenderAccountDeductedFunds
            |> AppSaga.Message.platformTransfer e.OrgId e.CorrelationId

         getSagaRef e.CorrelationId <! msg
      else
         let msg =
            PlatformTransferSagaStartEvent.SenderAccountDeductedFunds(
               e,
               partnerBankAccountLink
            )
            |> AppSaga.Message.platformTransferStart e.OrgId e.CorrelationId

         getSagaRef e.CorrelationId <! msg
   | AccountEvent.InternalTransferBetweenOrgsScheduled e ->
      let msg =
         PlatformTransferSagaStartEvent.ScheduleTransferRequest e
         |> AppSaga.Message.platformTransferStart e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.InternalTransferBetweenOrgsDeposited e ->
      let msg =
         partnerBankAccountLink
         |> PlatformTransferSagaEvent.RecipientAccountDepositedFunds
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

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.DomesticTransferPending e ->
      if e.Data.FromSchedule then
         let msg =
            DomesticTransferSagaEvent.SenderAccountDeductedFunds
            |> AppSaga.Message.domesticTransfer e.OrgId e.CorrelationId

         getSagaRef e.CorrelationId <! msg
      else
         let msg =
            DomesticTransferSagaStartEvent.SenderAccountDeductedFunds e
            |> AppSaga.Message.domesticTransferStart e.OrgId e.CorrelationId

         getSagaRef e.CorrelationId <! msg
   | AccountEvent.DomesticTransferFailed e ->
      let msg =
         DomesticTransferSagaEvent.SenderAccountRefunded
         |> AppSaga.Message.domesticTransfer e.OrgId e.CorrelationId

      getSagaRef e.CorrelationId <! msg
   | AccountEvent.DomesticTransferSettled e ->
      let msg =
         DomesticTransferSagaEvent.TransferMarkedAsSettled
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
   (confirmPersist: AccountMessage -> PersistentEffect<obj>)
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

      let toConfirm =
         transfer
         |> AccountEvent.InternalTransferWithinOrgDeducted
         |> AccountMessage.Event

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
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   (getBillingStatementActor: ActorSystem -> IActorRef<BillingStatementMessage>)
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
                  | Ok(InternalTransferWithinOrgDeducted transfer, state) ->
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
            | AccountMessage.ProcessBillingStatement(corrId, billingPeriod) ->
               if state.Status <> ParentAccountStatus.Active then
                  logError
                     $"Attempt to process billing statement for inactive account
                     {state.Status} {mailbox.Pid}"
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
                  let evts =
                     List.map (AccountMessage.Event >> box) autoTransferEvts

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

                  for (transfer: DomesticTransfer) in transfers do
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
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
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
         getSagaRef
         getBillingStatementActor
         getDomesticTransfersRetryableUponRecipientEdit

   persistenceSupervisor
      supervisorOpts
      isPersistableMessage
      childProps
      persistenceId
