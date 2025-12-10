[<RequireQualifiedAccess>]
module AccountActor

open System
open Akka.Persistence
open Akka.Persistence.Extras
open Akka.Delivery
open Akkling
open Akkling.Persistence
open FsToolkit.ErrorHandling
open System.Text.Json
open System.Threading.Tasks

open Lib.SharedTypes
open Lib.Types
open ActorUtil
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Payment.Domain
open Bank.Purchase.Domain
open BillingStatement
open AutomaticTransfer
open SignalRBroadcast
open EmailMessage
open OrgOnboardingSaga
open PurchaseSaga
open DomesticTransferSaga
open PlatformTransferSaga
open PaymentRequestSaga
open BillingSaga
open BankActorRegistry

/// Entries older than the cutoff will be pruned.
type LookbackHours = { Outbox: int; ProcessedCommands: int }

// Account events indicating a settled txn with an in/out money flow
// can produce an automatic transfer.
// Automated transfers have money flow but they can
// not generate an auto transfer.
let private canProduceAutoTransfer =
   function
   | AccountEvent.DepositedCash _
   | AccountEvent.DebitRefunded _
   | AccountEvent.DebitSettled _
   | AccountEvent.InternalTransferBetweenOrgsDeposited _
   | AccountEvent.InternalTransferBetweenOrgsSettled _
   | AccountEvent.DomesticTransferSettled _
   | AccountEvent.MaintenanceFeeDebited _ -> true
   | _ -> false

let private signalRBroadcast broadcaster state evt =
   match evt with
   | AccountEvent.ParentAccount evt ->
      broadcaster.parentAccountEventPersisted evt
   | _ ->
      state.VirtualAccounts.TryFind evt.AccountId
      |> Option.iter (fun account ->
         broadcaster.accountEventPersisted evt {
            // NOTE:
            // Hack to avoid SignalR messages being over max allowed bytes,
            // which can happen during data seeding process in development
            // when increasing 'numberOfMonthsToGenerateDataFor'.
            //
            // In production this is likely unnecessary as an org will likely
            // not have near 500 pending transactions.
            account with
               PendingFunds =
                  if Env.isProd then
                     account.PendingFunds
                  else
                     Map._keep 500 account.PendingFunds
         })

let private onValidationError
   (registry: #ISagaGuaranteedDeliveryActor)
   (broadcaster: SignalRBroadcast)
   mailbox
   (cmd: AccountCommand)
   (err: Err)
   =
   logWarning
      mailbox
      $"Validation fail %s{string err} for command %s{cmd.GetType().Name}"

   let orgId = cmd.Envelope.OrgId
   let corrId = cmd.Envelope.CorrelationId

   broadcaster.accountEventError orgId cmd.AccountId corrId err

   let sagaRef = registry.SagaGuaranteedDeliveryActor()

   match cmd with
   | AccountCommand.Debit cmd -> option {
      let! reason =
         match err with
         | AccountStateTransitionError ParentAccountNotActive ->
            Some(PurchaseAccountFailReason.AccountNotActive "ParentAccount")
         | AccountStateTransitionError(AccountNotActive name) ->
            Some(PurchaseAccountFailReason.AccountNotActive name)
         | AccountStateTransitionError(InsufficientBalance(balance, accountName)) ->
            Some(
               PurchaseAccountFailReason.InsufficientAccountFunds(
                  balance,
                  accountName
               )
            )
         | _ -> None

      if
         not cmd.Data.EmployeePurchaseReference.PurchaseAuthType.IsBypassAuth
      then
         mailbox.Sender()
         <! PurchaseAuthorizationStatus.fromAccountFailReason reason

      let msg =
         PurchaseFailReason.Account reason
         |> PurchaseSagaEvent.PurchaseRejected
         |> AppSaga.Message.purchase orgId corrId
         |> GuaranteedDelivery.message corrId.Value

      sagaRef <! msg
     }
   | AccountCommand.DomesticTransfer cmd when cmd.Data.OriginatedFromSchedule -> option {
      let! reason =
         match err with
         | AccountStateTransitionError ParentAccountNotActive
         | AccountStateTransitionError(AccountNotActive _) ->
            Some DomesticTransferFailReason.AccountNotActive
         | AccountStateTransitionError(InsufficientBalance _) ->
            Some DomesticTransferFailReason.AccountInsufficientFunds
         | _ -> None

      let msg =
         DomesticTransferSagaEvent.SenderUnableToReserveFunds reason
         |> AppSaga.Message.domesticTransfer orgId corrId
         |> GuaranteedDelivery.message corrId.Value

      sagaRef <! msg
     }
   | AccountCommand.InternalTransferBetweenOrgs cmd when
      cmd.Data.OriginatedFromSchedule
      || cmd.Data.OriginatedFromPaymentRequest.IsSome
        ->
        option {
           let! reason =
              match err with
              | AccountStateTransitionError ParentAccountNotActive
              | AccountStateTransitionError(AccountNotActive _) ->
                 Some InternalTransferFailReason.AccountNotActive
              | AccountStateTransitionError(InsufficientBalance _) ->
                 Some InternalTransferFailReason.InsufficientFunds
              | _ -> None

           let msg =
              PlatformTransferSagaEvent.SenderUnableToReserveFunds reason
              |> AppSaga.Message.platformTransfer orgId corrId
              |> GuaranteedDelivery.message corrId.Value

           sagaRef <! msg

           let! paymentId = cmd.Data.OriginatedFromPaymentRequest
           let corrId = paymentId.AsCorrelationId

           let msg =
              PaymentRequestSagaEvent.PaymentFailed(
                 TransferId cmd.CorrelationId.Value,
                 PaymentFailReason.AccountClosed
              )
              |> AppSaga.Message.paymentRequest orgId corrId
              |> GuaranteedDelivery.message corrId.Value

           sagaRef <! msg
        }
   | AccountCommand.DepositTransferBetweenOrgs cmd -> option {
      let msg =
         InternalTransferFailReason.AccountNotActive
         |> PlatformTransferSagaEvent.RecipientUnableToDepositFunds
         |> AppSaga.Message.platformTransfer orgId corrId
         |> GuaranteedDelivery.message corrId.Value

      sagaRef <! msg

      let! paymentId = cmd.Data.BaseInfo.FromPaymentRequest
      let corrId = paymentId.AsCorrelationId

      let msg =
         PaymentRequestSagaEvent.PaymentFailed(
            cmd.Data.BaseInfo.TransferId,
            PaymentFailReason.AccountClosed
         )
         |> AppSaga.Message.paymentRequest orgId corrId
         |> GuaranteedDelivery.message corrId.Value

      sagaRef <! msg
     }
   | _ -> None
   |> ignore

let private computeOutboxMessage
   (state: ParentAccountSnapshot)
   (evt: AccountEvent)
   : AccountOutboxMessage option
   =
   match evt with
   | AccountEvent.InitializedPrimaryCheckingAccount e ->
      let msg =
         AppSaga.Message.orgOnboard
            e.OrgId
            e.CorrelationId
            OrgOnboardingSagaEvent.InitializedPrimaryVirtualAccount
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.DebitPending e when
      e.Data.EmployeePurchaseReference.PurchaseAuthType.IsBypassAuth
      ->
      let msg =
         AppSaga.Message.purchase
            e.OrgId
            e.CorrelationId
            PurchaseSagaEvent.AccountReservedFunds
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.DebitSettled e ->
      let msg =
         PurchaseSagaEvent.PurchaseSettledWithAccount e.Data.Clearing
         |> AppSaga.Message.purchase e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.DebitFailed e ->
      let msg =
         PurchaseSagaEvent.PurchaseFailureAcknowledgedByAccount
         |> AppSaga.Message.purchase e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.MaintenanceFeeDebited e ->
      let msg =
         BillingSagaEvent.MaintenanceFeeProcessed
         |> AppSaga.Message.billing e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.MaintenanceFeeSkipped e ->
      let msg =
         BillingSagaEvent.MaintenanceFeeProcessed
         |> AppSaga.Message.billing e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.PaymentRequested e ->
      let msg =
         PaymentRequestSagaStartEvent.PaymentRequested e
         |> AppSaga.Message.paymentRequestStart e.OrgId e.CorrelationId

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.PaymentRequestDeclined e ->
      let msg =
         PaymentRequestSagaEvent.PaymentRequestDeclined
         |> AppSaga.Message.paymentRequest e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.PaymentRequestCancelled e ->
      let msg =
         PaymentRequestSagaEvent.PaymentRequestCancelled
         |> AppSaga.Message.paymentRequest e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.InternalTransferBetweenOrgsPending e ->
      if e.Data.FromSchedule then
         let msg =
            state.PartnerBankLink
            |> PlatformTransferSagaEvent.SenderReservedFunds
            |> AppSaga.Message.platformTransfer e.OrgId e.CorrelationId
            |> GuaranteedDelivery.message e.CorrelationId.Value

         Some(AccountOutboxMessage.Saga msg)
      else
         let msg =
            (e, state.PartnerBankLink)
            |> PlatformTransferSagaStartEvent.SenderReservedFunds
            |> AppSaga.Message.platformTransferStart e.OrgId e.CorrelationId

         Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.InternalTransferBetweenOrgsScheduled e ->
      let msg =
         PlatformTransferSagaStartEvent.ScheduleTransferRequest e
         |> AppSaga.Message.platformTransferStart e.OrgId e.CorrelationId

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.InternalTransferBetweenOrgsDeposited e ->
      let msg =
         state.PartnerBankLink
         |> PlatformTransferSagaEvent.RecipientDepositedFunds
         |> AppSaga.Message.platformTransfer e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.InternalTransferBetweenOrgsSettled e ->
      let info = e.Data.BaseInfo

      let paymentFulfillment =
         info.FromPaymentRequest
         |> Option.map (fun payId -> {
            PaymentRequestId = payId
            TransferId = info.TransferId
            FulfilledAt = e.Timestamp
         })

      let msg =
         PlatformTransferSagaEvent.TransferSettled paymentFulfillment
         |> AppSaga.Message.platformTransfer e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.InternalTransferBetweenOrgsFailed e ->
      let msg =
         PlatformTransferSagaEvent.SenderReleasedReservedFunds
         |> AppSaga.Message.platformTransfer e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.DomesticTransferScheduled e ->
      let msg =
         DomesticTransferSagaStartEvent.ScheduleTransferRequest(
            e,
            state.PartnerBankLink
         )
         |> AppSaga.Message.domesticTransferStart e.OrgId e.CorrelationId

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.DomesticTransferPending e ->
      if e.Data.FromSchedule then
         let msg =
            DomesticTransferSagaEvent.SenderReservedFunds
            |> AppSaga.Message.domesticTransfer e.OrgId e.CorrelationId
            |> GuaranteedDelivery.message e.CorrelationId.Value

         Some(AccountOutboxMessage.Saga msg)
      else
         let msg =
            (e, state.PartnerBankLink)
            |> DomesticTransferSagaStartEvent.SenderReservedFunds
            |> AppSaga.Message.domesticTransferStart e.OrgId e.CorrelationId

         Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.DomesticTransferFailed e ->
      let msg =
         DomesticTransferSagaEvent.SenderReleasedReservedFunds
         |> AppSaga.Message.domesticTransfer e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | AccountEvent.DomesticTransferSettled e ->
      let msg =
         DomesticTransferSagaEvent.SenderDeductedFunds
         |> AppSaga.Message.domesticTransfer e.OrgId e.CorrelationId
         |> GuaranteedDelivery.message e.CorrelationId.Value

      Some(AccountOutboxMessage.Saga msg)
   | _ -> None

let private applyComputedOutboxToState
   (state: ParentAccountSnapshot)
   (evt: AccountEvent)
   =
   let computed = computeOutboxMessage state evt
   let _, envelope = AccountEnvelope.unwrap evt

   match computed with
   | Some msg -> {
      state with
         Outbox = state.Outbox |> Map.add envelope.Id (envelope.Timestamp, msg)
     }
   | None -> state

let private forwardMessagesAfterEventPersistence
   (registry: #IEmailActor & #ISagaActor & #ISagaGuaranteedDeliveryActor)
   (getRetryableTransfers:
      CounterpartyId -> Task<Result<DomesticTransfer list option, Err>>)
   (mailbox: Eventsourced<obj>)
   (state: ParentAccountSnapshot)
   (evt: AccountEvent)
   =
   let _, envelope = AccountEnvelope.unwrap evt

   match evt with
   | AccountEvent.CreatedVirtualAccount e ->
      let msg =
         EmailMessage.create
            state.OrgId
            e.CorrelationId
            (EmailInfo.AccountOpen e.Data.Name)

      registry.EmailActor() <! msg
   | AccountEvent.ParentAccount(ParentAccountEvent.EditedCounterparty e) ->
      let retryable =
         getRetryableTransfers e.Data.Counterparty.CounterpartyId
         |> Async.AwaitTask
         |> Async.map AccountMessage.DomesticTransfersRetryableUponRecipientEdit

      retype mailbox.Self <!| retryable
   | AccountEvent.DebitPending e when
      not e.Data.EmployeePurchaseReference.PurchaseAuthType.IsBypassAuth
      ->
      mailbox.Sender() <! PurchaseAuthorizationStatus.Approved
   | _ ->
      match computeOutboxMessage state evt with
      | Some(AccountOutboxMessage.Saga sagaMsg) ->
         match sagaMsg with
         | :? AppSaga.AppSagaMessage as msg ->
            registry.SagaActor envelope.CorrelationId <! msg
         | :? GuaranteedDelivery.Message<AppSaga.AppSagaMessage> as msg ->
            registry.SagaGuaranteedDeliveryActor() <! msg
         | msg -> logError mailbox $"Unknown outbox message {msg}"
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

   let autoSenderTransitions state =
      ParentAccount.computeAutoTransferStateTransitions
         Frequency.PerTransaction
         transfer.Data.BaseInfo.Sender.AccountId
         state

   let autoRecipientTransitions state =
      ParentAccount.computeAutoTransferStateTransitions
         Frequency.PerTransaction
         transfer.Data.BaseInfo.Recipient.AccountId
         state

   match correspondingTransferDeposit with
   | Error err ->
      logError
         $"Not able to deposit transfer into recipient account.
         Will not deduct transfer from sender account. {err}"

      ignored ()
   | Ok(transferDepositEvt, state) ->
      let autoTransferStateTransitions =
         autoSenderTransitions state
         |> Option.map (function
            | Error e -> Error e
            | Ok(state, evts) ->
               let senderTransitions =
                  ParentAccount.computeAutoTransferStateTransitions
                     Frequency.PerTransaction
                     transfer.Data.BaseInfo.Recipient.AccountId
                     state

               senderTransitions
               |> Option.sequenceResult
               |> Result.map (function
                  | None -> state, evts
                  | Some(state, evts2) -> state, evts @ evts2))
         |> Option.orElse (autoRecipientTransitions state)
         |> Option.sequenceResult

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
   (registry:
      #IBillingStatementActor & #ISagaActor & #ISagaGuaranteedDeliveryActor)
   (broadcaster: SignalRBroadcast)
   (lookbackHours: LookbackHours)
   (getDomesticTransfersRetryableUponRecipientEdit:
      CounterpartyId -> Task<Result<DomesticTransfer list option, Err>>)
   (onLifeCycleEvent: Eventsourced<obj> -> obj -> Effect<obj>)
   =
   let handler (mailbox: Eventsourced<obj>) =
      let logError = logError mailbox

      let rec loop (stateOpt: ParentAccountSnapshot option) = actor {
         let! msg = mailbox.Receive()

         let state = stateOpt |> Option.defaultValue ParentAccountSnapshot.empty

         let onValidationError = onValidationError registry broadcaster mailbox

         match msg with
         | Persisted mailbox (:? AccountEvent as evt) ->
            let state = ParentAccount.applyEvent state evt

            signalRBroadcast broadcaster state evt

            forwardMessagesAfterEventPersistence
               registry
               getDomesticTransfersRetryableUponRecipientEdit
               mailbox
               state
               evt

            let state = applyComputedOutboxToState state evt
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
               // Redeliver messages in outbox to saga actor.
               // See AccountOutbox type for more information.
               | AccountMessage.StateChange cmd when
                  state.Outbox.ContainsKey cmd.Envelope.Id
                  ->
                  let corrId = cmd.Envelope.CorrelationId

                  match state.Outbox |> Map.tryFind cmd.Envelope.Id with
                  | Some(timestamp, AccountOutboxMessage.Saga msg) ->
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
               | AccountMessage.StateChange cmd when
                  state.ProcessedCommands.ContainsKey cmd.Envelope.Id
                  ->
                  logWarning mailbox $"Received duplicate command {cmd}"
                  return ignored ()
               | AccountMessage.StateChange cmd ->
                  let validation = ParentAccount.stateTransition state cmd

                  match validation with
                  | Error err ->
                     onValidationError cmd err
                     return ignored ()
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
                     |> GuaranteedDelivery.message corrId.Value

                  registry.SagaGuaranteedDeliveryActor() <! msg

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

                  registry.BillingStatementActor()
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
                     let corrId = transfer.TransferId.AsCorrelationId

                     let evt =
                        Some transfer.Counterparty
                        |> DomesticTransferSagaEvent.RetryTransferServiceRequest

                     let msg =
                        AppSaga.Message.domesticTransfer
                           transfer.Originator.OrgId
                           corrId
                           evt
                        |> GuaranteedDelivery.message corrId.Value

                     registry.SagaGuaranteedDeliveryActor() <! msg

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
            | AccountMessage.StateChange(AccountCommand.Debit _ as cmd) ->
               match ParentAccount.stateTransition state cmd with
               | Ok(evt, _) -> return! Persist evt
               | Error err ->
                  onValidationError cmd err
                  return ignored ()
            // Some messages are sent through traditional AtMostOnceDelivery via
            // a reference to the cluster sharded entity ref rather than Akka.Delivery
            // AtLeastOnceDelivery producer ref so will not hit the
            // ConsumerController.Delivery match case above. Need to send message
            // to parent actor (Persistence Supervisor) so the command gets wrapped in a
            // ConfirmableMessageEnvelope for Akka.Persistence.Extras.Confirmation
            | AccountMessage.StateChange _ ->
               mailbox.Parent() <! msg
               return ignored ()
            | AccountMessage.PruneIdempotencyChecker ->
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

               return! loop stateOpt
            | AccountMessage.PruneOutbox ->
               let cutoff = DateTime.UtcNow.AddHours -lookbackHours.Outbox

               let stateOpt =
                  stateOpt
                  |> Option.map (fun state -> {
                     state with
                        Outbox =
                           state.Outbox
                           |> Map.filter (fun _ (date, _) -> date > cutoff)
                  })

               return! loop stateOpt
            | _ -> return unhandled ()
         // Event replay on actor start
         | :? AccountEvent as e when mailbox.IsRecovering() ->
            let state = ParentAccount.applyEvent state e
            let state = applyComputedOutboxToState state e
            return! loop (Some state)
         | msg -> return onLifeCycleEvent mailbox msg
      }

      loop None

   propsPersist handler

let private handleLifeCycleEvent
   (guaranteedDeliveryConsumerControllerRef:
      IActorRef<ConsumerController.IConsumerCommand<AccountMessage>>)
   (mailbox: Eventsourced<obj>)
   (msg: obj)
   =
   let logDebug = logDebug mailbox

   PersistentActorEventHandler.handleEvent
      {
         PersistentActorEventHandler.init with
            (*
            DeleteMessagesSuccess =
               fun _ ->
                  if account.Status = AccountStatus.ReadyForDelete then
                     logDebug "<Passivate>"
                     passivate ()
                  else
                     ignored ()
            *)
            LifecyclePreStart =
               fun _ ->
                  logDebug "<PreStart Account Actor>"

                  // Start Guaranteed Delivery Consumer Controller
                  guaranteedDeliveryConsumerControllerRef
                  <! new ConsumerController.Start<AccountMessage>(
                     untyped mailbox.Self
                  )

                  mailbox.ScheduleRepeatedly
                     TimeSpan.Zero
                     (TimeSpan.FromHours 4)
                     mailbox.Self
                     AccountMessage.PruneIdempotencyChecker
                  |> ignore

                  mailbox.ScheduleRepeatedly
                     TimeSpan.Zero
                     (TimeSpan.FromHours 4)
                     mailbox.Self
                     AccountMessage.PruneOutbox
                  |> ignore

                  ignored ()
      }
      mailbox
      msg

let initProps
   registry
   (broadcaster: SignalRBroadcast)
   (supervisorEnvConfig: PersistenceSupervisorEnvConfig)
   (persistenceId: string)
   (getDomesticTransfersRetryableUponRecipientEdit:
      CounterpartyId -> Task<Result<DomesticTransfer list option, Err>>)
   (guaranteedDeliveryConsumerControllerRef:
      IActorRef<ConsumerController.IConsumerCommand<AccountMessage>>)
   =
   let lookbackHours = { Outbox = 24; ProcessedCommands = 24 }

   let childProps =
      actorProps
         registry
         broadcaster
         lookbackHours
         getDomesticTransfersRetryableUponRecipientEdit
         (handleLifeCycleEvent guaranteedDeliveryConsumerControllerRef)

   PersistenceSupervisor.create {
      EnvConfig = supervisorEnvConfig
      ChildProps = childProps.ToProps()
      PersistenceId = persistenceId
      CompatibleWithGuaranteedDelivery = true
      IsPersistableMessage =
         function
         | :? AccountMessage as msg ->
            match msg with
            | AccountMessage.StateChange(AccountCommand.Debit _) -> false
            | AccountMessage.StateChange _ -> true
            | _ -> false
         | _ -> false
   }
