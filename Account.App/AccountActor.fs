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
open ActorUtil
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Employee.Domain
open BillingStatement
open AutomaticTransfer
open SignalRBroadcast
open Email

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

   let msg = EmailMessage.BillingStatement(account.FullName, account.OrgId)

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

let private handleValidationError
   (broadcaster: SignalRBroadcast)
   mailbox
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   (account: Account)
   (cmd: AccountCommand)
   (err: Err)
   =
   logWarning
      mailbox
      $"Validation fail %s{string err} for command %s{cmd.GetType().Name}"

   let signalRBroadcastValidationErr () =
      broadcaster.accountEventError
         account.OrgId
         account.AccountId
         cmd.Envelope.CorrelationId
         err

   match err with
   | AccountStateTransitionError e ->
      match e with
      // NOOP
      | TransferProgressNoChange
      | TransferAlreadyProgressedToCompletedOrFailed
      | AccountNotReadyToActivate ->
         logDebug mailbox $"AccountTransferActor NOOP msg {e}"
      | InsufficientBalance e ->
         match cmd with
         | AccountCommand.Debit cmd ->
            let info = cmd.Data
            let employee = cmd.Data.EmployeePurchaseReference

            let msg =
               AccountRejectsPurchaseCommand.create
                  (employee.EmployeeId, cmd.OrgId)
                  {
                     Reason =
                        PurchaseFailReason.InsufficientAccountFunds(
                           account.Balance,
                           account.FullName
                        )
                     Info = {
                        AccountId = account.AccountId
                        CorrelationId = cmd.CorrelationId
                        InitiatedBy = {
                           Id = InitiatedById employee.EmployeeId
                           Name = employee.EmployeeName
                        }
                        CardId = employee.CardId
                        CardNumberLast4 = employee.EmployeeCardNumberLast4
                        Date = info.Date
                        Amount = info.Amount
                        Merchant = info.Merchant
                        Reference = info.Reference
                     }
                  }
               |> EmployeeCommand.AccountRejectsPurchase
               |> EmployeeMessage.StateChange

            getEmployeeRef employee.EmployeeId <! msg
         | _ -> ()

         signalRBroadcastValidationErr ()
      | _ -> signalRBroadcastValidationErr ()
   | _ -> ()

let actorProps
   (broadcaster: SignalRBroadcast)
   (getOrStartInternalTransferRef: Actor<_> -> IActorRef<InternalTransferMsg>)
   (getDomesticTransferRef: ActorSystem -> IActorRef<DomesticTransferMessage>)
   (getEmailRef: ActorSystem -> IActorRef<EmailMessage>)
   (getAccountClosureRef: ActorSystem -> IActorRef<AccountClosureMessage>)
   (getBillingStatementRef: ActorSystem -> IActorRef<BillingStatementMessage>)
   (getSchedulingRef: ActorSystem -> IActorRef<SchedulingActor.Message>)
   (getOrgRef: OrgId -> IEntityRef<OrgMessage>)
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   =
   let handler (mailbox: Eventsourced<obj>) =
      let logError = logError mailbox

      let rec loop (stateOpt: AccountSnapshot option) = actor {
         let! msg = mailbox.Receive()

         let state = stateOpt |> Option.defaultValue AccountSnapshot.empty

         let account = state.Info

         let handleValidationError =
            handleValidationError broadcaster mailbox getEmployeeRef account

         match msg with
         | Persisted mailbox e ->
            let (AccountMessage.Event evt) = unbox e
            let state = Account.applyEvent state evt
            let account = state.Info

            broadcaster.accountEventPersisted evt account

            match evt with
            | DebitedAccount e ->
               let info = e.Data
               let employee = info.EmployeePurchaseReference

               let msg =
                  AccountConfirmsPurchaseCommand.create
                     (employee.EmployeeId, e.OrgId)
                     {
                        Info = {
                           AccountId = account.AccountId
                           CorrelationId = e.CorrelationId
                           InitiatedBy = {
                              Id = InitiatedById employee.EmployeeId
                              Name = employee.EmployeeName
                           }
                           CardId = employee.CardId
                           CardNumberLast4 = employee.EmployeeCardNumberLast4
                           Date = info.Date
                           Amount = info.Amount
                           Merchant = info.Merchant
                           Reference = info.Reference
                        }
                     }
                  |> EmployeeCommand.AccountConfirmsPurchase
                  |> EmployeeMessage.StateChange

               getEmployeeRef employee.EmployeeId <! msg
            | InternalTransferWithinOrgPending e ->
               getOrStartInternalTransferRef mailbox
               <! InternalTransferMsg.TransferRequestWithinOrg e
            | InternalTransferBetweenOrgsPending e ->
               getOrStartInternalTransferRef mailbox
               <! InternalTransferMsg.TransferRequestBetweenOrgs e
            | InternalAutomatedTransferPending e ->
               getOrStartInternalTransferRef mailbox
               <! InternalTransferMsg.AutomatedTransferRequest e
            | InternalTransferBetweenOrgsScheduled e ->
               getSchedulingRef mailbox.System
               <! SchedulingActor.Message.ScheduleInternalTransferBetweenOrgs
                     e.Data
            | DomesticTransferScheduled e ->
               getSchedulingRef mailbox.System
               <! SchedulingActor.Message.ScheduleDomesticTransfer e.Data
            | DomesticTransferPending e ->
               let txn = TransferEventToDomesticTransfer.fromPending e

               let msg =
                  DomesticTransferMessage.TransferRequest(
                     DomesticTransferServiceAction.TransferAck,
                     txn
                  )

               for _ in [ 1..100 ] do
                  getDomesticTransferRef mailbox.System <! msg
            | DomesticTransferFailed e ->
               let info = e.Data.BaseInfo

               let failDueToRecipient =
                  match e.Data.Reason with
                  | DomesticTransferFailReason.InvalidAccountInfo ->
                     Some DomesticTransferRecipientFailReason.InvalidAccountInfo
                  | DomesticTransferFailReason.AccountClosed ->
                     Some DomesticTransferRecipientFailReason.ClosedAccount
                  | _ -> None

               match failDueToRecipient with
               | Some failReason ->
                  let cmd =
                     FailDomesticTransferRecipientCommand.create
                        e.OrgId
                        e.InitiatedBy
                        {
                           RecipientId = info.Recipient.RecipientAccountId
                           TransferId = info.TransferId
                           Reason = failReason
                        }
                     |> OrgCommand.FailDomesticTransferRecipient

                  getOrgRef e.OrgId <! OrgMessage.StateChange cmd
               | None -> ()
            | DomesticTransferCompleted e ->
               match e.Data.FromRetry with
               | Some DomesticTransferFailReason.InvalidAccountInfo ->
                  let info = e.Data.BaseInfo

                  let cmd =
                     DomesticTransferRetryConfirmsRecipientCommand.create
                        e.OrgId
                        e.InitiatedBy
                        {
                           RecipientId = info.Recipient.RecipientAccountId
                           TransferId = info.TransferId
                        }
                     |> OrgCommand.DomesticTransferRetryConfirmsRecipient

                  getOrgRef e.OrgId <! OrgMessage.StateChange cmd
               | _ -> ()
            | InternalTransferBetweenOrgsDeposited e ->
               let msg =
                  EmailMessage.InternalTransferBetweenOrgsDeposited(
                     {
                        OrgId = account.OrgId
                        AccountName = account.FullName
                        Amount = e.Data.BaseInfo.Amount
                        SenderBusinessName = e.Data.BaseInfo.Sender.Name
                     }
                  )

               for _ in [ 1..3_000 ] do
                  getEmailRef mailbox.System <! msg
            | CreatedAccount _ ->
               let msg =
                  EmailMessage.AccountOpen(account.FullName, account.OrgId)

               getEmailRef mailbox.System <! msg
            | AccountEvent.AccountClosed _ ->
               getAccountClosureRef mailbox.System
               <! AccountClosureMessage.Register account
            | BillingCycleStarted e ->
               billingCycle getBillingStatementRef getEmailRef mailbox state e
            | PlatformPaymentPaid e ->
               let payee = e.Data.BaseInfo.Payee

               let msg =
                  DepositPlatformPaymentCommand.create
                     (payee.AccountId, payee.OrgId)
                     e.InitiatedBy
                     {
                        BaseInfo = e.Data.BaseInfo
                        PaymentMethod = e.Data.PaymentMethod
                     }
                  |> AccountCommand.DepositPlatformPayment
                  |> AccountMessage.StateChange

               (getAccountRef payee.AccountId) <! msg
            (*
            | ThirdPartyPaymentRequested e ->
               // TODO: Send email requesting payment
            *)
            | _ -> ()

            if
               canProduceAutoTransfer evt
               && not account.AutoTransfersPerTransaction.IsEmpty
            then
               mailbox.Self
               <! AccountMessage.AutoTransferCompute Frequency.PerTransaction

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
                  | Error err -> handleValidationError cmd err
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
                  | Error(cmd, err) -> handleValidationError cmd err
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
   (getOrgRef: OrgId -> IEntityRef<OrgMessage>)
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   (getDomesticTransferActor: ActorSystem -> IActorRef<DomesticTransferMessage>)
   (getEmailActor: ActorSystem -> IActorRef<EmailMessage>)
   (getAccountClosureActor: ActorSystem -> IActorRef<AccountClosureMessage>)
   (getBillingStatementActor: ActorSystem -> IActorRef<BillingStatementMessage>)
   (getSchedulingRef: ActorSystem -> IActorRef<SchedulingActor.Message>)
   =
   let getOrStartInternalTransferActor mailbox =
      InternalTransferRecipientActor.getOrStart mailbox (get system)

   let childProps =
      actorProps
         broadcaster
         getOrStartInternalTransferActor
         getDomesticTransferActor
         getEmailActor
         getAccountClosureActor
         getBillingStatementActor
         getSchedulingRef
         getOrgRef
         getEmployeeRef
         (get system)

   persistenceSupervisor
      supervisorOpts
      isPersistableMessage
      childProps
      persistenceId
