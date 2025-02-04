[<RequireQualifiedAccess>]
module OrgActor

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

let handleValidationError mailbox (err: Err) (cmd: OrgCommand) =
   logWarning
      mailbox
      $"Validation fail %s{string err} for command %s{cmd.GetType().Name}"

// Sends the ApprovableCommand to the appropriate Account or Employee actor
// when the approval process is complete or no approval required.
let private sendApprovedCommand
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (cmd: ApprovableCommand)
   =
   match cmd with
   | ApprovableCommand.PerCommand c ->
      match c with
      | InviteEmployee cmd ->
         let employeeRef = getEmployeeRef (EmployeeId.fromEntityId cmd.EntityId)

         let cmd = EmployeeCommand.ApproveAccess cmd
         employeeRef <! EmployeeMessage.StateChange cmd
      | UpdateEmployeeRole cmd ->
         let employeeRef = getEmployeeRef (EmployeeId.fromEntityId cmd.EntityId)

         let cmd = EmployeeCommand.UpdateRole cmd
         employeeRef <! EmployeeMessage.StateChange cmd
   | ApprovableCommand.AmountBased c ->
      match c with
      | FulfillPlatformPayment cmd ->
         let accountRef = getAccountRef (AccountId.fromEntityId cmd.EntityId)

         let cmd = AccountCommand.FulfillPlatformPayment cmd
         accountRef <! AccountMessage.StateChange cmd
      | DomesticTransfer cmd ->
         let accountRef = getAccountRef (AccountId.fromEntityId cmd.EntityId)
         let cmd = AccountCommand.DomesticTransfer cmd
         accountRef <! AccountMessage.StateChange cmd
      | InternalTransferBetweenOrgs cmd ->
         let accountRef = getAccountRef cmd.Data.Sender.AccountId
         let cmd = AccountCommand.InternalTransferBetweenOrgs cmd
         accountRef <! AccountMessage.StateChange cmd

// Does the approvable command contribute to the OrgAccrualMetric
// daily tally?
let private hasAccruableTransaction
   (cmd: ApprovableCommand)
   : OrgAccrualMetric option
   =
   match cmd with
   | ApprovableCommand.PerCommand _ -> None
   | ApprovableCommand.AmountBased c ->
      Some {
         TransactionAmount = cmd.Amount
         EventType =
            match c with
            | FulfillPlatformPayment _ -> OrgAccrualMetricEventType.PaymentPaid
            | DomesticTransfer _ -> OrgAccrualMetricEventType.DomesticTransfer
            | InternalTransferBetweenOrgs _ ->
               OrgAccrualMetricEventType.InternalTransferBetweenOrgs
         CorrelationId = cmd.CorrelationId
         InitiatedById = cmd.InitiatedBy
         Timestamp = cmd.Timestamp
      }

// Add an OrgAccrualMetric to the daily tally so we can determine if
// an incoming ApprovableCommand should enter the approvable workflow
// due to exceeding the daily limit for a that command type.
let private updateStateWithAccruableTransaction
   (state: OrgWithEvents)
   (metrics: OrgAccrualMetric)
   =
   {
      state with
         AccrualMetrics =
            state.AccrualMetrics |> Map.add metrics.CorrelationId metrics
   }

// Undo the accrual associated with a command if the command is declined.
let private updateStateWithAccrualReversal
   (state: OrgWithEvents)
   (correlationId: CorrelationId)
   =
   {
      state with
         AccrualMetrics = state.AccrualMetrics |> Map.remove correlationId
   }

// Early termination of progress workflows will result in their
// ApprovableCommands being initiated, similar to what is expected when
// an approval workflow obtains all necessary approvals.
// This happens if the associated rule is deleted or an approval rule is
// edited such that the list of required approvers is reduced to at or below
// the count of approvals received thus far.
let private terminateProgressAssociatedWithRule
   (mailbox: IActorRef<OrgMessage>)
   (progressPertainingToRule: CommandApprovalProgress.T seq)
   (reason: CommandApprovalProgress.CommandApprovalTerminationReason)
   =
   for progress in progressPertainingToRule do
      let cmd =
         CommandApprovalProgress.TerminateCommandApproval.create progress.OrgId {
            RuleId = progress.RuleId
            ProgressId = progress.ProgressId
            Command = progress.CommandToInitiateOnApproval
            Reason = reason
         }
         |> OrgCommand.TerminateCommandApproval

      mailbox <! OrgMessage.StateChange cmd

let actorProps
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   =
   let sendApprovedCommand = sendApprovedCommand getEmployeeRef getAccountRef

   let handler (mailbox: Eventsourced<obj>) =
      let logError = logError mailbox

      let rec loop (stateOpt: OrgWithEvents option) = actor {
         let! msg = mailbox.Receive()

         let state = stateOpt |> Option.defaultValue OrgWithEvents.empty

         let org = state.Info

         match box msg with
         | Persisted mailbox e ->
            let (OrgMessage.Event evt) = unbox e
            let previousState = state
            let state = Org.applyEvent state evt

            match evt with
            | OrgCreated e ->
               // TODO: Research onboarding requirements for registering a
               // business bank account in the US.  For now, just finalize the
               // onboarding process to transition the org to an Active status.
               let cmd =
                  FinalizeOrgOnboardingCommand.create {
                     OrgId = e.OrgId
                     InitiatedBy = e.InitiatedById
                     EmployerIdentificationNumber = 123456789
                  }
                  |> OrgCommand.FinalizeOrgOnboarding

               mailbox.Parent() <! OrgMessage.StateChange cmd
            | CommandApprovalRuleConfigured e ->
               let newRuleConfig = e.Data.Rule
               let approversCnt = newRuleConfig.Approvers.Length

               let previousApproversCnt =
                  previousState.Info.CommandApprovalRules
                  |> Map.tryFind newRuleConfig.RuleId
                  |> Option.map _.Approvers.Length
                  |> Option.defaultValue approversCnt

               // If an approver was removed from the rule config then we should
               // see if there are any approval processes which can be
               // terminated to have their commands initiated immediately.
               if approversCnt < previousApproversCnt then
                  let progressPertainingToRule =
                     state.Info.CommandApprovalProgress.Values
                     |> Seq.filter (fun p ->
                        p.RuleId = newRuleConfig.RuleId
                        && p.ApprovedBy.Length = newRuleConfig.Approvers.Length)

                  terminateProgressAssociatedWithRule
                     (mailbox.Parent())
                     progressPertainingToRule
                     CommandApprovalProgress.CommandApprovalTerminationReason.AssociatedRuleApproverDeleted
            | CommandApprovalRuleDeleted e ->
               let progressPertainingToRule =
                  state.Info.CommandApprovalProgress.Values
                  |> Seq.filter (fun p -> p.RuleId = e.Data.RuleId)

               terminateProgressAssociatedWithRule
                  (mailbox.Parent())
                  progressPertainingToRule
                  CommandApprovalProgress.CommandApprovalTerminationReason.AssociatedRuleDeleted
            | CommandApprovalProcessCompleted e ->
               sendApprovedCommand e.Data.Command
            | CommandApprovalTerminated e -> sendApprovedCommand e.Data.Command
            | CommandApprovalDeclined e ->
               match e.Data.Command with
               | ApprovableCommand.PerCommand(InviteEmployee cmd) ->
                  let employeeId = EmployeeId.fromEntityId cmd.EntityId

                  let msg =
                     CancelInvitationCommand.create
                        (employeeId, e.OrgId)
                        e.InitiatedById
                        {
                           Reason =
                              Some
                                 $"Employee invite declined by {e.Data.DeclinedBy.EmployeeName}"
                        }
                     |> EmployeeCommand.CancelInvitation
                     |> EmployeeMessage.StateChange

                  (getEmployeeRef employeeId) <! msg
               | ApprovableCommand.AmountBased(FulfillPlatformPayment cmd) ->
                  let accountRef =
                     getAccountRef (AccountId.fromEntityId cmd.EntityId)

                  let msg =
                     DeclinePlatformPaymentCommand.create e.InitiatedById {
                        RequestedPayment = cmd.Data.RequestedPayment
                        Reason =
                           Some
                              $"Outgoing payment declined by {e.Data.DeclinedBy.EmployeeName}"
                     }
                     |> AccountCommand.DeclinePlatformPayment
                     |> AccountMessage.StateChange

                  accountRef <! msg
               | _ -> ()
            | _ -> ()

            let state =
               match evt with
               | CommandApprovalRequested e ->
                  match hasAccruableTransaction e.Data.Command with
                  | Some accrual ->
                     updateStateWithAccruableTransaction state accrual
                  | None -> state
               | CommandApprovalDeclined e ->
                  match hasAccruableTransaction e.Data.Command with
                  | Some accrual ->
                     updateStateWithAccrualReversal state accrual.CorrelationId
                  | None -> state
               | _ -> state

            let effect = loop (Some state)

            match state.Events.Length % 10 with
            | 0 -> return! effect <@> SaveSnapshot state
            | _ -> return! effect
         | :? SnapshotOffer as o -> return! loop <| Some(unbox o.Snapshot)
         | :? ConfirmableMessageEnvelope as envelope ->
            let unknownMsg msg =
               logError $"Unknown message in ConfirmableMessageEnvelope - {msg}"
               unhandled ()

            match envelope.Message with
            | :? OrgMessage as msg ->
               match msg with
               | OrgMessage.StateChange cmd ->
                  let validation = Org.stateTransition state cmd

                  match validation with
                  | Ok(evt, _) ->
                     return!
                        confirmPersist
                           mailbox
                           (OrgMessage.Event evt)
                           envelope.ConfirmationId
                  | Error err -> handleValidationError mailbox err cmd
               | msg -> return unknownMsg msg
            | msg -> return unknownMsg msg
         | :? OrgMessage as msg ->
            match msg with
            | OrgMessage.GetOrg ->
               let org = stateOpt |> Option.map _.Info
               mailbox.Sender() <! org
            | OrgMessage.GetCommandApprovalDailyAccrualByInitiatedBy initiatedBy ->
               let accrual = Org.dailyAccrual initiatedBy state.AccrualMetrics
               mailbox.Sender() <! accrual
            | OrgMessage.ApprovableRequest _ when org.Status <> OrgStatus.Active ->
               let errMsg =
                  $"Attempt to initiate an approvable request for an inactive org {org.Name}-{org.OrgId}"

               logError errMsg
               return unhandled ()
            | OrgMessage.ApprovableRequest cmd when
               org.Status = OrgStatus.Active
               ->
               // If the command requires approval then initiate the command
               // approval workflow.  Otherwise, forward the command to the
               // appropriate account or employee actor for processing.
               match Org.commandRequiresApproval cmd state with
               | Some rule ->
                  let cmd =
                     CommandApprovalProgress.RequestCommandApproval.create
                        org.OrgId
                        cmd.InitiatedBy
                        cmd.CorrelationId
                        {
                           RuleId = rule.RuleId
                           Command = cmd
                           Requester = {
                              // NOTE:
                              // Currently not able to nicely reference the EmployeeName so
                              // the snapshot will be saved with empty string for the name.
                              // This is currently okay since the read models are saved
                              // with just the EmployeeId anyway.  When it comes time to
                              // fetch the approval progress read model for display in the browser,
                              // the EmployeeId field of the command_approval_progress table
                              // is used to join with the employee table and
                              // select the name field on the employee record.
                              // TODO: Consider replacing the InitiatedById property on
                              // Command<T> & BankEvent<T> with an InitiatedBy type which
                              // contains both the id and the name.
                              EmployeeName = ""
                              EmployeeId =
                                 InitiatedById.toEmployeeId cmd.InitiatedBy
                           }
                           RequesterIsConfiguredAsAnApprover =
                              CommandApprovalRule.isRequesterOneOfManyApprovers
                                 cmd.InitiatedBy
                                 rule
                        }
                     |> OrgCommand.RequestCommandApproval

                  mailbox.Parent() <! OrgMessage.StateChange cmd
               | None ->
                  sendApprovedCommand cmd

                  match hasAccruableTransaction cmd with
                  | Some accrual ->
                     let state =
                        updateStateWithAccruableTransaction state accrual

                     return! loop (Some state)
                  | _ -> ()
         // Event replay on actor start
         | :? OrgEvent as e when mailbox.IsRecovering() ->
            return! loop <| Some(Org.applyEvent state e)
         | msg ->
            PersistentActorEventHandler.handleEvent
               {
                  PersistentActorEventHandler.init with
                     PersistFailed =
                        fun _ err evt sequenceNr ->
                           let msg =
                              $"Persistence failed in org actor for event: {evt}. Error: {err}"

                           logError msg
                           ignored ()
                     LifecyclePostStop =
                        fun _ ->
                           logInfo mailbox $"ORG POSTSTOP {org.Name}"
                           SaveSnapshot state
               }
               mailbox
               msg
      }

      loop None

   propsPersist handler

let get (sys: ActorSystem) (orgId: OrgId) : IEntityRef<OrgMessage> =
   getEntityRef sys ClusterMetadata.orgShardRegion (OrgId.get orgId)

let isPersistableMessage (msg: obj) =
   match msg with
   | :? OrgMessage as msg ->
      match msg with
      | OrgMessage.StateChange _ -> true
      | _ -> false
   | _ -> false

let initProps
   (system: ActorSystem)
   (supervisorOpts: PersistenceSupervisorOptions)
   (persistenceId: string)
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   =
   let childProps = actorProps getEmployeeRef getAccountRef

   persistenceSupervisor
      supervisorOpts
      isPersistableMessage
      childProps
      persistenceId
