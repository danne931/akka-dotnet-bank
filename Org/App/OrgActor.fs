[<RequireQualifiedAccess>]
module OrgActor

open Akka.Actor
open Akka.Persistence
open Akka.Persistence.Extras
open Akka.Delivery
open Akkling
open Akkling.Persistence
open Akkling.Cluster.Sharding
open System

open Lib.SharedTypes
open Lib.Types
open ActorUtil
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Payment.Domain
open Bank.Employee.Domain
open CommandApproval
open SignalRBroadcast
open OrgOnboardingSaga
open EmployeeOnboardingSaga
open BankActorRegistry

// Sends the ApprovableCommand to the appropriate Account or Employee actor
// when the approval process is complete or no approval required.
let private sendApprovedCommand
   (registry:
      #IEmployeeGuaranteedDeliveryActor & #IAccountGuaranteedDeliveryActor)
   (orgRef: IActorRef<OrgMessage>)
   (cmd: ApprovableCommand)
   =
   let getEmployeeRef = registry.EmployeeGuaranteedDeliveryActor
   let getAccountRef = registry.AccountGuaranteedDeliveryActor

   match cmd with
   | ApprovableCommand.PerCommand c ->
      match c with
      | InviteEmployee cmd ->
         let msg =
            { cmd with Timestamp = DateTime.UtcNow }
            |> EmployeeCommand.ApproveAccess
            |> EmployeeMessage.StateChange
            |> GuaranteedDelivery.message cmd.EntityId.Value

         getEmployeeRef () <! msg
      | UpdateEmployeeRole cmd ->
         let msg =
            { cmd with Timestamp = DateTime.UtcNow }
            |> EmployeeCommand.UpdateRole
            |> EmployeeMessage.StateChange
            |> GuaranteedDelivery.message cmd.EntityId.Value

         getEmployeeRef () <! msg
      | UnlockCard cmd ->
         let msg =
            { cmd with Timestamp = DateTime.UtcNow }
            |> EmployeeCommand.UnlockCard
            |> EmployeeMessage.StateChange
            |> GuaranteedDelivery.message cmd.EntityId.Value

         getEmployeeRef () <! msg
      | ManageApprovalRule cmd ->
         let info = cmd.Data

         let cmd =
            match info with
            | ManageApprovalRuleInput.Delete(rule, initiator) ->
               CommandApprovalRule.DeleteApprovalRuleCommand.create initiator {
                  RuleId = rule.RuleId
                  OrgId = rule.OrgId
                  CommandType = rule.CommandType
               }
               |> OrgCommand.DeleteApprovalRule
            | ManageApprovalRuleInput.CreateOrEdit(rule, _) ->
               CommandApprovalRule.ConfigureApprovalRuleCommand.create
                  cmd.OrgId
                  cmd.InitiatedBy
                  rule
               |> OrgCommand.ConfigureApprovalRule

         orgRef <! OrgMessage.StateChange cmd
   | ApprovableCommand.AmountBased c ->
      match c with
      | DomesticTransfer cmd ->
         let msg =
            { cmd with Timestamp = DateTime.UtcNow }
            |> AccountCommand.DomesticTransfer
            |> AccountMessage.StateChange
            |> GuaranteedDelivery.message cmd.EntityId.Value

         getAccountRef () <! msg
      | InternalTransferBetweenOrgs cmd ->
         let msg =
            { cmd with Timestamp = DateTime.UtcNow }
            |> AccountCommand.InternalTransferBetweenOrgs
            |> AccountMessage.StateChange
            |> GuaranteedDelivery.message cmd.EntityId.Value

         getAccountRef () <! msg

// Does the approvable command contribute to the OrgAccrualMetric daily tally?
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
            | DomesticTransfer _ -> OrgAccrualMetricEventType.DomesticTransfer
            | InternalTransferBetweenOrgs _ ->
               OrgAccrualMetricEventType.InternalTransferBetweenOrgs
         CorrelationId = cmd.CorrelationId
         InitiatedBy = cmd.InitiatedBy
         Timestamp = cmd.Timestamp
      }

// Add an OrgAccrualMetric to the daily tally so we can determine if
// an incoming ApprovableCommand should enter the approvable workflow
// due to exceeding the daily limit for a particular employee issuing
// a command.
let private updateStateWithAccruableTransaction
   (state: OrgSnapshot)
   (metrics: OrgAccrualMetric)
   =
   {
      state with
         AccrualMetrics =
            state.AccrualMetrics |> Map.add metrics.CorrelationId metrics
   }

// Undo the accrual associated with a command if the command is declined.
let private updateStateWithAccrualReversal
   (state: OrgSnapshot)
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

let onPersisted
   (registry: #ISagaActor & #ISagaGuaranteedDeliveryActor)
   (mailbox: Eventsourced<obj>)
   (previousState: OrgSnapshot)
   (state: OrgSnapshot)
   (evt: OrgEvent)
   =
   let sendApprovedCommand = sendApprovedCommand registry (mailbox.Parent())

   match evt with
   | OnboardingApplicationSubmitted e ->
      let msg =
         OrgOnboardingSagaStartEvent.ApplicationSubmitted e
         |> AppSaga.Message.orgOnboardStart e.OrgId e.CorrelationId

      registry.SagaGuaranteedDeliveryActor() <! msg
   | OnboardingFinished e ->
      let msg =
         OrgOnboardingSagaEvent.OrgActivated
         |> AppSaga.Message.orgOnboard e.OrgId e.CorrelationId

      registry.SagaActor e.CorrelationId <! msg
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
   | CommandApprovalRequested e ->
      match e.Data.Command with
      | ApprovableCommand.PerCommand(ApprovableCommandPerCommand.InviteEmployee _) ->
         let msg =
            EmployeeOnboardingSagaEvent.AccessRequestPending
            |> AppSaga.Message.employeeOnboard e.OrgId e.CorrelationId

         registry.SagaActor e.CorrelationId <! msg
      | _ -> ()
   | CommandApprovalProcessCompleted e -> sendApprovedCommand e.Data.Command
   | CommandApprovalTerminated e -> sendApprovedCommand e.Data.Command
   | CommandApprovalDeclined e ->
      match e.Data.Command with
      | ApprovableCommand.PerCommand(InviteEmployee cmd) ->
         let msg =
            CancelInvitationCommand.create
               (EmployeeId.fromEntityId cmd.EntityId, e.OrgId)
               e.InitiatedBy
               e.CorrelationId
               {
                  Reason =
                     Some
                        $"Employee invite declined by {e.Data.DeclinedBy.EmployeeName}"
               }
            |> EmployeeCommand.CancelInvitation
            |> EmployeeMessage.StateChange
            |> GuaranteedDelivery.message cmd.EntityId.Value

         registry.EmployeeGuaranteedDeliveryActor() <! msg
      | ApprovableCommand.AmountBased(InternalTransferBetweenOrgs cmd) ->
         let info = cmd.Data

         match info.OriginatedFromPaymentRequest with
         | Some paymentId ->
            let initiatorOfPaymentDecline = e.InitiatedBy

            let msg =
               DeclinePaymentRequestCommand.create initiatorOfPaymentDecline {
                  Reason =
                     Some
                        $"Outgoing payment declined by {e.Data.DeclinedBy.EmployeeName}"
                  PayerName = info.Sender.Name
                  SharedDetails = {
                     Payee = {
                        OrgId = info.Recipient.OrgId
                        OrgName = info.Recipient.Name
                        ParentAccountId = info.Recipient.ParentAccountId
                        AccountId = info.Recipient.AccountId
                     }
                     Amount = info.Amount
                     Id = paymentId
                     DueAt = DateTime.UtcNow
                     Memo = ""
                  }
               }
               |> AccountCommand.DeclinePaymentRequest
               |> AccountMessage.StateChange
               |> GuaranteedDelivery.message
                     info.Recipient.ParentAccountId.Value

            registry.AccountGuaranteedDeliveryActor() <! msg
         | None -> ()
      | _ -> ()
   | _ -> ()

let actorProps
   registry
   (broadcaster: SignalRBroadcast)
   (guaranteedDeliveryConsumerControllerRef:
      IActorRef<ConsumerController.IConsumerCommand<OrgMessage>>)
   =
   let handler (mailbox: Eventsourced<obj>) =
      let logError = logError mailbox

      let sendApprovedCommand = sendApprovedCommand registry (mailbox.Parent())

      let handleValidationError orgId (cmd: OrgCommand) (err: Err) =
         broadcaster.orgEventError orgId cmd.Envelope.CorrelationId err

         logWarning
            mailbox
            $"Validation fail %s{string err} for command %s{cmd.GetType().Name}"

      let rec loop (stateOpt: OrgSnapshot option) = actor {
         let! msg = mailbox.Receive()

         let state = stateOpt |> Option.defaultValue OrgSnapshot.empty

         let org = state.Info

         match msg with
         | Persisted mailbox (:? OrgEvent as evt) ->
            let previousState = state
            let state = Org.applyEvent state evt

            broadcaster.orgEventPersisted evt state.Info

            onPersisted registry mailbox previousState state evt

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
         | :? ConsumerController.Delivery<OrgMessage> as msg ->
            GuaranteedDelivery.ack msg

            // Send message to parent actor (Persistence Supervisor)
            // for message command to confirmed event persistence.
            mailbox.Parent() <! msg.Message

            return ignored ()
         | :? ConfirmableMessageEnvelope as envelope ->
            let unknownMsg msg =
               logError $"Unknown message in ConfirmableMessageEnvelope - {msg}"
               unhandled ()

            let confirmPersist =
               PersistenceSupervisor.confirmPersist
                  mailbox
                  envelope.ConfirmationId

            match envelope.Message with
            | :? OrgMessage as msg ->
               match msg with
               | OrgMessage.StateChange cmd ->
                  let validation = Org.stateTransition state cmd

                  match validation with
                  | Ok(evt, _) -> return! confirmPersist evt
                  | Error err -> handleValidationError org.OrgId cmd err
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
            | OrgMessage.ApprovableRequest cmd ->
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
                              EmployeeName = cmd.InitiatedBy.Name
                              EmployeeId = cmd.InitiatedBy.Id.AsEmployeeId
                           }
                           RequesterIsConfiguredAsAnApprover =
                              CommandApprovalRule.isRequesterOneOfManyApprovers
                                 cmd.InitiatedBy.Id
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
            // Some messages are sent through traditional AtMostOnceDelivery via
            // a reference to the cluster sharded entity ref rather than Akka.Delivery
            // AtLeastOnceDelivery producer ref so will not hit the
            // ConsumerController.Delivery match case above. Need to send message
            // to parent actor (Persistence Supervisor) so the command gets wrapped in a
            // ConfirmableMessageEnvelope for Akka.Persistence.Extras.Confirmation
            | OrgMessage.StateChange _ ->
               mailbox.Parent() <! msg
               return ignored ()
         // Event replay on actor start
         | :? OrgEvent as e when mailbox.IsRecovering() ->
            return! loop <| Some(Org.applyEvent state e)
         | msg ->
            PersistentActorEventHandler.handleEvent
               {
                  PersistentActorEventHandler.init with
                     LifecyclePreStart =
                        fun _ ->
                           logDebug mailbox $"ORG PRESTART"

                           // Start Guaranteed Delivery Consumer Controller
                           guaranteedDeliveryConsumerControllerRef
                           <! new ConsumerController.Start<OrgMessage>(
                              untyped mailbox.Self
                           )

                           ignored ()
                     LifecyclePostStop =
                        fun _ ->
                           logDebug mailbox $"ORG POSTSTOP {org.Name}"
                           SaveSnapshot state
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
   (guaranteedDeliveryConsumerControllerRef:
      IActorRef<ConsumerController.IConsumerCommand<OrgMessage>>)
   =
   let childProps =
      actorProps registry broadcaster guaranteedDeliveryConsumerControllerRef

   PersistenceSupervisor.create {
      EnvConfig = supervisorEnvConfig
      ChildProps = childProps.ToProps()
      PersistenceId = persistenceId
      CompatibleWithGuaranteedDelivery = true
      IsPersistableMessage =
         function
         | :? OrgMessage as msg -> msg.IsStateChange
         | _ -> false
   }
