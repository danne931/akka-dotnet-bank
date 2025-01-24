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
   | ApprovableCommand.InviteEmployee cmd ->
      let employeeRef = getEmployeeRef (EmployeeId.fromEntityId cmd.EntityId)

      let cmd = EmployeeCommand.ApproveAccess cmd
      employeeRef <! EmployeeMessage.StateChange cmd
   | ApprovableCommand.UpdateEmployeeRole cmd ->
      let employeeRef = getEmployeeRef (EmployeeId.fromEntityId cmd.EntityId)

      let cmd = EmployeeCommand.UpdateRole cmd
      employeeRef <! EmployeeMessage.StateChange cmd
   | ApprovableCommand.FulfillPlatformPayment cmd ->
      let accountRef = getAccountRef (AccountId.fromEntityId cmd.EntityId)

      let cmd = AccountCommand.FulfillPlatformPayment cmd
      accountRef <! AccountMessage.StateChange cmd
   | ApprovableCommand.DomesticTransfer cmd ->
      let accountId = cmd.Data.Sender.AccountId

      let cmd =
         DomesticTransferCommand.create
            (accountId, cmd.OrgId)
            cmd.CorrelationId
            cmd.InitiatedBy
            cmd.Data
         |> AccountCommand.DomesticTransfer

      getAccountRef accountId <! AccountMessage.StateChange cmd
   | ApprovableCommand.InternalTransferBetweenOrgs cmd ->
      let accountRef = getAccountRef cmd.Data.Sender.AccountId
      let cmd = AccountCommand.InternalTransferBetweenOrgs cmd
      accountRef <! AccountMessage.StateChange cmd

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

         let state =
            stateOpt |> Option.defaultValue { Info = Org.empty; Events = [] }

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
               let newRuleConfig = e.Data
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
               | ApprovableCommand.InviteEmployee cmd ->
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
               | _ -> ()
            | _ -> ()

            return! loop <| Some state
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
               mailbox.Sender() <! (stateOpt |> Option.map _.Info)
            | ApprovableRequest cmd ->
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
               | None -> sendApprovedCommand cmd
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
