[<RequireQualifiedAccess>]
module EmployeeActor

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
open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Transfer.Domain

let handleValidationError
   mailbox
   (getEmailActor: ActorSystem -> IActorRef<EmailActor.EmailMessage>)
   (employee: Employee)
   (cmd: EmployeeCommand)
   (err: Err)
   =
   logWarning
      mailbox
      $"Validation fail %s{string err} for command %s{cmd.GetType().Name}"

   match err with
   | EmployeeStateTransitionError e ->
      match e with
      // Noop
      | DebitAlreadyProgressedToApprovedOrDeclined -> ()
      | ExceededDailyDebit(limit, accrued) ->
         let msg =
            EmailActor.EmailMessage.PurchaseDeclined(
               {
                  OrgId = employee.OrgId
                  Email = employee.Email
                  Reason =
                     PurchaseDeclinedReason.ExceededDailyCardLimit(
                        limit,
                        accrued
                     )
               }
            )

         getEmailActor (mailbox.System) <! msg
      | ExceededMonthlyDebit(limit, accrued) ->
         let msg =
            EmailActor.EmailMessage.PurchaseDeclined(
               {
                  OrgId = employee.OrgId
                  Email = employee.Email
                  Reason =
                     PurchaseDeclinedReason.ExceededMonthlyCardLimit(
                        limit,
                        accrued
                     )
               }
            )

         getEmailActor (mailbox.System) <! msg
      | _ -> ()
   | _ -> ()

let supplementaryCardInfoToCreateCardCommand
   (employee: Employee)
   (initiatedBy: InitiatedById)
   (info: EmployeeInviteSupplementaryCardInfo)
   =
   CreateCardCommand.create {
      AccountId = info.LinkedAccountId
      DailyPurchaseLimit = Some info.DailyPurchaseLimit
      MonthlyPurchaseLimit = Some info.MonthlyPurchaseLimit
      PersonName = employee.Name
      CardNickname = None
      OrgId = employee.OrgId
      EmployeeId = employee.EmployeeId
      CardId = CardId <| Guid.NewGuid()
      Virtual = true
      CardType = CardType.Debit
      InitiatedBy = initiatedBy
   }
   |> EmployeeCommand.CreateCard

let actorProps
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (getEmailActor: ActorSystem -> IActorRef<EmailActor.EmailMessage>)
   (requiresApprovalForCommand: CommandApprovalRule.RequiresApprovalForCommand)
   =
   let handler (mailbox: Eventsourced<obj>) =
      let logError = logError mailbox

      let rec loop (stateOpt: EmployeeWithEvents option) = actor {
         let! msg = mailbox.Receive()

         let state =
            stateOpt
            |> Option.defaultValue { Info = Employee.empty; Events = [] }

         let employee = state.Info

         let handleValidationError =
            handleValidationError mailbox getEmailActor employee

         match box msg with
         | Persisted mailbox e ->
            let (EmployeeMessage.Event evt) = unbox e
            let state = Employee.applyEvent state evt
            let employee = state.Info

            match evt with
            | EmployeeEvent.CreatedAccountOwner e ->
               getEmailActor mailbox.System
               <! EmailActor.EmailMessage.EmployeeInvite {
                  OrgId = employee.OrgId
                  Name = employee.Name
                  Email = employee.Email
                  Token = e.Data.InviteToken
               }
            | EmployeeEvent.CreatedEmployee e ->
               match employee.Status with
               | EmployeeStatus.PendingInviteApproval approval ->
                  let cmd =
                     CommandApprovalProgress.RequestCommandApproval.create
                        employee.CompositeId
                        e.InitiatedById
                        e.CorrelationId
                        {
                           RuleId = approval.RuleId
                           Command =
                              ApprovableCommand.InviteEmployee
                              <| ApproveAccessCommand.create
                                 employee.CompositeId
                                 e.InitiatedById
                                 e.CorrelationId
                                 {
                                    Name = employee.Name
                                    Reference = None
                                 }
                        }
                     |> EmployeeCommand.RequestCommandApproval

                  mailbox.Parent() <! EmployeeMessage.StateChange cmd
               | EmployeeStatus.PendingInviteConfirmation token ->
                  getEmailActor mailbox.System
                  <! EmailActor.EmailMessage.EmployeeInvite {
                     OrgId = employee.OrgId
                     Name = employee.Name
                     Email = employee.Email
                     Token = token
                  }
               | _ -> ()
            | EmployeeEvent.InvitationTokenRefreshed e ->
               getEmailActor mailbox.System
               <! EmailActor.EmailMessage.EmployeeInvite {
                  OrgId = employee.OrgId
                  Name = employee.Name
                  Email = employee.Email
                  Token = e.Data.InviteToken
               }
            | EmployeeEvent.InvitationConfirmed e ->
               for task in employee.OnboardingTasks do
                  match task with
                  | EmployeeOnboardingTask.CreateCard info ->
                     let cmd =
                        supplementaryCardInfoToCreateCardCommand
                           employee
                           e.InitiatedById
                           info

                     mailbox.Parent() <! (EmployeeMessage.StateChange cmd)
            | EmployeeEvent.UpdatedRole e ->
               match e.Data.CardInfo with
               | Some info ->
                  let cmd =
                     supplementaryCardInfoToCreateCardCommand
                        employee
                        e.InitiatedById
                        info

                  mailbox.Parent() <! (EmployeeMessage.StateChange cmd)
               | None -> ()
            | EmployeeEvent.DebitRequested e ->
               let accountId = e.Data.Info.AccountId
               let info = e.Data.Info

               let cmd =
                  DebitCommand.create
                     (accountId, e.OrgId)
                     e.CorrelationId
                     e.InitiatedById
                     {
                        Date = info.Date
                        Amount = info.Amount
                        Origin = info.Origin
                        Reference = info.Reference
                        EmployeePurchaseReference = {
                           EmployeeName = employee.Name
                           EmployeeCardNumberLast4 = info.CardNumberLast4
                           EmployeeId = info.EmployeeId
                           CardId = info.CardId
                        }
                     }

               // Notify associated company account actor of
               // debit request and wait for approval before
               // sending a response to issuing card network.
               getAccountRef accountId
               <! AccountMessage.StateChange(AccountCommand.Debit cmd)
            | EmployeeEvent.DebitApproved e ->
               // TODO: Notify card network which issued the debit request to our bank.
               ()
            | EmployeeEvent.DebitDeclined e ->
               // TODO: Notify card network which issued the debit request to our bank.
               let msg =
                  EmailActor.EmailMessage.PurchaseDeclined(
                     {
                        Email = employee.Email
                        Reason = e.Data.Reason
                        OrgId = employee.OrgId
                     }
                  )

               getEmailActor mailbox.System <! msg
            | EmployeeEvent.DomesticTransferRequested e ->
               let accountId = e.Data.Info.Sender.AccountId

               let cmd =
                  DomesticTransferCommand.create
                     (accountId, e.OrgId)
                     e.CorrelationId
                     e.InitiatedById
                     e.Data.Info

               getAccountRef accountId
               <! AccountMessage.StateChange(
                  AccountCommand.DomesticTransfer cmd
               )
            | _ -> ()

            return! loop <| Some state
         | :? SnapshotOffer as o -> return! loop <| Some(unbox o.Snapshot)
         | :? ConfirmableMessageEnvelope as envelope ->
            match envelope.Message with
            | :? EmployeeMessage as msg ->
               match msg with
               | StateChange cmd ->
                  let validation = Employee.stateTransition state cmd

                  match validation with
                  | Ok(evt, _) ->
                     return!
                        confirmPersist
                           mailbox
                           (EmployeeMessage.Event evt)
                           envelope.ConfirmationId
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
            | GetEmployee -> mailbox.Sender() <! (stateOpt |> Option.map _.Info)
            | ApprovableStateChange cmd ->
               let! approvalRequiredRes =
                  requiresApprovalForCommand
                     cmd
                     (Employee.dailyAccrual state.Events)

               match approvalRequiredRes with
               | Error e ->
                  logError $"Error from requiresApprovalForCommand - {e}"
                  return unhandled ()
               | Ok(Some ruleId) ->
                  let cmd =
                     CommandApprovalProgress.RequestCommandApproval.create
                        employee.CompositeId
                        cmd.InitiatedBy
                        cmd.CorrelationId
                        { RuleId = ruleId; Command = cmd }
                     |> EmployeeCommand.RequestCommandApproval

                  mailbox.Parent() <! EmployeeMessage.StateChange cmd
               | Ok None ->
                  match cmd with
                  | ApprovableCommand.InviteEmployee cmd ->
                     let cmd = EmployeeCommand.ApproveAccess cmd
                     mailbox.Parent() <! EmployeeMessage.StateChange cmd
                  | ApprovableCommand.UpdateEmployeeRole cmd ->
                     let cmd = EmployeeCommand.UpdateRole cmd
                     mailbox.Parent() <! EmployeeMessage.StateChange cmd
                  | ApprovableCommand.FulfillPlatformPayment cmd ->
                     let accountRef =
                        getAccountRef (AccountId.fromEntityId cmd.EntityId)

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
            | Delete ->
               let newState = {
                  state with
                     Info.Status = EmployeeStatus.ReadyForDelete
               }

               return! loop (Some newState) <@> DeleteMessages Int64.MaxValue
         // Event replay on actor start
         | :? EmployeeEvent as e when mailbox.IsRecovering() ->
            return! loop <| Some(Employee.applyEvent state e)
         | msg ->
            PersistentActorEventHandler.handleEvent
               {
                  PersistentActorEventHandler.init with
                     DeleteMessagesSuccess =
                        fun _ ->
                           if
                              employee.Status = EmployeeStatus.ReadyForDelete
                           then
                              logDebug mailbox "<Passivate Employee Actor>"
                              passivate ()
                           else
                              ignored ()
               }
               mailbox
               msg
      }

      loop None

   propsPersist handler

let get
   (sys: ActorSystem)
   (employeeId: EmployeeId)
   : IEntityRef<EmployeeMessage>
   =
   getEntityRef
      sys
      ClusterMetadata.employeeShardRegion
      (EmployeeId.get employeeId)

let isPersistableMessage (msg: obj) =
   match msg with
   | :? EmployeeMessage as msg ->
      match msg with
      | EmployeeMessage.StateChange _ -> true
      | _ -> false
   | _ -> false

let initProps
   (supervisorOpts: PersistenceSupervisorOptions)
   (persistenceId: string)
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (requiresApprovalForCommand: CommandApprovalRule.RequiresApprovalForCommand)
   =
   persistenceSupervisor
      supervisorOpts
      isPersistableMessage
      (actorProps getAccountRef EmailActor.get requiresApprovalForCommand)
      persistenceId
