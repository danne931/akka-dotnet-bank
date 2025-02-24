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
open Bank.Org.Domain
open CommandApproval

let private handleValidationError
   (broadcaster: SignalRBroadcast)
   mailbox
   (getEmailActor: ActorSystem -> IActorRef<EmailActor.EmailMessage>)
   (employee: Employee)
   (cmd: EmployeeCommand)
   (err: Err)
   =
   logWarning
      mailbox
      $"Validation fail %s{string err} for command %s{cmd.GetType().Name}"

   broadcaster.employeeEventError employee.OrgId employee.EmployeeId err

   match err with
   | EmployeeStateTransitionError e ->
      match e with
      // Noop
      | DebitAlreadyProgressedToCompletedOrFailed -> ()
      | ExceededDailyDebit(limit, accrued) ->
         let msg =
            EmailActor.EmailMessage.PurchaseFailed(
               {
                  OrgId = employee.OrgId
                  Email = employee.Email
                  Reason =
                     PurchaseFailReason.ExceededDailyCardLimit(limit, accrued)
               }
            )

         getEmailActor (mailbox.System) <! msg
      | ExceededMonthlyDebit(limit, accrued) ->
         let msg =
            EmailActor.EmailMessage.PurchaseFailed(
               {
                  OrgId = employee.OrgId
                  Email = employee.Email
                  Reason =
                     PurchaseFailReason.ExceededMonthlyCardLimit(limit, accrued)
               }
            )

         getEmailActor (mailbox.System) <! msg
      | _ -> ()
   | _ -> ()

let supplementaryCardInfoToCreateCardCommand
   (employee: Employee)
   (initiatedBy: Initiator)
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
   (broadcaster: SignalRBroadcast)
   (getOrgRef: OrgId -> IEntityRef<OrgMessage>)
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (getEmailActor: ActorSystem -> IActorRef<EmailActor.EmailMessage>)
   =
   let handler (mailbox: Eventsourced<obj>) =
      let logError = logError mailbox

      let rec loop (stateOpt: EmployeeSnapshot option) = actor {
         let! msg = mailbox.Receive()

         let state =
            stateOpt
            |> Option.defaultValue { Info = Employee.empty; Events = [] }

         let employee = state.Info

         let handleValidationError =
            handleValidationError broadcaster mailbox getEmailActor employee

         match box msg with
         | Persisted mailbox e ->
            let (EmployeeMessage.Event evt) = unbox e
            let state = Employee.applyEvent state evt
            let employee = state.Info

            broadcaster.employeeEventPersisted evt employee

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
               | EmployeeStatus.PendingInviteApproval _ ->
                  let orgId = employee.OrgId

                  let cmd =
                     ApproveAccessCommand.create
                        employee.CompositeId
                        e.InitiatedBy
                        e.CorrelationId
                        {
                           Name = employee.Name
                           Reference = None
                        }
                     |> InviteEmployee
                     |> ApprovableCommand.PerCommand

                  getOrgRef orgId <! OrgMessage.ApprovableRequest cmd
               | EmployeeStatus.PendingInviteConfirmation token ->
                  getEmailActor mailbox.System
                  <! EmailActor.EmailMessage.EmployeeInvite {
                     OrgId = employee.OrgId
                     Name = employee.Name
                     Email = employee.Email
                     Token = token
                  }
               | _ -> ()
            | EmployeeEvent.AccessApproved e ->
               getEmailActor mailbox.System
               <! EmailActor.EmailMessage.EmployeeInvite {
                  OrgId = employee.OrgId
                  Name = employee.Name
                  Email = employee.Email
                  Token = e.Data.InviteToken
               }
            | EmployeeEvent.AccessRestored _ ->
               match employee.Status with
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
                           e.InitiatedBy
                           info

                     mailbox.Parent() <! (EmployeeMessage.StateChange cmd)
            | EmployeeEvent.UpdatedRole e ->
               match e.Data.CardInfo with
               | Some info ->
                  let cmd =
                     supplementaryCardInfoToCreateCardCommand
                        employee
                        e.InitiatedBy
                        info

                  mailbox.Parent() <! (EmployeeMessage.StateChange cmd)
               | None -> ()
            | EmployeeEvent.PurchasePending e ->
               let accountId = e.Data.Info.AccountId
               let info = e.Data.Info

               let cmd =
                  DebitCommand.create
                     (accountId, e.OrgId)
                     e.CorrelationId
                     e.InitiatedBy
                     {
                        Date = info.Date
                        Amount = info.Amount
                        Merchant = info.Merchant
                        Reference = info.Reference
                        EmployeePurchaseReference = {
                           EmployeeName = employee.Name
                           EmployeeCardNumberLast4 = info.CardNumberLast4
                           EmployeeId =
                              InitiatedById.toEmployeeId info.InitiatedBy.Id
                           CardId = info.CardId
                        }
                     }

               // Notify associated company account actor of
               // debit request and wait for approval before
               // sending a response to issuing card network.
               getAccountRef accountId
               <! AccountMessage.StateChange(AccountCommand.Debit cmd)
            | EmployeeEvent.PurchaseConfirmedByAccount e ->
               // TODO: Notify card network which issued the debit request to our bank.
               ()
            | EmployeeEvent.PurchaseRejectedByAccount e ->
               // TODO: Notify card network which issued the debit request to our bank.
               let msg =
                  EmailActor.EmailMessage.PurchaseFailed(
                     {
                        Email = employee.Email
                        Reason = e.Data.Reason
                        OrgId = employee.OrgId
                     }
                  )

               getEmailActor mailbox.System <! msg
            | _ -> ()

            return! loop <| Some state
         | :? SnapshotOffer as o -> return! loop <| Some(unbox o.Snapshot)
         | :? ConfirmableMessageEnvelope as envelope ->
            match envelope.Message with
            | :? EmployeeMessage as msg ->
               match msg with
               | EmployeeMessage.StateChange cmd ->
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
            | EmployeeMessage.GetEmployee ->
               mailbox.Sender() <! (stateOpt |> Option.map _.Info)
            | EmployeeMessage.Delete ->
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
                     PersistFailed =
                        fun _ err evt sequenceNr ->
                           broadcaster.employeeEventError
                              employee.OrgId
                              employee.EmployeeId
                              (Err.DatabaseError err)

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
   (broadcaster: SignalRBroadcast)
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (getOrgRef: OrgId -> IEntityRef<OrgMessage>)
   =
   persistenceSupervisor
      supervisorOpts
      isPersistableMessage
      (actorProps broadcaster getOrgRef getAccountRef EmailActor.get)
      persistenceId
