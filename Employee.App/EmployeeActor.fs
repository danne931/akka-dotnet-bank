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
open SignalRBroadcast
open Email
open Lib.Saga

let private handleValidationError
   (broadcaster: SignalRBroadcast)
   mailbox
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
   (employee: Employee)
   (cmd: EmployeeCommand)
   (err: Err)
   =
   logWarning
      mailbox
      $"Validation fail %s{string err} for command %s{cmd.GetType().Name}"

   broadcaster.employeeEventError
      employee.OrgId
      employee.EmployeeId
      cmd.Envelope.CorrelationId
      err

   let hasPurchaseFail =
      match cmd, err with
      | EmployeeCommand.Purchase cmd, EmployeeStateTransitionError e ->
         match e with
         | CardNotFound -> Some(cmd.Data, PurchaseCardFailReason.CardNotFound)
         | CardExpired -> Some(cmd.Data, PurchaseCardFailReason.CardExpired)
         | CardLocked -> Some(cmd.Data, PurchaseCardFailReason.CardLocked)
         | ExceededDailyDebit(limit, accrued) ->
            Some(
               cmd.Data,
               PurchaseCardFailReason.ExceededDailyCardLimit(limit, accrued)
            )
         | ExceededMonthlyDebit(limit, accrued) ->
            Some(
               cmd.Data,
               PurchaseCardFailReason.ExceededMonthlyCardLimit(limit, accrued)
            )
         | _ -> None
      | _ -> None

   match hasPurchaseFail with
   | Some(purchaseInfo, reason) ->
      let msg =
         (purchaseInfo, reason)
         |> PurchaseSaga.PurchaseSagaStartEvent.PurchaseRejectedByCard
         |> PurchaseSaga.PurchaseSagaEvent.Start
         |> AppSaga.Event.Purchase
         |> AppSaga.sagaMessage purchaseInfo.OrgId purchaseInfo.CorrelationId

      getSagaRef cmd.Envelope.CorrelationId <! msg
   | None -> ()

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
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
   (getEmailActor: ActorSystem -> IActorRef<EmailMessage>)
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
            handleValidationError broadcaster mailbox getSagaRef employee

         let employeeInviteMsg corrId token =
            EmailMessage.create
               employee.OrgId
               corrId
               (EmailInfo.EmployeeInvite {
                  Name = employee.Name
                  Email = employee.Email
                  Token = token
               })

         match box msg with
         | Persisted mailbox e ->
            let (EmployeeMessage.Event evt) = unbox e
            let state = Employee.applyEvent state evt
            let employee = state.Info

            broadcaster.employeeEventPersisted evt employee

            match evt with
            | EmployeeEvent.CreatedAccountOwner e ->
               getEmailActor mailbox.System
               <! employeeInviteMsg e.CorrelationId e.Data.InviteToken
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
                  <! employeeInviteMsg e.CorrelationId token
               | _ -> ()
            | EmployeeEvent.AccessApproved e ->
               getEmailActor mailbox.System
               <! employeeInviteMsg e.CorrelationId e.Data.InviteToken
            | EmployeeEvent.AccessRestored e ->
               match employee.Status with
               | EmployeeStatus.PendingInviteConfirmation token ->
                  getEmailActor mailbox.System
                  <! employeeInviteMsg e.CorrelationId token
               | _ -> ()
            | EmployeeEvent.InvitationTokenRefreshed e ->
               getEmailActor mailbox.System
               <! employeeInviteMsg e.CorrelationId e.Data.InviteToken
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
            | EmployeeEvent.PurchaseApplied e ->
               let msg =
                  e.Data.Info
                  |> PurchaseSaga.PurchaseSagaStartEvent.DeductedCardFunds
                  |> PurchaseSaga.PurchaseSagaEvent.Start
                  |> AppSaga.Event.Purchase
                  |> AppSaga.sagaMessage e.OrgId e.CorrelationId

               getSagaRef e.CorrelationId <! msg
            | EmployeeEvent.PurchaseRefunded e ->
               let msg =
                  PurchaseSaga.PurchaseSagaEvent.PurchaseRefundedToCard
                  |> AppSaga.Event.Purchase
                  |> AppSaga.sagaMessage e.OrgId e.CorrelationId

               getSagaRef e.CorrelationId <! msg
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
                           envelope.ConfirmationId
                           (EmployeeMessage.Event evt)
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
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
   (getOrgRef: OrgId -> IEntityRef<OrgMessage>)
   (getEmailActor: ActorSystem -> IActorRef<EmailMessage>)
   =
   persistenceSupervisor
      supervisorOpts
      isPersistableMessage
      (actorProps broadcaster getOrgRef getSagaRef getEmailActor)
      persistenceId
