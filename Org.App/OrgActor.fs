[<RequireQualifiedAccess>]
module OrgActor

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

let handleValidationError mailbox (err: Err) (cmd: OrgCommand) =
   logWarning
      mailbox
      $"Validation fail %s{string err} for command %s{cmd.GetType().Name}"

let actorProps
   (getEmailActor: ActorSystem -> IActorRef<EmailActor.EmailMessage>)
   //   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   //(getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   //(schedulingRef: IActorRef<SchedulingActor.Message>)
   =
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
            let state = Org.applyEvent state evt
            let org = state.Info

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
   =

   let childProps = actorProps EmailActor.get

   persistenceSupervisor
      supervisorOpts
      isPersistableMessage
      childProps
      persistenceId
