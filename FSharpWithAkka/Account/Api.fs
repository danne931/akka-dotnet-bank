module Bank.Account.Api

open System
open System.Threading.Tasks
open Microsoft.FSharp.Core.Option
open EventStore.Client
open FSharp.Control
open Akkling

open BankTypes
open Bank.Account.Domain
open Lib.Types

let processCommand
   (command: 't :> Command)
   (registry: AccountActor.AccountRegistry)
   (validate: Validators<'t>)
   =
   task {
      let! validation =
         match validate with
         | Validator v -> v command |> Task.FromResult
         | AsyncValidator v -> v command

      let getAccountVal (cmd: Command) =
         AccountActor.lookup registry cmd.EntityId

      if Result.isError validation then
         return validation
      else
         let! accountOpt = getAccountVal command

         if isSome accountOpt then
            registry.syncStateChange command
            return Ok()
         else
            return Error "UnknownAccountId(cmd.entityid)"
   }

let getAccountEvents esClient id =
   EventStoreManager.readStream esClient (Account.streamName id) false

let getAccount
   (getAccountEvents: Guid -> AccountEvent list option Task)
   (accountId: Guid)
   =
   task {
      let! evtsOption = getAccountEvents accountId
      return map Account.foldEventsIntoAccount evtsOption
   }

let softDeleteEvents
   esClient
   (registry: AccountActor.AccountRegistry)
   accountId
   =
   registry.delete accountId
   EventStoreManager.softDelete esClient (Account.streamName accountId)

/// <summary>
/// Get all CreatedAccount events for UI demonstration purposes.
/// Allows demonstration consumer to choose what account to process
/// transactions on.
/// </summary>
let getAccountCreationEvents esClient =
   EventStoreManager.readStream esClient "$et-CreatedAccount" true

let saveAndPublish
   esClient
   (mailbox: Actor<_>)
   ((event, envelope) as props: OpenEventEnvelope)
   =
   task {
      do!
         EventStoreManager.saveAndPublish
            esClient
            (Account.streamName envelope.EntityId)
            props
            None

      match event with
      | InternalTransferRecipient _
      | DomesticTransferRecipient _
      | InternationalTransferRecipient _ -> TransferRecipientActor.start mailbox
      | DebitedTransfer e ->
         printfn "tell child (Transfer recipient actor) (DebitedTransfer)"
         let selection = select mailbox TransferRecipientActor.ActorName
         selection <! e
      | _ -> ()
   }

let createAccount
   esClient
   (registry: AccountActor.AccountRegistry)
   (validate: CreateAccountCommand -> Result<CreateAccountCommand, string>)
   (command: CreateAccountCommand)
   =
   task {
      let validation = command |> validate |> Result.map (fun c -> c.EntityId)

      if Result.isError validation then
         return validation
      else
         let evt = CreatedAccountEvent.create command

         do!
            EventStoreManager.saveAndPublish
               esClient
               (Account.streamName evt.EntityId)
               (evt |> Envelope.wrap |> Envelope.unwrap)
               // Create event stream only if it doesn't already exist.
               (Some StreamState.NoStream)

         let acct = Account.create evt
         AccountActor.start acct registry |> ignore
         return Ok evt.EntityId
   }
