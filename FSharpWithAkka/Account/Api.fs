module Bank.Account.Api

open System
open System.Threading.Tasks
open Microsoft.FSharp.Core.Option
open EventStore.Client
open FSharp.Control

open BankTypes
open Bank.Account.Domain
open Bank.Transfer.Domain
open Lib.Types

(*
using static Lib.Validators;
using static Bank.Account.Domain.Errors;
using Bank.Transfer.Actors;
*)

type Valida =
   | Validator of Validator<AccountCommand>
   | AsyncValidator of AsyncValidator<AccountCommand>

let processCommand
   (command)
   (registry: AccountActor.AccountRegistry)
   (validate: Valida)
   : Task<Result<AccountCommand, string>>
   =
   task {
      let! validation =
         match validate with
         | Validator v -> v command |> Task.FromResult
         | AsyncValidator v -> v command

      let getAccountVal (cmd: AccountCommand) =
         AccountActor.Lookup registry Guid.Empty //cmd.EntityId
      //|> Option.bind (fun opt ->
      //      Option.)
      //opt.ToValidation(Error "UnknownAccountId(cmd.EntityId)"))

      if Result.isError validation then
         return validation
      else
         let! accountOpt = getAccountVal command

         if isSome accountOpt then
            AccountActor.syncStateChange command
            return Ok command
         else
            return Error "UnknownAccountId(cmd.entityid)"
   }

let getAccountEvents (esClient: EventStoreClient) id =
   EventStoreManager.readStream esClient (Account.streamName id) false

let private foldEventsIntoAccount evts =
   let (CreatedAccount createdEvt) = List.head evts
   List.fold Account.applyEvent (Account.create createdEvt) (List.tail evts)

let getAccount
   (getAccountEvents: Guid -> AccountEvent list option Task)
   (accountId: Guid)
   =
   task {
      let! evtsOption = getAccountEvents accountId
      return map foldEventsIntoAccount evtsOption
   }

let accountExists (es: EventStoreClient) (id: Guid) =
   EventStoreManager.exists es (Account.streamName id)

let softDeleteEvents (client: EventStoreClient) (accountId: Guid) =
   //AccountActor.delete accountId
   EventStoreManager.softDelete client (Account.streamName accountId)

/// <summary>
/// Get all CreatedAccount events for UI demonstration purposes.
/// Allows demonstration consumer to choose what account to process
/// transactions on.
/// </summary>
let getAccountCreationEvents (esClient: EventStoreClient) =
   EventStoreManager.readStream esClient "$et-CreatedAccount" true

let saveAndPublish
   (es: EventStoreClient)
   ((event, envelope) as props: OpenEventEnvelope)
   =
   task {
      do!
         EventStoreManager.saveAndPublish
            es
            (Account.streamName envelope.EntityId)
            props
            None

      (*
      match box evt with
      | :? TransferRecipientEvent ->
         printf "start transfer recipient actor"
         //TransferRecipientActor.start()
         ()
      | :? DebitedTransfer ->
         printf "tell child (Transfer recipient actor) (DebitedTransfer)"
         //tellChild(TransferRecipientActor.ActorName, (DebitedTransfer) evt);
         ()
      *)
      return ()
   }

let createAccount
   (es: EventStoreClient)
   //(registry: AccountRegistry)
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
               es
               (Account.streamName evt.EntityId)
               (Envelope.unwrap evt)
               // Create event stream only if it doesn't already exist.
               (Some StreamState.NoStream)

         let acct = Account.create evt
         //AccountActor.start acct registry
         return Ok evt.EntityId
   }
