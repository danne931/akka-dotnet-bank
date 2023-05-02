[<RequireQualifiedAccess>]
module AccountActor

open System
open System.Threading.Tasks
open Microsoft.FSharp.Core.Option
open FSharp.Control
open Akka.Actor
open Akkling

open BankTypes
open Lib.Types
open ActorUtil

let actorName (id: Guid) = $"accounts_{id}"
let PID (id: Guid) = $"akka://bank/user/{actorName id}"

type AccountRegistry =
   {
      loadAccount: Guid -> Account.AccountState option Task
      saveAndPublish: Actor<ActorCommand> -> OpenEventEnvelope -> unit Task
      startChildActors: Actor<ActorCommand> -> Guid -> IActorRef<Guid> list
      broadcast: AccountEvent * Account.AccountState -> Task
      broadcastError: string -> Task
      system: ActorSystem
   }

   member x.delete(id: Guid) =
      let aref = select x.system (PID id)
      stop aref |> ignore

   member x.syncStateChange(cmd: Command) =
      let ref = select x.system (PID cmd.EntityId)
      ref <! ActorStateChangeCommand.init cmd

let start (initialState: Account.AccountState) (registry: AccountRegistry) =
   let rec handler
      (account: Account.AccountState)
      (mailbox: Actor<ActorCommand>)
      =
      function
      | StartChildrenCommand(id: Guid) ->
         let refs = registry.startChildActors mailbox id
         printfn "AccountActor: Started child actors %A" refs
         ignored ()
      | LookupCommand _ ->
         mailbox.Sender() <! account
         ignored ()
      | StateChangeCommand cmd ->
         let validation = Account.stateTransition account cmd

         match validation with
         | Error(err) ->
            registry.broadcastError err |> ignore
            printfn "AccountActor: validation fail %A" err
            become (handler account mailbox)
         | Ok((event, newState) as validationResult) ->
            try
               (registry.saveAndPublish mailbox (Envelope.unwrap event)).Wait()
               registry.broadcast validationResult |> ignore
            with err when true ->
               registry.broadcastError err.Message |> ignore
               printfn "%A" err
               reraise ()

            become (handler newState mailbox)

   let aref =
      spawn
         registry.system
         (actorName initialState.EntityId)
         (initialState |> handler |> actorOf2 |> props)

   aref <! StartChildrenCommand initialState.EntityId
   aref

let lookup
   (registry: AccountRegistry)
   (id: Guid)
   : Account.AccountState option Task
   =
   task {
      let! aref = getActorRef registry.system (PID id)

      if isSome aref then
         let! account = aref.Value <? (LookupCommand id)
         return Some account
      else
         let! accountOpt = registry.loadAccount id

         return
            accountOpt
            |> Option.map (fun acct ->
               start acct registry |> ignore
               acct)
   }
