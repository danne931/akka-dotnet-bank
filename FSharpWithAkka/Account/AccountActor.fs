[<RequireQualifiedAccess>]
module AccountActor

open System
open System.Threading.Tasks
open Echo
open type Echo.Process
open Microsoft.FSharp.Core.Option
open FSharp.Control

open BankTypes
open Lib.Types

let private PID (id: Guid) = $"accounts_{id}"

type private ActorCommand =
   | StartChildrenCommand of Guid
   | LookupCommand of Guid
   | StateChangeCommand of Command

type AccountRegistry = {
   loadAccount: Guid -> Account.AccountState option Task
   saveAndPublish: OpenEventEnvelope -> unit Task
   startChildActors: Guid -> ProcessId list
   broadcast: AccountEvent * Account.AccountState -> Task
   broadcastError: string -> Task
}

let syncStateChange (cmd: Command) =
   let pid = "@" + PID cmd.EntityId
   tell (pid, cmd |> StateChangeCommand) |> ignore
   ()

let delete (id: Guid) =
   let pid = "@" + PID id
   kill pid |> ignore
   printfn "Killed process %A" pid
   ()

let start
   (initialState: Account.AccountState)
   (registry: AccountRegistry)
   : ProcessId
   =
   let pid =
      spawn<Account.AccountState, ActorCommand> (
         PID initialState.EntityId,
         (fun () -> initialState),
         (fun (account: Account.AccountState) (command: ActorCommand) ->
            match command with
            | StartChildrenCommand id ->
               let pids = registry.startChildActors id
               printfn "AccountActor: Started child actors %A" pids
               account
            | LookupCommand _ ->
               reply account |> ignore
               account
            | StateChangeCommand cmd ->
               let validation = Account.stateTransition account cmd

               match validation with
               | Error(err) ->
                  registry.broadcastError err |> ignore
                  printfn "AccountActor: validation fail %A" err
                  account
               | Ok((event, newState) as validationResult) ->
                  try
                     registry.saveAndPublish(Envelope.unwrap event).Wait()
                     registry.broadcast validationResult |> ignore
                  with err when true ->
                     registry.broadcastError err.Message |> ignore
                     printfn "%A" err
                     reraise ()

                  newState)
      )

   register (pid.Name, pid) |> ignore
   tell (pid, StartChildrenCommand initialState.EntityId) |> ignore
   pid

let lookup
   (registry: AccountRegistry)
   (id: Guid)
   : Account.AccountState option Task
   =
   task {
      let pid = "@" + PID id

      if ActorUtil.isAlive pid then
         let account = ask<Account.AccountState> (pid, (LookupCommand id))
         return Some account
      else
         let! accountOpt = registry.loadAccount id

         if isSome accountOpt then
            start accountOpt.Value registry |> ignore
            return accountOpt
         else
            return None
   }
