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

type private Command =
   | StartChildrenCommand of Guid
   | LookupCommand of Guid
   | AccountCommand of AccountCommand

type AccountRegistry = {
   loadAccount: Guid -> Account.AccountState option Task
   saveAndPublish: OpenEventEnvelope -> unit Task
   startChildActors: Guid -> ProcessId list
   broadcast: AccountEvent * Account.AccountState -> Task
   broadcastError: string -> Task
}

let syncStateChange (cmd: AccountCommand) =
   //let pid = "@" + PID cmd.EntityId
   //tell (pid, cmd) |> ignore
   ()

let delete (id: Guid) =
   let pid = "@" + PID id
   kill pid |> ignore
   printfn "Killed process %A" pid
   ()

let Start
   (initialState: Account.AccountState)
   (registry: AccountRegistry)
   : ProcessId
   =
   let pid =
      spawn<Account.AccountState, Command> (
         PID initialState.EntityId,
         (fun () -> initialState),
         (fun (account: Account.AccountState) (command: Command) ->
            match command with
            | StartChildrenCommand id ->
               let pids = registry.startChildActors id
               printfn "AccountActor: Started child actors %A" pids
               account
            | LookupCommand _ ->
               reply account |> ignore
               account
            | AccountCommand cmd ->
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

let Lookup
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
            Start accountOpt.Value registry |> ignore
            return accountOpt
         else
            return None
   }
