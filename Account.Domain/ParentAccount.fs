[<RequireQualifiedAccess>]
module ParentAccount

open Validus
open System

open Bank.Account.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes
open AutomaticTransfer

let applyEvent (state: ParentAccountSnapshot) (evt: AccountEvent) = {
   state with
      Info =
         state.Info
         |> Map.change evt.AccountId (fun accountSnapshotOpt ->
            let snap =
               accountSnapshotOpt |> Option.defaultValue AccountSnapshot.empty

            let updated = Account.applyEvent snap evt
            Some updated)
}

let stateTransition
   (state: ParentAccountSnapshot)
   (command: AccountCommand)
   : Result<(AccountEvent * ParentAccountSnapshot), Err>
   =
   let accountId = command.AccountId

   let transitionErr (err: AccountStateTransitionError) =
      Error(Err.AccountStateTransitionError err)

   match command with
   | AccountCommand.CreateAccount _ ->
      if state.Info.ContainsKey(accountId) then
         transitionErr AccountStateTransitionError.AccountNotReadyToActivate
      else
         Account.stateTransition AccountSnapshot.empty command
         |> Result.map (fun (evt, snap) -> (evt, (applyEvent state evt)))
   | _ ->
      match state.Info.TryFind accountId with
      | None -> transitionErr (AccountNotFound accountId)
      | Some accountSnapshot ->
         Account.stateTransition accountSnapshot command
         |> Result.map (fun (evt, snap) -> (evt, (applyEvent state evt)))

/// Compute auto transfer events and updated Parent Account or return a
/// violating auto transfer command with error causing state transition fail.
let computeAutoTransferStateTransitions
   (frequency: AutomaticTransfer.Frequency)
   (accountId: AccountId)
   (parentAccount: ParentAccountSnapshot)
   : Result<ParentAccountSnapshot * AccountEvent list, AccountCommand * Err> option
   =
   parentAccount.Info.TryFind(accountId)
   |> Option.bind (fun accountSnapshot ->
      let account = accountSnapshot.Info

      let transfers =
         match frequency with
         | Frequency.PerTransaction -> account.AutoTransfersPerTransaction
         | Frequency.Schedule CronSchedule.Daily -> account.AutoTransfersDaily
         | Frequency.Schedule CronSchedule.TwiceMonthly ->
            account.AutoTransfersTwiceMonthly


      let transferOutCommands =
         transfers |> List.map (InternalAutoTransferCommand.create)

      let transferDepositCommands =
         transferOutCommands
         |> List.map (fun transferCmd ->
            let info = transferCmd.Data.Transfer

            DepositInternalAutoTransferCommand.create
               transferCmd.CorrelationId
               transferCmd.InitiatedBy
               {
                  BaseInfo = {
                     TransferId =
                        transferCmd.CorrelationId
                        |> CorrelationId.get
                        |> TransferId
                     InitiatedBy = transferCmd.InitiatedBy
                     Sender = info.Sender
                     Recipient = info.Recipient
                     Amount = PositiveAmount.get info.Amount
                     ScheduledDate = transferCmd.Timestamp
                     Memo = None
                  }
                  Rule = transferCmd.Data.Rule
               }
            |> AccountCommand.DepositInternalAutoTransfer)

      let transferCommands =
         (transferOutCommands |> List.map AccountCommand.InternalAutoTransfer)
         @ transferDepositCommands

      if transferCommands.IsEmpty then
         None
      else
         let validations =
            List.fold
               (fun acc cmd ->
                  match acc with
                  | Ok(accountState, events) ->
                     stateTransition accountState cmd
                     |> Result.map (fun (evt, newState) ->
                        newState, evt :: events)
                     |> Result.mapError (fun err -> cmd, err)
                  | Error err -> Error err)
               (Ok(parentAccount, []))
               transferCommands

         Some validations)
