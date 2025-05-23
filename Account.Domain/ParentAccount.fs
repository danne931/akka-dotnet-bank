[<RequireQualifiedAccess>]
module ParentAccount

open System

open Bank.Account.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes
open Lib.Time
open AutomaticTransfer

module TransferLimits =
   /// Transfers & payments between orgs on the platform
   let DailyPlatformLimit = 999_999_999m

   /// Transfers to entities outside the platform (ex: via ACH)
   let DailyDomesticLimit = 100_000m

   let private transferAccrued
      (satisfiesDate: DateTime -> bool)
      (eventToAccrual: AccountEvent -> (DateTime * decimal) option)
      (events: AccountEvent list)
      : decimal
      =
      List.fold
         (fun acc evt ->
            match eventToAccrual evt with
            | Some(date, amount) ->
               if satisfiesDate date then acc + amount else acc
            | None -> acc)
         0m
         events

   let dailyPlatformTransferAccrued =
      transferAccrued DateTime.isToday (function
         | AccountEvent.InternalTransferBetweenOrgsPending e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, info.Amount)
         | AccountEvent.InternalTransferBetweenOrgsFailed e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, -info.Amount)
         | AccountEvent.PlatformPaymentPaid e ->
            Some(e.Timestamp, e.Data.BaseInfo.Amount)
         | _ -> None)

   let dailyDomesticTransferAccrued =
      transferAccrued DateTime.isToday (function
         | AccountEvent.DomesticTransferPending e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, info.Amount)
         | AccountEvent.DomesticTransferFailed e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, -info.Amount)
         | _ -> None)

   let exceedsDailyInternalTransferLimit
      (evts: AccountEvent list)
      (amount: decimal)
      (scheduledDate: DateTime)
      =
      DateTime.isToday scheduledDate
      && (dailyPlatformTransferAccrued evts) + amount > DailyPlatformLimit

   let exceedsDailyDomesticTransferLimit
      (evts: AccountEvent list)
      (amount: decimal)
      (scheduledDate: DateTime)
      =
      DateTime.isToday scheduledDate
      && (dailyDomesticTransferAccrued evts) + amount > DailyDomesticLimit

let applyEvent (state: ParentAccountSnapshot) (evt: AccountEvent) =
   let updatedState = {
      state with
         Info =
            state.Info
            |> Map.change evt.AccountId (fun accountOpt ->
               let updated =
                  Account.applyEvent
                     (accountOpt |> Option.defaultValue Account.empty)
                     evt

               Some updated)
   }

   let updatedEvents = evt :: state.Events

   (*
   let updatedEvents =
      match evt, account.LastBillingCycleDate with
      | MaintenanceFeeDebited _, Some date
      | MaintenanceFeeSkipped _, Some date ->
         // Keep events that occurred after the previous billing statement period.
         let cutoff = DateTime(date.Year, date.Month, 1).ToUniversalTime()

         state.Events
         |> List.filter (fun evt ->
            let _, env = AccountEnvelope.unwrap evt
            env.Timestamp >= cutoff)
      | _ -> updatedEvents
   *)

   {
      updatedState with
         Events = updatedEvents
   }

let private virtualAccountTransition
   (state: ParentAccountSnapshot)
   (account: Account)
   (cmd: AccountCommand)
   =
   Account.stateTransition account cmd
   |> Result.map (fun (evt, _) -> (evt, (applyEvent state evt)))

let validatePlatformTransferLimit amount timestamp (evt, state) =
   if
      TransferLimits.exceedsDailyInternalTransferLimit
         state.Events
         amount
         timestamp
   then
      TransferLimits.DailyPlatformLimit
      |> ExceededDailyInternalTransferLimit
      |> Account.transitionErr
   else
      Ok(evt, state)

let validateDomesticTransferLimit amount timestamp (evt, state) =
   if
      TransferLimits.exceedsDailyDomesticTransferLimit
         state.Events
         amount
         timestamp
   then
      TransferLimits.DailyDomesticLimit
      |> ExceededDailyDomesticTransferLimit
      |> Account.transitionErr
   else
      Ok(evt, state)

let stateTransition
   (state: ParentAccountSnapshot)
   (command: AccountCommand)
   : Result<(AccountEvent * ParentAccountSnapshot), Err>
   =
   let accountId = command.AccountId
   let virtualAccount = state.Info.TryFind accountId

   match command, virtualAccount with
   | AccountCommand.CreateAccount _, Some _ ->
      Account.transitionErr
         AccountStateTransitionError.AccountNotReadyToActivate
   | AccountCommand.CreateAccount _, None ->
      virtualAccountTransition state Account.empty command
   | AccountCommand.InternalTransferBetweenOrgs cmd, Some account ->
      virtualAccountTransition state account command
      |> Result.bind (
         validatePlatformTransferLimit cmd.Data.Amount cmd.Timestamp
      )
   | AccountCommand.FulfillPlatformPayment cmd, Some account ->
      virtualAccountTransition state account command
      |> Result.bind (
         validatePlatformTransferLimit
            cmd.Data.RequestedPayment.BaseInfo.Amount
            cmd.Timestamp
      )
   | AccountCommand.DomesticTransfer cmd, Some account ->
      virtualAccountTransition state account command
      |> Result.bind (
         validateDomesticTransferLimit cmd.Data.Amount cmd.Timestamp
      )
   | _, None -> Account.transitionErr (AccountNotFound accountId)
   | _, Some account -> virtualAccountTransition state account command

/// Compute auto transfer events and updated Parent Account or return a
/// violating auto transfer command with error causing state transition fail.
let computeAutoTransferStateTransitions
   (frequency: AutomaticTransfer.Frequency)
   (accountId: AccountId)
   (parentAccount: ParentAccountSnapshot)
   : Result<ParentAccountSnapshot * AccountEvent list, AccountCommand * Err> option
   =
   parentAccount.Info.TryFind(accountId)
   |> Option.bind (fun account ->
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
