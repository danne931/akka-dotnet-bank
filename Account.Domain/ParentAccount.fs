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
   let updated =
      match evt with
      | AccountEvent.ParentAccount _ -> state
      | _ -> {
         state with
            VirtualAccounts =
               state.VirtualAccounts
               |> Map.change evt.AccountId (fun accountOpt ->
                  Account.applyEvent
                     (accountOpt |> Option.defaultValue Account.empty)
                     evt
                  |> Some)
        }

   let updated =
      match evt with
      | AccountEvent.ParentAccount(ParentAccountEvent.BillingCycleStarted e) -> {
         updated with
            LastBillingCycleDate = Some e.Timestamp
        }
      | AccountEvent.InitializedPrimaryCheckingAccount e -> {
         updated with
            OrgId = e.OrgId
            ParentAccountId = e.Data.ParentAccountId
            PrimaryVirtualAccountId = e.Data.PrimaryChecking.AccountId
            Status = ParentAccountStatus.Active
        }
      | AccountEvent.MaintenanceFeeSkipped _
      | AccountEvent.MaintenanceFeeDebited _ -> {
         updated with
            MaintenanceFeeCriteria = MaintenanceFee.reset updated.Balance
        }
      | AccountEvent.DebitedAccount _
      | AccountEvent.DomesticTransferPending _
      | AccountEvent.InternalTransferWithinOrgPending _
      | AccountEvent.InternalTransferBetweenOrgsPending _
      | AccountEvent.InternalAutomatedTransferPending _ -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDebit
                  state.MaintenanceFeeCriteria
                  updated.Balance
        }
      | AccountEvent.PlatformPaymentPaid e ->
         match e.Data.PaymentMethod with
         | PaymentMethod.Platform _ -> {
            updated with
               MaintenanceFeeCriteria =
                  MaintenanceFee.fromDebit
                     state.MaintenanceFeeCriteria
                     updated.Balance
           }
         | PaymentMethod.ThirdParty _ -> updated
      | AccountEvent.DomesticTransferFailed _
      | AccountEvent.RefundedDebit _
      | AccountEvent.PlatformPaymentRefunded _
      | AccountEvent.InternalTransferWithinOrgFailed _
      | AccountEvent.InternalTransferBetweenOrgsFailed _
      | AccountEvent.InternalAutomatedTransferFailed _ -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDebitReversal
                  state.MaintenanceFeeCriteria
                  updated.Balance
        }
      | AccountEvent.DepositedCash e -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDeposit
                  state.MaintenanceFeeCriteria
                  e.Data.Amount
        }
      | AccountEvent.InternalTransferWithinOrgDeposited e -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDeposit
                  state.MaintenanceFeeCriteria
                  e.Data.BaseInfo.Amount
        }
      | AccountEvent.InternalTransferBetweenOrgsDeposited e -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDeposit
                  state.MaintenanceFeeCriteria
                  e.Data.BaseInfo.Amount
        }
      | AccountEvent.PlatformPaymentDeposited e -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDeposit
                  state.MaintenanceFeeCriteria
                  e.Data.BaseInfo.Amount
        }
      | AccountEvent.InternalAutomatedTransferDeposited e -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDeposit
                  state.MaintenanceFeeCriteria
                  e.Data.BaseInfo.Amount
        }
      | _ -> updated

   let updatedEvents = evt :: state.Events

   let updatedEvents =
      match evt, state.LastBillingCycleDate with
      | MaintenanceFeeDebited _, Some date
      | MaintenanceFeeSkipped _, Some date ->
         // Keep events that occurred after the previous billing statement period.
         let cutoff = DateTime(date.Year, date.Month, 1).ToUniversalTime()

         state.Events
         |> List.filter (fun evt ->
            let _, env = AccountEnvelope.unwrap evt
            env.Timestamp >= cutoff)
      | _ -> updatedEvents

   { updated with Events = updatedEvents }

let private virtualAccountTransition
   (account: Account)
   (cmd: AccountCommand)
   (state: ParentAccountSnapshot)
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

let validateParentAccountActive (state: ParentAccountSnapshot) =
   if state.Status <> ParentAccountStatus.Active then
      Account.transitionErr AccountStateTransitionError.ParentAccountNotActive
   else
      Ok state

let stateTransition
   (state: ParentAccountSnapshot)
   (command: AccountCommand)
   : Result<(AccountEvent * ParentAccountSnapshot), Err>
   =
   let accountId = command.AccountId
   let virtualAccount = state.VirtualAccounts.TryFind accountId

   match command, virtualAccount with
   | AccountCommand.InitializePrimaryCheckingAccount _, _ when
      not state.VirtualAccounts.IsEmpty
      ->
      Account.transitionErr
         AccountStateTransitionError.ParentAccountAlreadyInitialized
   | AccountCommand.InitializePrimaryCheckingAccount _, None ->
      virtualAccountTransition Account.empty command state
   | AccountCommand.CreateAccount _, Some _ ->
      Account.transitionErr
         AccountStateTransitionError.AccountNotReadyToActivate
   | AccountCommand.CreateAccount _, None ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition Account.empty command)
   | AccountCommand.InternalTransferBetweenOrgs cmd, Some account ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)
      |> Result.bind (
         validatePlatformTransferLimit cmd.Data.Amount cmd.Timestamp
      )
   | AccountCommand.FulfillPlatformPayment cmd, Some account ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)
      |> Result.bind (
         validatePlatformTransferLimit
            cmd.Data.RequestedPayment.BaseInfo.Amount
            cmd.Timestamp
      )
   | AccountCommand.DomesticTransfer cmd, Some account ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)
      |> Result.bind (
         validateDomesticTransferLimit cmd.Data.Amount cmd.Timestamp
      )
   | AccountCommand.ParentAccount(ParentAccountCommand.StartBillingCycle cmd), _ ->
      validateParentAccountActive state
      |> Result.bind (fun _ ->
         StartBillingCycleCommand.toEvent cmd
         |> Result.mapError ValidationError)
      |> Result.map (fun evt ->
         let evt =
            AccountEvent.ParentAccount(
               ParentAccountEvent.BillingCycleStarted evt
            )

         evt, (applyEvent state evt))
   | _, None -> Account.transitionErr (AccountNotFound accountId)
   | _, Some account ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)

/// Compute auto transfer events and updated Parent Account or return a
/// violating auto transfer command with error causing state transition fail.
let computeAutoTransferStateTransitions
   (frequency: AutomaticTransfer.Frequency)
   (accountId: AccountId)
   (parentAccount: ParentAccountSnapshot)
   : Result<ParentAccountSnapshot * AccountEvent list, AccountCommand * Err> option
   =
   parentAccount.VirtualAccounts.TryFind(accountId)
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
