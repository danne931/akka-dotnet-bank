[<RequireQualifiedAccess>]
module ParentAccount

open System
open Validus
open FsToolkit.ErrorHandling

open Bank.Account.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes
open Lib.Time
open AutomaticTransfer

module AutoTransfer =
   let hasCycle
      (accounts: Map<AccountId, Account>)
      (newRule: AutomaticTransferConfig)
      =
      accounts.Values
      |> Seq.toList
      |> List.choose _.AutoTransferRule
      |> CycleDetection.cycleDetected newRule

/// Transfer amounts accrued include settled and in flight transactions.
module TransferLimits =
   /// Transfers between orgs on the platform
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
      && dailyPlatformTransferAccrued evts + amount > DailyPlatformLimit

   let exceedsDailyDomesticTransferLimit
      (evts: AccountEvent list)
      (amount: decimal)
      (scheduledDate: DateTime)
      =
      DateTime.isToday scheduledDate
      && dailyDomesticTransferAccrued evts + amount > DailyDomesticLimit

let applyEvent (state: ParentAccountSnapshot) (evt: AccountEvent) =
   let updated =
      match evt with
      | AccountEvent.ParentAccount evt ->
         match evt with
         | ParentAccountEvent.RegisteredCounterparty e ->
            let counterparty = e.Data.Counterparty

            {
               state with
                  Counterparties =
                     state.Counterparties.Add(
                        counterparty.CounterpartyId,
                        counterparty
                     )
            }
         | ParentAccountEvent.EditedCounterparty e ->
            let counterparty = e.Data.Counterparty

            {
               state with
                  Counterparties =
                     state.Counterparties.Change(
                        counterparty.CounterpartyId,
                        Option.map (fun _ -> counterparty)
                     )
            }
         | ParentAccountEvent.NicknamedCounterparty e -> {
            state with
               Counterparties =
                  Map.change
                     e.Data.CounterpartyId
                     (Option.map (fun acct -> {
                        acct with
                           Nickname = e.Data.Nickname
                     }))
                     state.Counterparties
           }
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
      | AccountEvent.InitializedPrimaryCheckingAccount e -> {
         updated with
            OrgId = e.OrgId
            ParentAccountId = e.Data.ParentAccountId
            PartnerBankLink = e.Data.PartnerBankLink
            PrimaryVirtualAccountId = e.Data.PrimaryChecking.AccountId
            Status = ParentAccountStatus.Active
        }
      | AccountEvent.MaintenanceFeeSkipped e -> {
         updated with
            MaintenanceFeeCriteria = MaintenanceFee.reset updated.Balance
            LastBillingCycleDate = Some e.Data.BillingDate
        }
      | AccountEvent.MaintenanceFeeDebited e -> {
         updated with
            MaintenanceFeeCriteria = MaintenanceFee.reset updated.Balance
            LastBillingCycleDate = Some e.Data.BillingDate
        }
      | AccountEvent.DebitSettled _
      | AccountEvent.InternalTransferBetweenOrgsSettled _ -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDebit
                  state.MaintenanceFeeCriteria
                  updated.Balance
        }
      | AccountEvent.DomesticTransferSettled e -> {
         updated with
            MaintenanceFeeCriteria =
               match e.Data.BaseInfo.MoneyFlow with
               | MoneyFlow.Out ->
                  MaintenanceFee.fromDebit
                     state.MaintenanceFeeCriteria
                     updated.Balance
               | MoneyFlow.In ->
                  MaintenanceFee.fromDeposit
                     state.MaintenanceFeeCriteria
                     updated.Balance
        }
      | AccountEvent.DomesticTransferFailed _ -> updated
      | AccountEvent.DebitRefunded _ -> {
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
      | AccountEvent.InternalAutomatedTransferDeposited e -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDeposit
                  state.MaintenanceFeeCriteria
                  e.Data.BaseInfo.Amount
        }
      | _ -> updated

   let updatedEvents = evt :: state.Events
   let previousBillingDate = state.LastBillingCycleDate

   let updatedEvents =
      match evt, previousBillingDate with
      | MaintenanceFeeDebited _, Some date
      | MaintenanceFeeSkipped _, Some date ->
         // Keep events that occurred after the previous billing statement period.
         let cutoff = DateTime(date.Year, date.Month, 1).ToUniversalTime()

         updatedEvents
         |> List.filter (fun evt ->
            let _, env = AccountEnvelope.unwrap evt
            env.Timestamp >= cutoff)
      | _ -> updatedEvents

   {
      updated with
         Events = updatedEvents
         ProcessedCommands =
            let _, env = AccountEnvelope.unwrap evt
            state.ProcessedCommands |> Map.add env.Id env.Timestamp
   }

let private virtualAccountTransition
   (account: Account)
   (cmd: AccountCommand)
   (state: ParentAccountSnapshot)
   =
   Account.stateTransition account cmd
   |> Result.map (fun (evt, _) -> evt, applyEvent state evt)

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

module private StateTransition =
   let mapParent
      (eventTransform: BankEvent<'t> -> ParentAccountEvent)
      (state: ParentAccountSnapshot)
      (eventValidation: ValidationResult<BankEvent<'t>>)
      =
      eventValidation
      |> Result.mapError ValidationError
      |> Result.map (fun evt ->
         let evt = AccountEvent.ParentAccount(eventTransform evt)
         evt, applyEvent state evt)

   let registerCounterparty
      (state: ParentAccountSnapshot)
      (cmd: RegisterCounterpartyCommand)
      =
      if state.Status <> ParentAccountStatus.Active then
         Account.transitionErr
            AccountStateTransitionError.ParentAccountNotActive
      elif
         state.Counterparties
         |> Map.exists (fun _ recipient ->
            string recipient.AccountNumber = cmd.Data.AccountNumber
            && string recipient.RoutingNumber = cmd.Data.RoutingNumber)
      then
         Account.transitionErr AccountStateTransitionError.RecipientRegistered
      else
         mapParent
            ParentAccountEvent.RegisteredCounterparty
            state
            (RegisterCounterpartyCommand.toEvent cmd)

   let editCounterparty
      (state: ParentAccountSnapshot)
      (cmd: EditCounterpartyCommand)
      =
      if state.Status <> ParentAccountStatus.Active then
         Account.transitionErr
            AccountStateTransitionError.ParentAccountNotActive
      else
         mapParent
            ParentAccountEvent.EditedCounterparty
            state
            (EditCounterpartyCommand.toEvent cmd)

   let nicknameCounterparty
      (state: ParentAccountSnapshot)
      (cmd: NicknameCounterpartyCommand)
      =
      let recipient = state.Counterparties.TryFind cmd.Data.CounterpartyId

      if state.Status <> ParentAccountStatus.Active then
         Account.transitionErr
            AccountStateTransitionError.ParentAccountNotActive
      elif recipient.IsNone then
         Account.transitionErr AccountStateTransitionError.RecipientNotFound
      else
         mapParent
            ParentAccountEvent.NicknamedCounterparty
            state
            (NicknameCounterpartyCommand.toEvent cmd)

let stateTransition
   (state: ParentAccountSnapshot)
   (command: AccountCommand)
   : Result<(AccountEvent * ParentAccountSnapshot), Err>
   =
   let accountId = command.AccountId
   let virtualAccount = state.VirtualAccounts.TryFind accountId

   match command, virtualAccount with
   | AccountCommand.ParentAccount cmd, _ ->
      match cmd with
      | ParentAccountCommand.RegisterCounterparty cmd ->
         StateTransition.registerCounterparty state cmd
      | ParentAccountCommand.EditCounterparty cmd ->
         StateTransition.editCounterparty state cmd
      | ParentAccountCommand.NicknameCounterparty cmd ->
         StateTransition.nicknameCounterparty state cmd
   | AccountCommand.InitializePrimaryCheckingAccount _, _ when
      not state.VirtualAccounts.IsEmpty
      ->
      Account.transitionErr
         AccountStateTransitionError.ParentAccountAlreadyInitialized
   | AccountCommand.InitializePrimaryCheckingAccount _, None ->
      virtualAccountTransition Account.empty command state
   | AccountCommand.CreateVirtualAccount _, Some _ ->
      Account.transitionErr
         AccountStateTransitionError.AccountNotReadyToActivate
   | AccountCommand.CreateVirtualAccount _, None ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition Account.empty command)
   | AccountCommand.InternalTransferBetweenOrgs cmd, Some account ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)
      |> Result.bind (
         validatePlatformTransferLimit cmd.Data.Amount cmd.Timestamp
      )
   | AccountCommand.DomesticTransfer cmd, Some account ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)
      |> Result.bind (
         validateDomesticTransferLimit cmd.Data.Amount cmd.Timestamp
      )
   | AccountCommand.ConfigureAutoTransferRule cmd, Some account ->
      let newRule = {
         Id = cmd.Data.RuleIdToUpdate |> Option.defaultValue (Guid.NewGuid())
         Info = cmd.Data.Rule
      }

      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)
      |> Result.bind (fun res ->
         if AutoTransfer.hasCycle state.VirtualAccounts newRule then
            Account.transitionErr AutoTransferCycleDetected
         else
            Ok res)
   | _, None -> Account.transitionErr (AccountNotFound accountId)
   | _, Some account ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)

module AutoTransferStateTransition =
   /// Compute auto transfer events and updated Parent Account or return
   /// error causing state transition fail.
   let compute
      (frequency: AutomaticTransfer.Frequency)
      (accountId: AccountId)
      (parentAccount: ParentAccountSnapshot)
      : Result<AccountEvent list * ParentAccountSnapshot, Err> option
      =
      parentAccount.VirtualAccounts.TryFind accountId
      |> Option.bind (fun account ->
         let transfers =
            match frequency with
            | Frequency.PerTransaction -> account.AutoTransfersPerTransaction
            | Frequency.Schedule CronSchedule.Daily ->
               account.AutoTransfersDaily
            | Frequency.Schedule CronSchedule.TwiceMonthly ->
               account.AutoTransfersTwiceMonthly

         let transferOutCommands =
            transfers |> List.map InternalAutoTransferCommand.create

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
                           TransferId transferCmd.CorrelationId.Value
                        InitiatedBy = transferCmd.InitiatedBy
                        Sender = info.Sender
                        Recipient = info.Recipient
                        Amount = info.Amount.Value
                        ScheduledDate = transferCmd.Timestamp
                        Memo = None
                     }
                     Rule = transferCmd.Data.Rule
                  }
               |> AccountCommand.DepositInternalAutoTransfer)

         let transferCommands =
            (transferOutCommands
             |> List.map AccountCommand.InternalAutoTransfer)
            @ transferDepositCommands

         if transferCommands.IsEmpty then
            None
         else
            let validations =
               List.fold
                  (fun acc cmd ->
                     match acc with
                     | Ok(events, accountState) ->
                        stateTransition accountState cmd
                        |> Result.map (fun (evt, newState) ->
                           evt :: events, newState)
                     | Error err -> Error err)
                  (Ok([], parentAccount))
                  transferCommands
               // NOTE:
               // Ensure auto-transfer events in intended order of
               // transfer-out preceding transfer-in.

               // This is necessary for AccountReadModelSyncActor to
               // behave predictably. There is a sortByDescendingTimestamp
               // within a batch but the chunking limit may result in a
               // transfer-out & transfer-in in different batches.
               // If they are in different batches and transfer-in was processed
               // first, the postgres upsert will fail as it expects a
               // transfer record which is only created during transfer-out evt.
               |> Result.map (fun (evts, state) -> List.rev evts, state)

            Some validations)

   // May return (Deducted, Deposited) and some combination of
   // Deduct/Deposit auto transfer events based on if auto
   // transfer events are produced from inc/dec of the sender/recipient.
   let computeForInternalTransferWithinOrg
      (deduction: BankEvent<InternalTransferWithinOrgDeducted>)
      (state: ParentAccountSnapshot)
      : Result<AccountEvent list * ParentAccountSnapshot, Err>
      =
      let correspondingTransferDeposit =
         deduction
         |> DepositInternalTransferWithinOrgCommand.fromDeduction
         |> AccountCommand.DepositTransferWithinOrg
         |> stateTransition state

      let transfer = deduction.Data.BaseInfo

      let autoSenderTransitions state =
         compute Frequency.PerTransaction transfer.Sender.AccountId state

      let autoRecipientTransitions state =
         compute Frequency.PerTransaction transfer.Recipient.AccountId state

      correspondingTransferDeposit
      |> Result.map (fun (deposit, state) ->
         let autoTransferStateTransitions =
            autoSenderTransitions state
            |> Option.map (function
               | Error e -> Error e
               | Ok(evts, state) ->
                  autoSenderTransitions state
                  |> Option.sequenceResult
                  |> Result.map (function
                     | None -> evts, state
                     | Some(evts2, state) -> evts @ evts2, state))
            |> Option.orElse (autoRecipientTransitions state)
            |> Option.sequenceResult

         let evts = [
            AccountEvent.InternalTransferWithinOrgDeducted deduction
            deposit
         ]

         let withoutAutoTransitions = evts, state

         match autoTransferStateTransitions with
         | Error _ -> withoutAutoTransitions
         | Ok None -> withoutAutoTransitions
         | Ok(Some(autoTransferEvts, state)) -> evts @ autoTransferEvts, state)

   // Account events indicating a settled txn with an in/out money flow
   // can produce an automatic transfer.
   // Automated transfers have money flow but they can not generate
   // an auto transfer.
   let private eventProducesAutoTransfer =
      function
      | AccountEvent.DepositedCash _
      | AccountEvent.DebitRefunded _
      | AccountEvent.DebitSettled _
      // AccountEvent.InternalTransferWithinOrgDeducted, Deposited handled
      // separately with computeForInternalTransferWithinOrg function.
      | AccountEvent.InternalTransferBetweenOrgsDeposited _
      | AccountEvent.InternalTransferBetweenOrgsSettled _
      | AccountEvent.DomesticTransferSettled _
      | AccountEvent.MaintenanceFeeDebited _ -> true
      | _ -> false

   /// Returns accumulated state, the originating event, and
   /// any subsequent events such as auto transfers to be persisted.
   let transition
      (state: ParentAccountSnapshot)
      (evt: AccountEvent)
      : Result<AccountEvent list * ParentAccountSnapshot, Err>
      =
      match evt with
      | AccountEvent.InternalTransferWithinOrgDeducted e ->
         computeForInternalTransferWithinOrg e state
      | _ ->
         if eventProducesAutoTransfer evt then
            compute Frequency.PerTransaction evt.AccountId state
            |> Option.sequenceResult
            |> Result.map (function
               | None -> [ evt ], state
               | Some(autoTransferEvts, state) -> evt :: autoTransferEvts, state)
         else
            Ok([ evt ], state)
