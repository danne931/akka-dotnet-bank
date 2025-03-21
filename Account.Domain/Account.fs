[<RequireQualifiedAccess>]
module Account

open Validus
open System

open Bank.Account.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes
open Lib.Time

module TransferLimits =
   let DailyInternalLimit = 999_999_999m
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

   let dailyInternalTransferAccrued =
      transferAccrued DateTime.isToday (function
         | AccountEvent.InternalTransferWithinOrgPending e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, info.Amount)
         | AccountEvent.InternalTransferWithinOrgFailed e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, -info.Amount)
         | AccountEvent.InternalTransferBetweenOrgsPending e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, info.Amount)
         | AccountEvent.InternalTransferBetweenOrgsFailed e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, -info.Amount)
         | AccountEvent.PlatformPaymentPaid e ->
            Some(e.Timestamp, e.Data.BaseInfo.Amount)
         | AccountEvent.InternalAutomatedTransferPending e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, info.Amount)
         | AccountEvent.InternalAutomatedTransferFailed e ->
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
      && (dailyInternalTransferAccrued evts) + amount > DailyInternalLimit

   let exceedsDailyDomesticTransferLimit
      (evts: AccountEvent list)
      (amount: decimal)
      (scheduledDate: DateTime)
      =
      DateTime.isToday scheduledDate
      && (dailyDomesticTransferAccrued evts) + amount > DailyDomesticLimit

let private applyInternalTransferPending
   (info: BaseInternalTransferInfo)
   (state: AccountSnapshot)
   =
   let account = state.Info
   let balance = account.Balance - info.Amount

   {
      state with
         Info.Balance = balance
         Info.MaintenanceFeeCriteria =
            MaintenanceFee.fromDebit account.MaintenanceFeeCriteria balance
         InProgressInternalTransfers =
            Map.add
               info.TransferId
               {
                  TransferId = info.TransferId
                  Info = info
               }
               state.InProgressInternalTransfers
   }

let private applyInternalTransferCompleted
   (info: BaseInternalTransferInfo)
   (state: AccountSnapshot)
   =
   {
      state with
         InProgressInternalTransfers =
            Map.remove info.TransferId state.InProgressInternalTransfers
   }

let private applyInternalTransferFailed
   (info: BaseInternalTransferInfo)
   (state: AccountSnapshot)
   =
   let account = state.Info
   let balance = account.Balance + info.Amount

   {
      state with
         Info.Balance = balance
         Info.MaintenanceFeeCriteria =
            MaintenanceFee.fromDebitReversal
               account.MaintenanceFeeCriteria
               balance
         InProgressInternalTransfers =
            Map.remove info.TransferId state.InProgressInternalTransfers
   }

let private applyTransferDeposit (state: AccountSnapshot) (amount: decimal) = {
   state with
      Info.Balance = state.Info.Balance + amount
      Info.MaintenanceFeeCriteria =
         MaintenanceFee.fromDeposit state.Info.MaintenanceFeeCriteria amount
}

let applyEvent (state: AccountSnapshot) (evt: AccountEvent) =
   let account = state.Info

   let updatedState: AccountSnapshot =
      match evt with
      | BillingCycleStarted e -> {
         state with
            Info.LastBillingCycleDate = Some e.Timestamp
        }
      | CreatedAccount e -> {
         state with
            Info = {
               AccountId = AccountId.fromEntityId e.EntityId
               AccountNumber = e.Data.AccountNumber
               RoutingNumber = e.Data.RoutingNumber
               OrgId = e.OrgId
               Name = e.Data.Name
               Depository = e.Data.Depository
               Currency = e.Data.Currency
               Balance = e.Data.Balance
               Status = AccountStatus.Active
               LastBillingCycleDate = None
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = false
               }
               AutoTransferRule = None
            }
        }
      | AccountEvent.AccountClosed _ -> {
         state with
            Info.Status = AccountStatus.Closed
        }
      | DepositedCash e -> {
         state with
            Info.Balance = account.Balance + e.Data.Amount
            Info.MaintenanceFeeCriteria =
               MaintenanceFee.fromDeposit
                  account.MaintenanceFeeCriteria
                  e.Data.Amount
        }
      | DebitedAccount e ->
         let balance = account.Balance - e.Data.Amount

         {
            state with
               Info.Balance = balance
               Info.MaintenanceFeeCriteria =
                  MaintenanceFee.fromDebit
                     account.MaintenanceFeeCriteria
                     balance
         }
      | MaintenanceFeeDebited e ->
         let balance = account.Balance - e.Data.Amount

         {
            state with
               Info.Balance = balance
               Info.MaintenanceFeeCriteria = MaintenanceFee.reset balance
         }
      | MaintenanceFeeSkipped _ -> {
         state with
            Info.MaintenanceFeeCriteria = MaintenanceFee.reset account.Balance
        }
      | DomesticTransferScheduled _ -> state
      | DomesticTransferPending e ->
         let info = e.Data.BaseInfo
         let balance = account.Balance - info.Amount

         {
            state with
               Info.Balance = balance
               Info.MaintenanceFeeCriteria =
                  MaintenanceFee.fromDebit
                     account.MaintenanceFeeCriteria
                     balance
               InProgressDomesticTransfers =
                  Map.add
                     info.TransferId
                     (TransferEventToDomesticTransfer.fromPending e)
                     state.InProgressDomesticTransfers
         }
      | DomesticTransferProgress e -> {
         state with
            InProgressDomesticTransfers =
               Map.change
                  e.Data.BaseInfo.TransferId
                  (Option.map (fun txn -> {
                     txn with
                        Status =
                           DomesticTransferProgress.InProgress
                              e.Data.InProgressInfo
                  }))
                  state.InProgressDomesticTransfers
        }
      | DomesticTransferCompleted e -> {
         state with
            InProgressDomesticTransfers =
               Map.remove
                  e.Data.BaseInfo.TransferId
                  state.InProgressDomesticTransfers
            FailedDomesticTransfers =
               Map.remove
                  e.Data.BaseInfo.TransferId
                  state.FailedDomesticTransfers
        }
      | DomesticTransferFailed e ->
         let info = e.Data.BaseInfo
         let balance = account.Balance + info.Amount

         let account = {
            account with
               Balance = balance
               MaintenanceFeeCriteria =
                  MaintenanceFee.fromDebitReversal
                     account.MaintenanceFeeCriteria
                     balance
         }

         {
            state with
               Info = account
               InProgressDomesticTransfers =
                  Map.remove info.TransferId state.InProgressDomesticTransfers
               FailedDomesticTransfers =
                  Map.add
                     info.TransferId
                     (TransferEventToDomesticTransfer.fromFailure e)
                     state.FailedDomesticTransfers
         }
      | InternalTransferWithinOrgPending e ->
         applyInternalTransferPending e.Data.BaseInfo state
      | InternalTransferWithinOrgCompleted e ->
         applyInternalTransferCompleted e.Data.BaseInfo state
      | InternalTransferWithinOrgFailed e ->
         applyInternalTransferFailed e.Data.BaseInfo state
      | InternalTransferBetweenOrgsScheduled _ -> state
      | InternalTransferBetweenOrgsPending e ->
         applyInternalTransferPending e.Data.BaseInfo state
      | InternalTransferBetweenOrgsCompleted e ->
         applyInternalTransferCompleted e.Data.BaseInfo state
      | InternalTransferBetweenOrgsFailed e ->
         applyInternalTransferFailed e.Data.BaseInfo state
      | InternalTransferWithinOrgDeposited e ->
         applyTransferDeposit state e.Data.BaseInfo.Amount
      | InternalTransferBetweenOrgsDeposited e ->
         applyTransferDeposit state e.Data.BaseInfo.Amount
      | PlatformPaymentRequested _ -> state
      | PlatformPaymentCancelled _ -> state
      | PlatformPaymentDeclined _ -> state
      | PlatformPaymentPaid e ->
         // If payment fulfilled with account funds then deduct the
         // payment amount from the account.
         match e.Data.PaymentMethod with
         | PaymentMethod.Platform _ ->
            let balance = account.Balance - e.Data.BaseInfo.Amount

            {
               state with
                  Info.Balance = balance
                  Info.MaintenanceFeeCriteria =
                     MaintenanceFee.fromDebit
                        account.MaintenanceFeeCriteria
                        balance
            }
         | PaymentMethod.ThirdParty _ -> state
      | PlatformPaymentDeposited e ->
         applyTransferDeposit state e.Data.BaseInfo.Amount
      | AutoTransferRuleConfigured e -> {
         state with
            Info.AutoTransferRule = Some e.Data.Config
        }
      | AutoTransferRuleDeleted _ -> {
         state with
            Info.AutoTransferRule = None
        }
      | InternalAutomatedTransferPending e ->
         applyInternalTransferPending e.Data.BaseInfo state
      | InternalAutomatedTransferCompleted e ->
         applyInternalTransferCompleted e.Data.BaseInfo state
      | InternalAutomatedTransferFailed e ->
         applyInternalTransferFailed e.Data.BaseInfo state
      | InternalAutomatedTransferDeposited e ->
         applyTransferDeposit state e.Data.BaseInfo.Amount

   let updatedEvents = evt :: state.Events

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

   {
      updatedState with
         Events = updatedEvents
   }

module private StateTransition =
   let transitionErr (err: AccountStateTransitionError) =
      Error <| AccountStateTransitionError err

   let map
      (eventTransform: BankEvent<'t> -> AccountEvent)
      (state: AccountSnapshot)
      (eventValidation: ValidationResult<BankEvent<'t>>)
      =
      eventValidation
      |> Result.mapError ValidationError
      |> Result.map (fun evt ->
         let evt = eventTransform evt
         (evt, applyEvent state evt))

   let create (state: AccountSnapshot) (cmd: CreateAccountCommand) =
      if state.Info.Status <> AccountStatus.InitialEmptyState then
         transitionErr AccountNotReadyToActivate
      else
         map CreatedAccount state (CreateAccountCommand.toEvent cmd)

   let startBillingcycle
      (state: AccountSnapshot)
      (cmd: StartBillingCycleCommand)
      =
      if state.Info.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         map BillingCycleStarted state (StartBillingCycleCommand.toEvent cmd)

   let deposit (state: AccountSnapshot) (cmd: DepositCashCommand) =
      if state.Info.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         map DepositedCash state (DepositCashCommand.toEvent cmd)

   let debit (state: AccountSnapshot) (cmd: DebitCommand) =
      let input = cmd.Data
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance account.Balance
      else
         map DebitedAccount state (DebitCommand.toEvent cmd)

   let maintenanceFee (state: AccountSnapshot) (cmd: MaintenanceFeeCommand) =
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      elif account.Balance - cmd.Data.Amount < 0m then
         transitionErr <| InsufficientBalance account.Balance
      else
         map MaintenanceFeeDebited state (MaintenanceFeeCommand.toEvent cmd)

   let skipMaintenanceFee
      (state: AccountSnapshot)
      (cmd: SkipMaintenanceFeeCommand)
      =
      if state.Info.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         map MaintenanceFeeSkipped state (SkipMaintenanceFeeCommand.toEvent cmd)

   let internalTransfer
      (state: AccountSnapshot)
      (cmd: InternalTransferWithinOrgCommand)
      =
      let input = cmd.Data
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance account.Balance
      elif input.Recipient.OrgId <> account.OrgId then
         transitionErr TransferExpectedToOccurWithinOrg
      elif
         TransferLimits.exceedsDailyInternalTransferLimit
            state.Events
            input.Amount
            cmd.Timestamp
      then
         TransferLimits.DailyInternalLimit
         |> ExceededDailyInternalTransferLimit
         |> transitionErr
      else
         map
            InternalTransferWithinOrgPending
            state
            (InternalTransferWithinOrgCommand.toEvent cmd)

   let completeInternalTransfer
      (state: AccountSnapshot)
      (cmd: CompleteInternalTransferWithinOrgCommand)
      =
      let account = state.Info
      let transferId = cmd.Data.BaseInfo.TransferId

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else if
         Option.isNone
         <| Map.tryFind transferId state.InProgressInternalTransfers
      then
         transitionErr TransferAlreadyProgressedToCompletedOrFailed
      else
         map
            InternalTransferWithinOrgCompleted
            state
            (CompleteInternalTransferWithinOrgCommand.toEvent cmd)

   let failInternalTransfer
      (state: AccountSnapshot)
      (cmd: FailInternalTransferWithinOrgCommand)
      =
      let account = state.Info
      let transferId = cmd.Data.BaseInfo.TransferId

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else if
         Option.isNone
         <| Map.tryFind transferId state.InProgressInternalTransfers
      then
         transitionErr TransferAlreadyProgressedToCompletedOrFailed
      else
         map
            InternalTransferWithinOrgFailed
            state
            (FailInternalTransferWithinOrgCommand.toEvent cmd)

   let scheduleInternalTransferBetweenOrgs
      (state: AccountSnapshot)
      (cmd: ScheduleInternalTransferBetweenOrgsCommand)
      =
      let input = cmd.Data.TransferInput
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance account.Balance
      else
         map
            InternalTransferBetweenOrgsScheduled
            state
            (ScheduleInternalTransferBetweenOrgsCommand.toEvent cmd)

   let internalTransferBetweenOrgs
      (state: AccountSnapshot)
      (cmd: InternalTransferBetweenOrgsCommand)
      =
      let input = cmd.Data
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance account.Balance
      elif
         TransferLimits.exceedsDailyInternalTransferLimit
            state.Events
            input.Amount
            cmd.Timestamp
      then
         TransferLimits.DailyInternalLimit
         |> ExceededDailyInternalTransferLimit
         |> transitionErr
      else
         map
            InternalTransferBetweenOrgsPending
            state
            (InternalTransferBetweenOrgsCommand.toEvent cmd)

   let completeInternalTransferBetweenOrgs
      (state: AccountSnapshot)
      (cmd: CompleteInternalTransferBetweenOrgsCommand)
      =
      let account = state.Info
      let transferId = cmd.Data.BaseInfo.TransferId

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else if
         Option.isNone
         <| Map.tryFind transferId state.InProgressInternalTransfers
      then
         transitionErr TransferAlreadyProgressedToCompletedOrFailed
      else
         map
            InternalTransferBetweenOrgsCompleted
            state
            (CompleteInternalTransferBetweenOrgsCommand.toEvent cmd)

   let failInternalTransferBetweenOrgs
      (state: AccountSnapshot)
      (cmd: FailInternalTransferBetweenOrgsCommand)
      =
      let account = state.Info
      let transferId = cmd.Data.BaseInfo.TransferId

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else if
         Option.isNone
         <| Map.tryFind transferId state.InProgressInternalTransfers
      then
         transitionErr TransferAlreadyProgressedToCompletedOrFailed
      else
         map
            InternalTransferBetweenOrgsFailed
            state
            (FailInternalTransferBetweenOrgsCommand.toEvent cmd)

   let scheduleDomesticTransfer
      (state: AccountSnapshot)
      (cmd: ScheduleDomesticTransferCommand)
      =
      let input = cmd.Data.TransferInput
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance account.Balance
      else
         map
            DomesticTransferScheduled
            state
            (ScheduleDomesticTransferCommand.toEvent cmd)

   let domesticTransfer
      (state: AccountSnapshot)
      (cmd: DomesticTransferCommand)
      =
      let input = cmd.Data
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance account.Balance
      elif
         TransferLimits.exceedsDailyDomesticTransferLimit
            state.Events
            input.Amount
            cmd.Timestamp
      then
         TransferLimits.DailyDomesticLimit
         |> ExceededDailyDomesticTransferLimit
         |> transitionErr
      else
         map DomesticTransferPending state (DomesticTransferCommand.toEvent cmd)

   let domesticTransferProgress
      (state: AccountSnapshot)
      (cmd: UpdateDomesticTransferProgressCommand)
      =
      let account = state.Info
      let transferId = cmd.Data.BaseInfo.TransferId

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         let existing = Map.tryFind transferId state.InProgressDomesticTransfers

         match existing with
         | None ->
            map
               DomesticTransferProgress
               state
               (UpdateDomesticTransferProgressCommand.toEvent cmd)
         | Some txn ->
            let existingStatus = txn.Status

            if
               existingStatus = (DomesticTransferProgress.InProgress
                  cmd.Data.InProgressInfo)
            then
               transitionErr TransferProgressNoChange
            else
               map
                  DomesticTransferProgress
                  state
                  (UpdateDomesticTransferProgressCommand.toEvent cmd)

   let completeDomesticTransfer
      (state: AccountSnapshot)
      (cmd: CompleteDomesticTransferCommand)
      =
      let account = state.Info
      let transferId = cmd.Data.BaseInfo.TransferId

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else if
         Option.isNone
         <| Map.tryFind transferId state.InProgressDomesticTransfers
      then
         transitionErr TransferAlreadyProgressedToCompletedOrFailed
      else
         let retriedDueTo =
            state.FailedDomesticTransfers.TryFind(transferId)
            |> Option.bind (fun transfer ->
               match transfer.Status with
               | DomesticTransferProgress.Failed failReason -> Some failReason
               | _ -> None)

         let cmd = {
            cmd with
               Data.FromRetry = retriedDueTo
         }

         map
            DomesticTransferCompleted
            state
            (CompleteDomesticTransferCommand.toEvent cmd)

   let failDomesticTransfer
      (state: AccountSnapshot)
      (cmd: FailDomesticTransferCommand)
      =
      let account = state.Info
      let transferId = cmd.Data.BaseInfo.TransferId

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else if
         Option.isNone
         <| Map.tryFind transferId state.InProgressDomesticTransfers
      then
         transitionErr TransferAlreadyProgressedToCompletedOrFailed
      else
         map
            DomesticTransferFailed
            state
            (FailDomesticTransferCommand.toEvent cmd)

   let depositTransferWithinOrg
      (state: AccountSnapshot)
      (cmd: DepositInternalTransferWithinOrgCommand)
      =
      if state.Info.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         map InternalTransferWithinOrgDeposited state
         <| (DepositInternalTransferWithinOrgCommand.toEvent cmd)

   let depositTransferBetweenOrgs
      (state: AccountSnapshot)
      (cmd: DepositInternalTransferBetweenOrgsCommand)
      =
      if state.Info.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         map InternalTransferBetweenOrgsDeposited state
         <| (DepositInternalTransferBetweenOrgsCommand.toEvent cmd)

   let platformPaymentRequested
      (state: AccountSnapshot)
      (cmd: RequestPlatformPaymentCommand)
      =
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         map
            PlatformPaymentRequested
            state
            (RequestPlatformPaymentCommand.toEvent cmd)

   let platformPaymentCancelled
      (state: AccountSnapshot)
      (cmd: CancelPlatformPaymentCommand)
      =
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         map
            PlatformPaymentCancelled
            state
            (CancelPlatformPaymentCommand.toEvent cmd)

   let platformPaymentDeclined
      (state: AccountSnapshot)
      (cmd: DeclinePlatformPaymentCommand)
      =
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         map
            PlatformPaymentDeclined
            state
            (DeclinePlatformPaymentCommand.toEvent cmd)

   let platformPaymentPaid
      (state: AccountSnapshot)
      (cmd: FulfillPlatformPaymentCommand)
      =
      let input = cmd.Data.RequestedPayment.BaseInfo
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance account.Balance
      elif
         TransferLimits.exceedsDailyInternalTransferLimit
            state.Events
            input.Amount
            cmd.Timestamp
      then
         TransferLimits.DailyInternalLimit
         |> ExceededDailyInternalTransferLimit
         |> transitionErr
      else
         map
            PlatformPaymentPaid
            state
            (FulfillPlatformPaymentCommand.toEvent cmd)

   let platformPaymentDeposited
      (state: AccountSnapshot)
      (cmd: DepositPlatformPaymentCommand)
      =
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         map
            PlatformPaymentDeposited
            state
            (DepositPlatformPaymentCommand.toEvent cmd)

   let closeAccount (state: AccountSnapshot) (cmd: CloseAccountCommand) =
      map AccountEvent.AccountClosed state (CloseAccountCommand.toEvent cmd)

   let configureAutoTransferRule
      (state: AccountSnapshot)
      (cmd: ConfigureAutoTransferRuleCommand)
      =
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         match account.AutoTransferRule, cmd.Data.RuleIdToUpdate with
         | Some _, None -> transitionErr OnlyOneAutoTransferRuleMayExistAtATime
         | Some r, Some existingId when r.Id <> existingId ->
            transitionErr AutoTransferRuleDoesNotExist
         | None, Some _ -> transitionErr AutoTransferRuleDoesNotExist
         | _ ->
            map
               AutoTransferRuleConfigured
               state
               (ConfigureAutoTransferRuleCommand.toEvent cmd)

   let deleteAutoTransferRule
      (state: AccountSnapshot)
      (cmd: DeleteAutoTransferRuleCommand)
      =
      let account = state.Info

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         map
            AutoTransferRuleDeleted
            state
            (DeleteAutoTransferRuleCommand.toEvent cmd)

   let internalAutoTransfer
      (state: AccountSnapshot)
      (cmd: InternalAutoTransferCommand)
      =
      let input = cmd.Data.Transfer
      let account = state.Info
      let amount = PositiveAmount.get input.Amount

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      elif account.Balance - amount < 0m then
         transitionErr <| InsufficientBalance account.Balance
      elif input.Recipient.OrgId <> account.OrgId then
         transitionErr TransferExpectedToOccurWithinOrg
      elif
         TransferLimits.exceedsDailyInternalTransferLimit
            state.Events
            amount
            cmd.Timestamp
      then
         TransferLimits.DailyInternalLimit
         |> ExceededDailyInternalTransferLimit
         |> transitionErr
      else
         map
            InternalAutomatedTransferPending
            state
            (InternalAutoTransferCommand.toEvent cmd)

   let completeInternalAutoTransfer
      (state: AccountSnapshot)
      (cmd: CompleteInternalAutoTransferCommand)
      =
      let account = state.Info
      let transferId = cmd.Data.BaseInfo.TransferId

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else if
         Option.isNone
         <| Map.tryFind transferId state.InProgressInternalTransfers
      then
         transitionErr TransferAlreadyProgressedToCompletedOrFailed
      else
         map
            InternalAutomatedTransferCompleted
            state
            (CompleteInternalAutoTransferCommand.toEvent cmd)

   let failInternalAutoTransfer
      (state: AccountSnapshot)
      (cmd: FailInternalAutoTransferCommand)
      =
      let account = state.Info
      let transferId = cmd.Data.BaseInfo.TransferId

      if account.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else if
         Option.isNone
         <| Map.tryFind transferId state.InProgressInternalTransfers
      then
         transitionErr TransferAlreadyProgressedToCompletedOrFailed
      else
         map
            InternalAutomatedTransferFailed
            state
            (FailInternalAutoTransferCommand.toEvent cmd)

   let depositInternalAutoTransfer
      (state: AccountSnapshot)
      (cmd: DepositInternalAutoTransferCommand)
      =
      if state.Info.Status <> AccountStatus.Active then
         transitionErr AccountNotActive
      else
         map InternalAutomatedTransferDeposited state
         <| (DepositInternalAutoTransferCommand.toEvent cmd)

let stateTransition (state: AccountSnapshot) (command: AccountCommand) =
   match command with
   | AccountCommand.CreateAccount cmd -> StateTransition.create state cmd
   | AccountCommand.StartBillingCycle cmd ->
      StateTransition.startBillingcycle state cmd
   | AccountCommand.DepositCash cmd -> StateTransition.deposit state cmd
   | AccountCommand.Debit cmd -> StateTransition.debit state cmd
   | AccountCommand.MaintenanceFee cmd ->
      StateTransition.maintenanceFee state cmd
   | AccountCommand.SkipMaintenanceFee cmd ->
      StateTransition.skipMaintenanceFee state cmd
   | AccountCommand.InternalTransfer cmd ->
      StateTransition.internalTransfer state cmd
   | AccountCommand.CompleteInternalTransfer cmd ->
      StateTransition.completeInternalTransfer state cmd
   | AccountCommand.FailInternalTransfer cmd ->
      StateTransition.failInternalTransfer state cmd
   | AccountCommand.ScheduleInternalTransferBetweenOrgs cmd ->
      StateTransition.scheduleInternalTransferBetweenOrgs state cmd
   | AccountCommand.InternalTransferBetweenOrgs cmd ->
      StateTransition.internalTransferBetweenOrgs state cmd
   | AccountCommand.CompleteInternalTransferBetweenOrgs cmd ->
      StateTransition.completeInternalTransferBetweenOrgs state cmd
   | AccountCommand.FailInternalTransferBetweenOrgs cmd ->
      StateTransition.failInternalTransferBetweenOrgs state cmd
   | AccountCommand.DepositTransferWithinOrg cmd ->
      StateTransition.depositTransferWithinOrg state cmd
   | AccountCommand.DepositTransferBetweenOrgs cmd ->
      StateTransition.depositTransferBetweenOrgs state cmd
   | AccountCommand.ScheduleDomesticTransfer cmd ->
      StateTransition.scheduleDomesticTransfer state cmd
   | AccountCommand.DomesticTransfer cmd ->
      StateTransition.domesticTransfer state cmd
   | AccountCommand.CompleteDomesticTransfer cmd ->
      StateTransition.completeDomesticTransfer state cmd
   | AccountCommand.FailDomesticTransfer cmd ->
      StateTransition.failDomesticTransfer state cmd
   | AccountCommand.UpdateDomesticTransferProgress cmd ->
      StateTransition.domesticTransferProgress state cmd
   | AccountCommand.CloseAccount cmd -> StateTransition.closeAccount state cmd
   | AccountCommand.RequestPlatformPayment cmd ->
      StateTransition.platformPaymentRequested state cmd
   | AccountCommand.FulfillPlatformPayment cmd ->
      StateTransition.platformPaymentPaid state cmd
   | AccountCommand.DepositPlatformPayment cmd ->
      StateTransition.platformPaymentDeposited state cmd
   | AccountCommand.CancelPlatformPayment cmd ->
      StateTransition.platformPaymentCancelled state cmd
   | AccountCommand.DeclinePlatformPayment cmd ->
      StateTransition.platformPaymentDeclined state cmd
   | AccountCommand.ConfigureAutoTransferRule cmd ->
      StateTransition.configureAutoTransferRule state cmd
   | AccountCommand.DeleteAutoTransferRule cmd ->
      StateTransition.deleteAutoTransferRule state cmd
   | AccountCommand.InternalAutoTransfer cmd ->
      StateTransition.internalAutoTransfer state cmd
   | AccountCommand.CompleteInternalAutoTransfer cmd ->
      StateTransition.completeInternalAutoTransfer state cmd
   | AccountCommand.FailInternalAutoTransfer cmd ->
      StateTransition.failInternalAutoTransfer state cmd
   | AccountCommand.DepositInternalAutoTransfer cmd ->
      StateTransition.depositInternalAutoTransfer state cmd
