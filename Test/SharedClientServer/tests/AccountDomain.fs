module AccountDomainTests

open System

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Bank.Account.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes

module Stub = AccountStub

let update = ParentAccount.stateTransition
let initState = Stub.accountStateWithEvents

let getAccount (state: ParentAccountSnapshot) (accountId: AccountId) =
   state.VirtualAccounts.TryFind accountId |> Option.defaultValue Account.empty

let accountId = Stub.accountId

let tests =
   testList "Account Domain State Transitions" [
      test "DepositCashCommand with invalid amount" {
         for amount in [ 0m; -1m ] do
            let command =
               AccountCommand.DepositCash <| Stub.command.depositCash amount

            let res = update initState command
            let err = Expect.wantError res "should be Result.Error"

            Expect.stringContains
               (string err)
               "ValidationError"
               "should be a validation error"
      }

      test "DepositCashCommand with valid amount" {
         for amount in [ 0.5m; 100m ] do
            let command = Stub.command.depositCash amount

            let res = update initState (AccountCommand.DepositCash command)

            let _, state = Expect.wantOk res "should be Result.Ok"
            let account = getAccount state accountId
            let initAccount = getAccount initState accountId

            Expect.equal
               account.Balance
               (initAccount.Balance + command.Data.Amount)
               "should result in account balanced incremented by command amount"
      }

      test "DepositCashCommand should recompute maintenance fee criteria" {
         let initState = {
            initState with
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = false
               }
         }

         let command =
            Stub.command.depositCash <| MaintenanceFee.QualifyingDeposit - 1m

         let res = update initState (AccountCommand.DepositCash command)
         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.isFalse
            state.MaintenanceFeeCriteria.QualifyingDepositFound
            "Deposit amount < qualifying amount should not skip maintenance fee"

         let command = Stub.command.depositCash MaintenanceFee.QualifyingDeposit
         let res = update initState (AccountCommand.DepositCash command)
         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.isTrue
            state.MaintenanceFeeCriteria.QualifyingDepositFound
            "Deposit amount >= qualifying amount should skip maintenance fee"
      }

      test "TransferDepositCommand should recompute maintenance fee criteria" {
         let initState = {
            initState with
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = true
               }
         }

         let command =
            Stub.command.depositTransfer
            <| MaintenanceFee.QualifyingDeposit - 10m

         let res =
            update initState <| AccountCommand.DepositTransferWithinOrg command

         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.isFalse
            state.MaintenanceFeeCriteria.QualifyingDepositFound
            "Transfer deposit amount < qualifying amount should not skip maintenance fee"

         let command =
            Stub.command.depositTransfer MaintenanceFee.QualifyingDeposit

         let res =
            update initState (AccountCommand.DepositTransferWithinOrg command)

         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.isTrue
            state.MaintenanceFeeCriteria.QualifyingDepositFound
            "Deposit amount >= qualifying amount should skip maintenance fee"
      }

      test "InternalTransferBetweenOrgsCommand" {
         let transferAmount = 101m

         let transferCmd =
            Stub.command.internalTransferBetweenOrgs transferAmount

         let res =
            update initState
            <| AccountCommand.InternalTransferBetweenOrgs transferCmd

         let AccountEvent.InternalTransferBetweenOrgsPending evt, state =
            Expect.wantOk res "should be Result.Ok"

         let account = getAccount state accountId
         let initAccount = getAccount initState accountId

         Expect.equal
            account.Balance
            initAccount.Balance
            "A pending transfer reserves balance"

         Expect.equal
            account.AvailableBalance
            (initAccount.AvailableBalance - transferAmount)
            "A pending transfer adds a PendingDeduction"

         let settleCmd = Stub.settlePlatformTransfer evt

         let res =
            update
               state
               (AccountCommand.SettleInternalTransferBetweenOrgs settleCmd)

         let _, state = Expect.wantOk res "should be Result.Ok"
         let account = getAccount state accountId

         Expect.equal
            account.Balance
            (initAccount.Balance - transferAmount)
            "A settled transfer deducts amount from balance"
      }

      test "FailTransferCommand releases reserved funds" {
         let amount = 101m

         let res =
            update initState
            <| AccountCommand.InternalTransferBetweenOrgs(
               Stub.command.internalTransferBetweenOrgs amount
            )

         let InternalTransferBetweenOrgsPending evt, state =
            Expect.wantOk res "should be Result.Ok"

         let account = getAccount state accountId
         let initAccount = getAccount initState accountId

         Expect.equal
            account.AvailableBalance
            (initAccount.AvailableBalance - amount)
            "reserve the transfer amount from the balance"

         let command = Stub.failInternalTransferBetweenOrgs evt

         let res =
            update state
            <| AccountCommand.FailInternalTransferBetweenOrgs command

         let _, state = Expect.wantOk res "should be Result.Ok"
         let account = getAccount state accountId
         let initAccount = getAccount initState accountId

         Expect.equal
            account.AvailableBalance
            initAccount.AvailableBalance
            "should add the transfer amount back to the balance"
      }

      test "RefundDebitCommand recomputes maintenance fee" {
         let initAccount = getAccount initState accountId

         let initState = {
            initState with
               VirtualAccounts =
                  Map [
                     accountId,
                     {
                        initAccount with
                           Balance = MaintenanceFee.DailyBalanceThreshold + 100m
                     }
                  ]
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = true
               }
         }

         let amount = 101m

         let command = Stub.command.debit amount
         let res = update initState <| AccountCommand.Debit command

         let AccountEvent.DebitPending evt, state =
            Expect.wantOk res "should be Result.Ok"

         let settleCommand = Stub.settleDebitFromPending evt
         let res = update state <| AccountCommand.SettleDebit settleCommand

         let AccountEvent.DebitSettled settledEvt, state =
            Expect.wantOk res "should be Result.Ok"

         Expect.equal
            state.MaintenanceFeeCriteria.DailyBalanceThreshold
            false
            "the debit should invalidate the daily balance threshold"

         let refundCommand = Stub.refundDebitFromSettled settledEvt

         let res = update state <| AccountCommand.RefundDebit refundCommand

         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            state.MaintenanceFeeCriteria.DailyBalanceThreshold
            true
            "should enable skipping maintenance fee if a refund
             brings the balance back up"
      }

      test "PlatformTransferSettled should recompute maintenance fee criteria" {
         let initAccount = getAccount initState accountId

         let initState = {
            initState with
               VirtualAccounts =
                  Map [
                     accountId,
                     {
                        initAccount with
                           Balance = MaintenanceFee.DailyBalanceThreshold + 100m
                     }
                  ]
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = true
               }
         }

         let command = Stub.command.internalTransferBetweenOrgs 90m

         let res =
            update initState
            <| AccountCommand.InternalTransferBetweenOrgs command

         let AccountEvent.InternalTransferBetweenOrgsPending evt, state =
            Expect.wantOk res "should be Result.Ok"

         let command = Stub.settlePlatformTransfer evt

         let res =
            update state
            <| AccountCommand.SettleInternalTransferBetweenOrgs command

         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.isTrue
            state.MaintenanceFeeCriteria.DailyBalanceThreshold
            "Balance is still above threshold."

         let command = Stub.command.internalTransferBetweenOrgs 11m

         let res =
            update state <| AccountCommand.InternalTransferBetweenOrgs command

         let AccountEvent.InternalTransferBetweenOrgsPending evt, state =
            Expect.wantOk res "should be Result.Ok"

         let command = Stub.settlePlatformTransfer evt

         let res =
            update state
            <| AccountCommand.SettleInternalTransferBetweenOrgs command

         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.isFalse
            state.MaintenanceFeeCriteria.DailyBalanceThreshold
            "Balance must meet threshold at all times to avoid maintenance fee"
      }

      test "Debit should recompute maintenance fee criteria" {
         let initAccount = getAccount initState accountId

         let initState = {
            initState with
               VirtualAccounts =
                  Map [
                     accountId,
                     {
                        initAccount with
                           Balance = MaintenanceFee.DailyBalanceThreshold + 100m
                     }
                  ]
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = true
               }
         }

         let command = Stub.command.debit 90m
         let res = update initState <| AccountCommand.Debit command

         let AccountEvent.DebitPending evt, state =
            Expect.wantOk res "should be Result.Ok"

         let command = Stub.settleDebitFromPending evt
         let res = update state <| AccountCommand.SettleDebit command
         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.isTrue
            state.MaintenanceFeeCriteria.DailyBalanceThreshold
            "Balance is still above threshold."

         let debitCommand = Stub.command.debit 11m
         let res = update state <| AccountCommand.Debit debitCommand

         let AccountEvent.DebitPending evt, state =
            Expect.wantOk res "should be Result.Ok"

         let command = Stub.settleDebitFromPending evt
         let res = update state <| AccountCommand.SettleDebit command
         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.isFalse
            state.MaintenanceFeeCriteria.DailyBalanceThreshold
            "Balance must meet threshold at all times to avoid maintenance fee"
      }

      test "DebitCommand with invalid amount" {
         for amount in [ 0m; -1m ] do
            let command = Stub.command.debit amount
            let res = update initState <| AccountCommand.Debit command
            let err = Expect.wantError res "should be Result.Error"

            Expect.stringContains
               (string err)
               "ValidationError"
               "should be a validation error"
      }

      test "DebitCommand with valid amount" {
         for amount in [ 25.5m; 100m ] do
            let command = Stub.command.debit amount
            let res = update initState <| AccountCommand.Debit command
            let _, state = Expect.wantOk res "should be Result.Ok"
            let account = getAccount state accountId
            let initAccount = getAccount initState accountId

            Expect.equal
               account.AvailableBalance
               (initAccount.AvailableBalance - command.Data.Amount)
               "should result in available balance decremented by command amount"
      }

      test "DebitCommand against an account with insufficient balance" {
         let initAccount = getAccount initState accountId

         let state = {
            initState with
               VirtualAccounts =
                  Map.add
                     accountId
                     { initAccount with Balance = 200m }
                     initState.VirtualAccounts
         }

         let command = Stub.command.debit 300m
         let res = update state <| AccountCommand.Debit command
         let err = Expect.wantError res "should be Result.Error"

         Expect.stringContains
            (string err)
            "InsufficientBalance"
            "should be an InsufficientBalance validation error"
      }

      test "Platform transfers accrue a daily transfer balance" {
         let updates =
            List.fold
               (fun acc amount ->
                  let account, total = acc
                  let cmd = Stub.command.internalTransferBetweenOrgs amount

                  let res =
                     update account
                     <| AccountCommand.InternalTransferBetweenOrgs cmd

                  let _, newState = Expect.wantOk res "should be Result.Ok"

                  newState, total + amount)
               (initState, 0m)
               [ 25m; 30m; 100m ]

         let newState, transferTotal = updates

         Expect.equal
            (ParentAccount.TransferLimits.dailyPlatformTransferAccrued
               newState.Events)
            transferTotal
            "DailyPlatformTransferAccrued should accrue transfers for the day"

         let command = Stub.command.internalTransfer 10m

         let command = {
            command with
               // TODO
               Data.ScheduledDateSeedOverride =
                  Some(DateTime.UtcNow.AddDays(-1))
         }

         let res = update newState <| AccountCommand.InternalTransfer command
         let _, newState = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            (ParentAccount.TransferLimits.dailyPlatformTransferAccrued
               newState.Events)
            transferTotal
            "DailyPlatformTransferAccrued should not accrue transfers older than a day"
      }

      test "Failed internal transfers are removed from daily transfer accrual" {
         let transferAmount = 100m
         let command = Stub.command.internalTransferBetweenOrgs transferAmount

         let res =
            update initState
            <| AccountCommand.InternalTransferBetweenOrgs command

         let InternalTransferBetweenOrgsPending evt, state =
            Expect.wantOk res "should be Result.Ok"

         Expect.equal
            (ParentAccount.TransferLimits.dailyPlatformTransferAccrued
               state.Events)
            transferAmount
            "DailyInternalTransferAccrued should accrue transfers for the day"

         let cmd = Stub.failInternalTransferBetweenOrgs evt

         let res =
            update state <| AccountCommand.FailInternalTransferBetweenOrgs cmd

         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            (ParentAccount.TransferLimits.dailyPlatformTransferAccrued
               state.Events)
            0m
            "DailyInternalTransferAccrued should not include a failed transfer"
      }

      test "Domestic transfers accrue a daily transfer balance" {
         let updates =
            List.fold
               (fun acc amount ->
                  let account, total = acc
                  let cmd = Stub.command.domesticTransfer amount

                  let res =
                     update account <| AccountCommand.DomesticTransfer cmd

                  let _, newState = Expect.wantOk res "should be Result.Ok"

                  newState, total + amount)
               (initState, 0m)
               [ 25m; 30m; 100m ]

         let newState, transferTotal = updates

         Expect.equal
            (ParentAccount.TransferLimits.dailyDomesticTransferAccrued
               newState.Events)
            transferTotal
            "DailyDomesticTransferAccrued should accrue transfers for the day"

         let command = Stub.command.domesticTransfer 10m

         let command = {
            command with
               // TODO
               Data.ScheduledDateSeedOverride =
                  Some(DateTime.UtcNow.AddDays(-1))
         }

         let res = update newState <| AccountCommand.DomesticTransfer command
         let _, newState = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            (ParentAccount.TransferLimits.dailyDomesticTransferAccrued
               newState.Events)
            transferTotal
            "DailyDomesticTransferAccrued should not accrue transfers older than a day"
      }

      test "Rejected domestic transfers are removed from daily transfer accrual" {
         let transferAmount = 100m
         let command = Stub.command.domesticTransfer transferAmount

         let res = update initState <| AccountCommand.DomesticTransfer command

         let (AccountEvent.DomesticTransferPending transferPendingEvt), state =
            Expect.wantOk res "should be Result.Ok"

         Expect.equal
            (ParentAccount.TransferLimits.dailyDomesticTransferAccrued
               state.Events)
            transferAmount
            "DailyDomesticTransferAccrued should accrue transfers for the day"

         let account = getAccount state accountId

         let cmd =
            FailDomesticTransferCommand.create
               transferPendingEvt.CorrelationId
               transferPendingEvt.InitiatedBy
               {
                  BaseInfo = transferPendingEvt.Data.BaseInfo
                  Reason = DomesticTransferFailReason.AccountNotActive
               }

         let res = update state <| AccountCommand.FailDomesticTransfer cmd
         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            (ParentAccount.TransferLimits.dailyDomesticTransferAccrued
               state.Events)
            0m
            "DailyDomesticTransferAccrued should not include a rejected transfer"
      }

      test
         "Maintenance fee command should recompute fee criteria for next
          billing cycle" {
         let initAccount = getAccount initState accountId

         let initState = {
            initState with
               VirtualAccounts =
                  Map [
                     accountId,
                     {
                        initAccount with
                           Balance = MaintenanceFee.DailyBalanceThreshold
                     }
                  ]
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = true
               }
         }

         let billingDate = DateTime.UtcNow
         let command = Stub.command.maintenanceFee billingDate
         let res = update initState <| AccountCommand.MaintenanceFee command
         let _, state = Expect.wantOk res "should be Result.Ok"
         let account = getAccount state accountId
         let initAccount = getAccount initState accountId

         Expect.equal
            account.Balance
            (initAccount.Balance - command.Data.Amount)
            "maintenance fee decrements balance by configured amount"

         Expect.equal
            state.MaintenanceFeeCriteria.DailyBalanceThreshold
            false
            "maintenance fee dropping balance below threshold invalidates
             daily balance threshold criteria for next billing cycle"

         let initAccount = getAccount initState accountId

         let state = {
            initState with
               VirtualAccounts =
                  Map [
                     accountId,
                     {
                        initAccount with
                           Balance = MaintenanceFee.DailyBalanceThreshold + 100m
                     }
                  ]
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = false
               }
         }

         let cmd =
            AccountCommand.MaintenanceFee(
               Stub.command.maintenanceFee billingDate
            )

         let res = update state cmd
         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            state.MaintenanceFeeCriteria.DailyBalanceThreshold
            true
            "daily balance threshold criteria for next billing cycle
             recomputed based on balance"
      }

      test
         "Skip maintenance fee command should recompute fee criteria for next
          billing cycle" {
         let initAccount = getAccount initState accountId

         let initState = {
            initState with
               VirtualAccounts =
                  Map [
                     accountId,
                     {
                        initAccount with
                           Balance = MaintenanceFee.DailyBalanceThreshold
                     }
                  ]
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = true
               }
         }

         let billingDate = DateTime.UtcNow
         let command = Stub.command.skipMaintenanceFee billingDate
         let res = update initState <| AccountCommand.SkipMaintenanceFee command
         let _, state = Expect.wantOk res "should be Result.Ok"
         let account = getAccount state accountId
         let initAccount = getAccount initState accountId

         Expect.equal
            account.Balance
            initAccount.Balance
            "skip maintenance - balance should be unchanged"

         Expect.equal
            state.MaintenanceFeeCriteria.DailyBalanceThreshold
            true
            "daily balance threshold criteria for next billing cycle
             recomputed based on balance"

         let initAccount = getAccount initState accountId

         let state = {
            initState with
               VirtualAccounts =
                  Map [
                     accountId,
                     {
                        initAccount with
                           Balance = MaintenanceFee.DailyBalanceThreshold - 1m
                     }
                  ]
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = false
               }
         }

         let cmd = Stub.command.skipMaintenanceFee billingDate
         let res = update state <| AccountCommand.SkipMaintenanceFee cmd
         let _, state = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            state.MaintenanceFeeCriteria.DailyBalanceThreshold
            false
            "daily balance threshold criteria for next billing cycle
             recomputed based on balance"
      }

      test "Commands against account with closed status" {
         let state = {
            initState with
               VirtualAccounts =
                  Map [
                     for account in initState.VirtualAccounts.Values do
                        account.AccountId,
                        {
                           account with
                              Status = AccountStatus.Closed
                        }
                  ]
         }

         let billingDate = DateTime.UtcNow

         let (commands: AccountCommand list) = [
            AccountCommand.DepositCash <| Stub.command.depositCash 10m
            AccountCommand.InternalTransfer <| Stub.command.internalTransfer 33m
            AccountCommand.DomesticTransfer <| Stub.command.domesticTransfer 31m
            AccountCommand.DepositTransferWithinOrg
            <| Stub.command.depositTransfer 931m
            AccountCommand.MaintenanceFee(
               Stub.command.maintenanceFee billingDate
            )
            AccountCommand.SkipMaintenanceFee(
               Stub.command.skipMaintenanceFee billingDate
            )
         ]

         for command in commands do
            let res = update state command
            let err = Expect.wantError res "should be Result.Error"

            Expect.stringContains
               (string err)
               "AccountNotActive"
               "should be an AccountNotActive StateTransition error"

         let cmd = Stub.command.debit 13m
         let res = update state <| AccountCommand.Debit cmd
         let err = Expect.wantError res "should be Result.Error"

         Expect.stringContains
            (string err)
            "AccountNotActive"
            "debit should result in an AccountNotActive StateTransition error"

         let res =
            update state
            <| AccountCommand.CreateVirtualAccount Stub.command.createAccount

         let err = Expect.wantError res "should be Result.Error"

         Expect.stringContains
            (string err)
            "AccountNotReadyToActivate"
            "createshould be an AccountNotReadyToActivate StateTransition error"

         let res =
            update state
            <| AccountCommand.CloseAccount Stub.command.closeAccount

         Expect.wantOk res "should be Result.Ok" |> ignore
      }
   ]
