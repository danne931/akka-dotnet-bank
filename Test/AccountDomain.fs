module AccountDomainTests

open System
open Expecto

open BankTypes
open Lib.Types

let update = Account.stateTransition

[<Tests>]
let tests =
   testList "Account Domain State Transitions" [
      test "DepositCashCommand with invalid amount" {
         for amount in [ 0m; -1m ] do
            let command = Stub.command.depositCash amount
            let res = update Stub.accountState command
            let err = Expect.wantError res "should be Result.Error"

            Expect.stringContains
               (string err)
               "ValidationError"
               "should be a validation error"
      }

      test "DepositCashCommand with valid amount" {
         for amount in [ 0.5m; 100m ] do
            let command = Stub.command.depositCash amount
            let res = update Stub.accountState command
            let _, account = Expect.wantOk res "should be Result.Ok"

            Expect.equal
               account.Balance
               (Stub.accountState.Balance + command.Amount)
               "should result in account balanced incremented by command amount"
      }

      test "DepositCashCommand should recompute maintenance fee criteria" {
         let initState = {
            Stub.accountState with
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = false
               }
         }

         let command =
            Stub.command.depositCash <| MaintenanceFee.QualifyingDeposit - 1m

         let res = update initState command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.isFalse
            account.MaintenanceFeeCriteria.QualifyingDepositFound
            "Deposit amount < qualifying amount should not skip maintenance fee"

         let command = Stub.command.depositCash MaintenanceFee.QualifyingDeposit
         let res = update initState command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.isTrue
            account.MaintenanceFeeCriteria.QualifyingDepositFound
            "Deposit amount >= qualifying amount should skip maintenance fee"
      }

      test "TransferDepositCommand should recompute maintenance fee criteria" {
         let initState = {
            Stub.accountState with
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = false
               }
         }

         let command =
            Stub.command.depositTransfer
            <| MaintenanceFee.QualifyingDeposit - 10m

         let res = update initState command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.isFalse
            account.MaintenanceFeeCriteria.QualifyingDepositFound
            "Transfer deposit amount < qualifying amount should not skip maintenance fee"

         let command =
            Stub.command.depositTransfer MaintenanceFee.QualifyingDeposit

         let res = update initState command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.isTrue
            account.MaintenanceFeeCriteria.QualifyingDepositFound
            "Deposit amount >= qualifying amount should skip maintenance fee"
      }

      test "ApproveTransferCommand" {
         let res = update Stub.accountState Stub.command.approveTransfer
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            account.Balance
            Stub.accountState.Balance
            "Approving a transfer does not increment balance"
      }

      test "RejectTransferCommand updates balance" {
         let command = Stub.command.rejectTransfer 101m
         let res = update Stub.accountState command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            account.Balance
            (Stub.accountState.Balance + command.Amount)
            "should add the transfer amount back to the balance"
      }

      test "RejectTransferCommand recomputes maintenance fee" {
         let initState = {
            Stub.accountState with
               Balance = MaintenanceFee.DailyBalanceThreshold + 100m
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = true
               }
         }

         let command = Stub.command.registerInternalRecipient
         let res = update initState command
         let _, account = Expect.wantOk res "should be Result.Ok"

         let amount = 101m

         let res = update account <| Stub.command.internalTransfer amount
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            account.MaintenanceFeeCriteria.DailyBalanceThreshold
            false
            "the transfer should invalidate the daily balance threshold"

         let res = update account <| Stub.command.rejectTransfer amount
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            account.MaintenanceFeeCriteria.DailyBalanceThreshold
            true
            "should enable skipping maintenance fee if a rejected transfer
             brings the balance back up to the daily balance threshold"
      }

      test "TransferPending should recompute maintenance fee criteria" {
         let initState = {
            Stub.accountState with
               Balance = MaintenanceFee.DailyBalanceThreshold + 100m
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = true
               }
         }

         let command = Stub.command.registerInternalRecipient
         let res = update initState command
         let _, account = Expect.wantOk res "should be Result.Ok"

         let command = Stub.command.internalTransfer 90m
         let res = update account command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.isTrue
            account.MaintenanceFeeCriteria.DailyBalanceThreshold
            "Balance is still above threshold."

         let command = Stub.command.internalTransfer 11m
         let res = update account command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.isFalse
            account.MaintenanceFeeCriteria.DailyBalanceThreshold
            "Balance must meet threshold at all times to avoid maintenance fee"
      }

      test "Debit should recompute maintenance fee criteria" {
         let initState = {
            Stub.accountState with
               Balance = MaintenanceFee.DailyBalanceThreshold + 100m
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = true
               }
         }

         let command = Stub.command.debit 90m
         let res = update initState command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.isTrue
            account.MaintenanceFeeCriteria.DailyBalanceThreshold
            "Balance is still above threshold."

         let command = Stub.command.debit 11m
         let res = update account command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.isFalse
            account.MaintenanceFeeCriteria.DailyBalanceThreshold
            "Balance must meet threshold at all times to avoid maintenance fee"
      }

      test "DebitCommand with invalid amount" {
         for amount in [ 0m; -1m ] do
            let command = Stub.command.debit amount
            let res = update Stub.accountState command
            let err = Expect.wantError res "should be Result.Error"

            Expect.stringContains
               (string err)
               "ValidationError"
               "should be a validation error"
      }

      test "DebitCommand with valid amount" {
         for amount in [ 25.5m; 100m ] do
            let command = Stub.command.debit amount
            let res = update Stub.accountState command
            let _, account = Expect.wantOk res "should be Result.Ok"

            Expect.equal
               account.Balance
               (Stub.accountState.Balance - command.Amount)
               "should result in account balanced decremented by command amount"
      }

      test "DebitCommand against an account with locked card" {
         let state = {
            Stub.accountState with
               Status = AccountStatus.CardLocked
         }

         let command = Stub.command.debit 9.31m
         let res = update state command
         let err = Expect.wantError res "should be Result.Error"

         Expect.stringContains
            (string err)
            "AccountCardLocked"
            "should be an AccountCardLocked validation error"
      }

      test "DebitCommand against an account with insufficient balance" {
         let state = {
            Stub.accountState with
               Balance = 200m
         }

         let command = Stub.command.debit 300m
         let res = update state command
         let err = Expect.wantError res "should be Result.Error"

         Expect.stringContains
            (string err)
            "InsufficientBalance"
            "should be an InsufficientBalance validation error"
      }

      test "Debits accrue a daily debit balance" {
         let initState = {
            Stub.accountState with
               DailyDebitAccrued = 0m
         }

         let updates =
            List.fold
               (fun acc amount ->
                  let account, total = acc
                  let res = update account <| Stub.command.debit amount

                  let _, newState = Expect.wantOk res "should be Result.Ok"

                  newState, total + amount)
               (initState, 0m)
               [ 21m; 33m; 109m ]

         let newState, debitTotal = updates

         Expect.equal
            newState.DailyDebitAccrued
            debitTotal
            "DailyDebitAccrued should accrue debits for the day"

         let command =
            Stub.command.debitWithDate 10m <| DateTime.UtcNow.AddDays(-1)

         let res = update newState command
         let _, newState = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            newState.DailyDebitAccrued
            debitTotal
            "DailyDebitAccrued should not accrue debits older than a day"
      }

      test "Setting a daily debit limit" {
         let state = {
            Stub.accountState with
               Balance = 1000m
               DailyDebitAccrued = 0m
               DailyDebitLimit = 0m
         }

         let command = Stub.command.limitDailyDebits 100m
         let res = update state command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            account.DailyDebitLimit
            100m
            "Daily debit limit should be set"

         let command = Stub.command.debit 99m
         let res = update account command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            (account.Balance, account.DailyDebitAccrued)
            (state.Balance - 99m, 99m)
            "Daily debit should be accrued if under daily limit"

         let command = Stub.command.debit 2m
         let res = update account command
         let err = Expect.wantError res "should be Result.Error"

         Expect.stringContains
            (string err)
            "ExceededDailyDebit"
            "should be an ExceededDailyDebit validation error"
      }

      test
         "Maintenance fee command should recompute fee criteria for next
          billing cycle" {
         let initState = {
            Stub.accountState with
               Balance = MaintenanceFee.DailyBalanceThreshold
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = true
               }
         }

         let command = Stub.command.maintenanceFee
         let res = update initState command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            account.Balance
            (initState.Balance - command.Amount)
            "maintenance fee decrements balance by configured amount"

         Expect.equal
            account.MaintenanceFeeCriteria.DailyBalanceThreshold
            false
            "maintenance fee dropping balance below threshold invalidates
             daily balance threshold criteria for next billing cycle"

         let state = {
            Stub.accountState with
               Balance = MaintenanceFee.DailyBalanceThreshold + 100m
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = false
               }
         }

         let res = update state Stub.command.maintenanceFee
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            account.MaintenanceFeeCriteria.DailyBalanceThreshold
            true
            "daily balance threshold criteria for next billing cycle
             recomputed based on balance"
      }

      test
         "Skip maintenance fee command should recompute fee criteria for next
          billing cycle" {
         let initState = {
            Stub.accountState with
               Balance = MaintenanceFee.DailyBalanceThreshold
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = true
               }
         }

         let command = Stub.command.skipMaintenanceFee
         let res = update initState command
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            account.Balance
            initState.Balance
            "skip maintenance - balance should be unchanged"

         Expect.equal
            account.MaintenanceFeeCriteria.DailyBalanceThreshold
            true
            "daily balance threshold criteria for next billing cycle
             recomputed based on balance"

         let state = {
            Stub.accountState with
               Balance = MaintenanceFee.DailyBalanceThreshold - 1m
               MaintenanceFeeCriteria = {
                  QualifyingDepositFound = false
                  DailyBalanceThreshold = false
               }
         }

         let res = update state Stub.command.skipMaintenanceFee
         let _, account = Expect.wantOk res "should be Result.Ok"

         Expect.equal
            account.MaintenanceFeeCriteria.DailyBalanceThreshold
            false
            "daily balance threshold criteria for next billing cycle
             recomputed based on balance"
      }

      test "Commands against account with closed status" {
         let state = {
            Stub.accountState with
               Status = AccountStatus.Closed
         }

         let (commands: Command list) = [
            Stub.command.depositCash 10m
            Stub.command.internalTransfer 33m
            Stub.command.domesticTransfer 31m
            Stub.command.limitDailyDebits 101m
            Stub.command.registerInternalRecipient
            Stub.command.registerDomesticRecipient
            Stub.command.depositTransfer 931m
            Stub.command.lockCard
            Stub.command.unlockCard
            Stub.command.maintenanceFee
            Stub.command.skipMaintenanceFee
         ]

         for command in commands do
            let res = update state command
            let err = Expect.wantError res "should be Result.Error"

            Expect.stringContains
               (string err)
               "AccountTransactionProcessingDisabled"
               "should be an AccountTransactionProcessingDisabled StateTransition error"

         let res = update state <| Stub.command.debit 13m
         let err = Expect.wantError res "should be Result.Error"

         Expect.stringContains
            (string err)
            "AccountNotActive"
            "debit should result in an AccountNotActive StateTransition error"

         let res = update state Stub.command.createAccount
         let err = Expect.wantError res "should be Result.Error"

         Expect.stringContains
            (string err)
            "AccountNotReadyToActivate"
            "createshould be an AccountNotReadyToActivate StateTransition error"

         let res = update state Stub.command.closeAccount
         Expect.wantOk res "should be Result.Ok" |> ignore
      }

      test "Commands against account with locked card" {
         let state = {
            Stub.accountState with
               Status = AccountStatus.CardLocked
         }

         let (commands: Command list) = [
            Stub.command.depositCash 10m
            Stub.command.limitDailyDebits 101m
            Stub.command.registerInternalRecipient
            Stub.command.registerDomesticRecipient
            Stub.command.depositTransfer 931m
            Stub.command.lockCard
            Stub.command.unlockCard
            Stub.command.maintenanceFee
            Stub.command.skipMaintenanceFee
         ]

         for command in commands do
            let res = update state command
            Expect.wantOk res "should be Result.Ok" |> ignore

         let state = {
            Stub.accountState with
               Status = AccountStatus.CardLocked
               TransferRecipients =
                  Map [
                     Account.recipientLookupKey (Stub.internalRecipient),
                     Stub.internalRecipient
                     Account.recipientLookupKey (Stub.domesticRecipient),
                     Stub.domesticRecipient
                  ]
         }

         let (commands: Command list) = [
            Stub.command.internalTransfer 33m
            Stub.command.domesticTransfer 31m
         ]

         for command in commands do
            let res = update state command
            Expect.wantOk res "should be Result.Ok" |> ignore

         let res = update state <| Stub.command.debit 13m
         let err = Expect.wantError res "should be Result.Error"

         Expect.stringContains
            (string err)
            "AccountCardLocked"
            "debit should result in an AccountCardLocked StateTransition error"

         let res = update state Stub.command.closeAccount
         Expect.wantOk res "should be Result.Ok" |> ignore
      }
   ]