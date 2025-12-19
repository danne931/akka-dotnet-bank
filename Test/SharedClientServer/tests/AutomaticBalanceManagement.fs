module AutomaticBalanceManagementTests

open System

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Bank.Account.Domain
open Bank.Transfer.Domain
open AutomaticTransfer
open Lib.SharedTypes
open ParentAccount.AutoTransferStateTransition

module Stub =
   let orgId = Guid.NewGuid() |> OrgId
   let parentAccountId = Guid.NewGuid() |> ParentAccountId

   let accountA = Guid.NewGuid() |> AccountId
   let accountB = Guid.NewGuid() |> AccountId
   let accountC = Guid.NewGuid() |> AccountId
   let accountD = Guid.NewGuid() |> AccountId
   let accountE = Guid.NewGuid() |> AccountId

   let accountState = {
      Account.empty with
         AccountId = accountA
         ParentAccountId = parentAccountId
         OrgId = orgId
         Status = AccountStatus.Active
         Balance = 1000m
   }

   let parentAccountState = {
      ParentAccountSnapshot.empty with
         Status = ParentAccountStatus.Active
         ParentAccountId = parentAccountId
         OrgId = orgId
         VirtualAccounts = Map [ accountA, accountState ]
   }

   let sender: InternalTransferSender = {
      Name = "Operations"
      AccountId = accountA
      ParentAccountId = parentAccountId
      OrgId = orgId
   }

   let recipient: InternalTransferRecipient = {
      Name = "Savings"
      AccountId = accountB
      ParentAccountId = parentAccountId
      OrgId = orgId
   }

   let percentDistributionDestinationAccount
      : UnvalidatedDistributionDestinationAccount = {
      Recipient = recipient
      ProposedPercentAllocated = 100m
   }

   let targetBalanceRule = {
      TargetAccount = {
         Name = "Operations"
         AccountId = accountA
         ParentAccountId = parentAccountId
         OrgId = orgId
      }
      ManagingPartnerAccount = {
         Name = "Savings"
         AccountId = accountB
         ParentAccountId = parentAccountId
         OrgId = orgId
      }
      TargetAccountBalance =
         PositiveAmount.create 113_000m |> Result.toOption |> _.Value
      TargetBalanceRange = None
   }

   let createZeroBalanceConfig accountIdA accountIdB =
      let rule: ZeroBalanceRule = {
         Sender = { sender with AccountId = accountIdA }
         Recipient = {
            recipient with
               AccountId = accountIdB
         }
      }

      {
         Id = Guid.NewGuid()
         Info = AutomaticTransferRule.ZeroBalance rule
      }

   let createTargetBalanceConfig accountIdA accountIdB =
      let rule = {
         TargetAccount = {
            Name = "Operations"
            AccountId = accountIdA
            ParentAccountId = parentAccountId
            OrgId = orgId
         }
         ManagingPartnerAccount = {
            Name = "Savings"
            AccountId = accountIdB
            ParentAccountId = parentAccountId
            OrgId = orgId
         }
         TargetAccountBalance =
            PositiveAmount.create 113_000m |> Result.toOption |> _.Value
         TargetBalanceRange = None
      }

      {
         Id = Guid.NewGuid()
         Info = AutomaticTransferRule.TargetBalance rule
      }

   let createTargetBalanceConfigWithRange accountIdA accountIdB range =
      let rule = {
         TargetAccount = {
            Name = "Operations"
            AccountId = accountIdA
            ParentAccountId = parentAccountId
            OrgId = orgId
         }
         ManagingPartnerAccount = {
            Name = "Savings"
            AccountId = accountIdB
            ParentAccountId = parentAccountId
            OrgId = orgId
         }
         TargetAccountBalance =
            PositiveAmount.create 113_000m |> Result.toOption |> _.Value
         TargetBalanceRange = Some range
      }

      {
         Id = Guid.NewGuid()
         Info = AutomaticTransferRule.TargetBalance rule
      }

   let createPercentDistributionConfig a b c =
      let sender = { sender with AccountId = a }

      let rule =
         PercentDistributionRule.create Frequency.PerTransaction sender [
            {
               Recipient = { recipient with AccountId = b }
               ProposedPercentAllocated = 50m
            }
            {
               Recipient = { recipient with AccountId = c }
               ProposedPercentAllocated = 50m
            }
         ]

      let rule = (Result.toOption rule).Value

      {
         Id = Guid.NewGuid()
         Info = AutomaticTransferRule.PercentDistribution rule
      }

   let initiator = {
      Id = Guid.NewGuid() |> EmployeeId |> InitiatedById
      Name = "Test User"
   }

type PercentErr = PercentDistributionRule.ValidationError

let tests =
   testList "Automatic transfer domain" [
      test "Percent distribution should have at least 1 destination" {
         let res =
            PercentDistributionRule.create Frequency.PerTransaction Stub.sender []

         Expect.equal
            res
            (Error PercentErr.LessThan1Destination)
            "needs >= 1 destination"

         let res =
            PercentDistributionRule.create Frequency.PerTransaction Stub.sender [
               Stub.percentDistributionDestinationAccount
            ]

         Expect.isOk res "accept with at least 1 valid destination"
      }

      test "Percent distribution sender should be includes in destinations" {
         let res =
            PercentDistributionRule.create Frequency.PerTransaction Stub.sender [
               {
                  Recipient = {
                     Stub.recipient with
                        AccountId = Stub.sender.AccountId
                  }
                  ProposedPercentAllocated = 50m
               }
               {
                  Recipient = Stub.recipient
                  ProposedPercentAllocated = 50m
               }
            ]

         Expect.equal
            res
            (Error PercentErr.SenderIsDestination)
            "sender can not be recipient"
      }

      test "Percent distribution allocation invalid when less than 100" {
         let res =
            PercentDistributionRule.create Frequency.PerTransaction Stub.sender [
               {
                  Recipient = {
                     Stub.recipient with
                        AccountId = Guid.NewGuid() |> AccountId
                  }
                  ProposedPercentAllocated = 49m
               }
               {
                  Recipient = Stub.recipient
                  ProposedPercentAllocated = 50m
               }
            ]

         Expect.equal
            res
            (Error(PercentErr.TotalPercentAllocatedNot100 99m))
            "percent allocated expected to be 100"
      }

      test "Percent distribution allocation invalid when greater than 100" {
         let res =
            PercentDistributionRule.create Frequency.PerTransaction Stub.sender [
               {
                  Recipient = {
                     Stub.recipient with
                        AccountId = Guid.NewGuid() |> AccountId
                  }
                  ProposedPercentAllocated = 51m
               }
               {
                  Recipient = Stub.recipient
                  ProposedPercentAllocated = 50m
               }
            ]

         Expect.equal
            res
            (Error(PercentErr.TotalPercentAllocatedNot100 101m))
            "percent allocated expected to be 100"
      }

      test "Percent distribution allocation should equal 100" {
         let res =
            PercentDistributionRule.create Frequency.PerTransaction Stub.sender [
               {
                  Recipient = {
                     Stub.recipient with
                        AccountId = Guid.NewGuid() |> AccountId
                  }
                  ProposedPercentAllocated = 50m
               }
               {
                  Recipient = Stub.recipient
                  ProposedPercentAllocated = 50m
               }
            ]

         Expect.isOk res "percent allocated expected to be 100"
      }

      test "Percent distribution may not include a negative allocation" {
         let res =
            PercentDistributionRule.create Frequency.PerTransaction Stub.sender [
               {
                  Recipient = {
                     Stub.recipient with
                        AccountId = Guid.NewGuid() |> AccountId
                  }
                  ProposedPercentAllocated = 150m
               }
               {
                  Recipient = Stub.recipient
                  ProposedPercentAllocated = -50m
               }
            ]

         Expect.equal
            res
            (Error PercentErr.ContainsNegativePercentAllocation)
            "percent allocated may not include a negative %"
      }

      test "Percent distribution rule computeTransfer" {
         let recipient1 = {
            Stub.recipient with
               AccountId = Guid.NewGuid() |> AccountId
         }

         let recipient2 = Stub.recipient

         let res =
            PercentDistributionRule.create Frequency.PerTransaction Stub.sender [
               {
                  Recipient = recipient1
                  ProposedPercentAllocated = 75m
               }
               {
                  Recipient = recipient2
                  ProposedPercentAllocated = 25m
               }
            ]

         let rule = Expect.wantOk res ""

         let balance =
            PositiveAmount.create 1_000m |> Result.toOption |> _.Value

         let computed = PercentDistributionRule.computeTransfer rule balance

         let findComputed (accountId: AccountId) =
            computed |> List.find (fun t -> t.Recipient.AccountId = accountId)

         let compute1 = findComputed recipient1.AccountId

         Expect.equal
            compute1.Amount
            (Result.toOption (PositiveAmount.create 750m)).Value
            ""

         let compute2 = findComputed recipient2.AccountId

         Expect.equal
            compute2.Amount
            (Result.toOption (PositiveAmount.create 250m)).Value
            ""
      }

      test "Zero balance rule computeTransfer" {
         let rule: ZeroBalanceRule = {
            Sender = Stub.sender
            Recipient = Stub.recipient
         }

         let balance = (Result.toOption (PositiveAmount.create 1500m)).Value
         let compute = ZeroBalanceRule.computeTransfer rule balance

         Expect.equal compute.Amount balance ""
      }

      test "Target balance rule no transfer if target balance met" {
         let rule = Stub.targetBalanceRule

         let balance = rule.TargetAccountBalance.Value
         let compute = TargetBalanceRule.computeTransfer rule balance
         Expect.isNone compute "no computed transfer if target balance"

         let compute = TargetBalanceRule.computeTransfer rule (balance - 1m)
         Expect.isSome compute "computed transfer if balance dips below target"

         let compute = TargetBalanceRule.computeTransfer rule (balance + 1m)

         Expect.isSome
            compute
            "computed transfer if balance raises above target"
      }

      test "Target balance rule no transfer if target balance range met" {
         let rule = Stub.targetBalanceRule
         let balance = rule.TargetAccountBalance.Value

         let rule = {
            rule with
               TargetBalanceRange =
                  Some {
                     LowerBound =
                        PositiveAmount.create (balance - 100m)
                        |> Result.toOption
                        |> _.Value
                     UpperBound =
                        PositiveAmount.create (balance + 150m)
                        |> Result.toOption
                        |> _.Value
                  }
         }

         let compute = TargetBalanceRule.computeTransfer rule balance
         Expect.isNone compute ""

         let compute = TargetBalanceRule.computeTransfer rule (balance - 50m)
         Expect.isNone compute ""

         let compute = TargetBalanceRule.computeTransfer rule (balance + 50m)
         Expect.isNone compute ""

         let compute = TargetBalanceRule.computeTransfer rule (balance - 101m)

         Expect.isSome
            compute
            "computed transfer if balance dips below lower range"

         let compute = TargetBalanceRule.computeTransfer rule (balance + 151m)

         Expect.isSome
            compute
            "computed transfer if balance raises above upper range"
      }

      test
         "Cycle detection prevents adding a rule where transfer
            direction is A->B if a rule where B->A exists already" {
         let existing = [
            Stub.createZeroBalanceConfig Stub.accountA Stub.accountB
         ]

         let configToAdd =
            Stub.createZeroBalanceConfig Stub.accountB Stub.accountA

         let hasCycle = CycleDetection.cycleDetected configToAdd existing
         Expect.isTrue hasCycle "A->B, B->A = cycle"

         let configToAdd =
            Stub.createZeroBalanceConfig Stub.accountC Stub.accountA

         let hasCycle = CycleDetection.cycleDetected configToAdd existing
         Expect.isFalse hasCycle "A->B, C->A = no cycle"

         let configToAdd =
            Stub.createPercentDistributionConfig
               Stub.accountC
               Stub.accountB
               Stub.accountA

         let hasCycle = CycleDetection.cycleDetected configToAdd existing
         Expect.isFalse hasCycle "A->B, C->B, C->A = no cycle"

         let configToAdd =
            Stub.createTargetBalanceConfig Stub.accountB Stub.accountC

         let hasCycle = CycleDetection.cycleDetected configToAdd existing
         Expect.isFalse hasCycle "A->B, B<->C = no cycle"

         let configToAdd =
            Stub.createTargetBalanceConfig Stub.accountB Stub.accountA

         let hasCycle = CycleDetection.cycleDetected configToAdd existing
         Expect.isTrue hasCycle "A->B, B<->A = cycle"
      }

      test "Cycle detection accommodates updating an existing rule" {
         let config = Stub.createZeroBalanceConfig Stub.accountA Stub.accountB
         let existing = [ config ]

         let configToAdd =
            Stub.createZeroBalanceConfig Stub.accountB Stub.accountA

         let hasCycle = CycleDetection.cycleDetected configToAdd existing
         Expect.isTrue hasCycle "A->B, B->A = cycle"

         // configToAdd has same Id as config, so consider it an update
         let configToAdd =
            Stub.createZeroBalanceConfig Stub.accountB Stub.accountA

         let configToAdd = { configToAdd with Id = config.Id }
         let hasCycle = CycleDetection.cycleDetected configToAdd existing
         Expect.isFalse hasCycle "A->B, B->A = no cycle update existing"
      }

      test
         "Cycle detection prevents adding a rule where transfer
            direction is A->B if a rule exists where the transfer path will
            eventually lead back to A, such as A->B, B->C, C->A" {
         // A->B
         let zero = Stub.createZeroBalanceConfig Stub.accountA Stub.accountB
         // B<->C
         let target = Stub.createTargetBalanceConfig Stub.accountB Stub.accountC
         // C->A, C->D
         let percentDist =
            Stub.createPercentDistributionConfig
               Stub.accountC
               Stub.accountA
               Stub.accountD

         let existing = [ zero; target ]
         let hasCycle = CycleDetection.cycleDetected percentDist existing
         // cycle = A->B->C->A
         Expect.isTrue hasCycle "A->B, B<->C, C->A, C->D = cycle"

         // B<->D
         let target = Stub.createTargetBalanceConfig Stub.accountB Stub.accountD
         let existing = [ zero; target ]
         let hasCycle = CycleDetection.cycleDetected percentDist existing
         Expect.isFalse hasCycle "A->B, B<->D, C->A, C->D = no cycle"

         // D->A
         let zero2 = Stub.createZeroBalanceConfig Stub.accountD Stub.accountA
         let existing = [ zero; target; percentDist ]
         let hasCycle = CycleDetection.cycleDetected zero2 existing
         // cycle = D->A->B->D
         Expect.isTrue hasCycle "A->B, B<->D, C->A, C->D, D->A = cycle"

         // D->E
         let zero2 = Stub.createZeroBalanceConfig Stub.accountD Stub.accountE
         let existing = [ zero; target; percentDist ]
         let hasCycle = CycleDetection.cycleDetected zero2 existing
         Expect.isFalse hasCycle "A->B, B<->D, C->A, C->D, D->E = no cycle"
      }

      test "transition returns event when event does not produce auto transfer" {
         let state = Stub.parentAccountState

         let createdEvent: BankEvent<CreatedVirtualAccount> = {
            Id = Guid.NewGuid() |> EventId
            EntityId = state.ParentAccountId.AsEntityId
            OrgId = state.OrgId
            CorrelationId = Guid.NewGuid() |> CorrelationId
            InitiatedBy = Stub.initiator
            Timestamp = DateTime.UtcNow
            Data = {
               AccountId = Stub.accountB
               Name = "Test Account B"
               Depository = AccountDepository.Checking
               Currency = Currency.USD
               AccountNumber = AccountNumber.generate ()
               RoutingNumber = RoutingNumber.Empty
            }
         }

         let evt = AccountEnvelope.wrap createdEvent

         let result = transition state evt

         let events, _ = Expect.wantOk result "should return Ok"

         Expect.equal events.Length 1 "should return only the original event"

         Expect.equal events.[0] evt "should return the original event"
      }

      test
         "transition returns event when event produces auto transfer but account has no rule" {
         let account = {
            Stub.accountState with
               AutoTransferRule = None
         }

         let state = {
            Stub.parentAccountState with
               VirtualAccounts = Map [ account.AccountId, account ]
         }

         let depositedEvent: BankEvent<DepositedCash> = {
            Id = Guid.NewGuid() |> EventId
            EntityId = state.ParentAccountId.AsEntityId
            OrgId = state.OrgId
            CorrelationId = Guid.NewGuid() |> CorrelationId
            InitiatedBy = Stub.initiator
            Timestamp = DateTime.UtcNow
            Data = {
               AccountId = Stub.accountA
               Amount = 500m
               Origin = "ATM"
            }
         }

         let evt = AccountEnvelope.wrap depositedEvent

         let result = transition state evt

         let events, _ = Expect.wantOk result "should return Ok"

         Expect.equal
            events.Length
            1
            "should return only the original event when no auto transfer rule"

         Expect.equal events.[0] evt "should return the original event"
      }

      test
         "transition returns event and auto transfer events when zero balance rule configured" {
         let zeroBalanceConfig =
            Stub.createZeroBalanceConfig Stub.accountA Stub.accountB

         let account = {
            Stub.accountState with
               AutoTransferRule = Some zeroBalanceConfig
         }

         let recipientAccount = {
            Account.empty with
               AccountId = Stub.accountB
               ParentAccountId = Stub.parentAccountId
               OrgId = Stub.orgId
               Status = AccountStatus.Active
               Balance = 0m
         }

         let state = {
            Stub.parentAccountState with
               VirtualAccounts =
                  Map [
                     Stub.accountA, account
                     Stub.accountB, recipientAccount
                  ]
         }

         let depositedEvent: BankEvent<DepositedCash> = {
            Id = Guid.NewGuid() |> EventId
            EntityId = Stub.parentAccountId.AsEntityId
            OrgId = Stub.orgId
            CorrelationId = Guid.NewGuid() |> CorrelationId
            InitiatedBy = Stub.initiator
            Timestamp = DateTime.UtcNow
            Data = {
               AccountId = Stub.accountA
               Amount = 500m
               Origin = "ATM"
            }
         }

         let evt = AccountEnvelope.wrap depositedEvent

         let result = transition state evt

         let events, _ = Expect.wantOk result "should return Ok"

         Expect.isTrue
            (events.Length > 1)
            "should return original event plus auto transfer events"

         Expect.equal
            events.[0]
            evt
            "first event should be the original DepositedCash event"

         let hasAutoTransferDeducted =
            events
            |> List.exists (function
               | AccountEvent.InternalAutomatedTransferDeducted _ -> true
               | _ -> false)

         Expect.isTrue
            hasAutoTransferDeducted
            "should include InternalAutomatedTransferDeducted event"

         let hasAutoTransferDeposited =
            events
            |> List.exists (function
               | AccountEvent.InternalAutomatedTransferDeposited _ -> true
               | _ -> false)

         Expect.isTrue
            hasAutoTransferDeposited
            "should include InternalAutomatedTransferDeposited event"
      }

      test "compute returns auto transfer events when balance below target" {
         let targetBalance = 113_000m

         let targetBalanceConfig =
            Stub.createTargetBalanceConfig Stub.accountA Stub.accountB

         let account = {
            Stub.accountState with
               AccountId = Stub.accountA
               Balance = 100_000m // Below target of 113_000m
               AutoTransferRule = Some targetBalanceConfig
         }

         let managingPartnerAccount = {
            Account.empty with
               AccountId = Stub.accountB
               ParentAccountId = Stub.parentAccountId
               OrgId = Stub.orgId
               Status = AccountStatus.Active
               Balance = 50_000m
         }

         let state = {
            Stub.parentAccountState with
               VirtualAccounts =
                  Map [
                     Stub.accountA, account
                     Stub.accountB, managingPartnerAccount
                  ]
         }

         let res =
            compute
               (Frequency.Schedule CronSchedule.Daily)
               account.AccountId
               state

         let res = Expect.wantSome res "should return Some"
         let events, state = Expect.wantOk res "should return Ok"

         let updatedTargetAccount = state.VirtualAccounts[account.AccountId]

         Expect.equal
            updatedTargetAccount.Balance
            targetBalance
            "updated account balance should = the configured target balance"

         let updatedManagingPartnerAccount =
            state.VirtualAccounts[managingPartnerAccount.AccountId]

         Expect.equal
            updatedManagingPartnerAccount.Balance
            (managingPartnerAccount.Balance + (account.Balance - targetBalance))
            "updated managing partner account balance should be decremented by the amount needed to satisfy balance in target account"

         let hasAutoDeduction =
            events
            |> List.exists (function
               | AccountEvent.InternalAutomatedTransferDeducted _ -> true
               | _ -> false)

         Expect.isTrue
            hasAutoDeduction
            "should include auto transfer deduction event when target balance rule triggers"

         let hasAutoDeposit =
            events
            |> List.exists (function
               | AccountEvent.InternalAutomatedTransferDeposited _ -> true
               | _ -> false)

         Expect.isTrue
            hasAutoDeposit
            "should include auto transfer deposit event when target balance rule triggers"
      }

      test "compute returns auto transfer events when balance above target" {
         let targetBalance = 113_000m

         let targetBalanceConfig =
            Stub.createTargetBalanceConfig Stub.accountA Stub.accountB

         let account = {
            Stub.accountState with
               AccountId = Stub.accountA
               Balance = 117_000m // Above target of 113_000m
               AutoTransferRule = Some targetBalanceConfig
         }

         let managingPartnerAccount = {
            Account.empty with
               AccountId = Stub.accountB
               ParentAccountId = Stub.parentAccountId
               OrgId = Stub.orgId
               Status = AccountStatus.Active
               Balance = 50_000m
         }

         let state = {
            Stub.parentAccountState with
               VirtualAccounts =
                  Map [
                     Stub.accountA, account
                     Stub.accountB, managingPartnerAccount
                  ]
         }

         let res =
            compute
               (Frequency.Schedule CronSchedule.Daily)
               account.AccountId
               state

         let res = Expect.wantSome res "should return Some"
         let events, state = Expect.wantOk res "should return Ok"

         let updatedTargetAccount = state.VirtualAccounts[account.AccountId]

         Expect.equal
            updatedTargetAccount.Balance
            targetBalance
            "updated account balance should = the configured target balance"

         let updatedManagingPartnerAccount =
            state.VirtualAccounts[managingPartnerAccount.AccountId]

         Expect.equal
            updatedManagingPartnerAccount.Balance
            (managingPartnerAccount.Balance + (account.Balance - targetBalance))
            "updated managing partner account should absorb excess cash from target account"

         let hasAutoDeduction =
            events
            |> List.exists (function
               | AccountEvent.InternalAutomatedTransferDeducted _ -> true
               | _ -> false)

         Expect.isTrue
            hasAutoDeduction
            "should include auto transfer deduction event when managing partner absorbs excess cash from target"

         let hasAutoDeposit =
            events
            |> List.exists (function
               | AccountEvent.InternalAutomatedTransferDeposited _ -> true
               | _ -> false)

         Expect.isTrue
            hasAutoDeposit
            "should include auto transfer deposit event when managing partner absorbs excess cash from target"
      }

      test
         "compute returns no auto transfer events when target balance rule does not trigger" {
         let range = {
            LowerBound =
               PositiveAmount.create 110_000m |> Result.toValueOption |> _.Value
            UpperBound =
               PositiveAmount.create 115_000m |> Result.toValueOption |> _.Value
         }

         let targetBalanceConfig =
            Stub.createTargetBalanceConfigWithRange
               Stub.accountA
               Stub.accountB
               range

         // Set balance within target to not trigger transfer
         let account = {
            Stub.accountState with
               Balance = 114_000m
               AutoTransferRule = Some targetBalanceConfig
         }

         let recipientAccount = {
            Account.empty with
               AccountId = Stub.accountB
               ParentAccountId = Stub.parentAccountId
               OrgId = Stub.orgId
               Status = AccountStatus.Active
               Balance = 50_000m
         }

         let state: ParentAccountSnapshot = {
            Stub.parentAccountState with
               VirtualAccounts =
                  Map [
                     Stub.accountA, account
                     Stub.accountB, recipientAccount
                  ]
         }

         let res =
            compute
               (Frequency.Schedule CronSchedule.Daily)
               account.AccountId
               state

         Expect.isNone
            res
            "should return no auto transfer events when target balance is met"
      }

      test
         "transition handles InternalTransferWithinOrgDeducted by including a corresponding deposit" {
         let account = Stub.accountState

         let recipientAccount = {
            Account.empty with
               AccountId = Stub.accountB
               ParentAccountId = Stub.parentAccountId
               OrgId = Stub.orgId
               Status = AccountStatus.Active
               Balance = 0m
         }

         let state: ParentAccountSnapshot = {
            Stub.parentAccountState with
               VirtualAccounts =
                  Map [
                     Stub.accountA, account
                     Stub.accountB, recipientAccount
                  ]
         }

         let transferEvent: BankEvent<InternalTransferWithinOrgDeducted> = {
            Id = Guid.NewGuid() |> EventId
            EntityId = Stub.parentAccountId.AsEntityId
            OrgId = Stub.orgId
            CorrelationId = Guid.NewGuid() |> CorrelationId
            InitiatedBy = Stub.initiator
            Timestamp = DateTime.UtcNow
            Data = {
               BaseInfo = {
                  Sender = Stub.sender
                  Recipient = Stub.recipient
                  InitiatedBy = Stub.initiator
                  TransferId = TransferId(Guid.NewGuid())
                  Amount = 100m
                  ScheduledDate = DateTime.UtcNow
                  Memo = None
               }
            }
         }

         let evt = AccountEvent.InternalTransferWithinOrgDeducted transferEvent

         let result = transition state evt

         let events, _ = Expect.wantOk result "should return Ok"

         Expect.isTrue
            (events.Length >= 2)
            "should return deducted and deposited events"

         let hasDeducted =
            events
            |> List.exists (function
               | AccountEvent.InternalTransferWithinOrgDeducted _ -> true
               | _ -> false)

         let hasDeposited =
            events
            |> List.exists (function
               | AccountEvent.InternalTransferWithinOrgDeposited _ -> true
               | _ -> false)

         Expect.isTrue hasDeducted "should include deducted event"
         Expect.isTrue hasDeposited "should include deposited event"
      }

      test
         "transition includes automated transfers for a InternalTransferWithinOrg when the recipient has a zero balance rule configured" {
         let account = Stub.accountState

         let accountB = {
            Account.empty with
               AccountId = Stub.accountB
               ParentAccountId = Stub.parentAccountId
               OrgId = Stub.orgId
               Status = AccountStatus.Active
               Balance = 1400m
               AutoTransferRule =
                  Some(Stub.createZeroBalanceConfig Stub.accountB Stub.accountA)
         }

         let accountIdOfAccountWithZeroBalanceRule = accountB.AccountId

         let state: ParentAccountSnapshot = {
            Stub.parentAccountState with
               VirtualAccounts =
                  Map [ Stub.accountA, account; Stub.accountB, accountB ]
         }

         let transferEvent: BankEvent<InternalTransferWithinOrgDeducted> = {
            Id = Guid.NewGuid() |> EventId
            EntityId = Stub.parentAccountId.AsEntityId
            OrgId = Stub.orgId
            CorrelationId = Guid.NewGuid() |> CorrelationId
            InitiatedBy = Stub.initiator
            Timestamp = DateTime.UtcNow
            Data = {
               BaseInfo = {
                  Sender = {
                     Stub.sender with
                        AccountId = account.AccountId
                  }
                  Recipient = {
                     Stub.recipient with
                        AccountId = accountB.AccountId
                  }
                  InitiatedBy = Stub.initiator
                  TransferId = TransferId(Guid.NewGuid())
                  Amount = 100m
                  ScheduledDate = DateTime.UtcNow
                  Memo = None
               }
            }
         }

         let evt = AccountEvent.InternalTransferWithinOrgDeducted transferEvent

         let result = transition state evt

         let events, state = Expect.wantOk result "should return Ok"

         let accountWithZeroBalance =
            state.VirtualAccounts[accountIdOfAccountWithZeroBalanceRule]

         Expect.equal
            accountWithZeroBalance.Balance
            0m
            "expected account with ZeroBalanceRule configured to have balance zero after txn"

         let accountToReceiveAutoTransfer =
            state.VirtualAccounts[account.AccountId]

         Expect.equal
            accountToReceiveAutoTransfer.Balance
            (account.Balance
             + transferEvent.Data.BaseInfo.Amount
             + accountB.Balance)
            "expected account receiving auto transfer to have balance of account that was configured with zero balance rule"

         Expect.equal
            events.Length
            4
            "should return deducted, deposited, & auto transfer events"

         let hasDeducted =
            events
            |> List.exists (function
               | AccountEvent.InternalTransferWithinOrgDeducted _ -> true
               | _ -> false)

         let hasDeposited =
            events
            |> List.exists (function
               | AccountEvent.InternalTransferWithinOrgDeposited _ -> true
               | _ -> false)

         let hasAutoDeduction =
            events
            |> List.exists (function
               | AccountEvent.InternalAutomatedTransferDeducted _ -> true
               | _ -> false)

         let hasAutoDeposit =
            events
            |> List.exists (function
               | AccountEvent.InternalAutomatedTransferDeposited _ -> true
               | _ -> false)

         Expect.isTrue hasDeducted "should include deducted event"
         Expect.isTrue hasDeposited "should include deposited event"
         Expect.isTrue hasAutoDeduction "should include auto deduction event"
         Expect.isTrue hasAutoDeposit "should include auto deposit event"
      }

      test
         "transition includes automated transfers for a InternalTransferWithinOrg when the sender has a zero balance rule configured" {
         let accountIdOfAccountWithZeroBalanceRule = Stub.accountA

         let account = {
            Stub.accountState with
               AutoTransferRule =
                  Some(
                     Stub.createZeroBalanceConfig
                        accountIdOfAccountWithZeroBalanceRule
                        Stub.accountB
                  )
         }

         let accountB = {
            Account.empty with
               AccountId = Stub.accountB
               ParentAccountId = Stub.parentAccountId
               OrgId = Stub.orgId
               Status = AccountStatus.Active
               Balance = 1400m
         }

         let state: ParentAccountSnapshot = {
            Stub.parentAccountState with
               VirtualAccounts =
                  Map [
                     account.AccountId, account
                     accountB.AccountId, accountB
                  ]
         }

         let transferEvent: BankEvent<InternalTransferWithinOrgDeducted> = {
            Id = Guid.NewGuid() |> EventId
            EntityId = Stub.parentAccountId.AsEntityId
            OrgId = Stub.orgId
            CorrelationId = Guid.NewGuid() |> CorrelationId
            InitiatedBy = Stub.initiator
            Timestamp = DateTime.UtcNow
            Data = {
               BaseInfo = {
                  Sender = {
                     Stub.sender with
                        AccountId = accountB.AccountId
                  }
                  Recipient = {
                     Stub.recipient with
                        AccountId = accountIdOfAccountWithZeroBalanceRule
                  }
                  InitiatedBy = Stub.initiator
                  TransferId = TransferId(Guid.NewGuid())
                  Amount = 100m
                  ScheduledDate = DateTime.UtcNow
                  Memo = None
               }
            }
         }

         let evt = AccountEvent.InternalTransferWithinOrgDeducted transferEvent

         let result = transition state evt

         let events, state = Expect.wantOk result "should return Ok"

         let accountWithZeroBalance =
            state.VirtualAccounts[accountIdOfAccountWithZeroBalanceRule]

         Expect.equal
            accountWithZeroBalance.Balance
            0m
            "expected account with ZeroBalanceRule configured to have balance zero after txn"

         let accountToReceiveAutoTransfer =
            state.VirtualAccounts[accountB.AccountId]

         Expect.equal
            accountToReceiveAutoTransfer.Balance
            (account.Balance
             + transferEvent.Data.BaseInfo.Amount
             + accountB.Balance)
            "expected account receiving auto transfer to have balance of account that was configured with zero balance rule"

         Expect.equal
            events.Length
            4
            "should return deducted, deposited, & auto transfer events"

         let hasDeducted =
            events
            |> List.exists (function
               | AccountEvent.InternalTransferWithinOrgDeducted _ -> true
               | _ -> false)

         let hasDeposited =
            events
            |> List.exists (function
               | AccountEvent.InternalTransferWithinOrgDeposited _ -> true
               | _ -> false)

         let hasAutoDeduction =
            events
            |> List.exists (function
               | AccountEvent.InternalAutomatedTransferDeducted _ -> true
               | _ -> false)

         let hasAutoDeposit =
            events
            |> List.exists (function
               | AccountEvent.InternalAutomatedTransferDeposited _ -> true
               | _ -> false)

         Expect.isTrue hasDeducted "should include deducted event"
         Expect.isTrue hasDeposited "should include deposited event"
         Expect.isTrue hasAutoDeduction "should include auto deduction event"
         Expect.isTrue hasAutoDeposit "should include auto deposit event"
      }

      test
         "compute ensures auto transfer deducted event precedes deposited event" {
         let zeroBalanceConfig =
            Stub.createZeroBalanceConfig Stub.accountA Stub.accountB

         let account = {
            Stub.accountState with
               Balance = 500m
               AutoTransferRule = Some zeroBalanceConfig
         }

         let recipientAccount = {
            Account.empty with
               AccountId = Stub.accountB
               ParentAccountId = Stub.parentAccountId
               OrgId = Stub.orgId
               Status = AccountStatus.Active
               Balance = 0m
         }

         let state = {
            Stub.parentAccountState with
               VirtualAccounts =
                  Map [
                     Stub.accountA, account
                     Stub.accountB, recipientAccount
                  ]
         }

         let res = compute Frequency.PerTransaction account.AccountId state

         let res = Expect.wantSome res "should return Some"
         let events, _ = Expect.wantOk res "should return Ok"

         let deductedIndex =
            events
            |> List.findIndex (function
               | AccountEvent.InternalAutomatedTransferDeducted _ -> true
               | _ -> false)

         let depositedIndex =
            events
            |> List.findIndex (function
               | AccountEvent.InternalAutomatedTransferDeposited _ -> true
               | _ -> false)

         Expect.isTrue
            (deductedIndex < depositedIndex)
            "InternalAutomatedTransferDeducted should precede InternalAutomatedTransferDeposited in event order"
      }
   ]
