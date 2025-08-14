module CommandApprovalTests

open System

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Bank.Org.Domain
open Bank.Employee.Domain
open CommandApproval
open CommandApprovalRule
open CommandApprovalProgress
open Lib.SharedTypes

module Stub = OrganizationStub

let update = Org.stateTransition
let initState = Stub.orgStateWithEvents

let tests =
   testList "Command Approval domain" [
      test "AmountPerCommandRange has no gaps if no existing ranges" {
         let ranges = []

         let range = {
            LowerBound = Some 15m
            UpperBound = None
         }

         Expect.isNone
            (AmountPerCommandRange.hasGap ranges range)
            "should not have a gap"
      }

      test
         "AmountPerCommandRange detects between existing ranges & a new range
            when new range is at end of sorted ranges" {
         let ranges = [
            {
               LowerBound = None
               UpperBound = Some 3m
            }
            {
               LowerBound = Some 3m
               UpperBound = Some 10m
            }
         ]

         let range = {
            LowerBound = Some 15m
            UpperBound = None
         }

         let gap =
            Expect.wantSome
               (AmountPerCommandRange.hasGap ranges range)
               "should have a gap"

         Expect.equal gap.Gap 5m "expected gap"

         Expect.equal
            gap.SetToAmountToCloseGap
            10m
            "expected amount to close gap should be equivalent of preceding
            upper bound"

         Expect.equal
            gap.Direction
            GapDirection.Precedes
            "gap is between item to add & previous item in sorted ranges"

         let range = {
            range with
               LowerBound = Some gap.SetToAmountToCloseGap
         }

         Expect.isNone
            (AmountPerCommandRange.hasGap ranges range)
            "No gap detected when range adjusted to SetToAmountToCloseGap"
      }

      test
         "AmountPerCommandRange detects between existing ranges & a new range
          when new range is at start of sorted ranges" {
         let ranges = [
            {
               LowerBound = Some 3m
               UpperBound = Some 10m
            }
            {
               LowerBound = Some 10m
               UpperBound = Some 16m
            }
            {
               LowerBound = Some 16m
               UpperBound = None
            }
         ]

         let range = {
            LowerBound = None
            UpperBound = Some 1m
         }

         let gap =
            Expect.wantSome
               (AmountPerCommandRange.hasGap ranges range)
               "should have a gap"

         Expect.equal gap.Gap 2m "expected gap"

         Expect.equal
            gap.SetToAmountToCloseGap
            3m
            "expected amount to close gap should be equivalent of following
            lower bound"

         Expect.equal
            gap.Direction
            GapDirection.Follows
            "gap is between item to add & following item in sorted ranges"

         let range = {
            range with
               UpperBound = Some gap.SetToAmountToCloseGap
         }

         Expect.isNone
            (AmountPerCommandRange.hasGap ranges range)
            "No gap detected when range adjusted to SetToAmountToCloseGap"
      }

      test
         "AmountPerCommandRange detects between existing ranges & a new range
          when new range is somewhere in the middle of sorted ranges" {
         let ranges = [
            {
               LowerBound = None
               UpperBound = Some 3m
            }
            {
               LowerBound = Some 3m
               UpperBound = Some 10m
            }
            {
               LowerBound = Some 16m
               UpperBound = None
            }
         ]

         let range = {
            LowerBound = Some 13m
            UpperBound = Some 16m
         }

         let gap =
            Expect.wantSome
               (AmountPerCommandRange.hasGap ranges range)
               "should have a gap"

         Expect.equal gap.Gap 3m "expected gap"

         Expect.equal
            gap.SetToAmountToCloseGap
            10m
            "expected amount to close gap should be equivalent to preceding
            upper bound"

         Expect.equal
            gap.Direction
            GapDirection.Precedes
            "gap is between item to add & preceding item in sorted ranges"

         let range = {
            range with
               LowerBound = Some gap.SetToAmountToCloseGap
         }

         Expect.isNone
            (AmountPerCommandRange.hasGap ranges range)
            "No gap detected when range adjusted to SetToAmountToCloseGap"

         let range = {
            LowerBound = Some 10m
            UpperBound = Some 12m
         }

         let gap =
            Expect.wantSome
               (AmountPerCommandRange.hasGap ranges range)
               "should have a gap"

         Expect.equal gap.Gap 4m "expected gap"

         Expect.equal
            gap.SetToAmountToCloseGap
            16m
            "expected amount to close gap should be equivalent to following
            lower bound"

         Expect.equal
            gap.Direction
            GapDirection.Follows
            "gap is between item to add & following item in sorted ranges"

         let range = {
            range with
               UpperBound = Some gap.SetToAmountToCloseGap
         }

         Expect.isNone
            (AmountPerCommandRange.hasGap ranges range)
            "No gap detected when range adjusted to SetToAmountToCloseGap"
      }

      test "AmountPerCommandRange detects overlap in Some, Some, Some, None" {
         let existingRange = {
            LowerBound = Some 2m
            UpperBound = Some 5m
         }

         let range = {
            LowerBound = Some 3m
            UpperBound = None
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isTrue overlap "(3, infinity) should overlap with (2, 5)"

         let overlap =
            AmountPerCommandRange.hasOverlap existingRange {
               range with
                  LowerBound = Some 1m
            }

         Expect.isTrue overlap "(1, infinity) should overlap with (2, 5)"

         let overlap =
            AmountPerCommandRange.hasOverlap existingRange {
               range with
                  LowerBound = Some 6m
            }

         Expect.isFalse
            overlap
            "(6, infinity) is outside of range (2, 5) so should not overlap"

         let overlap =
            AmountPerCommandRange.hasOverlap existingRange {
               range with
                  LowerBound = Some 5m
            }

         Expect.isFalse
            overlap
            "(x >= 5) is outside of range (2 >= x < 5) so should not overlap"
      }

      test "AmountPerCommandRange detects overlap in Some, Some, None, Some" {
         let existingRange = {
            LowerBound = Some 2m
            UpperBound = Some 5m
         }

         let range = {
            LowerBound = None
            UpperBound = Some 3m
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isTrue overlap "(0, 3) overlaps with (2, 5)"

         let overlap =
            AmountPerCommandRange.hasOverlap existingRange {
               range with
                  UpperBound = Some 1m
            }

         Expect.isFalse
            overlap
            "(0, 1) is outside of range (2, 5) so should not overlap"

         let overlap =
            AmountPerCommandRange.hasOverlap existingRange {
               range with
                  UpperBound = Some 6m
            }

         Expect.isTrue overlap "(0, 6) should overlap with (2, 5)"

         let overlap =
            AmountPerCommandRange.hasOverlap existingRange {
               range with
                  UpperBound = Some 2m
            }

         Expect.isFalse
            overlap
            "(x < 2) is outside of range (2 >= x < 5) so should not overlap"
      }

      test "AmountPerCommandRange detects overlap in None, Some, Some, Some" {
         let existingRange = {
            LowerBound = None
            UpperBound = Some 5m
         }

         let range = {
            LowerBound = Some 2m
            UpperBound = Some 4m
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isTrue overlap "(2, 4) overlaps with (0, 5)"

         let range = {
            LowerBound = Some 6m
            UpperBound = Some 8m
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isFalse
            overlap
            "(6, 8) is outside of range (0, 5) so should not overlap"


         let range = {
            LowerBound = Some 5m
            UpperBound = Some 8m
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isFalse
            overlap
            "(5 >= x < 8) is outside of range (x < 5) so should not overlap"
      }

      test "AmountPerCommandRange detects overlap in None, Some, None, Some" {
         let existingRange = {
            LowerBound = None
            UpperBound = Some 5m
         }

         let range = {
            LowerBound = None
            UpperBound = Some 4m
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isTrue overlap "(0, 4) overlaps with (0, 5)"

         let existingRange = {
            LowerBound = None
            UpperBound = Some 1m
         }

         let range = {
            LowerBound = None
            UpperBound = Some 5m
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isTrue overlap "(0, 5) overlaps with (0, 1)"
      }

      test "AmountPerCommandRange detects overlap in None, Some, Some, None" {
         let existingRange = {
            LowerBound = None
            UpperBound = Some 5m
         }

         let range = {
            LowerBound = Some 4m
            UpperBound = None
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isTrue overlap "(4, infinity) overlaps with (0, 5)"

         let overlap =
            AmountPerCommandRange.hasOverlap existingRange {
               range with
                  LowerBound = Some 6m
            }

         Expect.isFalse overlap "(6, infinity) does not overlap with (0, 5)"

         let overlap =
            AmountPerCommandRange.hasOverlap existingRange {
               range with
                  LowerBound = Some 5m
            }

         Expect.isFalse overlap "(x >= 5) does not overlap with (x < 5)"
      }

      test "AmountPerCommandRange detects overlap in Some, None, Some, Some" {
         let existingRange = {
            LowerBound = Some 2m
            UpperBound = None
         }

         let range = {
            LowerBound = Some 3m
            UpperBound = Some 5m
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isTrue overlap "(3, 5) should overlap with (2, infinity)"

         let overlap =
            AmountPerCommandRange.hasOverlap
               {
                  existingRange with
                     LowerBound = Some 6m
               }
               range

         Expect.isFalse overlap "(3, 5) should not overlap with (6, infinity)"

         let overlap =
            AmountPerCommandRange.hasOverlap
               {
                  LowerBound = Some 5m
                  UpperBound = None
               }
               {
                  LowerBound = Some 3m
                  UpperBound = Some 5m
               }

         Expect.isFalse overlap "(3 >= x < 5) should not overlap with (x >= 5)"
      }

      test "AmountPerCommandRange detects overlap in Some, None, None, Some" {
         let existingRange = {
            LowerBound = Some 2m
            UpperBound = None
         }

         let range = {
            LowerBound = None
            UpperBound = Some 5m
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isTrue overlap "(0, 5) should overlap with (2, infinity)"

         let overlap =
            AmountPerCommandRange.hasOverlap existingRange {
               range with
                  UpperBound = Some 1m
            }

         Expect.isFalse overlap "(0, 1) should not overlap with (2, infinity)"

         let overlap =
            AmountPerCommandRange.hasOverlap existingRange {
               range with
                  UpperBound = Some 2m
            }

         Expect.isFalse overlap "(x < 2) should not overlap with (x >= 2)"
      }

      test "AmountPerCommandRange detects overlap in Some, None, Some, None" {
         let existingRange = {
            LowerBound = Some 2m
            UpperBound = None
         }

         let range = {
            LowerBound = Some 5m
            UpperBound = None
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isTrue overlap "(5, infinity) should overlap with (2, infinity)"

         let existingRange = {
            LowerBound = Some 5m
            UpperBound = None
         }

         let range = {
            LowerBound = Some 2m
            UpperBound = None
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isTrue overlap "(2, infinity) should overlap with (5, infinity)"
      }

      test "AmountPerCommandRange detects overlap in Some, Some, Some, Some" {
         let existingRange = {
            LowerBound = Some 2m
            UpperBound = Some 5m
         }

         let range = {
            LowerBound = Some 6m
            UpperBound = Some 10m
         }

         let overlap = AmountPerCommandRange.hasOverlap existingRange range

         Expect.isFalse overlap "(6, 10) should not overlap with (2, 5)"

         let overlap =
            AmountPerCommandRange.hasOverlap existingRange {
               range with
                  LowerBound = Some 4m
            }

         Expect.isTrue overlap "(4, 10) should overlap with (2, 5)"

         let overlap =
            AmountPerCommandRange.hasOverlap
               {
                  LowerBound = Some 7m
                  UpperBound = Some 9m
               }
               range

         Expect.isTrue overlap "(6, 10) should overlap with (7, 9)"

         let overlap =
            AmountPerCommandRange.hasOverlap
               {
                  LowerBound = Some 7m
                  UpperBound = Some 15m
               }
               range

         Expect.isTrue overlap "(6, 10) should overlap with (7, 15)"

         let overlap =
            AmountPerCommandRange.hasOverlap
               {
                  LowerBound = Some 1m
                  UpperBound = Some 15m
               }
               range

         Expect.isTrue overlap "(6, 10) should overlap with (1, 15)"

         let overlap =
            AmountPerCommandRange.hasOverlap
               {
                  LowerBound = Some 1m
                  UpperBound = Some 7m
               }
               range

         Expect.isTrue overlap "(6, 10) should overlap with (1, 7)"

         let overlap =
            AmountPerCommandRange.hasOverlap
               {
                  LowerBound = Some 11m
                  UpperBound = Some 20m
               }
               range

         Expect.isFalse overlap "(6, 10) should not overlap with (11, 20)"

         let overlap =
            AmountPerCommandRange.hasOverlap
               {
                  LowerBound = Some 10m
                  UpperBound = Some 20m
               }
               range

         Expect.isFalse overlap "(6, 10) should not overlap with (10, 20)"

         let overlap =
            AmountPerCommandRange.hasOverlap range {
               LowerBound = Some 10m
               UpperBound = Some 20m
            }

         Expect.isFalse
            overlap
            "(10 <= x < 20) should not overlap with (6 <= x < 10)"
      }

      test
         "Daily Limit expected to be greater than amount based rules configured for the same command type" {
         let existingRule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.Payment
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = None
                  UpperBound = Some 50m
               }
            Approvers = [ CommandApprover.AnyAdmin ]
         }

         let newRule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.Payment
            Criteria = ApprovalCriteria.AmountDailyLimit 60m
            Approvers = [ CommandApprover.AnyAdmin ]
         }

         let conflicts =
            CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
               [ existingRule ]
               newRule

         Expect.isFalse
            conflicts
            "Should be no conflict when daily limit (60) > upper bound (50) of existing rule"

         let newRule = {
            newRule with
               Criteria = ApprovalCriteria.AmountDailyLimit 40m
         }

         let conflicts =
            CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
               [ existingRule ]
               newRule

         Expect.isTrue
            conflicts
            "Should conflict when daily limit (40) < upper bound (50) of existing rule"

         let existingRule = {
            existingRule with
               Criteria =
                  ApprovalCriteria.AmountPerCommand {
                     LowerBound = Some 10m
                     UpperBound = None
                  }
         }

         let conflicts =
            CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
               [ existingRule ]
               newRule

         Expect.isFalse
            conflicts
            "Should be no conflict when daily limit (40) > lower bound (10) of existing rule"

         let existingRule = {
            existingRule with
               Criteria =
                  ApprovalCriteria.AmountPerCommand {
                     LowerBound = Some 50m
                     UpperBound = None
                  }
         }

         let conflicts =
            CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
               [ existingRule ]
               newRule

         Expect.isTrue
            conflicts
            "Should conflict when daily limit (40) < lower bound (50) of existing rule"

         let existingRule = {
            existingRule with
               Criteria =
                  ApprovalCriteria.AmountPerCommand {
                     LowerBound = Some 10m
                     UpperBound = Some 30m
                  }
         }

         let newRule = {
            newRule with
               Criteria = ApprovalCriteria.AmountDailyLimit 20m
         }

         let conflicts =
            CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
               [ existingRule ]
               newRule

         Expect.isTrue
            conflicts
            "Should conflict when daily limit (20) between lower & upper bound of existing rule"

         let newRule = {
            newRule with
               Criteria = ApprovalCriteria.AmountDailyLimit 40m
         }

         let conflicts =
            CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
               [ existingRule ]
               newRule

         Expect.isFalse
            conflicts
            "Should not conflict when daily limit (40) above upper bound of existing rule"
      }

      test
         "AmountPerCommand based rules expected to have amounts lower than Daily Limit rules configured for the same command type" {
         let existingRule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.Payment
            Criteria = ApprovalCriteria.AmountDailyLimit 60m
            Approvers = [ CommandApprover.AnyAdmin ]
         }

         let newRule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.Payment
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = None
                  UpperBound = Some 50m
               }
            Approvers = [ CommandApprover.AnyAdmin ]
         }

         let conflicts =
            CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
               [ existingRule ]
               newRule

         Expect.isFalse
            conflicts
            "Should be no conflict when upper bound (50) < daily limit (60) of existing rule"

         let newRule = {
            newRule with
               Criteria =
                  ApprovalCriteria.AmountPerCommand {
                     LowerBound = None
                     UpperBound = Some 70m
                  }
         }

         let conflicts =
            CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
               [ existingRule ]
               newRule

         Expect.isTrue
            conflicts
            "Should conflict when upper bound (70) > daily limit (60) of existing rule"

         let newRule = {
            newRule with
               Criteria =
                  ApprovalCriteria.AmountPerCommand {
                     LowerBound = Some 50m
                     UpperBound = Some 70m
                  }
         }

         let conflicts =
            CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
               [ existingRule ]
               newRule

         Expect.isTrue
            conflicts
            "Should conflict when daily limit (60) between range (50,70)"

         let newRule = {
            newRule with
               Criteria =
                  ApprovalCriteria.AmountPerCommand {
                     LowerBound = Some 80m
                     UpperBound = None
                  }
         }

         let conflicts =
            CommandApprovalRule.newRuleCriteriaConflictsWithExistingRule
               [ existingRule ]
               newRule

         Expect.isTrue
            conflicts
            "Should conflict when lower bound (80) > existing rule's daily limit (60)"
      }

      test "isValidApprover should check the approver is configured in the rule" {
         let approver: EmployeeReference = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let initiatedBy = InitiatedById approver.EmployeeId

         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.InviteEmployee
            Criteria = ApprovalCriteria.PerCommand
            Approvers = [
               CommandApprover.Admin approver

               CommandApprover.Admin {
                  EmployeeName = "B"
                  EmployeeId = Guid.NewGuid() |> EmployeeId
               }
            ]
         }

         Expect.isTrue
            (isValidApprover initiatedBy rule)
            "should be true when corresponding Admin configured in rule"

         let rule = {
            rule with
               Approvers = [
                  CommandApprover.Admin {
                     EmployeeName = "B"
                     EmployeeId = Guid.NewGuid() |> EmployeeId
                  }
               ]
         }

         Expect.isFalse
            (isValidApprover initiatedBy rule)
            "should be false when approver not found in rule"

         let rule = {
            rule with
               Approvers = [
                  CommandApprover.AnyAdmin
                  CommandApprover.AnyAdmin
               ]
         }

         Expect.isTrue
            (isValidApprover initiatedBy rule)
            "should be true when approver not found in rule but AnyAdmin configured"
      }

      test
         "newRuleCommandTypeConflictsWithExistingRule should check for
          duplicate rules by (ApprovableCommandType, ApprovalCriteria.PerCommand)" {
         let cmdTypes = [
            Stub.commandTypes.InviteEmployee
            Stub.commandTypes.UpdateEmployeeRole
         ]

         for cmdType in cmdTypes do
            let ruleA = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria = ApprovalCriteria.PerCommand
               Approvers = [ CommandApprover.AnyAdmin ]
            }

            let ruleB = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria = ApprovalCriteria.PerCommand
               Approvers = [
                  CommandApprover.Admin {
                     EmployeeName = "A"
                     EmployeeId = Guid.NewGuid() |> EmployeeId
                  }
               ]
            }

            Expect.isTrue
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleB)
               $"should have conflict if configure {cmdType} rule when
                one already exists"

            let ruleB = {
               ruleB with
                  CommandType =
                     if cmdType = Stub.commandTypes.InviteEmployee then
                        Stub.commandTypes.UpdateEmployeeRole
                     else
                        Stub.commandTypes.InviteEmployee
            }

            Expect.isFalse
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleB)
               $"should not conflict if configure a rule of a command type other
               than {cmdType}"

            Expect.isFalse
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleA)
               "should not conflict if editing existing rule"
      }

      test
         "newRuleCommandTypeConflictsWithExistingRule should check for
          duplicate rules by (ApprovableCommandType, ApprovalCriteria.AmountDailyLimit)" {
         let cmdTypes = [
            Stub.commandTypes.InternalTransfer
            Stub.commandTypes.DomesticTransfer
            Stub.commandTypes.Payment
         ]

         for cmdType in cmdTypes do
            let ruleA = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria = ApprovalCriteria.AmountDailyLimit 15000m
               Approvers = [ CommandApprover.AnyAdmin ]
            }

            let ruleB = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria = ApprovalCriteria.AmountDailyLimit 25000m
               Approvers = [
                  CommandApprover.Admin {
                     EmployeeName = "A"
                     EmployeeId = Guid.NewGuid() |> EmployeeId
                  }
               ]
            }

            Expect.isTrue
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleB)
               $"should have conflict if configure {cmdType} rule when
                one already exists"

            let ruleB = {
               ruleB with
                  CommandType =
                     match cmdType with
                     | ApprovableCommandType.ApprovableAmountBased InternalTransferBetweenOrgsCommandType ->
                        Stub.commandTypes.DomesticTransfer
                     | ApprovableCommandType.ApprovableAmountBased DomesticTransferCommandType ->
                        Stub.commandTypes.Payment
                     | _ -> Stub.commandTypes.InternalTransfer
            }

            Expect.isFalse
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleB)
               $"should not conflict if configure a rule of a command type other
               than {cmdType}"

            Expect.isFalse
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleA)
               "should not conflict if editing existing rule"
      }

      test
         "newRuleCommandTypeConflictsWithExistingRule should not conflict when
          duplicate rules by (ApprovableCommandType, ApprovalCriteria.AmountPerCommand)" {
         let cmdTypes = [
            Stub.commandTypes.InternalTransfer
            Stub.commandTypes.DomesticTransfer
            Stub.commandTypes.Payment
         ]

         for cmdType in cmdTypes do
            let ruleA = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria =
                  ApprovalCriteria.AmountPerCommand {
                     LowerBound = Some 2000m
                     UpperBound = Some 5000m
                  }
               Approvers = [ CommandApprover.AnyAdmin ]
            }

            let ruleB = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria =
                  ApprovalCriteria.AmountPerCommand {
                     LowerBound = Some 6000m
                     UpperBound = None
                  }
               Approvers = [
                  CommandApprover.Admin {
                     EmployeeName = "A"
                     EmployeeId = Guid.NewGuid() |> EmployeeId
                  }
               ]
            }

            Expect.isFalse
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleB)
               $"should not conflict if configure {cmdType} rule when
                one already exists"

            let ruleB = {
               ruleB with
                  CommandType =
                     match cmdType with
                     | ApprovableCommandType.ApprovableAmountBased InternalTransferBetweenOrgsCommandType ->
                        Stub.commandTypes.DomesticTransfer
                     | ApprovableCommandType.ApprovableAmountBased DomesticTransferCommandType ->
                        Stub.commandTypes.Payment
                     | _ -> Stub.commandTypes.InternalTransfer
            }

            Expect.isFalse
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleB)
               $"should not conflict if configure a rule of a command type other
               than {cmdType}"

            Expect.isFalse
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleA)
               "should not conflict if editing existing rule"
      }

      test
         "isRequesterOneOfManyApprovers should check the approver is one of
          many approvers configured for the rule" {
         let approver: EmployeeReference = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let initiatedBy = InitiatedById approver.EmployeeId

         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.InviteEmployee
            Criteria = ApprovalCriteria.PerCommand
            Approvers = [
               CommandApprover.Admin approver

               CommandApprover.Admin {
                  EmployeeName = "B"
                  EmployeeId = Guid.NewGuid() |> EmployeeId
               }
            ]
         }

         Expect.isTrue
            (isRequesterOneOfManyApprovers initiatedBy rule)
            "should be true when Admin one of many"

         let rule = {
            rule with
               Approvers = [ CommandApprover.Admin approver ]
         }

         Expect.isFalse
            (isRequesterOneOfManyApprovers initiatedBy rule)
            "should be false when approver only 1 configured"

         let rule = {
            rule with
               Approvers = [
                  CommandApprover.AnyAdmin
                  CommandApprover.AnyAdmin
               ]
         }

         Expect.isTrue
            (isRequesterOneOfManyApprovers initiatedBy rule)
            "should be true when approver not found but rule config contains
            AnyAdmin more than once"
      }

      test
         "isRequesterTheOnlyConfiguredApprover should check the approver is the
          only approver configured for the rule" {
         let approver: EmployeeReference = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let initiatedBy = InitiatedById approver.EmployeeId

         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.InviteEmployee
            Criteria = ApprovalCriteria.PerCommand
            Approvers = [
               CommandApprover.Admin approver
               CommandApprover.AnyAdmin
            ]
         }

         Expect.isFalse
            (isRequesterTheOnlyConfiguredApprover initiatedBy rule)
            "should be false when Admin one of many"

         let rule = {
            rule with
               Approvers = [ CommandApprover.Admin approver ]
         }

         Expect.isTrue
            (isRequesterTheOnlyConfiguredApprover initiatedBy rule)
            "should be true when approver only 1 configured"

         let rule = {
            rule with
               Approvers = [ CommandApprover.AnyAdmin ]
         }

         Expect.isTrue
            (isRequesterTheOnlyConfiguredApprover initiatedBy rule)
            "should be true when approver not found but rule config contains
            AnyAdmin"
      }

      test
         "isNewApproval should check if the person approving the command already did" {
         let approver: EmployeeReference = {
            EmployeeId = Guid.NewGuid() |> EmployeeId
            EmployeeName = "A"
         }

         let progress = {
            Stub.progress Stub.command.updateRole with
               ApprovedBy = [ approver ]
         }

         Expect.isFalse
            (isNewApproval approver.EmployeeId progress)
            "should detect a duplicate approval"

         let progress = { progress with ApprovedBy = [] }

         Expect.isTrue
            (isNewApproval approver.EmployeeId progress)
            "should be a fresh approval"
      }

      test
         "canManageProgress should check if an employee can approve or deny an
          existing command approval progress" {
         let approver: EmployeeReference = {
            EmployeeId = Guid.NewGuid() |> EmployeeId
            EmployeeName = "A"
         }

         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.InviteEmployee
            Criteria = ApprovalCriteria.PerCommand
            Approvers = [ CommandApprover.Admin approver ]
         }

         let progress = {
            Stub.progress Stub.command.updateRole with
               RuleId = rule.RuleId
               ApprovedBy = [ approver ]
         }

         Expect.isFalse
            (canManageProgress rule progress approver.EmployeeId)
            "should not be allowed to approve the rule since they already have"

         let rule2 = {
            rule with
               Approvers = [ CommandApprover.AnyAdmin ]
         }

         Expect.isFalse
            (canManageProgress rule2 progress approver.EmployeeId)
            "should not be allowed to approve the rule since already have2"

         let progress = {
            progress with
               RuleId = rule.RuleId
               ApprovedBy = []
         }

         Expect.isTrue
            (canManageProgress rule progress approver.EmployeeId)
            "should be allowed to approve the rule since they haven't already"

         Expect.isFalse
            (canManageProgress rule progress (Guid.NewGuid() |> EmployeeId))
            "should not be allowed to approve the rule since they are not
            configured as an approver"

         let progress2 = {
            progress with
               Status = Status.Declined
         }

         Expect.isFalse
            (canManageProgress rule progress2 approver.EmployeeId)
            "should not be allowed to approve the rule since the progress status
            is not Pending"
      }

      test
         "remainingApprovalRequiredBy returns configured approvers who have
          not yet approved a command " {
         let approver: EmployeeReference = {
            EmployeeId = Guid.NewGuid() |> EmployeeId
            EmployeeName = "A"
         }

         let approverB: EmployeeReference = {
            EmployeeId = Guid.NewGuid() |> EmployeeId
            EmployeeName = "B"
         }

         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.InviteEmployee
            Criteria = ApprovalCriteria.PerCommand
            Approvers = [
               CommandApprover.Admin approver
               CommandApprover.Admin approverB
            ]
         }

         let progress = {
            Stub.progress Stub.command.updateRole with
               RuleId = rule.RuleId
               ApprovedBy = []
         }

         Expect.equal
            (remainingApprovalRequiredBy rule progress)
            rule.Approvers
            "should be 2 equivalent to the 2 approvers configured for the rule"

         let progress = {
            progress with
               ApprovedBy = [ approverB ]
         }

         Expect.equal
            (remainingApprovalRequiredBy rule progress)
            [ CommandApprover.Admin approver ]
            "should be 1 equivalent to one of the 2 approvers configured for the rule"

         let progress = {
            progress with
               ApprovedBy = [ approverB; approver ]
         }

         Expect.hasLength
            (remainingApprovalRequiredBy rule progress)
            0
            "should be zero approvals required when all configured approvers approved"

         let rule = {
            rule with
               Approvers = [
                  CommandApprover.AnyAdmin
                  CommandApprover.Admin approver
                  CommandApprover.Admin approverB
                  CommandApprover.AnyAdmin
               ]
         }

         Expect.equal
            (remainingApprovalRequiredBy rule progress)
            [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
            "should have 2 approvals required (AnyAdmin)"
      }

      test "numberOfApprovalsUserCanManage" {
         let approverA: EmployeeReference = {
            EmployeeId = Guid.NewGuid() |> EmployeeId
            EmployeeName = "A"
         }

         let approverB: EmployeeReference = {
            EmployeeId = Guid.NewGuid() |> EmployeeId
            EmployeeName = "B"
         }

         let rule1 = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.UpdateEmployeeRole
            Criteria = ApprovalCriteria.PerCommand
            Approvers = [ CommandApprover.Admin approverA ]
         }

         let rule2 = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.InviteEmployee
            Criteria = ApprovalCriteria.PerCommand
            Approvers = [
               CommandApprover.AnyAdmin
               CommandApprover.AnyAdmin
               CommandApprover.AnyAdmin
            ]
         }

         let progress1 = {
            Stub.progress Stub.command.updateRole with
               RuleId = rule1.RuleId
               ApprovedBy = [ approverA ]
         }

         let progress2 = {
            Stub.progress Stub.command.updateRole with
               ProgressId = Stub.progressId ()
               RuleId = rule1.RuleId
               ApprovedBy = []
         }

         let progress3 = {
            Stub.progress Stub.command.updateRole with
               ProgressId = Stub.progressId ()
               RuleId = rule2.RuleId
               ApprovedBy = [ approverB ]
         }

         let progress4 = {
            Stub.progress Stub.command.updateRole with
               ProgressId = Stub.progressId ()
               RuleId = rule2.RuleId
               ApprovedBy = [ approverA ]
         }

         let progress5 = {
            Stub.progress Stub.command.updateRole with
               ProgressId = Stub.progressId ()
               RuleId = rule2.RuleId
               ApprovedBy = []
         }

         let progress =
            Map [
               progress1.ProgressId, progress1
               progress2.ProgressId, progress2
               progress3.ProgressId, progress3
               progress4.ProgressId, progress4
               progress5.ProgressId, progress5
            ]

         let rules = Map [ rule1.RuleId, rule1; rule2.RuleId, rule2 ]

         Expect.equal
            (numberOfApprovalsUserCanManage rules progress approverA.EmployeeId)
            3
            "5 potential approvals - 2 already approved by approverA = 3"

         Expect.equal
            (numberOfApprovalsUserCanManage rules progress approverB.EmployeeId)
            2
            "3 potential approvals - 1 already approved by approverB = 2"
      }

      test
         "commandRequiresApproval detects if an incoming command requires
          initiation in an approval workflow before issuing the command" {
         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.UpdateEmployeeRole
            Criteria = ApprovalCriteria.PerCommand
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rule2 = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.InviteEmployee
            Criteria = ApprovalCriteria.PerCommand
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule; rule2.RuleId, rule2 ]
         let progress = Map.empty

         let ruleRequiringCommandApproval =
            Expect.wantSome
               (commandRequiresApproval
                  Stub.command.updateRole
                  rules
                  progress
                  Stub.accrual)
               "command should require approval for UpdateEmployeeRole rule"

         Expect.equal ruleRequiringCommandApproval rule ""

         let rule = {
            rule with
               Approvers = [ CommandApprover.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule; rule2.RuleId, rule2 ]

         Expect.isNone
            (commandRequiresApproval
               Stub.command.updateRole
               rules
               progress
               Stub.accrual)
            "command does not require approval if requester is only
            configured approver"

         let rules = Map [ rule2.RuleId, rule2 ]

         Expect.isNone
            (commandRequiresApproval
               Stub.command.updateRole
               rules
               progress
               Stub.accrual)
            "command does not require approval if no rule pertaining to the
            command type"
      }

      test
         "commandRequiresApproval detects if an incoming command meets
          AmountDailyLimit criteria" {
         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.DomesticTransfer
            Criteria = ApprovalCriteria.AmountDailyLimit 10_000m
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule ]
         let progress = Map.empty

         let accrual = {
            Stub.accrual with
               DomesticTransfer = 5_000m
         }

         let cmd =
            AccountStub.command.domesticTransfer 2_000m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         Expect.isNone
            (commandRequiresApproval cmd rules progress accrual)
            "command does not require approval if txn amount + daily amount accrued under limit"

         let cmd =
            AccountStub.command.domesticTransfer 5_000m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         Expect.isNone
            (commandRequiresApproval cmd rules progress accrual)
            "command does not require approval if txn amount + daily amount accrued = limit"

         let cmd =
            AccountStub.command.domesticTransfer 5_001m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         Expect.isSome
            (commandRequiresApproval cmd rules progress accrual)
            "command requires approval if txn amount + daily amount accrued > limit"
      }

      test
         "commandRequiresApproval detects if an incoming command meets
          AmountPerCommand criteria" {
         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.DomesticTransfer
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = Some 3000m
                  UpperBound = None
               }
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule ]
         let progress = Map.empty

         let cmd =
            AccountStub.command.domesticTransfer 2_000m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         Expect.isNone
            (commandRequiresApproval cmd rules progress Stub.accrual)
            "command does not require approval if txn amount does not meet
            AmountPerCommand criteria"

         let cmd =
            AccountStub.command.domesticTransfer 4_000m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         Expect.isSome
            (commandRequiresApproval cmd rules progress Stub.accrual)
            "command requires approval if txn amount > LowerBound"

         let cmd =
            AccountStub.command.domesticTransfer 3_000m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         Expect.isSome
            (commandRequiresApproval cmd rules progress Stub.accrual)
            "command requires approval if txn amount = LowerBound"
      }

      test
         "commandRequiresApproval detects if an incoming command meets
          multiple rule criteria, and returns the rule with
          ApprovalCriteria.AmountDailyLimit rather than
          ApprovalCriteria.AmountPerCommand { LowerBound = Some; UpperBound = None }
          if both rules exist" {
         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.DomesticTransfer
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = Some 3000m
                  UpperBound = None
               }
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rule2 = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.DomesticTransfer
            Criteria = ApprovalCriteria.AmountDailyLimit 10_000m
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule; rule2.RuleId, rule2 ]
         let progress = Map.empty

         let cmd =
            AccountStub.command.domesticTransfer 11_000m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         let ruleCorrespondingToRequiredApproval =
            Expect.wantSome
               (commandRequiresApproval cmd rules progress Stub.accrual)
               "command requires approval, meeting criteria for 2 rules"

         Expect.equal
            ruleCorrespondingToRequiredApproval
            rule2
            "when multiple rules correspond to a command then favor the rule
            with DailyLimit criteria"

         // Reverse the order to ensure the rule being favored has nothing to do
         // with ordering.
         let rules = Map [ rule2.RuleId, rule2; rule.RuleId, rule ]

         let ruleCorrespondingToRequiredApproval =
            Expect.wantSome
               (commandRequiresApproval cmd rules progress Stub.accrual)
               "command requires approval, meeting criteria for 2 rules"

         Expect.equal
            ruleCorrespondingToRequiredApproval
            rule2
            "when multiple rules correspond to a command then favor the rule
            with DailyLimit criteria"

         let cmd =
            AccountStub.command.domesticTransfer 4_000m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         let ruleCorrespondingToRequiredApproval =
            Expect.wantSome
               (commandRequiresApproval cmd rules progress Stub.accrual)
               "command requires approval, meeting criteria for 1 rules
                (amount is above lower bound threshold & below daily limit)"

         Expect.equal ruleCorrespondingToRequiredApproval rule ""
      }

      test
         "commandRequiresApproval chooses the appropriate rule among (Some, Some) & (Some, Some)
          when the amount equals the lower bound of one rule & the upper bound of another" {
         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.DomesticTransfer
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = Some 2m
                  UpperBound = Some 4m
               }
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rule2 = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.DomesticTransfer
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = Some 4m
                  UpperBound = Some 7m
               }
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule; rule2.RuleId, rule2 ]
         let progress = Map.empty

         let cmd =
            AccountStub.command.domesticTransfer 4m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         Expect.hasLength
            (associatedCommandApprovalRulesForCommand cmd Stub.accrual rules)
            1
            "When the lower bound of 1 rule meets the upper bound of another, only 1 will win"

         let ruleCorrespondingToRequiredApproval =
            Expect.wantSome
               (commandRequiresApproval cmd rules progress Stub.accrual)
               "command requires approval"

         Expect.equal
            ruleCorrespondingToRequiredApproval
            rule2
            "when one rule has an upper bound = txn amount & another rule has
             lower bound = txn amount then the latter is chosen"

         let cmd =
            AccountStub.command.domesticTransfer 6m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         let ruleCorrespondingToRequiredApproval =
            Expect.wantSome
               (commandRequiresApproval cmd rules progress Stub.accrual)
               "command requires approval"

         Expect.equal ruleCorrespondingToRequiredApproval rule2 ""
      }

      test
         "commandRequiresApproval chooses the appropriate rule among (Some, None) & (None, Some)
          when the amount equals the lower bound of one rule & the upper bound of another" {
         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.DomesticTransfer
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = None
                  UpperBound = Some 4m
               }
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rule2 = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.DomesticTransfer
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = Some 4m
                  UpperBound = None
               }
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule; rule2.RuleId, rule2 ]
         let progress = Map.empty

         let cmd =
            AccountStub.command.domesticTransfer 4m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         Expect.hasLength
            (associatedCommandApprovalRulesForCommand cmd Stub.accrual rules)
            1
            "When the lower bound of 1 rule meets the upper bound of another, only 1 will win"

         let ruleCorrespondingToRequiredApproval =
            Expect.wantSome
               (commandRequiresApproval cmd rules progress Stub.accrual)
               "command requires approval"

         Expect.equal
            ruleCorrespondingToRequiredApproval
            rule2
            "when one rule has an upper bound = txn amount & another rule has
             lower bound = txn amount then the latter is chosen"

         let cmd =
            AccountStub.command.domesticTransfer 6m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         let ruleCorrespondingToRequiredApproval =
            Expect.wantSome
               (commandRequiresApproval cmd rules progress Stub.accrual)
               "command requires approval"

         Expect.equal ruleCorrespondingToRequiredApproval rule2 ""
      }

      test
         "commandRequiresApproval chooses the appropriate rule among (Some, Some) & (None, Some)
          when the amount equals the lower bound of one rule & the upper bound of another" {
         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.DomesticTransfer
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = None
                  UpperBound = Some 4m
               }
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rule2 = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.DomesticTransfer
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = Some 4m
                  UpperBound = Some 7m
               }
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule; rule2.RuleId, rule2 ]
         let progress = Map.empty

         let cmd =
            AccountStub.command.domesticTransfer 4m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         Expect.hasLength
            (associatedCommandApprovalRulesForCommand cmd Stub.accrual rules)
            1
            "When the lower bound of 1 rule meets the upper bound of another, only 1 will win"

         let ruleCorrespondingToRequiredApproval =
            Expect.wantSome
               (commandRequiresApproval cmd rules progress Stub.accrual)
               "command requires approval"

         Expect.equal
            ruleCorrespondingToRequiredApproval
            rule2
            "when one rule has an upper bound = txn amount & another rule has
             lower bound = txn amount then the latter is chosen"

         let cmd =
            AccountStub.command.domesticTransfer 6m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         let ruleCorrespondingToRequiredApproval =
            Expect.wantSome
               (commandRequiresApproval cmd rules progress Stub.accrual)
               "command requires approval"

         Expect.equal ruleCorrespondingToRequiredApproval rule2 ""
      }

      test
         "commandRequiresApproval detects if an incoming command's associated
          progress record is already completed" {
         let cmd =
            AccountStub.command.domesticTransfer 4_000m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.DomesticTransfer
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = Some 3000m
                  UpperBound = None
               }
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule ]

         let p = {
            Stub.progress Stub.command.updateRole with
               ProgressId = CommandApprovalProgressId cmd.CorrelationId
               RuleId = rule.RuleId
               Status = Status.Pending
               CommandToInitiateOnApproval = cmd
               ApprovedBy = []
         }

         let progress = Map [ p.ProgressId, p ]

         Expect.isSome
            (commandRequiresApproval cmd rules progress Stub.accrual)
            ""

         let progress =
            Map [ p.ProgressId, { p with Status = Status.Declined } ]

         Expect.isNone
            (commandRequiresApproval cmd rules progress Stub.accrual)
            "no approval required if associated command progress is declined"

         let progress =
            Map [
               p.ProgressId,
               {
                  p with
                     Status =
                        Status.Terminated
                           CommandApprovalTerminationReason.AssociatedRuleApproverDeleted
               }
            ]

         Expect.isNone
            (commandRequiresApproval cmd rules progress Stub.accrual)
            "no approval required if associated command progress is terminated"

         let progress =
            Map [ p.ProgressId, { p with Status = Status.Approved } ]

         Expect.isNone
            (commandRequiresApproval cmd rules progress Stub.accrual)
            "no approval required if associated command progress is complete"
      }

      test
         "Attempting to configure an additional approval rule of certain command types is not allowed" {
         let approverA: EmployeeReference = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let approverB: EmployeeReference = {
            EmployeeName = "B"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let commandTypes = [
            Stub.commandTypes.InviteEmployee, ApprovalCriteria.PerCommand
            Stub.commandTypes.UpdateEmployeeRole, ApprovalCriteria.PerCommand
            Stub.commandTypes.DomesticTransfer,
            ApprovalCriteria.AmountDailyLimit 10m

            Stub.commandTypes.Payment, ApprovalCriteria.AmountDailyLimit 10m

            Stub.commandTypes.InternalTransfer,
            ApprovalCriteria.AmountDailyLimit 10m
         ]

         for cmdType, criteria in commandTypes do
            let rule = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria = criteria
               Approvers = [
                  CommandApprover.Admin approverA
                  CommandApprover.Admin approverB
               ]
            }

            let cmd =
               ConfigureApprovalRuleCommand.create
                  Stub.orgId
                  Stub.initiator
                  rule
               |> OrgCommand.ConfigureApprovalRule

            let res = update Stub.orgStateWithEvents cmd
            let _, org = Expect.wantOk res ""

            Expect.hasLength org.Info.CommandApprovalRules 1 ""

            let cmd =
               ConfigureApprovalRuleCommand.create Stub.orgId Stub.initiator {
                  rule with
                     RuleId = Stub.ruleId ()
               }
               |> OrgCommand.ConfigureApprovalRule

            let res = update org cmd
            let err = Expect.wantError res ""

            match err with
            | Err.OrgStateTransitionError(OrgStateTransitionError.ApprovalRuleMultipleOfType _) ->
               Expect.isTrue true ""
            | _ ->
               Expect.isTrue
                  false
                  $"{cmdType} command type not allowed to have more than 1 rule"
      }

      test
         "Attempting to configure an additional approval rule with conflicting criteria is not allowed" {
         let approverA: EmployeeReference = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let approverB: EmployeeReference = {
            EmployeeName = "B"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let commandTypes = [
            Stub.commandTypes.DomesticTransfer
            Stub.commandTypes.InternalTransfer
            Stub.commandTypes.Payment
         ]

         let conflictingCriteria = [
            ApprovalCriteria.AmountDailyLimit 10m,
            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 15m
               UpperBound = None
            }

            ApprovalCriteria.AmountPerCommand {
               LowerBound = None
               UpperBound = Some 15m
            },
            ApprovalCriteria.AmountDailyLimit 10m

            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 10m
               UpperBound = None
            },
            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 15m
               UpperBound = None
            }

            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 10m
               UpperBound = Some 20m
            },
            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 15m
               UpperBound = None
            }

            ApprovalCriteria.AmountPerCommand {
               LowerBound = None
               UpperBound = Some 15m
            },
            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 10m
               UpperBound = Some 20m
            }
         ]

         for cmdType in commandTypes do
            for criteriaOfFirst, criteriaOfSecond in conflictingCriteria do
               let rule = {
                  RuleId = Stub.ruleId ()
                  OrgId = Stub.orgId
                  CommandType = cmdType
                  Criteria = criteriaOfFirst
                  Approvers = [
                     CommandApprover.Admin approverA
                     CommandApprover.Admin approverB
                  ]
               }

               let cmd =
                  ConfigureApprovalRuleCommand.create
                     Stub.orgId
                     Stub.initiator
                     rule
                  |> OrgCommand.ConfigureApprovalRule

               let res = update Stub.orgStateWithEvents cmd
               let _, org = Expect.wantOk res ""

               Expect.hasLength org.Info.CommandApprovalRules 1 ""

               let rule2 = {
                  RuleId = Stub.ruleId ()
                  OrgId = Stub.orgId
                  CommandType = cmdType
                  Criteria = criteriaOfSecond
                  Approvers = [
                     CommandApprover.Admin approverA
                     CommandApprover.Admin approverB
                  ]
               }

               let cmd =
                  ConfigureApprovalRuleCommand.create
                     Stub.orgId
                     Stub.initiator
                     rule2
                  |> OrgCommand.ConfigureApprovalRule

               let res = update org cmd
               let err = Expect.wantError res ""

               match err with
               | Err.OrgStateTransitionError OrgStateTransitionError.ApprovalRuleHasConflictingCriteria ->
                  Expect.isTrue true ""
               | _ ->
                  Expect.isTrue
                     false
                     $"{criteriaOfFirst} should conflict with {criteriaOfSecond}"
      }

      test
         "Attempting to configure an additional approval rule of certain CommandTypes without conflicting criteria is allowed" {
         let approverA: EmployeeReference = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let approverB: EmployeeReference = {
            EmployeeName = "B"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let commandTypes = [
            Stub.commandTypes.DomesticTransfer
            Stub.commandTypes.InternalTransfer
            Stub.commandTypes.Payment
         ]

         let criteria = [
            ApprovalCriteria.AmountDailyLimit 10m,
            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 5m
               UpperBound = None
            }

            ApprovalCriteria.AmountPerCommand {
               LowerBound = None
               UpperBound = Some 5m
            },
            ApprovalCriteria.AmountDailyLimit 10m

            ApprovalCriteria.AmountPerCommand {
               LowerBound = None
               UpperBound = Some 14m
            },
            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 14m
               UpperBound = None
            }

            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 10m
               UpperBound = Some 20m
            },
            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 20m
               UpperBound = None
            }

            ApprovalCriteria.AmountPerCommand {
               LowerBound = None
               UpperBound = Some 9m
            },
            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 9m
               UpperBound = Some 20m
            }

            ApprovalCriteria.AmountPerCommand {
               LowerBound = None
               UpperBound = Some 10m
            },
            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 10m
               UpperBound = Some 20m
            }

            ApprovalCriteria.AmountPerCommand {
               LowerBound = None
               UpperBound = Some 10m
            },
            ApprovalCriteria.AmountPerCommand {
               LowerBound = Some 10m
               UpperBound = None
            }
         ]

         for cmdType in commandTypes do
            for criteriaOfFirst, criteriaOfSecond in criteria do
               let rule = {
                  RuleId = Stub.ruleId ()
                  OrgId = Stub.orgId
                  CommandType = cmdType
                  Criteria = criteriaOfFirst
                  Approvers = [
                     CommandApprover.Admin approverA
                     CommandApprover.Admin approverB
                  ]
               }

               let cmd =
                  ConfigureApprovalRuleCommand.create
                     Stub.orgId
                     Stub.initiator
                     rule
                  |> OrgCommand.ConfigureApprovalRule

               let res = update Stub.orgStateWithEvents cmd
               let _, org = Expect.wantOk res ""

               Expect.hasLength org.Info.CommandApprovalRules 1 ""

               let rule2 = {
                  RuleId = Stub.ruleId ()
                  OrgId = Stub.orgId
                  CommandType = cmdType
                  Criteria = criteriaOfSecond
                  Approvers = [
                     CommandApprover.Admin approverA
                     CommandApprover.Admin approverB
                  ]
               }

               let cmd =
                  ConfigureApprovalRuleCommand.create
                     Stub.orgId
                     Stub.initiator
                     rule2
                  |> OrgCommand.ConfigureApprovalRule

               let res = update org cmd
               let _, org = Expect.wantOk res ""

               Expect.hasLength
                  org.Info.CommandApprovalRules
                  2
                  $"{criteriaOfFirst} should not conflict with {criteriaOfSecond}"
      }

      test "AcquireCommandApproval" {
         let approverA: EmployeeReference = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let approverB: EmployeeReference = {
            EmployeeName = "B"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.UpdateEmployeeRole
            Criteria = ApprovalCriteria.PerCommand
            Approvers = [
               CommandApprover.Admin approverA
               CommandApprover.Admin approverB
            ]
         }

         let initiatedBy = {
            Id = approverA.EmployeeId |> InitiatedById
            Name = approverA.EmployeeName
         }

         let cmd =
            ConfigureApprovalRuleCommand.create Stub.orgId initiatedBy rule
            |> OrgCommand.ConfigureApprovalRule

         let res = update Stub.orgStateWithEvents cmd
         let _, org = Expect.wantOk res ""

         let cmdData: CommandApprovalRequested = {
            RuleId = Stub.ruleId ()
            Requester = approverA
            RequesterIsConfiguredAsAnApprover = true
            Command = Stub.command.updateRole
         }

         let cmd =
            RequestCommandApproval.create
               Stub.orgId
               initiatedBy
               (Guid.NewGuid() |> CorrelationId)
               cmdData
            |> OrgCommand.RequestCommandApproval

         let res = update org cmd
         let err = Expect.wantError res ""

         match err with
         | Err.OrgStateTransitionError OrgStateTransitionError.ApprovalRuleNotFound ->
            Expect.isTrue true ""
         | _ ->
            Expect.isTrue
               false
               "Request command approval for a command with no
               associated rule should error"

         let cmd =
            RequestCommandApproval.create
               Stub.orgId
               initiatedBy
               (Guid.NewGuid() |> CorrelationId)
               { cmdData with RuleId = rule.RuleId }
            |> OrgCommand.RequestCommandApproval

         let res = update org cmd
         let _, org = Expect.wantOk res ""

         Expect.hasLength
            org.Info.CommandApprovalProgress
            1
            "in progress command approval associated with org"

         let progress = Seq.head org.Info.CommandApprovalProgress.Values

         let cmd =
            AcquireCommandApproval.create Stub.orgId {
               RuleId = rule.RuleId
               ApprovedBy = approverA
               ProgressId = progress.ProgressId
               Command = progress.CommandToInitiateOnApproval
            }
            |> OrgCommand.AcquireCommandApproval

         let res = update org cmd
         let err = Expect.wantError res ""

         match err with
         | Err.OrgStateTransitionError(OrgStateTransitionError.ApproverAlreadyApprovedCommand(a,
                                                                                              _)) ->
            Expect.equal
               a
               cmdData.Requester.EmployeeId
               "ApproverA should have been included on progress.ApprovedBy since
               it is one of the approvers configured the rule & the requester is
               equivalent to ApproverA."
         | _ ->
            Expect.isTrue
               false
               "It is not allowed for the same approver to
               approve an approval progress item multiple times"

         let cmd =
            AcquireCommandApproval.create Stub.orgId {
               RuleId = rule.RuleId
               // NOTE: change approver to one that hasn't already approved the
               // command
               ApprovedBy = approverB
               ProgressId = progress.ProgressId
               Command = progress.CommandToInitiateOnApproval
            }
            |> OrgCommand.AcquireCommandApproval

         let res = update org cmd
         let evt, org = Expect.wantOk res ""

         match evt with
         | OrgEvent.CommandApprovalProcessCompleted _ -> Expect.isTrue true ""
         | _ ->
            Expect.isTrue
               false
               "Recognize that the AcquireCommandApproval
               command will lead to all approvals being acquired for the command so
               go ahead & recognize it as an
               OrgEvent.CommandApprovalProcessCompleted instead of OrgEvent.CommandApprovalAcquired"

         let progress = Seq.head org.Info.CommandApprovalProgress.Values

         Expect.equal
            progress.Status
            CommandApprovalProgress.Status.Approved
            "Expect a completed status for the progress"

         Expect.hasLength
            progress.ApprovedBy
            rule.Approvers.Length
            "Progress ApprovedBy should be equivalent to Rule Approvers"

         // Issue another AcquireCommandApproval to validate progress
         // end-of-life
         let res = update org cmd
         let err = Expect.wantError res ""

         match err with
         | Err.OrgStateTransitionError OrgStateTransitionError.ApprovalProgressWorklowNotActive ->
            Expect.isTrue true ""
         | _ ->
            Expect.isTrue
               false
               "Should not be able to acquire additional
               approvals once progress item is no longer active"
      }

      test
         "Creating a ManageApprovalRule requires approval for future creation/editing of rules" {
         let admin = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let otherAdmin = {
            EmployeeName = "B"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let ruleIdForManagingApproval = Stub.ruleId ()

         let cmdToMakeManagingRulesRequireDualAdminApproval =
            ConfigureApprovalRuleCommand.create
               Stub.orgId
               {
                  Id = InitiatedById admin.EmployeeId
                  Name = admin.EmployeeName
               }
               {
                  RuleId = ruleIdForManagingApproval
                  OrgId = Stub.orgId
                  CommandType =
                     ApprovableCommandType.ApprovablePerCommand
                        ManageApprovalRuleCommandType
                  Criteria = ApprovalCriteria.PerCommand
                  Approvers = [
                     CommandApprover.Admin admin
                     CommandApprover.Admin otherAdmin
                  ]
               }
            |> OrgCommand.ConfigureApprovalRule

         let res =
            update
               Stub.orgStateWithEvents
               cmdToMakeManagingRulesRequireDualAdminApproval

         let _, org = Expect.wantOk res ""

         let ruleToInviteEmployee = {
            RuleId = Stub.ruleId ()
            OrgId = org.Info.OrgId
            CommandType = Stub.commandTypes.InviteEmployee
            Criteria = ApprovalCriteria.PerCommand
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let initiator = {
            Id = InitiatedById admin.EmployeeId
            Name = admin.EmployeeName
         }

         let approvalRequest: CommandApprovalRequested = {
            RuleId = ruleIdForManagingApproval
            Requester = admin
            RequesterIsConfiguredAsAnApprover = true
            Command =
               ManageApprovalRuleInput.CreateOrEdit(
                  ruleToInviteEmployee,
                  initiator
               )
               |> ManageApprovalRuleCommand.create
               |> ManageApprovalRule
               |> ApprovableCommand.PerCommand
         }

         let cmdRequestingApprovalToManageRule =
            RequestCommandApproval.create
               org.Info.OrgId
               initiator
               (Guid.NewGuid() |> CorrelationId)
               approvalRequest
            |> OrgCommand.RequestCommandApproval

         let res = update org cmdRequestingApprovalToManageRule
         let _, org = Expect.wantOk res ""

         let progress = Seq.head org.Info.CommandApprovalProgress.Values
         Expect.equal progress.Status CommandApprovalProgress.Status.Pending ""

         Expect.isFalse
            (org.Info.CommandApprovalRules.ContainsKey
               ruleToInviteEmployee.RuleId)
            "Invite Employee rule should not exist in org rules when approval pending"

         let cmd =
            AcquireCommandApproval.create org.Info.OrgId {
               RuleId = progress.RuleId
               ApprovedBy = otherAdmin
               ProgressId = progress.ProgressId
               Command = progress.CommandToInitiateOnApproval
            }
            |> OrgCommand.AcquireCommandApproval

         let res = update org cmd
         let _, org = Expect.wantOk res ""

         let progress = Seq.head org.Info.CommandApprovalProgress.Values
         Expect.equal progress.Status CommandApprovalProgress.Status.Approved ""

         Expect.isTrue
            (org.Info.CommandApprovalRules.ContainsKey
               ruleToInviteEmployee.RuleId)
            "Invite Employee rule should be present in org rules after approval completion"

         let ruleToInviteEmployeeEdited = {
            ruleToInviteEmployee with
               Approvers = [
                  CommandApprover.Admin admin
                  CommandApprover.Admin otherAdmin
               ]
         }

         let initiator = {
            Id = InitiatedById admin.EmployeeId
            Name = admin.EmployeeName
         }

         let approvalRequest: CommandApprovalRequested = {
            RuleId = ruleIdForManagingApproval
            Requester = admin
            RequesterIsConfiguredAsAnApprover = true
            Command =
               ManageApprovalRuleInput.CreateOrEdit(
                  ruleToInviteEmployeeEdited,
                  initiator
               )
               |> ManageApprovalRuleCommand.create
               |> ManageApprovalRule
               |> ApprovableCommand.PerCommand
         }

         let cmdRequestingApprovalToManageRule =
            RequestCommandApproval.create
               org.Info.OrgId
               initiator
               (Guid.NewGuid() |> CorrelationId)
               approvalRequest
            |> OrgCommand.RequestCommandApproval

         let res = update org cmdRequestingApprovalToManageRule
         let _, org = Expect.wantOk res ""

         let progress =
            org.Info.CommandApprovalProgress.Values
            |> Seq.tryFind (fun p ->
               p.Status = CommandApprovalProgress.Status.Pending)

         let progress = Expect.wantSome progress ""

         Expect.notEqual
            org.Info.CommandApprovalRules[ruleToInviteEmployee.RuleId].Approvers
            ruleToInviteEmployeeEdited.Approvers
            "Existing invite employee rule should not yet have updated Approvers
            since approval for editing the invite employee rule is in progress"

         let cmd =
            AcquireCommandApproval.create org.Info.OrgId {
               RuleId = progress.RuleId
               ApprovedBy = otherAdmin
               ProgressId = progress.ProgressId
               Command = progress.CommandToInitiateOnApproval
            }
            |> OrgCommand.AcquireCommandApproval

         let res = update org cmd
         let _, org = Expect.wantOk res ""

         Expect.equal
            org.Info.CommandApprovalRules[ruleToInviteEmployee.RuleId].Approvers
            ruleToInviteEmployeeEdited.Approvers
            "Existing invite employee rule should have updated Approvers upon approval of
            editing the invite employee rule"
      }

      test
         "Creating a ManageApprovalRule requires approval for future deletion of rules" {
         let admin = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let otherAdmin = {
            EmployeeName = "B"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let ruleToInviteEmployee = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.InviteEmployee
            Criteria = ApprovalCriteria.PerCommand
            Approvers = [ CommandApprover.AnyAdmin; CommandApprover.AnyAdmin ]
         }

         let configureInviteEmployeeApprovalRuleCmd =
            ConfigureApprovalRuleCommand.create
               Stub.orgId
               {
                  Id = InitiatedById admin.EmployeeId
                  Name = admin.EmployeeName
               }
               {
                  RuleId = ruleToInviteEmployee.RuleId
                  OrgId = Stub.orgId
                  CommandType =
                     ApprovableCommandType.ApprovablePerCommand
                        ManageApprovalRuleCommandType
                  Criteria = ApprovalCriteria.PerCommand
                  Approvers = [
                     CommandApprover.Admin admin
                     CommandApprover.Admin otherAdmin
                  ]
               }
            |> OrgCommand.ConfigureApprovalRule

         let res =
            update
               Stub.orgStateWithEvents
               configureInviteEmployeeApprovalRuleCmd

         let _, org = Expect.wantOk res ""

         Expect.isTrue
            (org.Info.CommandApprovalRules.ContainsKey
               ruleToInviteEmployee.RuleId)
            ""

         let ruleIdForManagingApproval = Stub.ruleId ()

         let cmdToMakeManagingRulesRequireDualAdminApproval =
            ConfigureApprovalRuleCommand.create
               Stub.orgId
               {
                  Id = InitiatedById admin.EmployeeId
                  Name = admin.EmployeeName
               }
               {
                  RuleId = ruleIdForManagingApproval
                  OrgId = Stub.orgId
                  CommandType =
                     ApprovableCommandType.ApprovablePerCommand
                        ManageApprovalRuleCommandType
                  Criteria = ApprovalCriteria.PerCommand
                  Approvers = [
                     CommandApprover.Admin admin
                     CommandApprover.Admin otherAdmin
                  ]
               }
            |> OrgCommand.ConfigureApprovalRule

         let res =
            update
               Stub.orgStateWithEvents
               cmdToMakeManagingRulesRequireDualAdminApproval

         let _, org = Expect.wantOk res ""

         let initiator = {
            Id = InitiatedById admin.EmployeeId
            Name = admin.EmployeeName
         }

         let cmdRequestingApprovalToDeleteRule =
            RequestCommandApproval.create
               org.Info.OrgId
               initiator
               (Guid.NewGuid() |> CorrelationId)
               {
                  RuleId = ruleIdForManagingApproval
                  Requester = admin
                  RequesterIsConfiguredAsAnApprover = true
                  Command =
                     ManageApprovalRuleInput.Delete(
                        ruleToInviteEmployee,
                        initiator
                     )
                     |> ManageApprovalRuleCommand.create
                     |> ManageApprovalRule
                     |> ApprovableCommand.PerCommand
               }
            |> OrgCommand.RequestCommandApproval

         let res = update org cmdRequestingApprovalToDeleteRule
         let _, org = Expect.wantOk res ""

         let progress = Seq.head org.Info.CommandApprovalProgress.Values
         Expect.equal progress.Status CommandApprovalProgress.Status.Pending ""

         let cmd =
            AcquireCommandApproval.create Stub.orgId {
               RuleId = progress.RuleId
               ApprovedBy = otherAdmin
               ProgressId = progress.ProgressId
               Command = progress.CommandToInitiateOnApproval
            }
            |> OrgCommand.AcquireCommandApproval

         let res = update org cmd
         let _, org = Expect.wantOk res ""
         let progress = Seq.head org.Info.CommandApprovalProgress.Values
         Expect.equal progress.Status CommandApprovalProgress.Status.Approved ""

         Expect.isFalse
            (org.Info.CommandApprovalRules.ContainsKey
               ruleToInviteEmployee.RuleId)
            "Rule should be removed from org after approval completion"
      }

      test
         "requestCommandApproval validates ManageApprovalRule commands with conflicting criteria detection" {
         let admin = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let otherAdmin = {
            EmployeeName = "B"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let existingPaymentRule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.Payment
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = Some 10m
                  UpperBound = Some 20m
               }
            Approvers = [ CommandApprover.AnyAdmin ]
         }

         let cmdToAddPaymentRule =
            ConfigureApprovalRuleCommand.create
               Stub.orgId
               {
                  Id = InitiatedById admin.EmployeeId
                  Name = admin.EmployeeName
               }
               existingPaymentRule
            |> OrgCommand.ConfigureApprovalRule

         let res = update Stub.orgStateWithEvents cmdToAddPaymentRule
         let _, org = Expect.wantOk res ""

         let ruleIdForManagingApproval = Stub.ruleId ()

         let cmdToMakeManagingRulesRequireDualAdminApproval =
            ConfigureApprovalRuleCommand.create
               Stub.orgId
               {
                  Id = InitiatedById admin.EmployeeId
                  Name = admin.EmployeeName
               }
               {
                  RuleId = ruleIdForManagingApproval
                  OrgId = Stub.orgId
                  CommandType =
                     ApprovableCommandType.ApprovablePerCommand
                        ManageApprovalRuleCommandType
                  Criteria = ApprovalCriteria.PerCommand
                  Approvers = [
                     CommandApprover.Admin admin
                     CommandApprover.Admin otherAdmin
                  ]
               }
            |> OrgCommand.ConfigureApprovalRule

         let res = update org cmdToMakeManagingRulesRequireDualAdminApproval
         let _, org = Expect.wantOk res ""

         let conflictingPaymentRule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.Payment
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = Some 19m
                  UpperBound = None
               }
            Approvers = [ CommandApprover.AnyAdmin ]
         }

         let initiator = {
            Id = InitiatedById admin.EmployeeId
            Name = admin.EmployeeName
         }

         let cmdRequestingApprovalForConflictingRule =
            RequestCommandApproval.create
               org.Info.OrgId
               initiator
               (Guid.NewGuid() |> CorrelationId)
               {
                  RuleId = ruleIdForManagingApproval
                  Requester = admin
                  RequesterIsConfiguredAsAnApprover = true
                  Command =
                     ManageApprovalRuleInput.CreateOrEdit(
                        conflictingPaymentRule,
                        initiator
                     )
                     |> ManageApprovalRuleCommand.create
                     |> ManageApprovalRule
                     |> ApprovableCommand.PerCommand
               }
            |> OrgCommand.RequestCommandApproval

         let res = update org cmdRequestingApprovalForConflictingRule
         let err = Expect.wantError res ""

         match err with
         | Err.OrgStateTransitionError OrgStateTransitionError.ApprovalRuleHasConflictingCriteria ->
            Expect.isTrue true ""
         | _ -> Expect.isTrue false "Should have detected conflicting criteria"
      }

      test
         "requestCommandApproval validates ManageApprovalRule commands with RangeGap detection" {
         let admin = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let otherAdmin = {
            EmployeeName = "B"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let existingPaymentRule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.Payment
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = Some 10m
                  UpperBound = Some 20m
               }
            Approvers = [ CommandApprover.AnyAdmin ]
         }

         let cmdToAddPaymentRule =
            ConfigureApprovalRuleCommand.create
               Stub.orgId
               {
                  Id = InitiatedById admin.EmployeeId
                  Name = admin.EmployeeName
               }
               existingPaymentRule
            |> OrgCommand.ConfigureApprovalRule

         let res = update Stub.orgStateWithEvents cmdToAddPaymentRule
         let _, org = Expect.wantOk res ""

         let ruleIdForManagingApproval = Stub.ruleId ()

         let cmdToMakeManagingRulesRequireDualAdminApproval =
            ConfigureApprovalRuleCommand.create
               Stub.orgId
               {
                  Id = InitiatedById admin.EmployeeId
                  Name = admin.EmployeeName
               }
               {
                  RuleId = ruleIdForManagingApproval
                  OrgId = Stub.orgId
                  CommandType =
                     ApprovableCommandType.ApprovablePerCommand
                        ManageApprovalRuleCommandType
                  Criteria = ApprovalCriteria.PerCommand
                  Approvers = [
                     CommandApprover.Admin admin
                     CommandApprover.Admin otherAdmin
                  ]
               }
            |> OrgCommand.ConfigureApprovalRule

         let res = update org cmdToMakeManagingRulesRequireDualAdminApproval
         let _, org = Expect.wantOk res ""

         let ruleWithAmountGap = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.Payment
            Criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = Some 25m
                  UpperBound = None
               }
            Approvers = [ CommandApprover.AnyAdmin ]
         }

         let initiator = {
            Id = InitiatedById admin.EmployeeId
            Name = admin.EmployeeName
         }

         let cmdRequestingApprovalForRuleWithGap =
            RequestCommandApproval.create
               org.Info.OrgId
               initiator
               (Guid.NewGuid() |> CorrelationId)
               {
                  RuleId = ruleIdForManagingApproval
                  Requester = admin
                  RequesterIsConfiguredAsAnApprover = true
                  Command =
                     ManageApprovalRuleInput.CreateOrEdit(
                        ruleWithAmountGap,
                        initiator
                     )
                     |> ManageApprovalRuleCommand.create
                     |> ManageApprovalRule
                     |> ApprovableCommand.PerCommand
               }
            |> OrgCommand.RequestCommandApproval

         let res = update org cmdRequestingApprovalForRuleWithGap
         let err = Expect.wantError res ""

         match err with
         | Err.OrgStateTransitionError(OrgStateTransitionError.ApprovalRuleHasGapInCriteria _) ->
            Expect.isTrue true ""
         | _ -> Expect.isTrue false "Should have detected amount gap"
      }

      test
         "requestCommandApproval validates ManageApprovalRule commands with
         detection of inner rule duplicates" {
         let admin = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let otherAdmin = {
            EmployeeName = "B"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let existingPaymentRule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.Payment
            Criteria = ApprovalCriteria.AmountDailyLimit 100m
            Approvers = [ CommandApprover.AnyAdmin ]
         }

         let cmdToAddPaymentRule =
            ConfigureApprovalRuleCommand.create
               Stub.orgId
               {
                  Id = InitiatedById admin.EmployeeId
                  Name = admin.EmployeeName
               }
               existingPaymentRule
            |> OrgCommand.ConfigureApprovalRule

         let res = update Stub.orgStateWithEvents cmdToAddPaymentRule
         let _, org = Expect.wantOk res ""

         let ruleIdForManagingApproval = Stub.ruleId ()

         let cmdToMakeManagingRulesRequireDualAdminApproval =
            ConfigureApprovalRuleCommand.create
               Stub.orgId
               {
                  Id = InitiatedById admin.EmployeeId
                  Name = admin.EmployeeName
               }
               {
                  RuleId = ruleIdForManagingApproval
                  OrgId = Stub.orgId
                  CommandType =
                     ApprovableCommandType.ApprovablePerCommand
                        ManageApprovalRuleCommandType
                  Criteria = ApprovalCriteria.PerCommand
                  Approvers = [
                     CommandApprover.Admin admin
                     CommandApprover.Admin otherAdmin
                  ]
               }
            |> OrgCommand.ConfigureApprovalRule

         let res = update org cmdToMakeManagingRulesRequireDualAdminApproval
         let _, org = Expect.wantOk res ""

         let duplicateCommandTypeRule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.Payment
            Criteria = ApprovalCriteria.AmountDailyLimit 10m
            Approvers = [ CommandApprover.AnyAdmin ]
         }

         let initiator = {
            Id = InitiatedById admin.EmployeeId
            Name = admin.EmployeeName
         }

         let cmdRequestingApprovalForDuplicateType =
            RequestCommandApproval.create
               org.Info.OrgId
               initiator
               (Guid.NewGuid() |> CorrelationId)
               {
                  RuleId = ruleIdForManagingApproval
                  Requester = admin
                  RequesterIsConfiguredAsAnApprover = true
                  Command =
                     ManageApprovalRuleInput.CreateOrEdit(
                        duplicateCommandTypeRule,
                        initiator
                     )
                     |> ManageApprovalRuleCommand.create
                     |> ManageApprovalRule
                     |> ApprovableCommand.PerCommand
               }
            |> OrgCommand.RequestCommandApproval

         let res = update org cmdRequestingApprovalForDuplicateType
         let err = Expect.wantError res ""

         match err with
         | Err.OrgStateTransitionError(OrgStateTransitionError.ApprovalRuleMultipleOfType _) ->
            Expect.isTrue true ""
         | _ ->
            Expect.isTrue false "Should have detected duplicate command type"
      }

      test
         "requestCommandApproval of ManageApprovalRule commands passes validation when intending to delete rule" {
         let admin = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let otherAdmin = {
            EmployeeName = "B"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let existingPaymentRule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.Payment
            Criteria = ApprovalCriteria.AmountDailyLimit 100m
            Approvers = [ CommandApprover.AnyAdmin ]
         }

         let cmdToAddPaymentRule =
            ConfigureApprovalRuleCommand.create
               Stub.orgId
               {
                  Id = InitiatedById admin.EmployeeId
                  Name = admin.EmployeeName
               }
               existingPaymentRule
            |> OrgCommand.ConfigureApprovalRule

         let res = update Stub.orgStateWithEvents cmdToAddPaymentRule
         let _, org = Expect.wantOk res ""

         let ruleIdForManagingApproval = Stub.ruleId ()

         let cmdToMakeManagingRulesRequireDualAdminApproval =
            ConfigureApprovalRuleCommand.create
               Stub.orgId
               {
                  Id = InitiatedById admin.EmployeeId
                  Name = admin.EmployeeName
               }
               {
                  RuleId = ruleIdForManagingApproval
                  OrgId = Stub.orgId
                  CommandType =
                     ApprovableCommandType.ApprovablePerCommand
                        ManageApprovalRuleCommandType
                  Criteria = ApprovalCriteria.PerCommand
                  Approvers = [
                     CommandApprover.Admin admin
                     CommandApprover.Admin otherAdmin
                  ]
               }
            |> OrgCommand.ConfigureApprovalRule

         let res = update org cmdToMakeManagingRulesRequireDualAdminApproval
         let _, org = Expect.wantOk res ""

         let duplicateCommandTypeRule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = Stub.commandTypes.Payment
            Criteria = ApprovalCriteria.AmountDailyLimit 10m
            Approvers = [ CommandApprover.AnyAdmin ]
         }

         let initiator = {
            Id = InitiatedById admin.EmployeeId
            Name = admin.EmployeeName
         }

         let cmdRequestingApprovalForDuplicateType =
            RequestCommandApproval.create
               org.Info.OrgId
               initiator
               (Guid.NewGuid() |> CorrelationId)
               {
                  RuleId = ruleIdForManagingApproval
                  Requester = admin
                  RequesterIsConfiguredAsAnApprover = true
                  Command =
                     // NOTE: DELETION bypasses validation
                     ManageApprovalRuleInput.Delete(
                        duplicateCommandTypeRule,
                        initiator
                     )
                     |> ManageApprovalRuleCommand.create
                     |> ManageApprovalRule
                     |> ApprovableCommand.PerCommand
               }
            |> OrgCommand.RequestCommandApproval

         let res = update org cmdRequestingApprovalForDuplicateType
         Expect.isOk res "expect deletion to bypass validation"
      }
   ]
