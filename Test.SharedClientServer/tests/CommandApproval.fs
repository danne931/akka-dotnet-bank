module CommandApprovalTests

open System

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Bank.Org.Domain
open CommandApprovalRule
open Lib.SharedTypes

module Stub =
   let orgId = Guid.NewGuid() |> OrgId

let tests =
   testList "Command Approval domain" [
      test "AmountPerCommandRange detects overlap in Some, Some, Some, None" {
         let existingRange = {
            LowerBound = Some 2m
            UpperBound = Some 5m
         }

         let range = {
            LowerBound = Some 3m
            UpperBound = None
         }

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               existingRange
               range

         Expect.isTrue overlap "(3, infinity) should overlap with (2, 5)"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap existingRange {
               range with
                  LowerBound = Some 1m
            }

         Expect.isTrue overlap "(1, infinity) should overlap with (2, 5)"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap existingRange {
               range with
                  LowerBound = Some 6m
            }

         Expect.isFalse
            overlap
            "(6, infinity) is outside of range (2, 5) so should not overlap"
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

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               existingRange
               range

         Expect.isTrue overlap "(0, 3) overlaps with (2, 5)"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap existingRange {
               range with
                  UpperBound = Some 1m
            }

         Expect.isFalse
            overlap
            "(0, 1) is outside of range (2, 5) so should not overlap"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap existingRange {
               range with
                  UpperBound = Some 6m
            }

         Expect.isTrue overlap "(0, 6) should overlap with (2, 5)"
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

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               existingRange
               range

         Expect.isTrue overlap "(2, 4) overlaps with (0, 5)"

         let range = {
            LowerBound = Some 6m
            UpperBound = Some 8m
         }

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               existingRange
               range

         Expect.isFalse
            overlap
            "(6, 8) is outside of range (0, 5) so should not overlap"
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

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               existingRange
               range

         Expect.isTrue overlap "(0, 4) overlaps with (0, 5)"

         let existingRange = {
            LowerBound = None
            UpperBound = Some 1m
         }

         let range = {
            LowerBound = None
            UpperBound = Some 5m
         }

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               existingRange
               range

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

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               existingRange
               range

         Expect.isTrue overlap "(4, infinity) overlaps with (0, 5)"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap existingRange {
               range with
                  LowerBound = Some 6m
            }

         Expect.isFalse overlap "(6, infinity) does not overlap with (0, 5)"
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

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               existingRange
               range

         Expect.isTrue overlap "(3, 5) should overlap with (2, infinity)"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               {
                  existingRange with
                     LowerBound = Some 6m
               }
               range

         Expect.isFalse overlap "(3, 5) should not overlap with (6, infinity)"
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

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               existingRange
               range

         Expect.isTrue overlap "(0, 5) should overlap with (2, infinity)"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap existingRange {
               range with
                  UpperBound = Some 1m
            }

         Expect.isFalse overlap "(0, 1) should not overlap with (2, infinity)"
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

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               existingRange
               range

         Expect.isTrue overlap "(5, infinity) should overlap with (2, infinity)"

         let existingRange = {
            LowerBound = Some 5m
            UpperBound = None
         }

         let range = {
            LowerBound = Some 2m
            UpperBound = None
         }

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               existingRange
               range

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

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               existingRange
               range

         Expect.isFalse overlap "(6, 10) should not overlap with (2, 5)"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap existingRange {
               range with
                  LowerBound = Some 4m
            }

         Expect.isTrue overlap "(4, 10) should overlap with (2, 5)"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               {
                  LowerBound = Some 7m
                  UpperBound = Some 9m
               }
               range

         Expect.isTrue overlap "(6, 10) should overlap with (7, 9)"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               {
                  LowerBound = Some 7m
                  UpperBound = Some 15m
               }
               range

         Expect.isTrue overlap "(6, 10) should overlap with (7, 15)"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               {
                  LowerBound = Some 1m
                  UpperBound = Some 15m
               }
               range

         Expect.isTrue overlap "(6, 10) should overlap with (1, 15)"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               {
                  LowerBound = Some 1m
                  UpperBound = Some 7m
               }
               range

         Expect.isTrue overlap "(6, 10) should overlap with (1, 7)"

         let overlap =
            CommandApprovalRule.AmountPerCommandRange.hasOverlap
               {
                  LowerBound = Some 11m
                  UpperBound = Some 20m
               }
               range

         Expect.isFalse overlap "(6, 10) should not overlap with (11, 20)"
      }

      test
         "Daily Limit expected to be greater than amount based rules configured for the same command type" {
         let existingRule = {
            RuleId = Guid.NewGuid() |> CommandApprovalRuleId
            OrgId = Stub.orgId
            CommandType = ApprovableCommandType.FulfillPlatformPayment
            Criteria =
               Criteria.AmountPerCommand {
                  LowerBound = None
                  UpperBound = Some 50m
               }
            Approvers = [ Approver.AnyAdmin ]
         }

         let newRule = {
            RuleId = Guid.NewGuid() |> CommandApprovalRuleId
            OrgId = Stub.orgId
            CommandType = ApprovableCommandType.FulfillPlatformPayment
            Criteria = Criteria.AmountDailyLimit 60m
            Approvers = [ Approver.AnyAdmin ]
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
               Criteria = Criteria.AmountDailyLimit 40m
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
                  Criteria.AmountPerCommand {
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
                  Criteria.AmountPerCommand {
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
                  Criteria.AmountPerCommand {
                     LowerBound = Some 10m
                     UpperBound = Some 30m
                  }
         }

         let newRule = {
            newRule with
               Criteria = Criteria.AmountDailyLimit 20m
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
               Criteria = Criteria.AmountDailyLimit 40m
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
            RuleId = Guid.NewGuid() |> CommandApprovalRuleId
            OrgId = Stub.orgId
            CommandType = ApprovableCommandType.FulfillPlatformPayment
            Criteria = Criteria.AmountDailyLimit 60m
            Approvers = [ Approver.AnyAdmin ]
         }

         let newRule = {
            RuleId = Guid.NewGuid() |> CommandApprovalRuleId
            OrgId = Stub.orgId
            CommandType = ApprovableCommandType.FulfillPlatformPayment
            Criteria =
               Criteria.AmountPerCommand {
                  LowerBound = None
                  UpperBound = Some 50m
               }
            Approvers = [ Approver.AnyAdmin ]
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
                  Criteria.AmountPerCommand {
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
                  Criteria.AmountPerCommand {
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
                  Criteria.AmountPerCommand {
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
   ]
