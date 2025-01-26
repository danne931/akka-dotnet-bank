module CommandApprovalTests

open System

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Bank.Org.Domain
open Bank.Employee.Domain
open CommandApprovalRule
open CommandApprovalProgress
open Lib.SharedTypes

module Stub = OrganizationStub

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
            RuleId = Stub.ruleId ()
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
            RuleId = Stub.ruleId ()
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
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = ApprovableCommandType.FulfillPlatformPayment
            Criteria = Criteria.AmountDailyLimit 60m
            Approvers = [ Approver.AnyAdmin ]
         }

         let newRule = {
            RuleId = Stub.ruleId ()
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

      test "isValidApprover should check the approver is configured in the rule" {
         let approver: EmployeeReference = {
            EmployeeName = "A"
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }

         let initiatedBy = InitiatedById approver.EmployeeId

         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = ApprovableCommandType.InviteEmployee
            Criteria = Criteria.PerCommand
            Approvers = [
               Approver.Admin approver

               Approver.Admin {
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
                  Approver.Admin {
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
               Approvers = [ Approver.AnyAdmin; Approver.AnyAdmin ]
         }

         Expect.isTrue
            (isValidApprover initiatedBy rule)
            "should be true when approver not found in rule but AnyAdmin configured"
      }

      test
         "newRuleCommandTypeConflictsWithExistingRule should check for
            duplicate rules by (ApprovableCommandType, Criteria.PerCommand)" {
         let cmdTypes = [
            ApprovableCommandType.InviteEmployee
            ApprovableCommandType.UpdateEmployeeRole
         ]

         for cmdType in cmdTypes do
            let ruleA = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria = Criteria.PerCommand
               Approvers = [ Approver.AnyAdmin ]
            }

            let ruleB = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria = Criteria.PerCommand
               Approvers = [
                  Approver.Admin {
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
                     if cmdType = ApprovableCommandType.InviteEmployee then
                        ApprovableCommandType.UpdateEmployeeRole
                     else
                        ApprovableCommandType.InviteEmployee
            }

            Expect.isFalse
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleB)
               "should not conflict if configure a rule of a command type other
               than {cmdType}"

            Expect.isFalse
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleA)
               "should not conflict if editing existing rule"
      }

      test
         "newRuleCommandTypeConflictsWithExistingRule should check for
            duplicate rules by (ApprovableCommandType, Criteria.AmountDailyLimit)" {
         let cmdTypes = [
            ApprovableCommandType.InternalTransferBetweenOrgs
            ApprovableCommandType.DomesticTransfer
            ApprovableCommandType.FulfillPlatformPayment
         ]

         for cmdType in cmdTypes do
            let ruleA = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria = Criteria.AmountDailyLimit 15000m
               Approvers = [ Approver.AnyAdmin ]
            }

            let ruleB = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria = Criteria.AmountDailyLimit 25000m
               Approvers = [
                  Approver.Admin {
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
                     | ApprovableCommandType.InternalTransferBetweenOrgs ->
                        ApprovableCommandType.DomesticTransfer
                     | ApprovableCommandType.DomesticTransfer ->
                        ApprovableCommandType.FulfillPlatformPayment
                     | _ -> ApprovableCommandType.InternalTransferBetweenOrgs
            }

            Expect.isFalse
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleB)
               "should not conflict if configure a rule of a command type other
               than {cmdType}"

            Expect.isFalse
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleA)
               "should not conflict if editing existing rule"
      }

      test
         "newRuleCommandTypeConflictsWithExistingRule should disregard
            duplicate rules by (ApprovableCommandType, Criteria.AmountPerCommand)" {
         let cmdTypes = [
            ApprovableCommandType.InternalTransferBetweenOrgs
            ApprovableCommandType.DomesticTransfer
            ApprovableCommandType.FulfillPlatformPayment
         ]

         for cmdType in cmdTypes do
            let ruleA = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria =
                  Criteria.AmountPerCommand {
                     LowerBound = Some 2000m
                     UpperBound = Some 5000m
                  }
               Approvers = [ Approver.AnyAdmin ]
            }

            let ruleB = {
               RuleId = Stub.ruleId ()
               OrgId = Stub.orgId
               CommandType = cmdType
               Criteria =
                  Criteria.AmountPerCommand {
                     LowerBound = Some 6000m
                     UpperBound = None
                  }
               Approvers = [
                  Approver.Admin {
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
                     | ApprovableCommandType.InternalTransferBetweenOrgs ->
                        ApprovableCommandType.DomesticTransfer
                     | ApprovableCommandType.DomesticTransfer ->
                        ApprovableCommandType.FulfillPlatformPayment
                     | _ -> ApprovableCommandType.InternalTransferBetweenOrgs
            }

            Expect.isFalse
               (newRuleCommandTypeConflictsWithExistingRule [ ruleA ] ruleB)
               "should not conflict if configure a rule of a command type other
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
            CommandType = ApprovableCommandType.InviteEmployee
            Criteria = Criteria.PerCommand
            Approvers = [
               Approver.Admin approver

               Approver.Admin {
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
               Approvers = [ Approver.Admin approver ]
         }

         Expect.isFalse
            (isRequesterOneOfManyApprovers initiatedBy rule)
            "should be false when approver only 1 configured"

         let rule = {
            rule with
               Approvers = [ Approver.AnyAdmin; Approver.AnyAdmin ]
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
            CommandType = ApprovableCommandType.InviteEmployee
            Criteria = Criteria.PerCommand
            Approvers = [ Approver.Admin approver; Approver.AnyAdmin ]
         }

         Expect.isFalse
            (isRequesterTheOnlyConfiguredApprover initiatedBy rule)
            "should be false when Admin one of many"

         let rule = {
            rule with
               Approvers = [ Approver.Admin approver ]
         }

         Expect.isTrue
            (isRequesterTheOnlyConfiguredApprover initiatedBy rule)
            "should be true when approver only 1 configured"

         let rule = {
            rule with
               Approvers = [ Approver.AnyAdmin ]
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
            Stub.progress with
               ApprovedBy = [ approver ]
         }

         Expect.isFalse
            (isNewApproval approver.EmployeeId progress)
            "should detect a duplicate approval"

         let progress = { Stub.progress with ApprovedBy = [] }

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
            CommandType = ApprovableCommandType.InviteEmployee
            Criteria = Criteria.PerCommand
            Approvers = [ Approver.Admin approver ]
         }

         let progress = {
            Stub.progress with
               RuleId = rule.RuleId
               ApprovedBy = [ approver ]
         }

         Expect.isFalse
            (canManageProgress rule progress approver.EmployeeId)
            "should not be allowed to approve the rule since they already have"

         let rule2 = {
            rule with
               Approvers = [ Approver.AnyAdmin ]
         }

         Expect.isFalse
            (canManageProgress rule2 progress approver.EmployeeId)
            "should not be allowed to approve the rule since already have2"

         let progress = {
            Stub.progress with
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
            CommandType = ApprovableCommandType.InviteEmployee
            Criteria = Criteria.PerCommand
            Approvers = [ Approver.Admin approver; Approver.Admin approverB ]
         }

         let progress = {
            Stub.progress with
               RuleId = rule.RuleId
               ApprovedBy = []
         }

         Expect.equal
            (remainingApprovalRequiredBy rule progress)
            rule.Approvers
            "should be 2 equivalent to the 2 approvers configured for the rule"

         let progress = {
            Stub.progress with
               RuleId = rule.RuleId
               ApprovedBy = [ approverB ]
         }

         Expect.equal
            (remainingApprovalRequiredBy rule progress)
            [ Approver.Admin approver ]
            "should be 1 equivalent to one of the 2 approvers configured for the rule"

         let progress = {
            Stub.progress with
               RuleId = rule.RuleId
               ApprovedBy = [ approverB; approver ]
         }

         Expect.hasLength
            (remainingApprovalRequiredBy rule progress)
            0
            "should be zero approvals required when all configured approvers approved"

         let rule = {
            rule with
               Approvers = [
                  Approver.AnyAdmin
                  Approver.Admin approver
                  Approver.Admin approverB
                  Approver.AnyAdmin
               ]
         }

         Expect.equal
            (remainingApprovalRequiredBy rule progress)
            [ Approver.AnyAdmin; Approver.AnyAdmin ]
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
            CommandType = ApprovableCommandType.UpdateEmployeeRole
            Criteria = Criteria.PerCommand
            Approvers = [ Approver.Admin approverA ]
         }

         let rule2 = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = ApprovableCommandType.InviteEmployee
            Criteria = Criteria.PerCommand
            Approvers = [
               Approver.AnyAdmin
               Approver.AnyAdmin
               Approver.AnyAdmin
            ]
         }

         let progress1 = {
            Stub.progress with
               RuleId = rule1.RuleId
               ApprovedBy = [ approverA ]
         }

         let progress2 = {
            Stub.progress with
               ProgressId = Stub.progressId ()
               RuleId = rule1.RuleId
               ApprovedBy = []
         }

         let progress3 = {
            Stub.progress with
               ProgressId = Stub.progressId ()
               RuleId = rule2.RuleId
               ApprovedBy = [ approverB ]
         }

         let progress4 = {
            Stub.progress with
               ProgressId = Stub.progressId ()
               RuleId = rule2.RuleId
               ApprovedBy = [ approverA ]
         }

         let progress5 = {
            Stub.progress with
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
            CommandType = ApprovableCommandType.UpdateEmployeeRole
            Criteria = Criteria.PerCommand
            Approvers = [ Approver.AnyAdmin; Approver.AnyAdmin ]
         }

         let rule2 = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = ApprovableCommandType.InviteEmployee
            Criteria = Criteria.PerCommand
            Approvers = [ Approver.AnyAdmin; Approver.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule; rule2.RuleId, rule2 ]
         let progress = Map.empty

         let ruleRequiringCommandApproval =
            Expect.wantSome
               (commandRequiresApproval
                  Stub.updateRoleCommand
                  rules
                  progress
                  Stub.accrual)
               "command should require approval for UpdateEmployeeRole rule"

         Expect.equal ruleRequiringCommandApproval rule ""

         let rule = {
            rule with
               Approvers = [ Approver.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule; rule2.RuleId, rule2 ]

         Expect.isNone
            (commandRequiresApproval
               Stub.updateRoleCommand
               rules
               progress
               Stub.accrual)
            "command does not require approval if requester is only
            configured approver"

         let rules = Map [ rule2.RuleId, rule2 ]

         Expect.isNone
            (commandRequiresApproval
               Stub.updateRoleCommand
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
            CommandType = ApprovableCommandType.DomesticTransfer
            Criteria = Criteria.AmountDailyLimit 10_000m
            Approvers = [ Approver.AnyAdmin; Approver.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule ]
         let progress = Map.empty

         let accrual = {
            Stub.accrual with
               DomesticTransfer = 5_000m
         }

         let cmd =
            AccountStub.command.domesticTransfer 2_000m
            |> ApprovableCommand.DomesticTransfer

         Expect.isNone
            (commandRequiresApproval cmd rules progress accrual)
            "command does not require approval if txn amount + daily amount accrued under limit"

         let cmd =
            AccountStub.command.domesticTransfer 5_000m
            |> ApprovableCommand.DomesticTransfer

         Expect.isNone
            (commandRequiresApproval cmd rules progress accrual)
            "command does not require approval if txn amount + daily amount accrued = limit"

         let cmd =
            AccountStub.command.domesticTransfer 5_001m
            |> ApprovableCommand.DomesticTransfer

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
            CommandType = ApprovableCommandType.DomesticTransfer
            Criteria =
               Criteria.AmountPerCommand {
                  LowerBound = Some 3000m
                  UpperBound = None
               }
            Approvers = [ Approver.AnyAdmin; Approver.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule ]
         let progress = Map.empty

         let cmd =
            AccountStub.command.domesticTransfer 2_000m
            |> ApprovableCommand.DomesticTransfer

         Expect.isNone
            (commandRequiresApproval cmd rules progress Stub.accrual)
            "command does not require approval if txn amount does not meet
            AmountPerCommand criteria"

         let cmd =
            AccountStub.command.domesticTransfer 4_000m
            |> ApprovableCommand.DomesticTransfer

         Expect.isSome
            (commandRequiresApproval cmd rules progress Stub.accrual)
            "command requires approval if txn amount > LowerBound"
      }

      test
         "commandRequiresApproval detects if an incoming command meets
            multiple rule criteria, and returns the rule with
            Criteria.AmountDailyLimit rather than
            Criteria.AmountPerCommand { LowerBound = Some; UpperBound = None }
            if both rules exist" {
         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = ApprovableCommandType.DomesticTransfer
            Criteria =
               Criteria.AmountPerCommand {
                  LowerBound = Some 3000m
                  UpperBound = None
               }
            Approvers = [ Approver.AnyAdmin; Approver.AnyAdmin ]
         }

         let rule2 = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = ApprovableCommandType.DomesticTransfer
            Criteria = Criteria.AmountDailyLimit 10_000m
            Approvers = [ Approver.AnyAdmin; Approver.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule; rule2.RuleId, rule2 ]
         let progress = Map.empty

         let cmd =
            AccountStub.command.domesticTransfer 11_000m
            |> ApprovableCommand.DomesticTransfer

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
            |> ApprovableCommand.DomesticTransfer

         let ruleCorrespondingToRequiredApproval =
            Expect.wantSome
               (commandRequiresApproval cmd rules progress Stub.accrual)
               "command requires approval, meeting criteria for 1 rules"

         Expect.equal ruleCorrespondingToRequiredApproval rule ""
      }

      test
         "commandRequiresApproval detects if an incoming command's associated
            progress record is already completed" {
         let cmd =
            AccountStub.command.domesticTransfer 4_000m
            |> ApprovableCommand.DomesticTransfer

         let rule = {
            RuleId = Stub.ruleId ()
            OrgId = Stub.orgId
            CommandType = ApprovableCommandType.DomesticTransfer
            Criteria =
               Criteria.AmountPerCommand {
                  LowerBound = Some 3000m
                  UpperBound = None
               }
            Approvers = [ Approver.AnyAdmin; Approver.AnyAdmin ]
         }

         let rules = Map [ rule.RuleId, rule ]

         let p = {
            Stub.progress with
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
   ]
