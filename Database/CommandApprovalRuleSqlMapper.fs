module CommandApprovalRuleSqlMapper

open System

open Lib.SharedTypes
open OrganizationSqlMapper
open CommandApproval

let table = "command_approval_rule"
let dailyLimitTable = "command_approval_rule_amount_daily_limit"
let amountPerCommandTable = "command_approval_rule_amount_per_command"

module TypeCast =
   let approvableCommand = "approvable_command"
   let approvalCriteria = "command_approval_criteria"

module Fields =
   let ruleId = "rule_id"
   let orgId = OrgFields.orgId
   let approvableCommandType = "command_type"
   let criteria = "criteria"
   let criteriaDetail = "criteria_detail"
   let permittedApprovers = "permitted_approvers"
   let deletedAt = "deleted_at"

   let dailyLimit = "daily_limit"
   let amountPerCommandLowerBound = "lower_bound"
   let amountPerCommandUpperBound = "upper_bound"

module Reader =
   let ruleId (read: RowReader) =
      Fields.ruleId |> read.uuid |> CommandApprovalRuleId

   let orgId = OrgSqlReader.orgId

   let criteriaDetail (read: RowReader) : ApprovalCriteria =
      Fields.criteriaDetail
      |> read.text
      |> Serialization.deserializeUnsafe<ApprovalCriteria>

   let approvableCommandType (read: RowReader) : ApprovableCommandType =
      Fields.approvableCommandType
      |> read.string
      |> ApprovableCommandType.fromStringUnsafe

   let permittedApprovers (read: RowReader) : EmployeeId list =
      Fields.permittedApprovers
      |> read.uuidArray
      |> Array.toList
      |> List.map EmployeeId

   let dailyLimit (read: RowReader) = read.decimal Fields.dailyLimit

   let amountPerCommandLowerBound (read: RowReader) =
      read.decimalOrNone Fields.amountPerCommandLowerBound

   let amountPerCommandUpperBound (read: RowReader) =
      read.decimalOrNone Fields.amountPerCommandUpperBound

module Writer =
   let ruleId (id: CommandApprovalRuleId) = Sql.uuid id.Value

   let orgId = OrgSqlWriter.orgId

   let approvableCommandType (t: ApprovableCommandType) = Sql.string (string t)

   let approvableCommandTypeFromCommand (c: ApprovableCommand) =
      c.CommandType |> string |> Sql.string

   let criteria (criteria: ApprovalCriteria) = criteria |> string |> Sql.string

   let criteriaDetail (criteria: ApprovalCriteria) =
      criteria |> Serialization.serialize |> Sql.jsonb

   let permittedApprovers (approvers: CommandApprover list) =
      approvers
      |> List.map (function
         // NOTE: SYSTEM_USER_ID used in the DB to indicate "AnyAdmin".
         | CommandApprover.AnyAdmin -> InitiatedById.get Initiator.System.Id
         | CommandApprover.Admin a -> EmployeeId.get a.EmployeeId)
      |> Array.ofList
      |> Sql.uuidArray

   let dailyLimit = Sql.decimal
   let amountPerCommandLowerBound = Sql.decimalOrNone
   let amountPerCommandUpperBound = Sql.decimalOrNone

   let deletedAt (date: DateTime option) = Sql.timestamptzOrNone date
