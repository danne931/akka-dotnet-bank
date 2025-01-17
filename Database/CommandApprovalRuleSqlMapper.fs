module CommandApprovalRuleSqlMapper

open Lib.SharedTypes
open Bank.Org.Domain
open OrganizationSqlMapper
open CommandApprovalRule

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

   let dailyLimit = "daily_limit"
   let amountPerCommandLowerBound = "lower_bound"
   let amountPerCommandUpperBound = "upper_bound"

module Reader =
   let ruleId (read: RowReader) =
      Fields.ruleId |> read.uuid |> CommandApprovalRuleId

   let orgId = OrgSqlReader.orgId

   let criteriaDetail (read: RowReader) : Criteria =
      Fields.criteriaDetail
      |> read.text
      |> Serialization.deserializeUnsafe<Criteria>

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

   let approvableCommandTypeFromCommand =
      (function
      | ApprovableCommand.InviteEmployee _ ->
         ApprovableCommandType.InviteEmployee
      | ApprovableCommand.UpdateEmployeeRole _ ->
         ApprovableCommandType.UpdateEmployeeRole
      | ApprovableCommand.FulfillPlatformPayment _ ->
         ApprovableCommandType.FulfillPlatformPayment
      | ApprovableCommand.InternalTransferBetweenOrgs _ ->
         ApprovableCommandType.InternalTransferBetweenOrgs
      | ApprovableCommand.DomesticTransfer _ ->
         ApprovableCommandType.DomesticTransfer)
      >> string
      >> Sql.string

   let criteria (criteria: Criteria) = criteria |> string |> Sql.string

   let criteriaDetail (criteria: Criteria) =
      criteria |> Serialization.serialize |> Sql.jsonb

   let permittedApprovers (ids: Approver list) =
      ids
      |> List.map (_.EmployeeId >> EmployeeId.get)
      |> Array.ofList
      |> Sql.uuidArray

   let dailyLimit = Sql.decimal
   let amountPerCommandLowerBound = Sql.decimalOrNone
   let amountPerCommandUpperBound = Sql.decimalOrNone
