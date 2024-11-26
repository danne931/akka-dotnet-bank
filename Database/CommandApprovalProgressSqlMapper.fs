module CommandApprovalProgressSqlMapper

open Lib.SharedTypes
open Bank.Employee.Domain

let table = "command_approval_progress"

module TypeCast =
   let status = "command_approval_status"

   let approvableCommandType =
      CommandApprovalRuleSqlMapper.TypeCast.approvableCommand

module Fields =
   let commandId = "command_id"
   let ruleId = CommandApprovalRuleSqlMapper.Fields.ruleId
   let orgId = OrganizationSqlMapper.OrgFields.orgId
   let employeeId = EmployeeSqlMapper.EmployeeFields.employeeId
   let status = "status"
   let approvedBy = "approved_by"

   let approvableCommandType =
      CommandApprovalRuleSqlMapper.Fields.approvableCommandType

   let commandToInitiateOnApproval = "command_to_initiate_on_approval"

module Reader =
   let commandId (read: RowReader) =
      Fields.commandId
      |> read.uuid
      |> CorrelationId
      |> CommandApprovalProgressId

   let ruleId = CommandApprovalRuleSqlMapper.Reader.ruleId

   let orgId = OrganizationSqlMapper.OrgSqlReader.orgId

   let employeeId = EmployeeSqlMapper.EmployeeSqlReader.employeeId

   let status (read: RowReader) : CommandApprovalProgress.Status =
      Fields.status
      |> read.string
      |> CommandApprovalProgress.Status.fromStringUnsafe

   let approvedBy (read: RowReader) : EmployeeId list =
      Fields.approvedBy |> read.uuidArray |> Array.toList |> List.map EmployeeId

   let commandToInitiateOnApproval (read: RowReader) : ApprovableCommand =
      Fields.commandToInitiateOnApproval
      |> read.text
      |> Serialization.deserializeUnsafe<ApprovableCommand>

module Writer =
   let commandId (id: CommandApprovalProgressId) = Sql.uuid id.Value
   let ruleId = CommandApprovalRuleSqlMapper.Writer.ruleId
   let orgId = OrganizationSqlMapper.OrgSqlWriter.orgId
   let employeeId = EmployeeSqlMapper.EmployeeSqlWriter.employeeId

   let status (cmd: CommandApprovalProgress.Status) =
      cmd |> string |> Sql.string

   let approvedBy (ids: EmployeeId list) =
      ids |> List.map EmployeeId.get |> Array.ofList |> Sql.uuidArray

   let approvableCommandType =
      CommandApprovalRuleSqlMapper.Writer.approvableCommandType

   let commandToInitiateOnApproval (cmd: ApprovableCommand) =
      cmd |> Serialization.serialize |> Sql.jsonb
