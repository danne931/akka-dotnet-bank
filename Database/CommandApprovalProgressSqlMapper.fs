module CommandApprovalProgressSqlMapper

open Lib.SharedTypes
open Bank.Org.Domain

let table = "command_approval_progress"

module TypeCast =
   let status = "command_approval_status"

   let approvableCommandType =
      CommandApprovalRuleSqlMapper.TypeCast.approvableCommand

module Fields =
   let commandId = "command_id"
   let ruleId = CommandApprovalRuleSqlMapper.Fields.ruleId
   let orgId = OrganizationSqlMapper.OrgFields.orgId
   let requestedBy = "requested_by_id"
   let status = "status"
   let approvedBy = "approved_by"
   let declinedBy = "declined_by"

   let approvableCommandType =
      CommandApprovalRuleSqlMapper.Fields.approvableCommandType

   let commandToInitiateOnApproval = "command_to_initiate_on_approval"
   let updatedAt = "updated_at"

module Reader =
   let commandId (read: RowReader) =
      Fields.commandId
      |> read.uuid
      |> CorrelationId
      |> CommandApprovalProgressId

   let ruleId = CommandApprovalRuleSqlMapper.Reader.ruleId

   let orgId = OrganizationSqlMapper.OrgSqlReader.orgId

   let requestedBy (read: RowReader) =
      Fields.requestedBy |> read.uuid |> EmployeeId |> InitiatedById

   let status (read: RowReader) : CommandApprovalProgress.Status =
      Fields.status
      |> read.string
      |> CommandApprovalProgress.Status.fromStringUnsafe

   let approvedBy (read: RowReader) : EmployeeId list =
      Fields.approvedBy |> read.uuidArray |> Array.toList |> List.map EmployeeId

   let declinedBy (read: RowReader) : EmployeeId option =
      Fields.declinedBy |> read.uuidOrNone |> Option.map EmployeeId

   let commandToInitiateOnApproval (read: RowReader) : ApprovableCommand =
      Fields.commandToInitiateOnApproval
      |> read.text
      |> Serialization.deserializeUnsafe<ApprovableCommand>

   let updatedAt (read: RowReader) = read.dateTime Fields.updatedAt

module Writer =
   let commandId (id: CommandApprovalProgressId) = Sql.uuid id.Value
   let ruleId = CommandApprovalRuleSqlMapper.Writer.ruleId
   let orgId = OrganizationSqlMapper.OrgSqlWriter.orgId

   let requestedBy = InitiatedById.get >> Sql.uuid

   let status (cmd: CommandApprovalProgress.Status) =
      cmd |> string |> Sql.string

   let approvedBy (ids: EmployeeId list) =
      ids |> List.map EmployeeId.get |> Array.ofList |> Sql.uuidArray

   let declinedBy (ids: EmployeeId option) =
      ids |> Option.map EmployeeId.get |> Sql.uuidOrNone

   let approvableCommandType =
      CommandApprovalRuleSqlMapper.Writer.approvableCommandType

   let commandToInitiateOnApproval (cmd: ApprovableCommand) =
      cmd |> Serialization.serialize |> Sql.jsonb
