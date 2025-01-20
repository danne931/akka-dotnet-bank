module Bank.CommandApproval.Api

open Akkling
open FsToolkit.ErrorHandling
open System.Threading.Tasks

open Lib.Postgres
open Lib.SharedTypes
open Bank.Org.Domain
open Bank.Employee.Domain

open CommandApprovalRuleSqlMapper

type private Approver = {
   Name: string
   EmployeeId: System.Guid
}

let getApprovalRules
   (orgId: OrgId)
   : Task<Result<CommandApprovalRule.T list option, Err>>
   =
   let query =
      $"""
      SELECT
         {Fields.ruleId},
         {Fields.approvableCommandType},
         {Fields.criteriaDetail},
         jsonb_agg(
            DISTINCT jsonb_build_object(
               'EmployeeId', e.{EmployeeSqlMapper.EmployeeFields.employeeId},
               'Name', e.{EmployeeSqlMapper.EmployeeFields.firstName} || ' ' || e.{EmployeeSqlMapper.EmployeeFields.lastName}
            )
         ) AS permitted_approvers
      FROM {table}
      JOIN LATERAL unnest({Fields.permittedApprovers}) AS permitted_approver_id ON TRUE
      JOIN {EmployeeSqlMapper.table} e ON e.{EmployeeSqlMapper.EmployeeFields.employeeId} = permitted_approver_id
      WHERE
         {table}.{Fields.orgId} = @orgId
         AND {table}.{Fields.deletedAt} IS NULL
      GROUP BY {Fields.ruleId}
      """

   pgQuery<CommandApprovalRule.T>
      query
      (Some [ "orgId", Writer.orgId orgId ])
      (fun reader -> {
         OrgId = orgId
         RuleId = Reader.ruleId reader
         CommandType = Reader.approvableCommandType reader
         Criteria = Reader.criteriaDetail reader
         Approvers =
            "permitted_approvers"
            |> reader.text
            |> Serialization.deserializeUnsafe<Approver list>
            |> List.map (fun o -> {
               EmployeeName = o.Name
               EmployeeId = EmployeeId o.EmployeeId
            })
      })

open CommandApprovalProgressSqlMapper

let private commandApprovalProgressQuery (whereClause: string option) =
   $"""
   SELECT
      progressT.{Fields.orgId},
      progressT.{Fields.ruleId},
      progressT.{Fields.commandId},
      progressT.{Fields.commandToInitiateOnApproval},
      progressT.{Fields.status},
      progressT.{Fields.statusDetail},
      progressT.{Fields.createdAt},
      progressT.{Fields.updatedAt},
      progressT.{Fields.declinedBy},
      progressT.{Fields.requestedBy},
      MIN(e2.{EmployeeSqlMapper.EmployeeFields.firstName} || ' ' || e2.{EmployeeSqlMapper.EmployeeFields.lastName}) as declined_by_name,
      MIN(e3.{EmployeeSqlMapper.EmployeeFields.firstName} || ' ' || e3.{EmployeeSqlMapper.EmployeeFields.lastName}) as requested_by_name,
      jsonb_agg(
         DISTINCT jsonb_build_object(
            'EmployeeId', e1.{EmployeeSqlMapper.EmployeeFields.employeeId},
            'Name', e1.{EmployeeSqlMapper.EmployeeFields.firstName} || ' ' || e1.{EmployeeSqlMapper.EmployeeFields.lastName}
         )
      ) FILTER (WHERE e1.{EmployeeSqlMapper.EmployeeFields.employeeId} IS NOT NULL) AS approved_by
   FROM {CommandApprovalProgressSqlMapper.table} progressT
   LEFT JOIN LATERAL unnest(progressT.{Fields.approvedBy}) AS approver_id ON TRUE
   LEFT JOIN {EmployeeSqlMapper.table} e1 ON e1.{EmployeeSqlMapper.EmployeeFields.employeeId} = approver_id
   LEFT JOIN {EmployeeSqlMapper.table} e2 ON e2.{EmployeeSqlMapper.EmployeeFields.employeeId} = {Fields.declinedBy}
   JOIN {EmployeeSqlMapper.table} e3 ON e3.{EmployeeSqlMapper.EmployeeFields.employeeId} = {Fields.requestedBy}
   WHERE {whereClause |> Option.defaultValue ""}
   GROUP BY
      {Fields.ruleId},
      {Fields.commandId},
      {Fields.commandToInitiateOnApproval}
   ORDER BY
      CASE progressT.{Fields.status}
         WHEN '{string CommandApprovalProgress.Status.Pending}' THEN 1
         WHEN '{string CommandApprovalProgress.Status.Declined}' THEN 2
         WHEN '{string CommandApprovalProgress.Status.Approved}' THEN 3
      END,
      progressT.{Fields.updatedAt} DESC
   """

let private commandApprovalProgressReader reader : CommandApprovalProgress.T = {
   RuleId = Reader.ruleId reader
   ProgressId = Reader.commandId reader
   OrgId = Reader.orgId reader
   CommandToInitiateOnApproval =
      CommandApprovalProgressSqlMapper.Reader.commandToInitiateOnApproval reader
   Status = CommandApprovalProgressSqlMapper.Reader.statusDetail reader

   ApprovedBy =
      "approved_by"
      |> reader.textOrNone
      |> Option.map Serialization.deserializeUnsafe<Approver list>
      |> Option.map (
         List.map (fun o -> {
            EmployeeName = o.Name
            EmployeeId = EmployeeId o.EmployeeId
         })
      )
      |> Option.defaultValue []

   DeclinedBy =
      Option.map2
         (fun employeeId declinedByName -> {
            EmployeeName = declinedByName
            EmployeeId = employeeId
         })
         (Reader.declinedBy reader)
         (reader.textOrNone "declined_by_name")

   RequestedBy = {
      EmployeeId = Reader.requestedBy reader
      EmployeeName = reader.text "requested_by_name"
   }

   CreatedAt = Reader.createdAt reader
   LastUpdate = Reader.updatedAt reader
}

/// Get all command approval progress infos for an org
/// Will retrieve:
///   1. All pending no matter the date
///   2. All declined/completed in the last 15 days
let getCommandApprovals
   (orgId: OrgId)
   : Result<CommandApprovalProgress.T list option, Err> Task
   =
   let query =
      commandApprovalProgressQuery (
         Some
            $"""
            progressT.{Fields.orgId} = @orgId
            AND (
               progressT.{Fields.status} = @pendingStatus::{TypeCast.status}
               OR progressT.{Fields.updatedAt} > CURRENT_TIMESTAMP - interval '15 days'
            )
            """
      )

   pgQuery<CommandApprovalProgress.T>
      query
      (Some [
         "orgId", Writer.orgId orgId
         "pendingStatus", Writer.status CommandApprovalProgress.Status.Pending
      ])
      commandApprovalProgressReader
