module Bank.CommandApproval.Api

open Akkling
open FsToolkit.ErrorHandling
open System.Threading.Tasks

open Lib.Postgres
open Lib.SharedTypes
open Bank.Employee.Domain

open CommandApprovalRuleSqlMapper

type private Approver = {
   Name: string
   EmployeeId: System.Guid
}

let getApprovalRules (orgId: OrgId) =
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
      WHERE {table}.{Fields.orgId} = @orgId
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
               Name = o.Name
               EmployeeId = EmployeeId o.EmployeeId
            })
      })

let approvalRuleExistsForCommandType
   (orgId: OrgId)
   (commandType: ApprovableCommandType)
   : Task<Result<CommandApprovalRuleId option, Err>>
   =
   let query =
      $"""
      SELECT {Fields.ruleId} FROM {table}
      WHERE
         {table}.{Fields.orgId} = @orgId
         AND {Fields.approvableCommandType} = @commandType::{TypeCast.approvableCommand}
      """

   pgQuerySingle<CommandApprovalRuleId>
      query
      (Some [
         "orgId", Writer.orgId orgId
         "commandType", Writer.approvableCommandType commandType
      ])
      Reader.ruleId

let commandRequiresApproval
   (command: ApprovableCommand)
   (accrual: EmployeeDailyAccrual)
   : Task<Result<CommandApprovalRuleId option, Err>>
   =
   taskResult {
      let query =
         $"""
         SELECT
            {Fields.ruleId},
            {Fields.criteriaDetail},
            {CommandApprovalProgressSqlMapper.Fields.status}
         FROM {table}
         LEFT JOIN {CommandApprovalProgressSqlMapper.table} using({Fields.ruleId})
         WHERE
            {table}.{Fields.orgId} = @orgId
            AND {table}.{Fields.approvableCommandType} = @commandType::{TypeCast.approvableCommand}
         """

      let! res =
         pgQuerySingle<
            CommandApprovalRuleId *
            CommandApprovalRule.Criteria *
            CommandApprovalProgress.Status option
          >
            query
            (Some [
               "orgId", Writer.orgId command.OrgId
               "commandType", Writer.approvableCommandTypeFromCommand command
            ])
            (fun reader ->
               Reader.ruleId reader,
               Reader.criteriaDetail reader,
               CommandApprovalProgressSqlMapper.Fields.status
               |> reader.stringOrNone
               |> Option.bind CommandApprovalProgress.Status.fromString)

      return
         match res with
         | None -> None
         | Some(_, _, Some status) when
            status = CommandApprovalProgress.Status.Approved
            ->
            None
         | Some(ruleId, criteria, _) ->
            match criteria with
            | CommandApprovalRule.Criteria.PerCommand ->
               match command with
               | ApprovableCommand.InviteEmployee _ -> Some ruleId
               | ApprovableCommand.UpdateEmployeeRole _ -> Some ruleId
               | ApprovableCommand.FulfillPlatformPayment _ -> None // should not be reached
               | ApprovableCommand.InternalTransferBetweenOrgs _ -> None // should not be reached
               | ApprovableCommand.DomesticTransfer _ -> None // should not be reached
            | CommandApprovalRule.Criteria.AmountPerCommand range ->
               match range.LowerBound, range.UpperBound with
               | None, None -> None // This case should not be reached
               | Some low, None ->
                  if command.Amount >= low then Some ruleId else None
               | None, Some high ->
                  if command.Amount <= high then Some ruleId else None
               | Some low, Some high ->
                  if command.Amount >= low && command.Amount <= high then
                     Some ruleId
                  else
                     None
            | CommandApprovalRule.Criteria.AmountDailyLimit limit ->
               match command with
               | ApprovableCommand.InviteEmployee _ -> None // Should not be reached
               | ApprovableCommand.UpdateEmployeeRole _ -> None // Should not be reached
               | ApprovableCommand.FulfillPlatformPayment cmd ->
                  let overLimit =
                     cmd.Data.RequestedPayment.BaseInfo.Amount
                     + accrual.PaymentsPaid > limit

                  if overLimit then Some ruleId else None
               | ApprovableCommand.InternalTransferBetweenOrgs cmd ->
                  let overLimit =
                     cmd.Data.Amount + accrual.InternalTransferBetweenOrgs > limit

                  if overLimit then Some ruleId else None
               | ApprovableCommand.DomesticTransfer cmd ->
                  let overLimit =
                     cmd.Data.Amount + accrual.DomesticTransfer > limit

                  if overLimit then Some ruleId else None
   }

open CommandApprovalProgressSqlMapper

let private commandApprovalProgressWithRuleQuery (whereClause: string option) =
   $"""
   SELECT
      {Fields.ruleId},
      {Fields.commandId},
      ruleT.{Fields.criteriaDetail},
      progressT.{Fields.commandToInitiateOnApproval},
      progressT.{Fields.status},
      progressT.{Fields.updatedAt},
      progressT.{Fields.declinedBy},
      progressT.{Fields.requestedBy},
      MIN(e3.{EmployeeSqlMapper.EmployeeFields.firstName} || ' ' || e3.{EmployeeSqlMapper.EmployeeFields.lastName}) as declined_by_name,
      MIN(e4.{EmployeeSqlMapper.EmployeeFields.firstName} || ' ' || e4.{EmployeeSqlMapper.EmployeeFields.lastName}) as requested_by_name,
      jsonb_agg(
         DISTINCT jsonb_build_object(
            'EmployeeId', e1.{EmployeeSqlMapper.EmployeeFields.employeeId},
            'Name', e1.{EmployeeSqlMapper.EmployeeFields.firstName} || ' ' || e1.{EmployeeSqlMapper.EmployeeFields.lastName}
         )
      ) AS permitted_approvers,
      jsonb_agg(
         DISTINCT jsonb_build_object(
            'EmployeeId', e2.{EmployeeSqlMapper.EmployeeFields.employeeId},
            'Name', e2.{EmployeeSqlMapper.EmployeeFields.firstName} || ' ' || e2.{EmployeeSqlMapper.EmployeeFields.lastName}
         )
      ) FILTER (WHERE e2.{EmployeeSqlMapper.EmployeeFields.employeeId} IS NOT NULL) AS approved_by
   FROM {CommandApprovalProgressSqlMapper.table} progressT
   JOIN {CommandApprovalRuleSqlMapper.table} ruleT using({Fields.ruleId})
   JOIN LATERAL unnest(ruleT.{Fields.permittedApprovers}) AS permitted_approver_id ON TRUE
   JOIN {EmployeeSqlMapper.table} e1 ON e1.{EmployeeSqlMapper.EmployeeFields.employeeId} = permitted_approver_id
   LEFT JOIN LATERAL unnest(progressT.{Fields.approvedBy}) AS approver_id ON TRUE
   LEFT JOIN {EmployeeSqlMapper.table} e2 ON e2.{EmployeeSqlMapper.EmployeeFields.employeeId} = approver_id
   LEFT JOIN {EmployeeSqlMapper.table} e3 ON e3.{EmployeeSqlMapper.EmployeeFields.employeeId} = {Fields.declinedBy}
   JOIN {EmployeeSqlMapper.table} e4 ON e4.{EmployeeSqlMapper.EmployeeFields.employeeId} = progressT.{Fields.requestedBy}

   WHERE {whereClause |> Option.defaultValue ""}
   GROUP BY
      {Fields.ruleId},
      {Fields.commandId},
      ruleT.{Fields.criteriaDetail},
      progressT.{Fields.commandToInitiateOnApproval}
   ORDER BY
      CASE progressT.{Fields.status}
         WHEN '{string CommandApprovalProgress.Status.Pending}' THEN 1
         WHEN '{string CommandApprovalProgress.Status.Declined}' THEN 2
         WHEN '{string CommandApprovalProgress.Status.Approved}' THEN 3
      END,
      progressT.{Fields.updatedAt} DESC
   """

let private commandApprovalProgressWithRuleReader reader = {
   RuleId = Reader.ruleId reader
   CommandProgressId = Reader.commandId reader
   Command =
      CommandApprovalProgressSqlMapper.Reader.commandToInitiateOnApproval reader
   Status = CommandApprovalProgressSqlMapper.Reader.status reader
   Criteria = Reader.criteriaDetail reader

   PermittedApprovers =
      "permitted_approvers"
      |> reader.text
      |> Serialization.deserializeUnsafe<Approver list>
      |> (List.map (fun o -> {
         Name = o.Name
         EmployeeId = EmployeeId o.EmployeeId
      }))

   ApprovedBy =
      "approved_by"
      |> reader.textOrNone
      |> Option.map Serialization.deserializeUnsafe<Approver list>
      |> Option.map (
         List.map (fun o -> {
            Name = o.Name
            EmployeeId = EmployeeId o.EmployeeId
         })
      )

   DeclinedBy =
      Option.map2
         (fun employeeId declinedByName -> {
            Name = declinedByName
            EmployeeId = employeeId
         })
         (Reader.declinedBy reader)
         (reader.textOrNone "declined_by_name")

   RequestedBy = {
      Id = Reader.requestedBy reader
      Name = reader.text "requested_by_name"
   }

   LastUpdate = Reader.updatedAt reader
}

/// Get command approval progress info.
/// Returns who has approved the command so far & the list of permitted approvers
/// for a rule as well as the rule criteria.
let getCommandApprovalProgressWithRule
   (progressId: CommandApprovalProgressId)
   : Result<CommandApprovalProgressWithRule option, Err> Task
   =
   let query =
      $"progressT.{Fields.commandId} = @progressId"
      |> Some
      |> commandApprovalProgressWithRuleQuery

   let queryParams = Some [ "progressId", Writer.commandId progressId ]

   pgQuerySingle<CommandApprovalProgressWithRule>
      query
      queryParams
      commandApprovalProgressWithRuleReader

/// Get all command approval progress infos for an org
/// Will retrieve:
///   1. All pending no matter the date
///   2. All declined/completed in the last 15 days
let getCommandApprovals
   (orgId: OrgId)
   : Result<CommandApprovalProgressWithRule list option, Err> Task
   =
   let query =
      commandApprovalProgressWithRuleQuery (
         Some
            $"""
            progressT.{Fields.orgId} = @orgId
            AND (
               progressT.{Fields.status} = @pendingStatus::{TypeCast.status}
               OR progressT.{Fields.updatedAt} > CURRENT_TIMESTAMP - interval '15 days'
            )
            """
      )

   pgQuery<CommandApprovalProgressWithRule>
      query
      (Some [
         "orgId", Writer.orgId orgId
         "pendingStatus", Writer.status CommandApprovalProgress.Status.Pending
      ])
      commandApprovalProgressWithRuleReader
