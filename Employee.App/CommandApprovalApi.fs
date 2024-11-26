module Bank.CommandApproval.Api

open Akkling
open FsToolkit.ErrorHandling
open System.Threading.Tasks

open Lib.Postgres
open Lib.SharedTypes
open Bank.Employee.Domain

open CommandApprovalRuleSqlMapper

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

type private Approver = {
   Name: string
   EmployeeId: System.Guid
}

/// Get command approval progress info for a particular employee / command type.
/// Returns who has approved the command so far & the list of permitted approvers
/// for a rule as well as the rule crtieria.
let getCommandApprovalProgressWithRule
   (employeeId: EmployeeId)
   (commandType: ApprovableCommandType)
   : Result<CommandApprovalProgressWithRule option, Err> Task
   =
   let query =
      $"""
      SELECT
         {Fields.ruleId},
         {Fields.commandId},
         ruleT.{CommandApprovalRuleSqlMapper.Fields.criteriaDetail},
         jsonb_agg(
            jsonb_build_object(
               'EmployeeId', e1.{Fields.employeeId},
               'Name', e1.{EmployeeSqlMapper.EmployeeFields.firstName} || ' ' || e1.{EmployeeSqlMapper.EmployeeFields.lastName}
            )
         ) AS permitted_approvers,
         jsonb_agg(
            jsonb_build_object(
               'EmployeeId', e2.{Fields.employeeId},
               'Name', e2.{EmployeeSqlMapper.EmployeeFields.firstName} || ' ' || e1.{EmployeeSqlMapper.EmployeeFields.lastName}
            )
         ) FILTER (WHERE e2.{Fields.employeeId} IS NOT NULL) AS approved_by
      FROM {CommandApprovalProgressSqlMapper.table} progressT
      JOIN {CommandApprovalRuleSqlMapper.table} ruleT using({Fields.ruleId})
      JOIN LATERAL unnest(ruleT.{Fields.permittedApprovers}) AS permitted_approver_id ON TRUE
      JOIN {EmployeeSqlMapper.table} e1 ON e1.{Fields.employeeId} = permitted_approver_id
      LEFT JOIN LATERAL unnest(progressT.{Fields.approvedBy}) AS approver_id ON TRUE
      LEFT JOIN {EmployeeSqlMapper.table} e2 ON e2.{Fields.employeeId} = approver_id
      WHERE
         progressT.{Fields.employeeId} = @employeeId
         AND progressT.{Fields.approvableCommandType} = @commandType::{TypeCast.approvableCommand}
         AND progressT.{Fields.status} = @status::{TypeCast.status}
      GROUP BY {Fields.ruleId}, {Fields.commandId}, ruleT.{CommandApprovalRuleSqlMapper.Fields.criteriaDetail}
      """

   pgQuerySingle<CommandApprovalProgressWithRule>
      query
      (Some [
         "employeeId", Writer.employeeId employeeId
         "commandType", Writer.approvableCommandType commandType
         "status", Writer.status CommandApprovalProgress.Status.Pending
      ])
      (fun reader -> {
         RuleId = Reader.ruleId reader
         CommandProgressId = Reader.commandId reader
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
      })
