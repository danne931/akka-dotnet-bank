[<RequireQualifiedAccess>]
module OrgReadModelSyncActor

open System

open Lib.SharedTypes
open Lib.Types
open Lib.Postgres
open Bank.Org.Domain
open Lib.ReadModelSyncActor
open OrganizationSqlMapper
open OrganizationEventSqlMapper
open CommandApproval

type private SqlParams = SqlParameter list list

type SqlParamsDerivedFromOrgEvents = {
   OrgCreate: SqlParams
   OrgUpdate: SqlParams
   OrgEvent: SqlParams
   FeatureFlags: SqlParams
   CommandApprovalRuleConfigured: SqlParams
   CommandApprovalRuleDeleted: SqlParams
   CommandApprovalRuleConfiguredWithAmountDailyLimit: SqlParams
   CommandApprovalRuleConfiguredWithAmountPerCommand: SqlParams
   CommandApprovalRequested: SqlParams
   CommandApprovalAcquired: SqlParams
   CommandApprovalDeclined: SqlParams
   CommandApprovalTerminated: SqlParams
}

let sqlParamReducer
   (acc: SqlParamsDerivedFromOrgEvents)
   (evt: OrgEvent)
   : SqlParamsDerivedFromOrgEvents
   =
   let evt, envelope = OrgEnvelope.unwrap evt

   let orgEventSqlParams = [
      "eventId", OrgEventSqlWriter.eventId envelope.Id

      "orgId",
      envelope.EntityId |> OrgId.fromEntityId |> OrgEventSqlWriter.orgId

      "initiatedById",
      envelope.InitiatedBy.Id |> OrgEventSqlWriter.initiatedById

      "orgId", OrgEventSqlWriter.orgId envelope.OrgId

      "correlationId", OrgEventSqlWriter.correlationId envelope.CorrelationId

      "name", OrgEventSqlWriter.name envelope.EventName
      "timestamp", OrgEventSqlWriter.timestamp envelope.Timestamp
      "event", OrgEventSqlWriter.event evt
   ]

   let acc = {
      acc with
         OrgEvent = orgEventSqlParams :: acc.OrgEvent
   }

   match evt with
   | OrgEvent.OnboardingApplicationSubmitted e ->
      let qParams = [
         "orgId", OrgSqlWriter.orgId e.OrgId
         "parentAccountId", OrgSqlWriter.parentAccountId e.Data.ParentAccountId
         "name", OrgSqlWriter.name e.Data.BusinessDetails.BusinessName
         "status", OrgSqlWriter.status OrgStatus.PendingOnboardingTasksFulfilled
         "statusDetail",
         OrgSqlWriter.statusDetail OrgStatus.PendingOnboardingTasksFulfilled
         "adminTeamEmail", OrgSqlWriter.adminTeamEmail e.Data.AdminTeamEmail
         "employerIdentificationNumber",
         OrgSqlWriter.employerIdentificationNumber
            e.Data.BusinessDetails.EmployerIdentificationNumber
         "address", OrgSqlWriter.address e.Data.BusinessDetails.Address
         "businessType",
         OrgSqlWriter.businessType e.Data.BusinessDetails.LegalType
         "description",
         OrgSqlWriter.description e.Data.BusinessDetails.Description
         "website", OrgSqlWriter.website e.Data.BusinessDetails.Website
      ]

      {
         acc with
            OrgCreate = qParams :: acc.OrgCreate
      }
   | OrgEvent.OnboardingFinished e ->
      let qParams = [
         "orgId", OrgSqlWriter.orgId e.OrgId
         "status", OrgSqlWriter.status OrgStatus.Active
         "statusDetail", OrgSqlWriter.statusDetail OrgStatus.Active
      ]

      {
         acc with
            OrgUpdate = qParams :: acc.OrgUpdate
      }
   | OrgEvent.FeatureFlagConfigured e ->
      let features = e.Data.Config

      let qParams = [
         "orgId", OrgSqlWriter.orgId e.OrgId

         "socialTransferDiscovery",
         OrgSqlWriter.socialTransferDiscoveryAccountId
            features.SocialTransferDiscoveryPrimaryAccountId
      ]

      {
         acc with
            FeatureFlags = qParams :: acc.FeatureFlags
      }
   | OrgEvent.CommandApprovalRuleConfigured e ->
      let rule = e.Data.Rule

      let qParams = [
         "ruleId", CommandApprovalRuleSqlMapper.Writer.ruleId rule.RuleId

         "orgId", CommandApprovalRuleSqlMapper.Writer.orgId rule.OrgId

         "approvableCommandType",
         CommandApprovalRuleSqlMapper.Writer.approvableCommandType
            rule.CommandType

         "criteria", CommandApprovalRuleSqlMapper.Writer.criteria rule.Criteria

         "criteriaDetail",
         CommandApprovalRuleSqlMapper.Writer.criteriaDetail rule.Criteria

         "approvers",
         CommandApprovalRuleSqlMapper.Writer.permittedApprovers rule.Approvers
      ]

      let dailyLimitQParams =
         match rule.Criteria with
         | ApprovalCriteria.AmountDailyLimit limit ->
            Some [
               "ruleId", CommandApprovalRuleSqlMapper.Writer.ruleId rule.RuleId
               "orgId", CommandApprovalRuleSqlMapper.Writer.orgId rule.OrgId
               "dailyLimit",
               CommandApprovalRuleSqlMapper.Writer.dailyLimit limit
            ]
         | _ -> None

      let amountPerCommandQParams =
         match rule.Criteria with
         | ApprovalCriteria.AmountPerCommand range ->
            Some [
               "ruleId", CommandApprovalRuleSqlMapper.Writer.ruleId rule.RuleId
               "orgId", CommandApprovalRuleSqlMapper.Writer.orgId rule.OrgId
               "lowerBound",
               CommandApprovalRuleSqlMapper.Writer.amountPerCommandLowerBound
                  range.LowerBound
               "upperBound",
               CommandApprovalRuleSqlMapper.Writer.amountPerCommandLowerBound
                  range.UpperBound
            ]
         | _ -> None

      {
         acc with
            CommandApprovalRuleConfigured =
               qParams :: acc.CommandApprovalRuleConfigured
            CommandApprovalRuleConfiguredWithAmountDailyLimit =
               dailyLimitQParams
               |> Option.fold
                     (fun acc qParams -> qParams :: acc)
                     acc.CommandApprovalRuleConfiguredWithAmountDailyLimit
            CommandApprovalRuleConfiguredWithAmountPerCommand =
               amountPerCommandQParams
               |> Option.fold
                     (fun acc qParams -> qParams :: acc)
                     acc.CommandApprovalRuleConfiguredWithAmountPerCommand
      }
   | OrgEvent.CommandApprovalRuleDeleted e ->
      let qParams = [
         "ruleId", CommandApprovalRuleSqlMapper.Writer.ruleId e.Data.RuleId

         "deletedAt",
         CommandApprovalRuleSqlMapper.Writer.deletedAt (Some e.Timestamp)
      ]

      {
         acc with
            CommandApprovalRuleDeleted =
               qParams :: acc.CommandApprovalRuleDeleted
      }
   | OrgEvent.CommandApprovalRequested e ->
      let status = CommandApprovalProgress.Status.Pending

      let qParams = [
         "commandId",
         CommandApprovalProgressId e.CorrelationId
         |> CommandApprovalProgressSqlMapper.Writer.commandId

         "ruleId", CommandApprovalProgressSqlMapper.Writer.ruleId e.Data.RuleId

         "orgId", CommandApprovalProgressSqlMapper.Writer.orgId e.OrgId

         "requestedById",
         e.InitiatedBy.Id |> CommandApprovalProgressSqlMapper.Writer.requestedBy

         "status", CommandApprovalProgressSqlMapper.Writer.status status

         "statusDetail",
         CommandApprovalProgressSqlMapper.Writer.statusDetail status

         "approvedBy",
         CommandApprovalProgressSqlMapper.Writer.approvedBy (
            if e.Data.RequesterIsConfiguredAsAnApprover then
               [ e.Data.Requester.EmployeeId ]
            else
               []
         )

         "approvableCommandType",
         CommandApprovalRuleSqlMapper.Writer.approvableCommandType
            e.Data.Command.CommandType

         "command",
         CommandApprovalProgressSqlMapper.Writer.commandToInitiateOnApproval
            e.Data.Command
      ]

      {
         acc with
            CommandApprovalRequested = qParams :: acc.CommandApprovalRequested
      }
   | OrgEvent.CommandApprovalAcquired e ->
      let status = CommandApprovalProgress.Status.Pending

      let qParams = [
         "commandId",
         CommandApprovalProgressId e.CorrelationId
         |> CommandApprovalProgressSqlMapper.Writer.commandId

         "approvedBy", Sql.uuid e.Data.ApprovedBy.EmployeeId.Value

         "expectedCurrentStatus",
         CommandApprovalProgressSqlMapper.Writer.status status

         "status", CommandApprovalProgressSqlMapper.Writer.status status

         "statusDetail",
         CommandApprovalProgressSqlMapper.Writer.statusDetail status
      ]

      {
         acc with
            CommandApprovalAcquired = qParams :: acc.CommandApprovalAcquired
      }
   | OrgEvent.CommandApprovalProcessCompleted e ->
      let updatedStatus = CommandApprovalProgress.Status.Approved

      let qParams = [
         "commandId",
         CommandApprovalProgressId e.CorrelationId
         |> CommandApprovalProgressSqlMapper.Writer.commandId

         "approvedBy", Sql.uuid e.Data.ApprovedBy.EmployeeId.Value

         "expectedCurrentStatus",
         CommandApprovalProgress.Status.Pending
         |> CommandApprovalProgressSqlMapper.Writer.status

         "status", CommandApprovalProgressSqlMapper.Writer.status updatedStatus

         "statusDetail",
         CommandApprovalProgressSqlMapper.Writer.statusDetail updatedStatus
      ]

      {
         acc with
            CommandApprovalAcquired = qParams :: acc.CommandApprovalAcquired
      }
   | OrgEvent.CommandApprovalDeclined e ->
      let updatedStatus = CommandApprovalProgress.Status.Declined

      let qParams = [
         "commandId",
         CommandApprovalProgressId e.CorrelationId
         |> CommandApprovalProgressSqlMapper.Writer.commandId

         "expectedCurrentStatus",
         CommandApprovalProgressSqlMapper.Writer.status
            CommandApprovalProgress.Status.Pending

         "status", CommandApprovalProgressSqlMapper.Writer.status updatedStatus

         "statusDetail",
         CommandApprovalProgressSqlMapper.Writer.statusDetail updatedStatus

         "declinedBy",
         e.Data.DeclinedBy.EmployeeId
         |> Some
         |> CommandApprovalProgressSqlMapper.Writer.declinedBy
      ]

      {
         acc with
            CommandApprovalDeclined = qParams :: acc.CommandApprovalDeclined
      }
   | OrgEvent.CommandApprovalTerminated e ->
      let updatedStatus =
         CommandApprovalProgress.Status.Terminated e.Data.Reason

      let qParams = [
         "commandId",
         CommandApprovalProgressId e.CorrelationId
         |> CommandApprovalProgressSqlMapper.Writer.commandId

         "expectedCurrentStatus",
         CommandApprovalProgressSqlMapper.Writer.status
            CommandApprovalProgress.Status.Pending

         "status", CommandApprovalProgressSqlMapper.Writer.status updatedStatus

         "statusDetail",
         CommandApprovalProgressSqlMapper.Writer.statusDetail updatedStatus
      ]

      {
         acc with
            CommandApprovalTerminated = qParams :: acc.CommandApprovalTerminated
      }

let upsertReadModels (orgEvents: OrgEvent list) =
   let sqlParams =
      orgEvents
      |> List.sortByDescending (OrgEnvelope.unwrap >> snd >> _.Timestamp)
      |> List.fold sqlParamReducer {
         OrgCreate = []
         OrgUpdate = []
         OrgEvent = []
         FeatureFlags = []
         CommandApprovalRuleConfigured = []
         CommandApprovalRuleDeleted = []
         CommandApprovalRuleConfiguredWithAmountDailyLimit = []
         CommandApprovalRuleConfiguredWithAmountPerCommand = []
         CommandApprovalRequested = []
         CommandApprovalAcquired = []
         CommandApprovalDeclined = []
         CommandApprovalTerminated = []
      }

   let query = [
      $"""
      INSERT into {OrganizationSqlMapper.table}
         ({OrgFields.orgId},
          {OrgFields.parentAccountId},
          {OrgFields.name},
          {OrgFields.status},
          {OrgFields.statusDetail},
          {OrgFields.adminTeamEmail},
          {OrgFields.employerIdentificationNumber},
          {OrgFields.address},
          {OrgFields.businessType},
          {OrgFields.description},
          {OrgFields.website})
      VALUES
         (@orgId,
          @parentAccountId,
          @name,
          @status::{OrgTypeCast.status},
          @statusDetail,
          @adminTeamEmail,
          @employerIdentificationNumber,
          @address,
          @businessType::{OrgTypeCast.businessType},
          @description,
          @website)
      ON CONFLICT ({OrgFields.orgId})
      DO NOTHING;
      """,
      sqlParams.OrgCreate

      $"""
      UPDATE {OrganizationSqlMapper.table}
      SET
         {OrgFields.status} = @status::{OrgTypeCast.status},
         {OrgFields.statusDetail} = @statusDetail
      WHERE {OrgFields.orgId} = @orgId;
      """,
      sqlParams.OrgUpdate

      $"""
      INSERT into {OrganizationEventSqlMapper.table}
         ({OrgEventFields.eventId},
          {OrgEventFields.orgId},
          {OrgEventFields.initiatedById},
          {OrgEventFields.correlationId},
          {OrgEventFields.name},
          {OrgEventFields.timestamp},
          {OrgEventFields.event})
      VALUES
         (@eventId,
          @orgId,
          @initiatedById,
          @correlationId,
          @name,
          @timestamp,
          @event)
      ON CONFLICT ({OrgEventFields.eventId})
      DO NOTHING;
      """,
      sqlParams.OrgEvent

      $"""
      INSERT into {OrganizationSqlMapper.featureFlagsTable}
         ({OrgFields.orgId},
          {OrgFields.socialTransferDiscoveryAccountId})
      VALUES (@orgId, @socialTransferDiscovery)
      ON CONFLICT ({OrgFields.orgId})
      DO UPDATE SET
         {OrgFields.socialTransferDiscoveryAccountId} = @socialTransferDiscovery;
      """,
      sqlParams.FeatureFlags

      let commandId = CommandApprovalProgressSqlMapper.Fields.commandId
      let status = CommandApprovalProgressSqlMapper.Fields.status
      let statusDetail = CommandApprovalProgressSqlMapper.Fields.statusDetail
      let statusTypecast = CommandApprovalProgressSqlMapper.TypeCast.status

      let commandTypecast =
         CommandApprovalRuleSqlMapper.TypeCast.approvableCommand

      let approvedBy = CommandApprovalProgressSqlMapper.Fields.approvedBy

      let criteriaTypecast =
         CommandApprovalRuleSqlMapper.TypeCast.approvalCriteria

      $"""
      INSERT into {CommandApprovalRuleSqlMapper.table}
         ({CommandApprovalRuleSqlMapper.Fields.ruleId},
          {CommandApprovalRuleSqlMapper.Fields.orgId},
          {CommandApprovalRuleSqlMapper.Fields.approvableCommandType},
          {CommandApprovalRuleSqlMapper.Fields.criteria},
          {CommandApprovalRuleSqlMapper.Fields.criteriaDetail},
          {CommandApprovalRuleSqlMapper.Fields.permittedApprovers})
      VALUES
         (@ruleId,
          @orgId,
          @approvableCommandType::{commandTypecast},
          @criteria::{criteriaTypecast},
          @criteriaDetail,
          @approvers)
      ON CONFLICT ({CommandApprovalRuleSqlMapper.Fields.ruleId})
      DO UPDATE SET
         {CommandApprovalRuleSqlMapper.Fields.criteria} = @criteria::{criteriaTypecast},
         {CommandApprovalRuleSqlMapper.Fields.criteriaDetail} = @criteriaDetail,
         {CommandApprovalRuleSqlMapper.Fields.permittedApprovers} = @approvers;
      """,
      sqlParams.CommandApprovalRuleConfigured

      $"""
      UPDATE {CommandApprovalRuleSqlMapper.table}
      SET {CommandApprovalRuleSqlMapper.Fields.deletedAt} = @deletedAt
      WHERE {CommandApprovalRuleSqlMapper.Fields.ruleId} = @ruleId;
      """,
      sqlParams.CommandApprovalRuleDeleted

      $"""
      INSERT into {CommandApprovalRuleSqlMapper.dailyLimitTable}
         ({CommandApprovalRuleSqlMapper.Fields.ruleId},
          {CommandApprovalRuleSqlMapper.Fields.orgId},
          {CommandApprovalRuleSqlMapper.Fields.dailyLimit})
      VALUES (@ruleId, @orgId, @dailyLimit)
      ON CONFLICT ({CommandApprovalRuleSqlMapper.Fields.ruleId})
      DO UPDATE SET {CommandApprovalRuleSqlMapper.Fields.dailyLimit} = @dailyLimit;
      """,
      sqlParams.CommandApprovalRuleConfiguredWithAmountDailyLimit

      $"""
      INSERT into {CommandApprovalRuleSqlMapper.amountPerCommandTable}
         ({CommandApprovalRuleSqlMapper.Fields.ruleId},
          {CommandApprovalRuleSqlMapper.Fields.orgId},
          {CommandApprovalRuleSqlMapper.Fields.amountPerCommandLowerBound},
          {CommandApprovalRuleSqlMapper.Fields.amountPerCommandUpperBound})
      VALUES (@ruleId, @orgId, @lowerBound, @upperBound)
      ON CONFLICT ({CommandApprovalRuleSqlMapper.Fields.ruleId})
      DO UPDATE SET
         {CommandApprovalRuleSqlMapper.Fields.amountPerCommandLowerBound} = @lowerBound,
         {CommandApprovalRuleSqlMapper.Fields.amountPerCommandUpperBound} = @upperBound;
      """,
      sqlParams.CommandApprovalRuleConfiguredWithAmountPerCommand

      let commandTypecast =
         CommandApprovalProgressSqlMapper.TypeCast.approvableCommandType

      $"""
      INSERT into {CommandApprovalProgressSqlMapper.table}
         ({CommandApprovalProgressSqlMapper.Fields.commandId},
          {CommandApprovalProgressSqlMapper.Fields.ruleId},
          {CommandApprovalProgressSqlMapper.Fields.orgId},
          {CommandApprovalProgressSqlMapper.Fields.requestedBy},
          {CommandApprovalProgressSqlMapper.Fields.status},
          {CommandApprovalProgressSqlMapper.Fields.statusDetail},
          {CommandApprovalProgressSqlMapper.Fields.approvedBy},
          {CommandApprovalProgressSqlMapper.Fields.approvableCommandType},
          {CommandApprovalProgressSqlMapper.Fields.commandToInitiateOnApproval})
      VALUES
         (@commandId,
          @ruleId,
          @orgId,
          @requestedById,
          @status::{CommandApprovalProgressSqlMapper.TypeCast.status},
          @statusDetail,
          @approvedBy,
          @approvableCommandType::{commandTypecast},
          @command)
      ON CONFLICT ({CommandApprovalProgressSqlMapper.Fields.commandId})
      DO NOTHING;
      """,
      sqlParams.CommandApprovalRequested

      $"""
      UPDATE {CommandApprovalProgressSqlMapper.table}
      SET
         {approvedBy} = {approvedBy} || @approvedBy,
         {status} = @status::{statusTypecast},
         {statusDetail} = @statusDetail
      WHERE
         {commandId} = @commandId
         AND {status} = @expectedCurrentStatus::{statusTypecast}
         AND NOT {approvedBy} @> ARRAY[@approvedBy::uuid]
      """,
      sqlParams.CommandApprovalAcquired

      $"""
      UPDATE {CommandApprovalProgressSqlMapper.table}
      SET
         {status} = @status::{statusTypecast},
         {statusDetail} = @statusDetail,
         {CommandApprovalProgressSqlMapper.Fields.declinedBy} = @declinedBy
      WHERE
         {commandId} = @commandId
         AND {status} = @expectedCurrentStatus::{statusTypecast};
      """,
      sqlParams.CommandApprovalDeclined

      $"""
      UPDATE {CommandApprovalProgressSqlMapper.table}
      SET
         {status} = @status::{statusTypecast},
         {statusDetail} = @statusDetail
      WHERE
         {commandId} = @commandId
         AND {status} = @expectedCurrentStatus::{statusTypecast};
      """,
      sqlParams.CommandApprovalTerminated
   ]

   pgTransaction query

let initProps
   (chunking: StreamChunkingEnvConfig)
   (restartSettings: Akka.Streams.RestartSettings)
   (retryPersistenceAfter: TimeSpan)
   =
   actorProps<Org, OrgEvent>
   <| ReadModelSyncConfig.DefaultMode {
      Chunking = chunking
      RestartSettings = restartSettings
      RetryPersistenceAfter = retryPersistenceAfter
      UpsertReadModels = upsertReadModels
      EventJournalTag = Constants.AKKA_ORG_JOURNAL
   }
