[<RequireQualifiedAccess>]
module OrgReadModelSyncActor

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Types
open Lib.Postgres
open Bank.Org.Domain
open Lib.ReadModelSyncActor
open OrganizationSqlMapper
open OrganizationEventSqlMapper

type SqlParamsDerivedFromOrgEvents = {
   OrgEvent: (string * SqlValue) list list
   FeatureFlags: (string * SqlValue) list list
   CommandApprovalRuleConfigured: (string * SqlValue) list list
   CommandApprovalRuleDeleted: (string * SqlValue) list list
   CommandApprovalRuleConfiguredWithAmountDailyLimit:
      (string * SqlValue) list list
   CommandApprovalRuleConfiguredWithAmountPerCommand:
      (string * SqlValue) list list
   CommandApprovalRequested: (string * SqlValue) list list
   CommandApprovalAcquired: (string * SqlValue) list list
   CommandApprovalDeclined: (string * SqlValue) list list
   CommandApprovalTerminated: (string * SqlValue) list list
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

      "initiatedById", envelope.InitiatedById |> OrgEventSqlWriter.initiatedById

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
      let qParams = [
         "ruleId", CommandApprovalRuleSqlMapper.Writer.ruleId e.Data.RuleId

         "orgId", CommandApprovalRuleSqlMapper.Writer.orgId e.Data.OrgId

         "approvableCommandType",
         CommandApprovalRuleSqlMapper.Writer.approvableCommandType
            e.Data.CommandType

         "criteria",
         CommandApprovalRuleSqlMapper.Writer.criteria e.Data.Criteria

         "criteriaDetail",
         CommandApprovalRuleSqlMapper.Writer.criteriaDetail e.Data.Criteria

         "approvers",
         CommandApprovalRuleSqlMapper.Writer.permittedApprovers e.Data.Approvers
      ]

      let dailyLimitQParams =
         match e.Data.Criteria with
         | CommandApprovalRule.Criteria.AmountDailyLimit limit ->
            Some [
               "ruleId",
               CommandApprovalRuleSqlMapper.Writer.ruleId e.Data.RuleId
               "orgId", CommandApprovalRuleSqlMapper.Writer.orgId e.Data.OrgId
               "dailyLimit",
               CommandApprovalRuleSqlMapper.Writer.dailyLimit limit
            ]
         | _ -> None

      let amountPerCommandQParams =
         match e.Data.Criteria with
         | CommandApprovalRule.Criteria.AmountPerCommand range ->
            Some [
               "ruleId",
               CommandApprovalRuleSqlMapper.Writer.ruleId e.Data.RuleId
               "orgId", CommandApprovalRuleSqlMapper.Writer.orgId e.Data.OrgId
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
         e.InitiatedById |> CommandApprovalProgressSqlMapper.Writer.requestedBy

         "status", CommandApprovalProgressSqlMapper.Writer.status status

         "statusDetail",
         CommandApprovalProgressSqlMapper.Writer.statusDetail status

         "approvedBy", CommandApprovalProgressSqlMapper.Writer.approvedBy []

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
      let qParams = [
         "commandId",
         CommandApprovalProgressId e.CorrelationId
         |> CommandApprovalProgressSqlMapper.Writer.commandId

         "approvedBy",
         e.Data.ApprovedBy.EmployeeId |> EmployeeId.get |> Sql.uuid

         "expectedCurrentStatus",
         CommandApprovalProgress.Status.Pending
         |> CommandApprovalProgressSqlMapper.Writer.status
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

         "approvedBy",
         e.Data.ApprovedBy.EmployeeId |> EmployeeId.get |> Sql.uuid

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
      let updatedStatus = CommandApprovalProgress.Status.Terminated

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
   | _ -> acc

let sqlParamsFromOrg (org: Org) : (string * SqlValue) list = [
   "orgId", OrgSqlWriter.orgId org.OrgId
   "name", OrgSqlWriter.name org.Name
   "status", OrgSqlWriter.status org.Status
   "statusDetail", OrgSqlWriter.statusDetail org.Status
]

let upsertReadModels (orgs: Org list, orgEvents: OrgEvent list) =
   let orgSqlParams = orgs |> List.map sqlParamsFromOrg

   let sqlParams =
      orgEvents
      |> List.sortByDescending (OrgEnvelope.unwrap >> snd >> _.Timestamp)
      |> List.fold sqlParamReducer {
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
          {OrgFields.name},
          {OrgFields.status},
          {OrgFields.statusDetail})
      VALUES
         (@orgId,
          @name,
          @status::{OrgTypeCast.status},
          @statusDetail)
      ON CONFLICT ({OrgFields.orgId})
      DO UPDATE SET
         {OrgFields.status} = @status::{OrgTypeCast.status},
         {OrgFields.statusDetail} = @statusDetail;
      """,
      orgSqlParams

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

      if not sqlParams.FeatureFlags.IsEmpty then
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

      if not sqlParams.CommandApprovalRuleConfigured.IsEmpty then
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

      if not sqlParams.CommandApprovalRuleDeleted.IsEmpty then
         $"""
         UPDATE {CommandApprovalRuleSqlMapper.table}
         SET {CommandApprovalRuleSqlMapper.Fields.deletedAt} = @deletedAt
         WHERE {CommandApprovalRuleSqlMapper.Fields.ruleId} = @ruleId;
         """,
         sqlParams.CommandApprovalRuleDeleted

      if
         not sqlParams.CommandApprovalRuleConfiguredWithAmountDailyLimit.IsEmpty
      then
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

      if
         not sqlParams.CommandApprovalRuleConfiguredWithAmountPerCommand.IsEmpty
      then
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

      if not sqlParams.CommandApprovalRequested.IsEmpty then
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

      if not sqlParams.CommandApprovalAcquired.IsEmpty then
         $"""
         UPDATE {CommandApprovalProgressSqlMapper.table}
         SET
            {approvedBy} = {approvedBy} || @approvedBy
            {status} = COALESCE(@status, {status})
            {statusDetail} = COALESCE(@statusDetail, {statusDetail})
         WHERE
            {commandId} = @commandId
            AND {status} = @expectedCurrentStatus::{statusTypecast}
            AND NOT {approvedBy} @> ARRAY[@approvedBy::uuid]
         """,
         sqlParams.CommandApprovalAcquired

      if not sqlParams.CommandApprovalDeclined.IsEmpty then
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

      if not sqlParams.CommandApprovalTerminated.IsEmpty then
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
   (getOrgRef: OrgId -> IEntityRef<OrgMessage>)
   (chunking: StreamChunking)
   (restartSettings: Akka.Streams.RestartSettings)
   (retryPersistenceAfter: TimeSpan)
   =
   actorProps<Org, OrgEvent> (
      {
         GetAggregateIdFromEvent =
            OrgEnvelope.unwrap >> snd >> _.EntityId >> EntityId.get
         GetAggregate =
            fun orgId -> task {
               let aref = getOrgRef (OrgId orgId)
               let! (opt: Org option) = aref <? OrgMessage.GetOrg
               return opt
            }
         Chunking = chunking
         RestartSettings = restartSettings
         RetryPersistenceAfter = retryPersistenceAfter
         UpsertReadModels = upsertReadModels
         EventJournalTag = Constants.AKKA_ORG_JOURNAL
      }
   )
