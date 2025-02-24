[<RequireQualifiedAccess>]
module OrgReadModelSyncActor

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Types
open Lib.Postgres
open Bank.Org.Domain
open Bank.Transfer.Domain
open Lib.ReadModelSyncActor
open OrganizationSqlMapper
open OrganizationEventSqlMapper
open TransferSqlMapper
open CommandApproval

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
   DomesticTransferRecipient: (string * SqlValue) list list
   UpdatedDomesticTransferRecipientStatus: (string * SqlValue) list list
}

let private domesticRecipientReducer
   (acc: SqlParamsDerivedFromOrgEvents)
   (recipient: DomesticTransferRecipient)
   =
   let qParams = [
      "recipientAccountId",
      TransferSqlWriter.DomesticRecipient.recipientAccountId
         recipient.RecipientAccountId
      "senderOrgId",
      TransferSqlWriter.DomesticRecipient.senderOrgId recipient.SenderOrgId
      "lastName",
      TransferSqlWriter.DomesticRecipient.lastName recipient.LastName
      "firstName",
      TransferSqlWriter.DomesticRecipient.firstName recipient.FirstName
      "accountNumber",
      TransferSqlWriter.DomesticRecipient.accountNumber recipient.AccountNumber
      "routingNumber",
      TransferSqlWriter.DomesticRecipient.routingNumber recipient.RoutingNumber
      "status", TransferSqlWriter.DomesticRecipient.status recipient.Status
      "depository",
      TransferSqlWriter.DomesticRecipient.depository recipient.Depository
      "paymentNetwork",
      TransferSqlWriter.DomesticRecipient.paymentNetwork
         recipient.PaymentNetwork
      "nickname",
      TransferSqlWriter.DomesticRecipient.nickname recipient.Nickname
   ]

   {
      acc with
         DomesticTransferRecipient = qParams :: acc.DomesticTransferRecipient
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
   | OrgEvent.OrgCreated _
   | OrgEvent.OrgOnboardingFinished _ -> acc
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

         "approvedBy",
         e.Data.ApprovedBy.EmployeeId |> EmployeeId.get |> Sql.uuid

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
   | OrgEvent.RegisteredDomesticTransferRecipient e ->
      domesticRecipientReducer acc e.Data.Recipient
   | OrgEvent.EditedDomesticTransferRecipient e ->
      domesticRecipientReducer acc e.Data.Recipient
   | OrgEvent.NicknamedDomesticTransferRecipient e ->
      let qParams = [
         "accountId",
         TransferSqlWriter.DomesticRecipient.recipientAccountId
            e.Data.RecipientId
         "nickname",
         TransferSqlWriter.DomesticRecipient.nickname e.Data.Nickname
      ]

      {
         acc with
            DomesticTransferRecipient = qParams :: acc.DomesticTransferRecipient
      }
   | OrgEvent.DomesticTransferRecipientFailed e ->
      let updatedStatus =
         match e.Data.Reason with
         | DomesticTransferRecipientFailReason.ClosedAccount ->
            RecipientRegistrationStatus.Closed
         | DomesticTransferRecipientFailReason.InvalidAccountInfo ->
            RecipientRegistrationStatus.InvalidAccount

      let qParams = [
         "recipientAccountId",
         TransferSqlWriter.DomesticRecipient.recipientAccountId
            e.Data.RecipientId

         "status", TransferSqlWriter.DomesticRecipient.status updatedStatus
      ]

      {
         acc with
            UpdatedDomesticTransferRecipientStatus =
               qParams :: acc.UpdatedDomesticTransferRecipientStatus
      }
   | OrgEvent.DomesticTransferRetryConfirmsRecipient e ->
      let qParams = [
         "recipientAccountId",
         TransferSqlWriter.DomesticRecipient.recipientAccountId
            e.Data.RecipientId

         "status",
         TransferSqlWriter.DomesticRecipient.status
            RecipientRegistrationStatus.Confirmed
      ]

      {
         acc with
            UpdatedDomesticTransferRecipientStatus =
               qParams :: acc.UpdatedDomesticTransferRecipientStatus
      }

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
         DomesticTransferRecipient = []
         UpdatedDomesticTransferRecipientStatus = []
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
            {approvedBy} = {approvedBy} || @approvedBy,
            {status} = @status::{statusTypecast},
            {statusDetail} = @statusDetail
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

      if not sqlParams.DomesticTransferRecipient.IsEmpty then
         $"""
         INSERT into {TransferSqlMapper.Table.domesticRecipient}
            ({TransferFields.DomesticRecipient.recipientAccountId},
             {TransferFields.DomesticRecipient.senderOrgId},
             {TransferFields.DomesticRecipient.firstName},
             {TransferFields.DomesticRecipient.lastName},
             {TransferFields.DomesticRecipient.nickname},
             {TransferFields.DomesticRecipient.routingNumber},
             {TransferFields.DomesticRecipient.accountNumber},
             {TransferFields.DomesticRecipient.status},
             {TransferFields.DomesticRecipient.depository},
             {TransferFields.DomesticRecipient.paymentNetwork})
         VALUES
            (@recipientAccountId,
             @senderOrgId,
             @firstName,
             @lastName,
             @nickname,
             @routingNumber,
             @accountNumber,
             @status::{TransferTypeCast.domesticRecipientStatus},
             @depository::{TransferTypeCast.domesticRecipientAccountDepository},
             @paymentNetwork::{TransferTypeCast.paymentNetwork})
         ON CONFLICT ({TransferFields.DomesticRecipient.recipientAccountId})
         DO UPDATE SET
            {TransferFields.DomesticRecipient.firstName} = @firstName,
            {TransferFields.DomesticRecipient.lastName} = @lastName,
            {TransferFields.DomesticRecipient.nickname} = @nickname,
            {TransferFields.DomesticRecipient.routingNumber} = @routingNumber,
            {TransferFields.DomesticRecipient.accountNumber} = @accountNumber,
            {TransferFields.DomesticRecipient.status} =
               @status::{TransferTypeCast.domesticRecipientStatus},
            {TransferFields.DomesticRecipient.depository} =
               @depository::{TransferTypeCast.domesticRecipientAccountDepository},
            {TransferFields.DomesticRecipient.paymentNetwork} =
               @paymentNetwork::{TransferTypeCast.paymentNetwork};
         """,
         sqlParams.DomesticTransferRecipient

      if not sqlParams.UpdatedDomesticTransferRecipientStatus.IsEmpty then
         $"""
         UPDATE {TransferSqlMapper.Table.domesticRecipient}
         SET {TransferFields.DomesticRecipient.status} = @status::{TransferTypeCast.domesticRecipientStatus}
         WHERE {TransferFields.DomesticRecipient.recipientAccountId} = @recipientAccountId;
         """,
         sqlParams.UpdatedDomesticTransferRecipientStatus
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
