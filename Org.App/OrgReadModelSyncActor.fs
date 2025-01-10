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
      |> List.fold sqlParamReducer { OrgEvent = []; FeatureFlags = [] }

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
