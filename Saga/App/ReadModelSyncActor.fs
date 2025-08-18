[<RequireQualifiedAccess>]
module AppSagaReadModelSyncActor

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Types
open Lib.Postgres
open Lib.ReadModelSyncActor
open AppSagaSqlMapper
open Lib.Saga

let sqlParamsFromSaga (saga: AppSaga.Saga) : (string * SqlValue) list = [
   "id", Writer.id (saga :> ISaga).SagaId
   "orgId", Writer.orgId (saga :> ISaga).OrgId
   "name", Writer.name saga
   "status", Writer.status saga
   "state", Writer.sagaState saga
   "activityInProgressCount", Writer.activityInProgressCount saga
   "activityAttemptsExhaustedCount", Writer.activityAttemptsExhaustedCount saga
   "inactivityTimeout", Writer.inactivityTimeout saga
]

let upsertReadModels
   (sagas: AppSaga.Saga list, _: AppSaga.AppSagaPersistableEvent list)
   =
   let sagaSqlParams = sagas |> List.map sqlParamsFromSaga

   let query = [
      $"""
      INSERT into {AppSagaSqlMapper.table}
         ({Fields.id},
          {Fields.orgId},
          {Fields.name},
          {Fields.status},
          {Fields.sagaState},
          {Fields.activityInProgressCount},
          {Fields.activityAttemptsExhaustedCount},
          {Fields.inactivityTimeout})
      VALUES
         (@id,
          @orgId,
          @name::{TypeCast.name},
          @status::{TypeCast.status},
          @state,
          @activityInProgressCount,
          @activityAttemptsExhaustedCount,
          @inactivityTimeout)
      ON CONFLICT ({Fields.id})
      DO UPDATE SET
         {Fields.status} = @status::{TypeCast.status},
         {Fields.sagaState} = @state,
         {Fields.activityInProgressCount} = @activityInProgressCount,
         {Fields.activityAttemptsExhaustedCount} = @activityAttemptsExhaustedCount,
         {Fields.inactivityTimeout} = @inactivityTimeout;
      """,
      sagaSqlParams
   ]

   pgTransaction query

let initProps
   (getSagaRef:
      CorrelationId
         -> IEntityRef<SagaMessage<AppSaga.StartEvent, AppSaga.Event>>)
   (chunking: StreamChunkingEnvConfig)
   (restartSettings: Akka.Streams.RestartSettings)
   (retryPersistenceAfter: TimeSpan)
   =
   actorProps<AppSaga.Saga, AppSaga.AppSagaPersistableEvent>
   <| ReadModelSyncConfig.AggregateLookupMode {
      GetAggregateIdFromEvent = _.CorrelationId.Value
      GetAggregate =
         fun correlationId -> task {
            let! (opt: AppSaga.Saga option) =
               getSagaRef (CorrelationId correlationId) <? SagaMessage.GetSaga

            return opt
         }
      Chunking = chunking
      RestartSettings = restartSettings
      RetryPersistenceAfter = retryPersistenceAfter
      UpsertReadModels = upsertReadModels
      EventJournalTag = Constants.AKKA_APP_SAGA_JOURNAL
   }
