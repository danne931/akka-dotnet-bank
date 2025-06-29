[<RequireQualifiedAccess>]
module EmployeeReadModelSyncActor

open System

open Lib.SharedTypes
open Lib.Types
open Lib.Postgres
open Bank.Employee.Domain
open Lib.ReadModelSyncActor
open EmployeeSqlMapper
open EmployeeEventSqlMapper
open CardSqlMapper

type private SqlParams = SqlParameter list list

type SqlParamsDerivedFromEmployeeEvents = {
   EmployeeCreate: SqlParams
   EmployeeUpdate: SqlParams
   EmployeeInviteUpdate: SqlParams
   CardCreate: SqlParams
   CardUpdate: SqlParams
   EmployeeEvent: SqlParams
}

let private employeeInviteParamReducer acc entityId status =
   let sqlParams = [
      "employeeId",
      EmployeeSqlWriter.employeeId (EmployeeId.fromEntityId entityId)
      "status", EmployeeSqlWriter.status status
      "statusDetail", EmployeeSqlWriter.statusDetail status
      "inviteToken", EmployeeSqlWriter.inviteTokenFromStatus status
      "inviteExpiration", EmployeeSqlWriter.inviteExpirationFromStatus status
   ]

   {
      acc with
         EmployeeInviteUpdate = sqlParams :: acc.EmployeeInviteUpdate
   }

let sqlParamReducer
   (acc: SqlParamsDerivedFromEmployeeEvents)
   (evt: EmployeeEvent)
   : SqlParamsDerivedFromEmployeeEvents
   =
   let evt, envelope = EmployeeEnvelope.unwrap evt

   let employeeEventSqlParams = [
      "eventId", EmployeeEventSqlWriter.eventId envelope.Id

      "employeeId",
      envelope.EntityId
      |> EmployeeId.fromEntityId
      |> EmployeeEventSqlWriter.employeeId

      "initiatedById",
      envelope.InitiatedBy.Id |> EmployeeEventSqlWriter.initiatedById

      "orgId", EmployeeEventSqlWriter.orgId envelope.OrgId

      "correlationId",
      EmployeeEventSqlWriter.correlationId envelope.CorrelationId

      "name", EmployeeEventSqlWriter.name envelope.EventName
      "timestamp", EmployeeEventSqlWriter.timestamp envelope.Timestamp
      "event", EmployeeEventSqlWriter.event evt
   ]

   let acc = {
      acc with
         EmployeeEvent = employeeEventSqlParams :: acc.EmployeeEvent
   }

   match evt with
   | EmployeeEvent.CreatedAccountOwner e ->
      let status =
         EmployeeStatus.PendingInviteConfirmation {
            Token = e.Data.InviteToken
            CorrelationId = e.CorrelationId
         }

      let sqlParams = [
         "employeeId",
         EmployeeSqlWriter.employeeId (EmployeeId.fromEntityId e.EntityId)
         "orgId", EmployeeSqlWriter.orgId e.OrgId
         "email", EmployeeSqlWriter.email e.Data.Email
         "firstName", EmployeeSqlWriter.firstName e.Data.FirstName
         "lastName", EmployeeSqlWriter.lastName e.Data.LastName
         "status", EmployeeSqlWriter.status status
         "statusDetail", EmployeeSqlWriter.statusDetail status
         "role", EmployeeSqlWriter.role Role.Admin
         "inviteToken", EmployeeSqlWriter.inviteTokenFromStatus status
         "inviteExpiration", EmployeeSqlWriter.inviteExpirationFromStatus status
         "authProviderUserId", EmployeeSqlWriter.authProviderUserId None
      ]

      {
         acc with
            EmployeeCreate = sqlParams :: acc.EmployeeCreate
      }
   | EmployeeEvent.CreatedEmployee e ->
      let status =
         match e.Data.OrgRequiresEmployeeInviteApproval with
         | Some ruleId ->
            EmployeeStatus.PendingInviteApproval {
               RuleId = ruleId
               ProgressId = CommandApprovalProgressId e.CorrelationId
            }
         | None ->
            EmployeeStatus.PendingInviteConfirmation {
               Token = e.Data.InviteToken
               CorrelationId = e.CorrelationId
            }

      let sqlParams = [
         "employeeId",
         EmployeeSqlWriter.employeeId (EmployeeId.fromEntityId e.EntityId)
         "orgId", EmployeeSqlWriter.orgId e.OrgId
         "email", EmployeeSqlWriter.email e.Data.Email
         "firstName", EmployeeSqlWriter.firstName e.Data.FirstName
         "lastName", EmployeeSqlWriter.lastName e.Data.LastName
         "status", EmployeeSqlWriter.status status
         "statusDetail", EmployeeSqlWriter.statusDetail status
         "role", EmployeeSqlWriter.role e.Data.Role
         "inviteToken", EmployeeSqlWriter.inviteTokenFromStatus status
         "inviteExpiration", EmployeeSqlWriter.inviteExpirationFromStatus status
         "authProviderUserId", EmployeeSqlWriter.authProviderUserId None
      ]

      {
         acc with
            EmployeeCreate = sqlParams :: acc.EmployeeCreate
      }
   | EmployeeEvent.CreatedCard e ->
      let card = e.Data.Card

      let sqlParams = [
         "cardId", CardSqlWriter.cardId card.CardId
         "thirdPartyProviderCardId", CardSqlWriter.thirdPartyProviderCardId None
         "accountId", CardSqlWriter.accountId card.AccountId
         "orgId", CardSqlWriter.orgId e.OrgId
         "employeeId",
         EmployeeId.fromEntityId e.EntityId |> CardSqlWriter.employeeId

         "cardNumberLast4", CardSqlWriter.cardNumberLast4 card.CardNumberLast4

         "cardType", CardSqlWriter.cardType card.CardType
         "status", CardSqlWriter.status card.Status
         "statusDetail", CardSqlWriter.statusDetail card.Status
         "virtual", CardSqlWriter.isVirtual card.Virtual

         "dailyPurchaseLimit",
         CardSqlWriter.dailyPurchaseLimit card.DailyPurchaseLimit

         "monthlyPurchaseLimit",
         CardSqlWriter.monthlyPurchaseLimit card.MonthlyPurchaseLimit

         "lastPurchaseAt", CardSqlWriter.lastPurchaseAt card.LastPurchaseAt

         "cardNickname", CardSqlWriter.cardNickname card.CardNickname

         "expMonth", CardSqlWriter.expMonth card.Expiration.Month

         "expYear", CardSqlWriter.expMonth card.Expiration.Year
      ]

      {
         acc with
            CardCreate = sqlParams :: acc.CardCreate
      }
   | EmployeeEvent.ThirdPartyProviderCardLinked e ->
      let status = CardStatus.Active

      let sqlParams = [
         "cardId", CardSqlWriter.cardId e.Data.CardId
         "status", CardSqlWriter.status status
         "statusDetail", CardSqlWriter.statusDetail status
         "thirdPartyProviderCardId",
         CardSqlWriter.thirdPartyProviderCardId (Some e.Data.ProviderCardId)
      ]

      {
         acc with
            CardUpdate = sqlParams :: acc.CardUpdate
      }
   | EmployeeEvent.PurchaseSettled e ->
      let sqlParams = [
         "cardId", CardSqlWriter.cardId e.Data.Info.CardId
         "lastPurchaseAt", CardSqlWriter.lastPurchaseAt (Some e.Timestamp)
      ]

      {
         acc with
            CardUpdate = sqlParams :: acc.CardUpdate
      }
   | EmployeeEvent.LockedCard e ->
      let status = CardStatus.Frozen CardFrozenReason.UserRequested

      let sqlParams = [
         "cardId", CardSqlWriter.cardId e.Data.CardId
         "status", CardSqlWriter.status status
         "statusDetail", CardSqlWriter.statusDetail status
      ]

      {
         acc with
            CardUpdate = sqlParams :: acc.CardUpdate
      }
   | EmployeeEvent.UnlockedCard e ->
      let status = CardStatus.Active

      let sqlParams = [
         "cardId", CardSqlWriter.cardId e.Data.CardId
         "status", CardSqlWriter.status status
         "statusDetail", CardSqlWriter.statusDetail status
      ]

      {
         acc with
            CardUpdate = sqlParams :: acc.CardUpdate
      }
   | EmployeeEvent.DailyDebitLimitUpdated e ->
      let sqlParams = [
         "cardId", CardSqlWriter.cardId e.Data.CardId
         "dailyPurchaseLimit",
         CardSqlWriter.dailyPurchaseLimit e.Data.DebitLimit
      ]

      {
         acc with
            CardUpdate = sqlParams :: acc.CardUpdate
      }
   | EmployeeEvent.MonthlyDebitLimitUpdated e ->
      let sqlParams = [
         "cardId", CardSqlWriter.cardId e.Data.CardId
         "monthlyPurchaseLimit",
         CardSqlWriter.monthlyPurchaseLimit e.Data.DebitLimit
      ]

      {
         acc with
            CardUpdate = sqlParams :: acc.CardUpdate
      }
   | EmployeeEvent.CardNicknamed e ->
      let sqlParams = [
         "cardId", CardSqlWriter.cardId e.Data.CardId
         "cardNickname", CardSqlWriter.cardNickname (Some e.Data.Name)
      ]

      {
         acc with
            CardUpdate = sqlParams :: acc.CardUpdate
      }
   | EmployeeEvent.UpdatedRole e ->
      let sqlParams = [
         "employeeId",
         EmployeeSqlWriter.employeeId (EmployeeId.fromEntityId e.EntityId)
         "role", EmployeeSqlWriter.role e.Data.Role
      ]

      {
         acc with
            EmployeeUpdate = sqlParams :: acc.EmployeeUpdate
      }
   | EmployeeEvent.InvitationTokenRefreshed e ->
      employeeInviteParamReducer
         acc
         e.EntityId
         (EmployeeStatus.PendingInviteConfirmation {
            Token = e.Data.InviteToken
            CorrelationId = e.CorrelationId
         })
   | EmployeeEvent.InvitationConfirmed e ->
      let status = EmployeeStatus.Active

      let sqlParams = [
         "employeeId",
         EmployeeSqlWriter.employeeId (EmployeeId.fromEntityId e.EntityId)
         "status", EmployeeSqlWriter.status status
         "statusDetail", EmployeeSqlWriter.statusDetail status
         "inviteToken", EmployeeSqlWriter.inviteTokenFromStatus status
         "inviteExpiration", EmployeeSqlWriter.inviteExpirationFromStatus status
         "authProviderUserId",
         EmployeeSqlWriter.authProviderUserId (Some e.Data.AuthProviderUserId)
         "email", EmployeeSqlWriter.email e.Data.Email
      ]

      {
         acc with
            EmployeeInviteUpdate = sqlParams :: acc.EmployeeInviteUpdate
      }
   | EmployeeEvent.AccessApproved e ->
      employeeInviteParamReducer
         acc
         e.EntityId
         (EmployeeStatus.PendingInviteConfirmation {
            Token = e.Data.InviteToken
            CorrelationId = e.CorrelationId
         })
   | EmployeeEvent.AccessRestored e ->
      employeeInviteParamReducer
         acc
         e.EntityId
         (EmployeeStatus.PendingInviteConfirmation {
            Token = e.Data.InviteToken
            CorrelationId = e.CorrelationId
         })
   | EmployeeEvent.InvitationCancelled e ->
      employeeInviteParamReducer acc e.EntityId EmployeeStatus.Closed
   | _ -> acc

let upsertReadModels (employeeEvents: EmployeeEvent list) =
   let sqlParams =
      employeeEvents
      |> List.sortByDescending (EmployeeEnvelope.unwrap >> snd >> _.Timestamp)
      |> List.fold sqlParamReducer {
         EmployeeCreate = []
         EmployeeUpdate = []
         EmployeeInviteUpdate = []
         EmployeeEvent = []
         CardCreate = []
         CardUpdate = []
      }

   let query = [
      $"""
      INSERT into {EmployeeSqlMapper.table}
         ({EmployeeFields.employeeId},
          {EmployeeFields.orgId},
          {EmployeeFields.role},
          {EmployeeFields.email},
          {EmployeeFields.firstName},
          {EmployeeFields.lastName},
          {EmployeeFields.status},
          {EmployeeFields.statusDetail},
          {EmployeeFields.inviteToken},
          {EmployeeFields.inviteExpiration},
          {EmployeeFields.authProviderUserId})
      VALUES
         (@employeeId,
          @orgId,
          @role::{EmployeeTypeCast.role},
          @email,
          @firstName,
          @lastName,
          @status::{EmployeeTypeCast.status},
          @statusDetail,
          @inviteToken,
          @inviteExpiration,
          @authProviderUserId)
      ON CONFLICT ({EmployeeFields.employeeId})
      DO NOTHING;
      """,
      sqlParams.EmployeeCreate

      $"""
      UPDATE {EmployeeSqlMapper.table}
      SET
         {EmployeeFields.status} = @status::{EmployeeTypeCast.status},
         {EmployeeFields.statusDetail} = @statusDetail,
         {EmployeeFields.inviteToken} = @inviteToken,
         {EmployeeFields.inviteExpiration} = @inviteExpiration,
         {EmployeeFields.email} = COALESCE(@email, {EmployeeFields.email}),
         {EmployeeFields.authProviderUserId} = COALESCE(@authProviderUserId, {EmployeeFields.authProviderUserId})
      WHERE {EmployeeFields.employeeId} = @employeeId;
      """,
      sqlParams.EmployeeInviteUpdate
      |> List.map (
         Lib.Postgres.addCoalescableParamsForUpdate [
            "authProviderUserId"
            "email"
         ]
      )

      $"""
      UPDATE {EmployeeSqlMapper.table}
      SET
         {EmployeeFields.status} = COALESCE(@status::{EmployeeTypeCast.status}, {EmployeeFields.status}),
         {EmployeeFields.statusDetail} = COALESCE(@statusDetail, {EmployeeFields.statusDetail}),
         {EmployeeFields.role} = COALESCE(@role::{EmployeeTypeCast.role}, {EmployeeFields.role})
      WHERE {EmployeeFields.employeeId} = @employeeId;
      """,
      sqlParams.EmployeeUpdate
      |> List.map (
         Lib.Postgres.addCoalescableParamsForUpdate [
            "status"
            "statusDetail"
            "role"
         ]
      )

      $"""
      INSERT into {EmployeeEventSqlMapper.table}
         ({EmployeeEventFields.eventId},
          {EmployeeEventFields.orgId},
          {EmployeeEventFields.employeeId},
          {EmployeeEventFields.initiatedById},
          {EmployeeEventFields.correlationId},
          {EmployeeEventFields.name},
          {EmployeeEventFields.timestamp},
          {EmployeeEventFields.event})
      VALUES
         (@eventId,
          @orgId,
          @employeeId,
          @initiatedById,
          @correlationId,
          @name,
          @timestamp,
          @event)
      ON CONFLICT ({EmployeeEventFields.eventId})
      DO NOTHING;
      """,
      sqlParams.EmployeeEvent

      $"""
      INSERT into {CardSqlMapper.table}
         ({CardFields.cardId},
          {CardFields.thirdPartyProviderCardId},
          {CardFields.accountId},
          {CardFields.orgId},
          {CardFields.employeeId},
          {CardFields.cardNumberLast4},
          {CardFields.cardType},
          {CardFields.status},
          {CardFields.statusDetail},
          {CardFields.isVirtual},
          {CardFields.dailyPurchaseLimit},
          {CardFields.monthlyPurchaseLimit},
          {CardFields.lastPurchaseAt},
          {CardFields.cardNickname},
          {CardFields.expMonth},
          {CardFields.expYear})
      VALUES
         (@cardId,
          @thirdPartyProviderCardId,
          @accountId,
          @orgId,
          @employeeId,
          @cardNumberLast4,
          @cardType::{CardTypeCast.cardType},
          @status::{CardTypeCast.status},
          @statusDetail,
          @virtual,
          @dailyPurchaseLimit,
          @monthlyPurchaseLimit,
          @lastPurchaseAt,
          @cardNickname,
          @expMonth,
          @expYear)
      ON CONFLICT ({CardFields.cardId})
      DO NOTHING;
      """,
      sqlParams.CardCreate

      $"""
      UPDATE {CardSqlMapper.table}
      SET
         {CardFields.status} = COALESCE(@status::{CardTypeCast.status}, {CardFields.status}),
         {CardFields.statusDetail} = COALESCE(@statusDetail, {CardFields.statusDetail}),
         {CardFields.dailyPurchaseLimit} = COALESCE(@dailyPurchaseLimit::money, {CardFields.dailyPurchaseLimit}),
         {CardFields.monthlyPurchaseLimit} = COALESCE(@monthlyPurchaseLimit::money, {CardFields.monthlyPurchaseLimit}),
         {CardFields.lastPurchaseAt} = COALESCE(@lastPurchaseAt, {CardFields.lastPurchaseAt}),
         {CardFields.cardNickname} = COALESCE(@cardNickname, {CardFields.cardNickname}),
         {CardFields.thirdPartyProviderCardId} = COALESCE(@thirdPartyProviderCardId, {CardFields.thirdPartyProviderCardId})
      WHERE {CardFields.cardId} = @cardId;
      """,
      sqlParams.CardUpdate
      |> List.map (
         Lib.Postgres.addCoalescableParamsForUpdate [
            "status"
            "statusDetail"
            "dailyPurchaseLimit"
            "monthlyPurchaseLimit"
            "lastPurchaseAt"
            "cardNickname"
            "thirdPartyProviderCardId"
         ]
      )
   ]

   pgTransaction query

let initProps
   (chunking: StreamChunkingEnvConfig)
   (restartSettings: Akka.Streams.RestartSettings)
   (retryPersistenceAfter: TimeSpan)
   =
   actorProps<Employee, EmployeeEvent>
   <| ReadModelSyncConfig.DefaultMode {
      Chunking = chunking
      RestartSettings = restartSettings
      RetryPersistenceAfter = retryPersistenceAfter
      UpsertReadModels = upsertReadModels
      EventJournalTag = Constants.AKKA_EMPLOYEE_JOURNAL
   }
