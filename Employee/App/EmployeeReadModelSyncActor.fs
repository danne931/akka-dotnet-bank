[<RequireQualifiedAccess>]
module EmployeeReadModelSyncActor

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
   CardIssuerMapping: SqlParams
   CardsByEmployeeUpdate: SqlParams
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
         "parentAccountId",
         EmployeeSqlWriter.parentAccountId e.Data.ParentAccountId
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
         "parentAccountId",
         EmployeeSqlWriter.parentAccountId e.Data.ParentAccountId
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
   | EmployeeEvent.CardLinked e ->
      let status = CardStatus.Active

      let cardSqlParams = [
         "cardId", CardSqlWriter.cardId e.Data.Link.CardId
         "status", CardSqlWriter.status status
         "statusDetail", CardSqlWriter.statusDetail status
         "cardNumberLast4", CardSqlWriter.cardNumberLast4 e.Data.CardNumberLast4
      ]

      let issuerMappingParams = [
         "internalCardId",
         CardIssuerMappingSqlMapper.Writer.internalCardId e.Data.Link.CardId
         "issuerCardId",
         CardIssuerMappingSqlMapper.Writer.issuerCardId
            e.Data.Link.CardIssuerCardId
         "issuerName",
         CardIssuerMappingSqlMapper.Writer.issuerName e.Data.Link.CardIssuerName
         "employeeId",
         EmployeeId.fromEntityId e.EntityId
         |> CardIssuerMappingSqlMapper.Writer.employeeId
      ]

      {
         acc with
            CardUpdate = cardSqlParams :: acc.CardUpdate
            CardIssuerMapping = issuerMappingParams :: acc.CardIssuerMapping
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
   | EmployeeEvent.ConfiguredRollingPurchaseLimit e ->
      let sqlParams = [
         "cardId", CardSqlWriter.cardId e.Data.CardId
         "dailyPurchaseLimit",
         CardSqlWriter.dailyPurchaseLimit e.Data.DailyLimit
         "monthlyPurchaseLimit",
         CardSqlWriter.monthlyPurchaseLimit e.Data.MonthlyLimit
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
      let employeeId = EmployeeId.fromEntityId e.EntityId

      let sqlParams = [
         "employeeId", EmployeeSqlWriter.employeeId employeeId
         "role", EmployeeSqlWriter.role e.Data.Role
      ]

      let acc = {
         acc with
            EmployeeUpdate = sqlParams :: acc.EmployeeUpdate
      }

      match e.Data.Role with
      | Role.Scholar ->
         let status = CardStatus.Closed CardClosedReason.EndUserRequest

         let cardParams = [
            "employeeId", CardSqlWriter.employeeId employeeId
            "status", CardSqlWriter.status status
            "statusDetail", CardSqlWriter.statusDetail status
         ]

         {
            acc with
               CardsByEmployeeUpdate = cardParams :: acc.CardsByEmployeeUpdate
         }
      | _ -> acc
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
         CardsByEmployeeUpdate = []
         CardIssuerMapping = []
      }

   let query = [
      $"""
      INSERT into {EmployeeSqlMapper.table}
         ({EmployeeFields.employeeId},
          {EmployeeFields.orgId},
          {EmployeeFields.parentAccountId},
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
          @parentAccountId,
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
      INSERT into {CardIssuerMappingSqlMapper.table}
         ({CardIssuerMappingSqlMapper.Fields.internalCardId},
          {CardIssuerMappingSqlMapper.Fields.issuerCardId},
          {CardIssuerMappingSqlMapper.Fields.employeeId},
          {CardIssuerMappingSqlMapper.Fields.issuerName})
      VALUES
         (@internalCardId,
          @issuerCardId,
          @employeeId,
          @issuerName::{CardIssuerMappingSqlMapper.TypeCast.issuerName});
      """,
      sqlParams.CardIssuerMapping

      $"""
      UPDATE {CardSqlMapper.table}
      SET
         {CardFields.status} = COALESCE(@status::{CardTypeCast.status}, {CardFields.status}),
         {CardFields.statusDetail} = COALESCE(@statusDetail, {CardFields.statusDetail}),
         {CardFields.dailyPurchaseLimit} = COALESCE(@dailyPurchaseLimit::money, {CardFields.dailyPurchaseLimit}),
         {CardFields.monthlyPurchaseLimit} = COALESCE(@monthlyPurchaseLimit::money, {CardFields.monthlyPurchaseLimit}),
         {CardFields.lastPurchaseAt} = COALESCE(@lastPurchaseAt, {CardFields.lastPurchaseAt}),
         {CardFields.cardNickname} = COALESCE(@cardNickname, {CardFields.cardNickname}),
         {CardFields.cardNumberLast4} = COALESCE(@cardNumberLast4, {CardFields.cardNumberLast4})
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
            "cardNumberLast4"
         ]
      )

      $"""
      UPDATE {CardSqlMapper.table}
      SET
         {CardFields.status} = @status::{CardTypeCast.status},
         {CardFields.statusDetail} = @statusDetail
      WHERE {CardFields.employeeId} = @employeeId;
      """,
      sqlParams.CardsByEmployeeUpdate
   ]

   pgTransaction query

let initProps
   (chunking: StreamChunkingEnvConfig)
   (restartSettings: Akka.Streams.RestartSettings)
   =
   actorProps<Employee, EmployeeEvent>
   <| ReadModelSyncConfig.DefaultMode {
      Chunking = chunking
      RestartSettings = restartSettings
      UpsertReadModels = upsertReadModels
      EventJournalTag = Constants.AKKA_EMPLOYEE_JOURNAL
   }
