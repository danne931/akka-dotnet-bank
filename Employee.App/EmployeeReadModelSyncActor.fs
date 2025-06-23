[<RequireQualifiedAccess>]
module EmployeeReadModelSyncActor

open System
open Akkling
open Akkling.Cluster.Sharding
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.Types
open Lib.Postgres
open Bank.Employee.Domain
open Lib.ReadModelSyncActor
open EmployeeSqlMapper
open EmployeeEventSqlMapper
open CardSqlMapper

type private SqlParams = (string * SqlValue) list list

type SqlParamsDerivedFromEmployeeEvents = {
   Card: SqlParams
   EmployeeEvent: SqlParams
}

let private cardReducer
   (employeeMap: Map<EmployeeId, Employee>)
   (acc: SqlParamsDerivedFromEmployeeEvents)
   (entityId: EntityId)
   (cardId: CardId)
   =
   let qParams =
      employeeMap
      |> Map.tryFind (EmployeeId.fromEntityId entityId)
      |> Option.bind (fun employee ->
         employee.Cards
         |> Map.tryFind cardId
         |> Option.map (fun card -> [
            "cardId", CardSqlWriter.cardId card.CardId
            "thirdPartyProviderCardId",
            CardSqlWriter.thirdPartyProviderCardId card.Status
            "accountId", CardSqlWriter.accountId card.AccountId
            "orgId", CardSqlWriter.orgId employee.OrgId
            "employeeId", CardSqlWriter.employeeId employee.EmployeeId

            "cardNumberLast4",
            CardSqlWriter.cardNumberLast4 card.CardNumberLast4

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
         ]))

   {
      acc with
         Card =
            match qParams with
            | None -> acc.Card
            | Some qParams -> qParams :: acc.Card
   }

let sqlParamReducer
   (employeeMap: Map<EmployeeId, Employee>)
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

   let cardReducer = cardReducer employeeMap acc

   match evt with
   | EmployeeEvent.CreatedCard e -> cardReducer e.EntityId e.Data.Card.CardId
   | EmployeeEvent.ThirdPartyProviderCardLinked e ->
      cardReducer e.EntityId e.Data.CardId
   | EmployeeEvent.PurchaseApplied e ->
      cardReducer e.EntityId e.Data.Info.CardId
   | EmployeeEvent.LockedCard e -> cardReducer e.EntityId e.Data.CardId
   | EmployeeEvent.UnlockedCard e -> cardReducer e.EntityId e.Data.CardId
   | EmployeeEvent.DailyDebitLimitUpdated e ->
      cardReducer e.EntityId e.Data.CardId
   | EmployeeEvent.MonthlyDebitLimitUpdated e ->
      cardReducer e.EntityId e.Data.CardId
   | EmployeeEvent.CardNicknamed e -> cardReducer e.EntityId e.Data.CardId
   | _ -> acc

let sqlParamsFromEmployee (employee: Employee) : (string * SqlValue) list = [
   "employeeId", EmployeeSqlWriter.employeeId employee.EmployeeId
   "orgId", EmployeeSqlWriter.orgId employee.OrgId
   "email", EmployeeSqlWriter.email employee.Email
   "firstName", EmployeeSqlWriter.firstName employee.FirstName
   "lastName", EmployeeSqlWriter.lastName employee.LastName
   "status", EmployeeSqlWriter.status employee.Status
   "statusDetail", EmployeeSqlWriter.statusDetail employee.Status
   "role", EmployeeSqlWriter.role employee.Role
   "cards", EmployeeSqlWriter.cards employee.Cards
   "inviteToken", EmployeeSqlWriter.inviteTokenFromStatus employee.Status
   "inviteExpiration",
   EmployeeSqlWriter.inviteExpirationFromStatus employee.Status
   "authProviderUserId",
   EmployeeSqlWriter.authProviderUserId employee.AuthProviderUserId
]

let upsertReadModels
   (employees: Employee list, employeeEvents: EmployeeEvent list)
   =
   let employeeSqlParams = employees |> List.map sqlParamsFromEmployee
   let employeeMap = [ for e in employees -> e.EmployeeId, e ] |> Map.ofList

   let sqlParams =
      employeeEvents
      |> List.sortByDescending (EmployeeEnvelope.unwrap >> snd >> _.Timestamp)
      |> List.fold (sqlParamReducer employeeMap) {
         EmployeeEvent = []
         Card = []
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
          {EmployeeFields.cards},
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
          @cards,
          @status::{EmployeeTypeCast.status},
          @statusDetail,
          @inviteToken,
          @inviteExpiration,
          @authProviderUserId)
      ON CONFLICT ({EmployeeFields.employeeId})
      DO UPDATE SET
         {EmployeeFields.status} = @status::{EmployeeTypeCast.status},
         {EmployeeFields.statusDetail} = @statusDetail,
         {EmployeeFields.cards} = @cards,
         {EmployeeFields.role} = @role::{EmployeeTypeCast.role},
         {EmployeeFields.inviteToken} = @inviteToken,
         {EmployeeFields.inviteExpiration} = @inviteExpiration,
         {EmployeeFields.authProviderUserId} = @authProviderUserId;
      """,
      employeeSqlParams

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
      DO UPDATE SET
         {CardFields.status} = @status::{CardTypeCast.status},
         {CardFields.dailyPurchaseLimit} = @dailyPurchaseLimit,
         {CardFields.monthlyPurchaseLimit} = @monthlyPurchaseLimit,
         {CardFields.lastPurchaseAt} = @lastPurchaseAt,
         {CardFields.cardNickname} = @cardNickname;
      """,
      sqlParams.Card
   ]

   pgTransaction query

let initProps
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   (chunking: StreamChunkingEnvConfig)
   (restartSettings: Akka.Streams.RestartSettings)
   (retryPersistenceAfter: TimeSpan)
   =
   actorProps<Employee, EmployeeEvent> {
      GetAggregateIdFromEvent =
         EmployeeEnvelope.unwrap >> snd >> _.EntityId >> EntityId.get
      GetAggregate =
         fun employeeId -> task {
            let aref = getEmployeeRef (EmployeeId employeeId)
            let! (opt: Employee option) = aref <? EmployeeMessage.GetEmployee
            return opt
         }
      Chunking = chunking
      RestartSettings = restartSettings
      RetryPersistenceAfter = retryPersistenceAfter
      UpsertReadModels = upsertReadModels
      EventJournalTag = Constants.AKKA_EMPLOYEE_JOURNAL
   }
