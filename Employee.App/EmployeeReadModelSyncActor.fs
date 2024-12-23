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

type SqlParamsDerivedFromEmployeeEvents = {
   Card: (string * SqlValue) list list
   EmployeeEvent: (string * SqlValue) list list
   CommandApprovalRuleConfigured: (string * SqlValue) list list
   CommandApprovalRuleConfiguredWithAmountDailyLimit:
      (string * SqlValue) list list
   CommandApprovalRuleConfiguredWithAmountPerCommand:
      (string * SqlValue) list list
   CommandApprovalRequested: (string * SqlValue) list list
   CommandApprovalAcquired: (string * SqlValue) list list
   CommandApprovalDeclined: (string * SqlValue) list list
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
            "accountId", CardSqlWriter.accountId card.AccountId
            "orgId", CardSqlWriter.orgId employee.OrgId
            "employeeId", CardSqlWriter.employeeId employee.EmployeeId

            "cardNumberLast4",
            CardSqlWriter.cardNumberLast4 card.CardNumberLast4

            "cardType", CardSqlWriter.cardType card.CardType
            "status", CardSqlWriter.status card.Status
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
      envelope.InitiatedById |> EmployeeEventSqlWriter.initiatedById

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
   | EmployeeEvent.DebitApproved e -> cardReducer e.EntityId e.Data.Info.CardId
   | EmployeeEvent.LockedCard e -> cardReducer e.EntityId e.Data.CardId
   | EmployeeEvent.UnlockedCard e -> cardReducer e.EntityId e.Data.CardId
   | EmployeeEvent.DailyDebitLimitUpdated e ->
      cardReducer e.EntityId e.Data.CardId
   | EmployeeEvent.MonthlyDebitLimitUpdated e ->
      cardReducer e.EntityId e.Data.CardId
   | EmployeeEvent.CardNicknamed e -> cardReducer e.EntityId e.Data.CardId
   | EmployeeEvent.CommandApprovalRuleConfigured e ->
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
   | EmployeeEvent.CommandApprovalRequested e ->
      let qParams = [
         "commandId",
         CommandApprovalProgressId e.CorrelationId
         |> CommandApprovalProgressSqlMapper.Writer.commandId

         "ruleId", CommandApprovalProgressSqlMapper.Writer.ruleId e.Data.RuleId

         "orgId", CommandApprovalProgressSqlMapper.Writer.orgId e.OrgId

         "requestedById",
         e.InitiatedById |> CommandApprovalProgressSqlMapper.Writer.requestedBy

         "status",
         CommandApprovalProgressSqlMapper.Writer.status
            CommandApprovalProgress.Status.Pending

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
   | EmployeeEvent.CommandApprovalAcquired e ->
      let qParams = [
         "commandId",
         CommandApprovalProgressId e.CorrelationId
         |> CommandApprovalProgressSqlMapper.Writer.commandId

         "approvedBy", e.Data.ApprovedBy |> InitiatedById.get |> Sql.uuid

         "expectedCurrentStatus",
         CommandApprovalProgress.Status.Pending
         |> CommandApprovalProgressSqlMapper.Writer.status
      ]

      {
         acc with
            CommandApprovalAcquired = qParams :: acc.CommandApprovalAcquired
      }
   | EmployeeEvent.CommandApprovalDeclined e ->
      let qParams = [
         "commandId",
         CommandApprovalProgressId e.CorrelationId
         |> CommandApprovalProgressSqlMapper.Writer.commandId

         "expectedCurrentStatus",
         CommandApprovalProgressSqlMapper.Writer.status
            CommandApprovalProgress.Status.Pending

         "updatedStatus",
         CommandApprovalProgressSqlMapper.Writer.status
            CommandApprovalProgress.Status.Declined

         "declinedBy",
         e.Data.DeclinedBy
         |> InitiatedById.toEmployeeId
         |> Some
         |> CommandApprovalProgressSqlMapper.Writer.declinedBy
      ]

      {
         acc with
            CommandApprovalDeclined = qParams :: acc.CommandApprovalDeclined
      }
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
   "pendingPurchases",
   EmployeeSqlWriter.pendingPurchases employee.PendingPurchases
   "onboardingTasks", EmployeeSqlWriter.onboardingTasks employee.OnboardingTasks
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
         CommandApprovalRuleConfigured = []
         CommandApprovalRuleConfiguredWithAmountDailyLimit = []
         CommandApprovalRuleConfiguredWithAmountPerCommand = []
         CommandApprovalRequested = []
         CommandApprovalAcquired = []
         CommandApprovalDeclined = []
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
          {EmployeeFields.pendingPurchases},
          {EmployeeFields.onboardingTasks},
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
          @pendingPurchases,
          @onboardingTasks,
          @inviteToken,
          @inviteExpiration,
          @authProviderUserId)
      ON CONFLICT ({EmployeeFields.employeeId})
      DO UPDATE SET
         {EmployeeFields.status} = @status::{EmployeeTypeCast.status},
         {EmployeeFields.statusDetail} = @statusDetail,
         {EmployeeFields.cards} = @cards,
         {EmployeeFields.pendingPurchases} = @pendingPurchases,
         {EmployeeFields.onboardingTasks} = @onboardingTasks,
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

      let commandId = CommandApprovalProgressSqlMapper.Fields.commandId
      let status = CommandApprovalProgressSqlMapper.Fields.status
      let statusTypecast = CommandApprovalProgressSqlMapper.TypeCast.status

      let commandTypecast =
         CommandApprovalRuleSqlMapper.TypeCast.approvableCommand

      let approvers = CommandApprovalProgressSqlMapper.Fields.approvedBy

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
             {CommandApprovalProgressSqlMapper.Fields.approvedBy},
             {CommandApprovalProgressSqlMapper.Fields.approvableCommandType},
             {CommandApprovalProgressSqlMapper.Fields.commandToInitiateOnApproval})
         VALUES
            (@commandId,
             @ruleId,
             @orgId,
             @requestedById,
             @status::{CommandApprovalProgressSqlMapper.TypeCast.status},
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
            {approvers} = {approvers} || @approvedBy
         WHERE
            {commandId} = @commandId
            AND {status} = @expectedCurrentStatus::{statusTypecast}
            AND NOT {approvers} @> ARRAY[@approvedBy::uuid]
         """,
         sqlParams.CommandApprovalAcquired

      if not sqlParams.CommandApprovalDeclined.IsEmpty then
         $"""
         UPDATE {CommandApprovalProgressSqlMapper.table}
         SET
            {status} = @updatedStatus::{statusTypecast},
            {CommandApprovalProgressSqlMapper.Fields.declinedBy} = @declinedBy
         WHERE
            {commandId} = @commandId
            AND {status} = @expectedCurrentStatus::{statusTypecast};
         """,
         sqlParams.CommandApprovalDeclined

      if not sqlParams.Card.IsEmpty then
         $"""
         INSERT into {CardSqlMapper.table}
            ({CardFields.cardId},
             {CardFields.accountId},
             {CardFields.orgId},
             {CardFields.employeeId},
             {CardFields.cardNumberLast4},
             {CardFields.cardType},
             {CardFields.status},
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
   (chunking: StreamChunking)
   (restartSettings: Akka.Streams.RestartSettings)
   (retryPersistenceAfter: TimeSpan)
   =
   actorProps<Employee, EmployeeEvent> (
      {
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
   )
