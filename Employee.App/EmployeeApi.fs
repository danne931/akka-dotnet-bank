module Bank.Employee.Api

open Akkling
open Akka.Actor
open FsToolkit.ErrorHandling
open Validus
open System

open Lib.Postgres
open Lib.SharedTypes
open Bank.Employee.Domain
open Lib.NetworkQuery

module AccountFields = AccountSqlMapper.AccountFields
module Fields = EmployeeSqlMapper.EmployeeFields
module Reader = EmployeeSqlMapper.EmployeeSqlReader
module Writer = EmployeeSqlMapper.EmployeeSqlWriter
let table = EmployeeSqlMapper.table

let searchEmployees (orgId: OrgId) (searchQuery: string) =
   pgQuery<Employee>
      $$"""
      SELECT * FROM {{table}}
      WHERE
         {{Fields.orgId}} = @orgId
         AND {{Fields.searchQuery}} %> @query
      ORDER BY {{Fields.searchQuery}} <-> @query DESC
      LIMIT 10
      """
      (Some [
         "orgId", Writer.orgId orgId
         "query", Writer.searchQuery searchQuery
      ])
      Reader.employee

let processCommand (system: ActorSystem) (command: EmployeeCommand) = taskResult {
   let validation =
      match command with
      | EmployeeCommand.CreateEmployee cmd ->
         CreateEmployeeCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
      | EmployeeCommand.CreateCard cmd ->
         CreateCardCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
      | EmployeeCommand.PurchasePending cmd ->
         PurchasePendingCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
      | EmployeeCommand.LimitDailyDebits cmd ->
         LimitDailyDebitsCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
      | EmployeeCommand.LimitMonthlyDebits cmd ->
         LimitMonthlyDebitsCommand.toEvent cmd
         |> Result.map EmployeeEnvelope.get
      | EmployeeCommand.LockCard cmd ->
         LockCardCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
      | EmployeeCommand.UnlockCard cmd ->
         UnlockCardCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
      | EmployeeCommand.EditCardNickname cmd ->
         EditCardNicknameCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
      | EmployeeCommand.CancelInvitation cmd ->
         CancelInvitationCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
      | EmployeeCommand.RefreshInvitationToken cmd ->
         RefreshInvitationTokenCommand.toEvent cmd
         |> Result.map EmployeeEnvelope.get
      | EmployeeCommand.ConfirmInvitation cmd ->
         ConfirmInvitationCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
      | EmployeeCommand.RestoreAccess cmd ->
         RestoreAccessCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
      | cmd ->
         Error
         <| ValidationErrors.create "" [
            $"API-based command processing not allowed for {cmd}"
         ]

   let! res = validation |> Result.mapError Err.ValidationError

   let actorRef =
      EmployeeActor.get system (EmployeeId.fromEntityId res.EntityId)

   actorRef <! EmployeeMessage.StateChange command

   return res
}

let getEmployee (id: EmployeeId) =
   pgQuerySingle<Employee>
      $"SELECT * FROM {table}
        WHERE {Fields.employeeId} = @id"
      (Some [ "id", Writer.employeeId id ])
      Reader.employee

let employeeReadyForInviteConfirmation
   (em: Employee)
   : (Employee * InviteToken) option
   =
   match em.Status with
   | EmployeeStatus.PendingInviteConfirmation token when not (token.IsExpired()) ->
      Some(em, token)
   | _ -> None

let getEmployeeByInviteToken (token: Guid) = taskResult {
   let! employee =
      pgQuerySingle<Employee>
         $"SELECT * FROM {table}
           WHERE {Fields.inviteToken} = @inviteToken"
         (Some [ "inviteToken", Writer.inviteToken (Some token) ])
         Reader.employee

   return Option.bind employeeReadyForInviteConfirmation employee
}

let getEmployeeByEmail (email: Email) =
   pgQuerySingle<Employee>
      $"SELECT * FROM {table}
        WHERE {Fields.email} = @email"
      (Some [ "email", Writer.email email ])
      Reader.employee

let getEmployeeInviteByEmail (email: Email) = taskResult {
   let! employee = getEmployeeByEmail email
   return Option.bind employeeReadyForInviteConfirmation employee
}

let getEmployees (orgId: OrgId) (query: EmployeeQuery) =
   let agg = [ "orgId", Writer.orgId orgId ], $"{Fields.orgId} = @orgId"

   let agg =
      Option.fold
         (fun (queryParams, where) employeeIds ->
            [ "eIds", Writer.employeeIds employeeIds ] @ queryParams,
            $"{where} AND {Fields.employeeId} = ANY(@eIds)")
         agg
         query.EmployeeIds

   let agg =
      Option.fold
         (fun (queryParams, where) roles ->
            [ "roles", Writer.roles roles ] @ queryParams,
            $"{where} AND {Fields.role} = ANY(SELECT UNNEST(@roles)::{EmployeeSqlMapper.EmployeeTypeCast.role})")
         agg
         query.Roles

   let agg =
      Option.fold
         (fun (queryParams, where) status ->
            [ "status", Writer.status status ] @ queryParams,
            $"{where} AND {Fields.status} = @status::{EmployeeSqlMapper.EmployeeTypeCast.status}")
         agg
         query.Status

   let queryParams, where = agg

   let query =
      $"SELECT * FROM {table}
        WHERE {where}
        ORDER BY {Fields.firstName} || {Fields.lastName}"

   pgQuery<Employee> query (Some queryParams) Reader.employee

module Fields = CardSqlMapper.CardFields
module Reader = CardSqlMapper.CardSqlReader
module Writer = CardSqlMapper.CardSqlWriter

let getCards (orgId: OrgId) (query: CardQuery) =
   let table = CardSqlMapper.table
   let employeeTable = EmployeeSqlMapper.table

   let dpaView =
      TransactionSqlMapper.TransactionViews.dailyPurchaseAccruedByCard

   let mpaView =
      TransactionSqlMapper.TransactionViews.monthlyPurchaseAccruedByCard

   let agg = [ "orgId", Writer.orgId orgId ], $"{table}.{Fields.orgId} = @orgId"

   let agg =
      Option.fold
         (fun (queryParams, where) ids ->
            [ "eIds", Writer.employeeIds ids ] @ queryParams,
            $"{where} AND {table}.{Fields.employeeId} = ANY(@eIds)")
         agg
         query.EmployeeIds

   let agg =
      Option.fold
         (fun (queryParams, where) ids ->
            [ "aIds", Writer.accountIds ids ] @ queryParams,
            $"{where} AND {table}.{Fields.accountId} = ANY(@aIds)")
         agg
         query.AccountIds

   let agg =
      Option.fold
         (fun (queryParams, where) (startDate, endDate) ->
            [
               "start", Writer.createdAt startDate
               "end", Writer.createdAt endDate
            ]
            @ queryParams,
            $"{where} AND {table}.{Fields.createdAt} >= @start 
              AND {table}.{Fields.createdAt} <= @end")
         agg
         query.CreatedAtDateRange

   let monthlyAccrued = $"{mpaView}.amount_accrued"

   let agg =
      Option.fold
         (fun (queryParams, where) amountFilter ->
            let where, amountParams =
               match amountFilter with
               | AmountFilter.LessThanOrEqualTo max ->
                  $"{where} AND {monthlyAccrued} <= @max",
                  [ "max", Sql.decimal max ]
               | AmountFilter.GreaterThanOrEqualTo min ->
                  $"{where} AND {monthlyAccrued} >= @min",
                  [ "min", Sql.decimal min ]
               | AmountFilter.Between(min, max) ->
                  $"{where} AND {monthlyAccrued} >= @min AND {monthlyAccrued} <= @max",
                  [ "min", Sql.decimal min; "max", Sql.decimal max ]

            amountParams @ queryParams, where)
         agg
         query.Amount

   let queryParams, where = agg

   let query =
      $"SELECT
           {table}.{Fields.cardNumberLast4},
           {table}.{Fields.cardNickname},
           {table}.{Fields.dailyPurchaseLimit},
           {table}.{Fields.monthlyPurchaseLimit},
           {table}.{Fields.cardId},
           {table}.{Fields.accountId},
           {table}.{Fields.cardType},
           {table}.{Fields.isVirtual},
           {table}.{Fields.status},
           {table}.{Fields.expYear},
           {table}.{Fields.expMonth},
           {table}.{Fields.lastPurchaseAt},
           {mpaView}.amount_accrued as mpa,
           {dpaView}.amount_accrued as dpa,
           {employeeTable}.*
        FROM {table}
        JOIN {employeeTable} using({Fields.employeeId})
        LEFT JOIN {dpaView} using({Fields.cardId})
        LEFT JOIN {mpaView} using({Fields.cardId})
        WHERE {where}
        ORDER BY {table}.{Fields.lastPurchaseAt} desc"

   pgQuery<CardWithMetrics> query (Some queryParams) (fun read -> {
      Card = Reader.card read
      DailyPurchaseAccrued = read.decimalOrNone "dpa" |> Option.defaultValue 0m
      MonthlyPurchaseAccrued =
         read.decimalOrNone "mpa" |> Option.defaultValue 0m
      Employee = Reader.employee read
   })

let getDemoUserSessions (orgId: OrgId) =
   let query =
      $"SELECT
           {Fields.employeeId},
           {Fields.orgId},
           {Fields.firstName},
           {Fields.lastName},
           {Fields.email},
           {Fields.role}
        FROM {table}
        WHERE {Fields.orgId} = @orgId
        ORDER BY {Fields.firstName} || {Fields.lastName}"

   pgQuery<UserSession>
      query
      (Some [ "orgId", Writer.orgId orgId ])
      (fun read -> {
         EmployeeId = Reader.employeeId read
         OrgId = Reader.orgId read
         FirstName = Reader.firstName read
         LastName = Reader.lastName read
         Email = Reader.email read
         Role = Reader.role read
      })
