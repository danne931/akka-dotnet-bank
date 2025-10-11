module Bank.Employee.Api

open Akkling
open FsToolkit.ErrorHandling
open Validus
open System

open Lib.Postgres
open Lib.SharedTypes
open Bank.Employee.Domain
open BankActorRegistry
open Email

module AccountFields = AccountSqlMapper.AccountFields
module CardFields = CardSqlMapper.CardFields
module Fields = EmployeeSqlMapper.EmployeeFields
module Reader = EmployeeSqlMapper.EmployeeSqlReader
module Writer = EmployeeSqlMapper.EmployeeSqlWriter
module Typecast = EmployeeSqlMapper.EmployeeTypeCast
let table = EmployeeSqlMapper.table

let searchEmployees (orgId: OrgId) (searchQuery: string) = taskResultOption {
   let query =
      $$"""
      WITH top_employees AS (
         SELECT * FROM {{table}}
         WHERE
            {{Fields.orgId}} = @orgId
            AND {{Fields.searchQuery}} %> @query
         ORDER BY {{Fields.searchQuery}} <-> @query DESC
         LIMIT 10
      )
      SELECT
         e.*,
         c.{{CardFields.cardId}},
         c.{{CardFields.accountId}},
         c.{{CardFields.cardType}},
         c.{{CardFields.isVirtual}},
         c.{{CardFields.statusDetail}},
         c.{{CardFields.expYear}},
         c.{{CardFields.expMonth}},
         c.{{CardFields.lastPurchaseAt}},
         c.{{CardFields.cardNumberLast4}},
         c.{{CardFields.cardNickname}},
         c.{{CardFields.dailyPurchaseLimit}},
         c.{{CardFields.monthlyPurchaseLimit}}
      FROM top_employees e
      LEFT JOIN {{CardSqlMapper.table}} c USING({{Fields.employeeId}})
      """

   let! result =
      pgQuery<Employee * (Card option)>
         query
         (Some [
            "orgId", Writer.orgId orgId
            "query", Writer.searchQuery searchQuery
         ])
         (fun read ->
            Reader.employee read,
            read.uuidOrNone CardFields.cardId
            |> Option.map (fun _ -> CardSqlMapper.CardSqlReader.card read))

   return
      result
      |> List.groupBy (fst >> _.EmployeeId)
      |> List.map (fun (_, employeesAndCards) ->
         let cards =
            employeesAndCards
            |> List.choose snd
            |> List.map (fun c -> c.CardId, c)
            |> Map.ofList

         let employee = fst employeesAndCards.Head

         { employee with Cards = cards })
}

let processCommand
   (registry: #IEmployeeGuaranteedDeliveryActor)
   (command: EmployeeCommand)
   =
   taskResult {
      let validation =
         match command with
         | EmployeeCommand.CreateEmployee cmd ->
            CreateEmployeeCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
         | EmployeeCommand.CreateCard cmd ->
            CreateCardCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
         | EmployeeCommand.ConfigureRollingPurchaseLimit cmd ->
            ConfigureRollingPurchaseLimitCommand.toEvent cmd
            |> Result.map EmployeeEnvelope.get
         | EmployeeCommand.LockCard cmd ->
            LockCardCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
         | EmployeeCommand.EditCardNickname cmd ->
            EditCardNicknameCommand.toEvent cmd
            |> Result.map EmployeeEnvelope.get
         | EmployeeCommand.CancelInvitation cmd ->
            CancelInvitationCommand.toEvent cmd
            |> Result.map EmployeeEnvelope.get
         | EmployeeCommand.RefreshInvitationToken cmd ->
            RefreshInvitationTokenCommand.toEvent cmd
            |> Result.map EmployeeEnvelope.get
         | EmployeeCommand.ConfirmInvitation cmd ->
            ConfirmInvitationCommand.toEvent cmd
            |> Result.map EmployeeEnvelope.get
         | EmployeeCommand.RestoreAccess cmd ->
            RestoreAccessCommand.toEvent cmd |> Result.map EmployeeEnvelope.get
         | cmd ->
            Error
            <| ValidationErrors.create "" [
               $"API-based command processing not allowed for {cmd}"
            ]

      let! envelope = validation |> Result.mapError Err.ValidationError

      let msg =
         GuaranteedDelivery.message
            envelope.EntityId.Value
            (EmployeeMessage.StateChange command)

      registry.EmployeeGuaranteedDeliveryActor() <! msg

      return envelope
   }

let getEmployee (id: EmployeeId) =
   pgQuerySingle<Employee>
      $"SELECT * FROM {table}
        WHERE {Fields.employeeId} = @id"
      (Some [ "id", Writer.employeeId id ])
      Reader.employee

let private employeeReadyForInviteConfirmation
   (em: Employee)
   : EmployeePendingInviteConfirmation option
   =
   match em.Status with
   | EmployeeStatus.PendingInviteConfirmation info when
      not (info.Token.IsExpired())
      ->
      Some {
         Email = em.Email
         Name = em.Name
         InviteConfirmation = info
         EmployeeId = em.EmployeeId
         OrgId = em.OrgId
      }
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
            $"{where} AND {Fields.role} = ANY(SELECT UNNEST(@roles)::{Typecast.role})")
         agg
         query.Roles

   let agg =
      Option.fold
         (fun (queryParams, where) status ->
            [ "status", Writer.status status ] @ queryParams,
            $"{where} AND {Fields.status} = @status::{Typecast.status}")
         agg
         query.Status

   let queryParams, where = agg

   let query =
      $"SELECT * FROM {table}
        WHERE {where}
        ORDER BY {Fields.firstName} || {Fields.lastName}"

   pgQuery<Employee> query (Some queryParams) Reader.employee

let getDemoUserSessions (orgId: OrgId) =
   let query =
      $"SELECT
           {Fields.employeeId},
           {Fields.orgId},
           {Fields.firstName},
           {Fields.lastName},
           {Fields.email},
           {Fields.role},
           {Fields.status}
        FROM {table}
        WHERE
           {Fields.orgId} = @orgId
           AND {Fields.status} = @activeStatus::{Typecast.status}
        ORDER BY {Fields.firstName} || {Fields.lastName}"

   pgQuery<UserSession>
      query
      (Some [
         "orgId", Writer.orgId orgId
         "activeStatus", Writer.status EmployeeStatus.Active
      ])
      (fun read -> {
         EmployeeId = Reader.employeeId read
         OrgId = Reader.orgId read
         Name = Reader.firstName read + " " + Reader.lastName read
         Email = Reader.email read
         Role = Reader.role read
      })
