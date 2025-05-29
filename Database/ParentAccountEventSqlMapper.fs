module ParentAccountEventSqlMapper

open System

open Bank.Account.Domain
open Lib.SharedTypes
open AccountSqlMapper
open OrganizationSqlMapper

let table = "parent_account_event"

module Fields =
   let eventId = "event_id"
   let parentAccountId = AccountFields.parentAccountId
   let orgId = OrgFields.orgId
   let correlationId = "correlation_id"
   let initiatedById = EmployeeEventSqlMapper.EmployeeEventFields.initiatedById
   let name = "name"
   let timestamp = "timestamp"
   let event = "event"

module SqlReader =
   let eventId (read: RowReader) = Fields.eventId |> read.uuid |> EventId

   let parentAccountId = AccountSqlReader.parentAccountId

   let orgId = OrgSqlReader.orgId

   let correlationId (read: RowReader) =
      Fields.correlationId |> read.uuid |> CorrelationId

   let initiatedById =
      EmployeeEventSqlMapper.EmployeeEventSqlReader.initiatedById

   let name (read: RowReader) = read.text Fields.name

   let timestamp (read: RowReader) = read.dateTime Fields.timestamp

   let event (read: RowReader) =
      read.text Fields.event
      |> Serialization.deserializeUnsafe<ParentAccountEvent>

module SqlWriter =
   let eventId (evtId: EventId) =
      let (EventId id) = evtId
      Sql.uuid id

   let correlationId (corrId: CorrelationId) =
      let (CorrelationId id) = corrId
      Sql.uuid id

   let initiatedById =
      EmployeeEventSqlMapper.EmployeeEventSqlWriter.initiatedById

   let initiatedByIds =
      EmployeeEventSqlMapper.EmployeeEventSqlWriter.initiatedByIds

   let parentAccountId = AccountSqlWriter.parentAccountId

   let orgId = OrgSqlWriter.orgId
   let name = Sql.text

   let timestamp (date: DateTime) = Sql.timestamptz date

   let event (evt: ParentAccountEvent) =
      Sql.jsonb <| Serialization.serialize evt
