module OrganizationEventSqlMapper

open System

open Bank.Org.Domain
open Lib.SharedTypes
open OrganizationSqlMapper

let table = "organization_event"

module OrgEventFields =
   let eventId = "event_id"
   let orgId = OrgFields.orgId
   let correlationId = "correlation_id"
   let initiatedById = "initiated_by_id"
   let name = "name"
   let timestamp = "timestamp"
   let event = "event"

module OrgEventSqlReader =
   let eventId (read: RowReader) =
      OrgEventFields.eventId |> read.uuid |> EventId

   let orgId = OrgSqlReader.orgId

   let correlationId (read: RowReader) =
      OrgEventFields.correlationId |> read.uuid |> CorrelationId

   let initiatedById (read: RowReader) =
      OrgEventFields.initiatedById |> read.uuid |> EmployeeId |> InitiatedById

   let name (read: RowReader) = read.text OrgEventFields.name

   let timestamp (read: RowReader) = read.dateTime OrgEventFields.timestamp

   let event (read: RowReader) =
      read.text OrgEventFields.event
      |> Serialization.deserializeUnsafe<OrgEvent>

module OrgEventSqlWriter =
   let eventId (EventId id) = Sql.uuid id

   let correlationId (CorrelationId id) = Sql.uuid id

   let initiatedById (id: InitiatedById) = id |> _.Value |> Sql.uuid

   let initiatedByIds (ids: InitiatedById list) =
      ids |> List.map _.Value |> List.toArray |> Sql.uuidArray

   let orgId = OrgSqlWriter.orgId
   let name = Sql.text
   let timestamp (date: DateTime) = Sql.timestamptz date

   let event (evt: OrgEvent) =
      Sql.jsonb <| Serialization.serialize evt
