module RecurringPaymentScheduleSqlMapper

open RecurringPaymentSchedule

let table = "recurring_payment_schedule"

module Typecast =
   let terminationType = "recurrence_termination_type"

module Fields =
   let id = "recurring_payment_schedule_id"
   let orgId = "org_id"
   let pattern = "pattern_detail"
   let terminationType = "termination_type"
   let terminationDetail = "termination_detail"
   let paymentsRequestedCount = "payments_requested_count"

module Reader =
   let id (read: RowReader) =
      read.uuid Fields.id |> RecurrenceScheduleId

   let orgId (read: RowReader) =
      OrganizationSqlMapper.OrgSqlReader.orgId read

   let pattern (read: RowReader) : RecurrencePattern =
      read.text Fields.pattern
      |> Serialization.deserializeUnsafe<RecurrencePattern>

   let termination (read: RowReader) : RecurrenceTerminationCondition =
      read.text Fields.terminationDetail
      |> Serialization.deserializeUnsafe<RecurrenceTerminationCondition>

   let paymentsRequestedCount (read: RowReader) =
      read.int Fields.paymentsRequestedCount

   let recurrenceSettings (read: RowReader) : RecurrenceSettings = {
      Id = id read
      Pattern = pattern read
      Termination = termination read
      PaymentsRequestedCount = paymentsRequestedCount read
   }

module Writer =
   let id (RecurrenceScheduleId id) = Sql.uuid id

   let orgId = OrganizationSqlMapper.OrgSqlWriter.orgId

   let pattern (pattern: RecurrencePattern) =
      Serialization.serialize pattern |> Sql.jsonb

   let terminationType (termination: RecurrenceTerminationCondition) =
      match termination with
      | RecurrenceTerminationCondition.EndDate _ -> Sql.string "EndDate"
      | RecurrenceTerminationCondition.MaxPayments _ -> Sql.string "MaxPayments"
      | RecurrenceTerminationCondition.Never -> Sql.string "Never"

   let termination (termination: RecurrenceTerminationCondition) =
      Serialization.serialize termination |> Sql.jsonb

   let paymentsRequestedCount = Sql.int
