module AppSagaSqlMapper

open System

open Lib.SharedTypes
open Lib.Saga
open SagaDTO

let table = "saga"

module TypeCast =
   let name = "saga_type"
   let status = "saga_status"

module Fields =
   let id = "id"
   let orgId = "org_id"
   let name = "name"
   let status = "status"
   let sagaState = "saga_state"
   let activityInProgressCount = "activity_in_progress_count"
   let activityAttemptsExhaustedCount = "activity_attempts_exhausted_count"
   let inactivityTimeout = "inactivity_timeout"
   let createdAt = "created_at"
   let updatedAt = "updated_at"

module Reader =
   let id (read: RowReader) = Fields.id |> read.uuid |> CorrelationId

   let orgId (read: RowReader) = Fields.orgId |> read.uuid |> OrgId

   let activityInProgressCount (read: RowReader) =
      read.int Fields.activityInProgressCount

   let activityAttemptsExhaustedCount (read: RowReader) =
      read.int Fields.activityAttemptsExhaustedCount

   let inactivityTimeout (read: RowReader) =
      read.intervalOrNone Fields.inactivityTimeout

   let createdAt (read: RowReader) = read.dateTime Fields.createdAt
   let updatedAt (read: RowReader) = read.dateTime Fields.updatedAt

   let status (read: RowReader) =
      read.string Fields.status |> SagaDTOStatus.fromString |> _.Value

   let sagaState (read: RowReader) =
      Fields.sagaState
      |> read.text
      |> Serialization.deserializeUnsafe<AppSaga.Saga>

   let sagaDTO (read: RowReader) =
      let dto = (sagaState read).AsDTO
      // Overwrite sagaDTO.StartedAt property (time when saga started in memory)
      // with postgres value of time when read model was created.
      // Pagination relies on using created_at postgres field as a cursor.
      // TODO: Consider storing sagaDTO.StartedAt in the postgres read model
      //       as its own field or in place of the automatically assigned
      //       created_at field & using that as the pagination cursor instead.
      { dto with StartedAt = createdAt read }

module Writer =
   let id (CorrelationId id) = Sql.uuid id
   let orgId (OrgId id) = Sql.uuid id

   let name (saga: AppSaga.Saga) =
      let name =
         match saga with
         | AppSaga.Saga.OrgOnboarding _ -> "OrgOnboarding"
         | AppSaga.Saga.EmployeeOnboarding _ -> "EmployeeOnboarding"
         | AppSaga.Saga.CardSetup _ -> "CardSetup"
         | AppSaga.Saga.Purchase _ -> "Purchase"
         | AppSaga.Saga.DomesticTransfer _ -> "DomesticTransfer"
         | AppSaga.Saga.PlatformTransfer _ -> "PlatformTransfer"
         | AppSaga.Saga.PaymentRequest _ -> "PaymentRequest"
         | AppSaga.Saga.Billing _ -> "BillingStatement"

      Sql.string name

   let fromSagaDTOStatus (sagaDTOStatus: SagaDTOStatus) =
      match sagaDTOStatus with
      | SagaDTOStatus.Scheduled -> "Scheduled"
      | SagaDTOStatus.InProgress -> "InProgress"
      | SagaDTOStatus.Completed -> "Completed"
      | SagaDTOStatus.Compensating -> "Compensating"
      | SagaDTOStatus.Failed -> "Failed"
      | SagaDTOStatus.Aborted -> "Aborted"
      | SagaDTOStatus.Exhausted -> "Exhausted"
      | SagaDTOStatus.CompensationExhausted -> "CompensationExhausted"

   let nameFromKind =
      function
      | SagaKind.OrgOnboarding -> "OrgOnboarding"
      | SagaKind.EmployeeOnboarding -> "EmployeeOnboarding"
      | SagaKind.CardSetup -> "CardSetup"
      | SagaKind.Purchase -> "Purchase"
      | SagaKind.DomesticTransfer -> "DomesticTransfer"
      | SagaKind.PlatformTransfer -> "PlatformTransfer"
      | SagaKind.PaymentRequest -> "PaymentRequest"
      | SagaKind.BillingStatement -> "BillingStatement"

   let status (saga: AppSaga.Saga) = Sql.string (string saga.Status)

   let activityInProgressCount (saga: ISaga) =
      Sql.int saga.ActivityInProgressCount

   let activityAttemptsExhaustedCount (saga: ISaga) =
      Sql.int saga.ActivityAttemptsExhaustedCount

   let inactivityTimeout (saga: ISaga) =
      Sql.intervalOrNone saga.InactivityTimeout

   let sagaState (saga: AppSaga.Saga) =
      saga |> Serialization.serialize |> Sql.jsonb

   let timestamp (date: DateTime) = Sql.timestamptz date
