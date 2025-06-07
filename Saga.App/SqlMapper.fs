module AppSagaSqlMapper

open Lib.SharedTypes
open Lib.Saga
open PurchaseSaga
open DomesticTransferSaga
open PlatformPaymentSaga
open PlatformTransferSaga
open OrgOnboardingSaga
open EmployeeOnboardingSaga
open CardSetupSaga
open BillingSaga
open Bank.Transfer.Domain

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

   let sagaState (read: RowReader) =
      Fields.sagaState
      |> read.text
      |> Serialization.deserializeUnsafe<AppSaga.Saga>

   let activityInProgressCount (read: RowReader) =
      Fields.activityInProgressCount |> read.int

   let activityAttemptsExhaustedCount (read: RowReader) =
      Fields.activityAttemptsExhaustedCount |> read.int

   let inactivityTimeout (read: RowReader) =
      Fields.inactivityTimeout |> read.intervalOrNone

   let createdAt (read: RowReader) = read.dateTime Fields.createdAt
   let updatedAt (read: RowReader) = read.dateTime Fields.updatedAt

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
         | AppSaga.Saga.PlatformPayment _ -> "PlatformPayment"
         | AppSaga.Saga.Billing _ -> "BillingStatement"

      Sql.string name

   let status (saga: AppSaga.Saga) =
      let status =
         match saga with
         | AppSaga.Saga.OrgOnboarding s ->
            match s.Status with
            | OrgOnboardingSagaStatus.InProgress -> "InProgress"
            | OrgOnboardingSagaStatus.Completed -> "Completed"
            | OrgOnboardingSagaStatus.Failed _ -> "Failed"
         | AppSaga.Saga.EmployeeOnboarding s ->
            match s.Status with
            | EmployeeOnboardingSagaStatus.InProgress -> "InProgress"
            | EmployeeOnboardingSagaStatus.Failed _ -> "Failed"
            | EmployeeOnboardingSagaStatus.Completed -> "Completed"
            | EmployeeOnboardingSagaStatus.Aborted _ -> "Aborted"
         | AppSaga.Saga.CardSetup s ->
            match s.Status with
            | CardSetupSagaStatus.InProgress -> "InProgress"
            | CardSetupSagaStatus.Completed -> "Completed"
            | CardSetupSagaStatus.Failed _ -> "Failed"
         | AppSaga.Saga.Purchase s ->
            match s.Status with
            | PurchaseSagaStatus.InProgress -> "InProgress"
            | PurchaseSagaStatus.Completed -> "Completed"
            | PurchaseSagaStatus.Failed _ -> "Failed"
         | AppSaga.Saga.DomesticTransfer s ->
            match s.Status with
            | DomesticTransferProgress.Scheduled -> "Scheduled"
            | DomesticTransferProgress.ProcessingSenderAccountDeduction
            | DomesticTransferProgress.WaitingForTransferServiceAck
            | DomesticTransferProgress.InProgress _ -> "InProgress"
            | DomesticTransferProgress.Completed -> "Completed"
            | DomesticTransferProgress.Failed _ -> "Failed"
         | AppSaga.Saga.PlatformTransfer s ->
            match s.Status with
            | PlatformTransferSagaStatus.Scheduled -> "Scheduled"
            | PlatformTransferSagaStatus.InProgress -> "InProgress"
            | PlatformTransferSagaStatus.Completed -> "Completed"
            | PlatformTransferSagaStatus.Failed _ -> "Failed"
         | AppSaga.Saga.PlatformPayment s ->
            match s.Status with
            | PlatformPaymentSagaStatus.InProgress _ -> "InProgress"
            | PlatformPaymentSagaStatus.Completed -> "Completed"
            | PlatformPaymentSagaStatus.Failed _ -> "Failed"
         | AppSaga.Saga.Billing s ->
            match s.Status with
            | BillingSagaStatus.InProgress -> "InProgress"
            | BillingSagaStatus.Completed -> "Completed"
            | BillingSagaStatus.Failed -> "Failed"

      Sql.string status

   let activityInProgressCount (saga: ISaga) =
      saga.ActivityInProgressCount |> Sql.int

   let activityAttemptsExhaustedCount (saga: ISaga) =
      saga.ActivityAttemptsExhaustedCount |> Sql.int

   let inactivityTimeout (saga: ISaga) =
      Sql.intervalOrNone saga.InactivityTimeout

   let sagaState (saga: AppSaga.Saga) =
      saga |> Serialization.serialize |> Sql.jsonb
