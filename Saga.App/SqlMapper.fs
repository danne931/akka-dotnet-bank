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
      read.int Fields.activityInProgressCount

   let activityAttemptsExhaustedCount (read: RowReader) =
      read.int Fields.activityAttemptsExhaustedCount

   let inactivityTimeout (read: RowReader) =
      read.intervalOrNone Fields.inactivityTimeout

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
      let inProgressOrExhausted =
         if (saga :> ISaga).ExhaustedAllAttempts then
            "Exhausted"
         else
            "InProgress"

      let compensatingOrFailed =
         let s = saga :> ISaga

         if s.ActivityInProgressCount > 0 then
            if s.ExhaustedAllAttempts then
               "CompensationExhausted"
            else
               "Compensating"
         else
            "Failed"

      let status =
         match saga with
         | AppSaga.Saga.OrgOnboarding s ->
            match s.Status with
            | OrgOnboardingSagaStatus.InProgress -> inProgressOrExhausted
            | OrgOnboardingSagaStatus.Completed -> "Completed"
            | OrgOnboardingSagaStatus.Failed _ -> compensatingOrFailed
         | AppSaga.Saga.EmployeeOnboarding s ->
            match s.Status with
            | EmployeeOnboardingSagaStatus.InProgress -> inProgressOrExhausted
            | EmployeeOnboardingSagaStatus.Failed _ -> compensatingOrFailed
            | EmployeeOnboardingSagaStatus.Completed -> "Completed"
            | EmployeeOnboardingSagaStatus.Aborted _ -> "Aborted"
         | AppSaga.Saga.CardSetup s ->
            match s.Status with
            | CardSetupSagaStatus.InProgress -> inProgressOrExhausted
            | CardSetupSagaStatus.Completed -> "Completed"
            | CardSetupSagaStatus.Failed _ -> compensatingOrFailed
         | AppSaga.Saga.Purchase s ->
            match s.Status with
            | PurchaseSagaStatus.InProgress -> inProgressOrExhausted
            | PurchaseSagaStatus.Completed -> "Completed"
            | PurchaseSagaStatus.Failed _ -> compensatingOrFailed
         | AppSaga.Saga.DomesticTransfer s ->
            match s.Status with
            | DomesticTransferProgress.Scheduled -> "Scheduled"
            | DomesticTransferProgress.ProcessingSenderAccountDeduction
            | DomesticTransferProgress.WaitingForTransferServiceAck
            | DomesticTransferProgress.InProgress _ -> inProgressOrExhausted
            | DomesticTransferProgress.Completed -> "Completed"
            | DomesticTransferProgress.Failed _ -> compensatingOrFailed
         | AppSaga.Saga.PlatformTransfer s ->
            match s.Status with
            | PlatformTransferSagaStatus.Scheduled -> "Scheduled"
            | PlatformTransferSagaStatus.InProgress -> inProgressOrExhausted
            | PlatformTransferSagaStatus.Completed -> "Completed"
            | PlatformTransferSagaStatus.Failed _ -> compensatingOrFailed
         | AppSaga.Saga.PlatformPayment s ->
            match s.Status with
            | PlatformPaymentSagaStatus.InProgress _ -> inProgressOrExhausted
            | PlatformPaymentSagaStatus.Completed -> "Completed"
            | PlatformPaymentSagaStatus.Failed _ -> compensatingOrFailed
         | AppSaga.Saga.Billing s ->
            match s.Status with
            | BillingSagaStatus.InProgress -> inProgressOrExhausted
            | BillingSagaStatus.Completed -> "Completed"
            | BillingSagaStatus.Failed -> compensatingOrFailed

      Sql.string status

   let activityInProgressCount (saga: ISaga) =
      Sql.int saga.ActivityInProgressCount

   let activityAttemptsExhaustedCount (saga: ISaga) =
      Sql.int saga.ActivityAttemptsExhaustedCount

   let inactivityTimeout (saga: ISaga) =
      Sql.intervalOrNone saga.InactivityTimeout

   let sagaState (saga: AppSaga.Saga) =
      saga |> Serialization.serialize |> Sql.jsonb
