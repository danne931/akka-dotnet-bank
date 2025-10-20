module AppSagaSqlMapper

open System

open Lib.SharedTypes
open Lib.Saga
open PurchaseSaga
open DomesticTransferSaga
open PaymentRequestSaga
open PlatformTransferSaga
open OrgOnboardingSaga
open EmployeeOnboardingSaga
open CardSetupSaga
open BillingSaga
open Bank.Transfer.Domain
open AppSaga
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

   let sagaState (read: RowReader) =
      Fields.sagaState
      |> read.text
      |> Serialization.deserializeUnsafe<AppSaga.Saga>

   let sagaDTO (read: RowReader) : SagaDTO =
      let saga = sagaState read

      let activityToDTO status (a: ActivityLifeCycle<'Activity>) = {
         Start = a.Start
         End = a.End
         Name = string a.Activity |> _.Split(" ") |> _.Head()
         Attempts = a.Attempts
         MaxAttempts = a.MaxAttempts
         Status = status
      }

      let sagaActivitiesToDTO (life: SagaLifeCycle<'Activity>) =
         let inProgress =
            life.InProgress
            |> List.map (activityToDTO SagaActivityDTOStatus.InProgress)

         let completed =
            life.Completed
            |> List.map (activityToDTO SagaActivityDTOStatus.Completed)

         let failed =
            life.Failed |> List.map (activityToDTO SagaActivityDTOStatus.Failed)

         let aborted =
            life.Aborted
            |> List.map (activityToDTO SagaActivityDTOStatus.Aborted)

         inProgress @ failed @ aborted @ completed
         |> List.sortBy (fun a ->
            match a.Status with
            | SagaActivityDTOStatus.Completed -> a.End
            | _ -> Some a.Start)

      match saga with
      | Saga.Purchase saga -> {
         Name = "Purchase"
         LifeCycle = sagaActivitiesToDTO saga.LifeCycle
        }
      | Saga.DomesticTransfer saga -> {
         Name = "Domestic Transfer"
         LifeCycle = sagaActivitiesToDTO saga.LifeCycle
        }
      | Saga.PlatformTransfer saga -> {
         Name = "Platform Transfer"
         LifeCycle = sagaActivitiesToDTO saga.LifeCycle
        }
      | Saga.PaymentRequest saga -> {
         Name = "Payment Request"
         LifeCycle = sagaActivitiesToDTO saga.LifeCycle
        }
      | Saga.Billing saga -> {
         Name = "Billing"
         LifeCycle = sagaActivitiesToDTO saga.LifeCycle
        }
      | Saga.CardSetup saga -> {
         Name = "Card Setup"
         LifeCycle = sagaActivitiesToDTO saga.LifeCycle
        }
      | Saga.EmployeeOnboarding saga -> {
         Name = "Employee Onboarding"
         LifeCycle = sagaActivitiesToDTO saga.LifeCycle
        }
      | Saga.OrgOnboarding saga -> {
         Name = "Organization Onboarding"
         LifeCycle = sagaActivitiesToDTO saga.LifeCycle
        }

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
            | EmployeeOnboardingSagaStatus.Failed -> compensatingOrFailed
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
            | DomesticTransferProgress.ProcessingAccountDeduction
            | DomesticTransferProgress.WaitingForTransferServiceAck
            | DomesticTransferProgress.PartnerBank _ -> inProgressOrExhausted
            | DomesticTransferProgress.Settled -> "Completed"
            | DomesticTransferProgress.Failed _ -> compensatingOrFailed
         | AppSaga.Saga.PlatformTransfer s ->
            match s.Status with
            | PlatformTransferSagaStatus.Scheduled -> "Scheduled"
            | PlatformTransferSagaStatus.InProgress -> inProgressOrExhausted
            | PlatformTransferSagaStatus.Completed -> "Completed"
            | PlatformTransferSagaStatus.Failed _ -> compensatingOrFailed
         | AppSaga.Saga.PaymentRequest s ->
            match s.Status with
            | PaymentRequestSagaStatus.InProgress _ -> inProgressOrExhausted
            | PaymentRequestSagaStatus.Completed -> "Completed"
            | PaymentRequestSagaStatus.Failed _ -> compensatingOrFailed
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

   let timestamp (date: DateTime) = Sql.timestamptz date
