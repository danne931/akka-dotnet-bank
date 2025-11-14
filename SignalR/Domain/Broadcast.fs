module SignalRBroadcast

open System

open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Org.Domain
open Lib.SharedTypes
open Lib.CircuitBreaker

/// Persisted an event specific to the parent account, rather
/// than one of its subaccounts.
type ParentAccountEventPersistedConfirmation = {
   EventPersisted: ParentAccountEvent
   Date: DateTime
}

/// Persisted an event specific to one of the subaccounts,
/// or virtual accounts, of a parent account.
type AccountEventPersistedConfirmation = {
   EventPersisted: AccountEvent
   Account: Account
   Date: DateTime
}

type EmployeeEventPersistedConfirmation = {
   EventPersisted: EmployeeEvent
   Employee: Employee
   Date: DateTime
}

type OrgEventPersistedConfirmation = {
   EventPersisted: OrgEvent
   Org: Org
   Date: DateTime
}

[<RequireQualifiedAccess>]
type EventPersistedConfirmation =
   | ParentAccount of ParentAccountEventPersistedConfirmation
   | Account of AccountEventPersistedConfirmation
   | Employee of EmployeeEventPersistedConfirmation
   | Org of OrgEventPersistedConfirmation

[<RequireQualifiedAccess>]
type EventProcessingError =
   | ParentAccount of OrgId * ParentAccountId * CorrelationId * Err * DateTime
   | Account of OrgId * AccountId * CorrelationId * Err * DateTime
   | Employee of OrgId * EmployeeId * CorrelationId * Err * DateTime
   | Org of OrgId * CorrelationId * Err * DateTime

   member x.OrgId: OrgId =
      match x with
      | ParentAccount(orgId, _, _, _, _) -> orgId
      | Account(orgId, _, _, _, _) -> orgId
      | Employee(orgId, _, _, _, _) -> orgId
      | Org(orgId, _, _, _) -> orgId

   member x.CorrelationId: CorrelationId =
      match x with
      | ParentAccount(_, _, corrId, _, _) -> corrId
      | Account(_, _, id, _, _) -> id
      | Employee(_, _, id, _, _) -> id
      | Org(_, id, _, _) -> id

   member x.Error: Err =
      match x with
      | ParentAccount(_, _, _, err, _) -> err
      | Account(_, _, _, err, _) -> err
      | Employee(_, _, _, err, _) -> err
      | Org(_, _, err, _) -> err

type SagaUpdated = {
   Date: DateTime
   Saga: SagaDTO.SagaDTO
   OrgId: OrgId
}

type SignalRBroadcast = {
   circuitBreaker: CircuitBreakerEvent -> unit
   parentAccountEventPersisted: ParentAccountEvent -> unit
   accountEventPersisted: AccountEvent -> Account -> unit
   accountEventError: OrgId -> AccountId -> CorrelationId -> Err -> unit
   employeeEventPersisted: EmployeeEvent -> Employee -> unit
   employeeEventError: OrgId -> EmployeeId -> CorrelationId -> Err -> unit
   orgEventPersisted: OrgEvent -> Org -> unit
   orgEventError: OrgId -> CorrelationId -> Err -> unit
   sagaUpdated: OrgId -> SagaDTO.SagaDTO -> unit
}
