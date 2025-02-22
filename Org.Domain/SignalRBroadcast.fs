namespace Bank.Org.Domain

open System
open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Org.Domain
open Lib.SharedTypes

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
type EventProcessingError =
   | Account of OrgId * AccountId * Err * DateTime
   | Employee of OrgId * EmployeeId * Err * DateTime
   | Org of OrgId * Err * DateTime

   member x.OrgId =
      match x with
      | Account(orgId, _, _, _) -> orgId
      | Employee(orgId, _, _, _) -> orgId
      | Org(orgId, _, _) -> orgId

type SignalRBroadcast = {
   circuitBreaker: CircuitBreakerEvent -> unit
   accountEventPersisted: AccountEvent -> Account -> unit
   accountEventError: OrgId -> AccountId -> Err -> unit
   employeeEventPersisted: EmployeeEvent -> Employee -> unit
   employeeEventError: OrgId -> EmployeeId -> Err -> unit
   orgEventPersisted: OrgEvent -> Org -> unit
   orgEventError: OrgId -> Err -> unit
}
