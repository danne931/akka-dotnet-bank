module Lib.CircuitBreaker

open System

[<RequireQualifiedAccess>]
type CircuitBreakerService =
   | DomesticTransfer
   | Email
   | KnowYourCustomer

[<RequireQualifiedAccess>]
type CircuitBreakerStatus =
   | Closed
   | HalfOpen
   | Open

type CircuitBreakerEvent = {
   Service: CircuitBreakerService
   Status: CircuitBreakerStatus
   Timestamp: DateTime
}

[<RequireQualifiedAccess>]
type CircuitBreakerMessage =
   | Lookup
   | CircuitBreaker of CircuitBreakerEvent

type CircuitBreakerState = {
   DomesticTransfer: CircuitBreakerStatus
   Email: CircuitBreakerStatus
   KnowYourCustomer: CircuitBreakerStatus
}
