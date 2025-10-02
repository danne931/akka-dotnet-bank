module Lib.CircuitBreaker

open System

[<RequireQualifiedAccess>]
type CircuitBreakerService =
   | Email
   | KnowYourCustomer
   | PartnerBank
   | CardIssuer

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
   Email: CircuitBreakerStatus
   KnowYourCustomer: CircuitBreakerStatus
   PartnerBank: CircuitBreakerStatus
   CardIssuer: CircuitBreakerStatus
}
