module SagaDTO

open System

open Lib.NetworkQuery
open Lib.SharedTypes

[<RequireQualifiedAccess>]
type SagaDTOStatus =
   | Scheduled
   | InProgress
   | Completed
   | Compensating
   | Failed
   | Aborted
   | Exhausted
   | CompensationExhausted

   member x.Display =
      match x with
      | Scheduled -> "Scheduled"
      | InProgress -> "In Progress"
      | Completed -> "Completed"
      | Compensating -> "Compensating"
      | Failed -> "Failed"
      | Aborted -> "Aborted"
      | Exhausted -> "Exhausted"
      | CompensationExhausted -> "Compensation Exhausted"

   static member All = [
      Scheduled
      InProgress
      Completed
      Compensating
      Failed
      Aborted
      Exhausted
      CompensationExhausted
   ]

   static member fromString =
      function
      | "Scheduled" -> Some Scheduled
      | "InProgress" -> Some InProgress
      | "Completed" -> Some Completed
      | "Compensating" -> Some Compensating
      | "Failed" -> Some Failed
      | "Aborted" -> Some Aborted
      | "Exhausted" -> Some Exhausted
      | "CompensationExhausted" -> Some CompensationExhausted
      | _ -> None

   static member fromQueryString: string -> SagaDTOStatus list option =
      listFromQueryString SagaDTOStatus.fromString

   static member listToDisplay(items: SagaDTOStatus list) =
      List.fold
         (fun acc (filter: SagaDTOStatus) ->
            if acc = "" then
               filter.Display
            else
               $"{acc}, {filter.Display}")
         ""
         items

[<RequireQualifiedAccess>]
type SagaKind =
   | OrgOnboarding
   | EmployeeOnboarding
   | CardSetup
   | Purchase
   | DomesticTransfer
   | PlatformTransfer
   | PaymentRequest
   | BillingStatement

   member x.Display =
      match x with
      | OrgOnboarding -> "Org Onboarding"
      | EmployeeOnboarding -> "Employee Onboarding"
      | CardSetup -> "Card Setup"
      | Purchase -> "Purchase"
      | DomesticTransfer -> "Domestic Transfer"
      | PlatformTransfer -> "Platform Transfer"
      | PaymentRequest -> "Payment Request"
      | BillingStatement -> "Billing Statement"

   static member All = [
      OrgOnboarding
      EmployeeOnboarding
      CardSetup
      Purchase
      DomesticTransfer
      PlatformTransfer
      PaymentRequest
      BillingStatement
   ]

   static member fromString =
      function
      | "OrgOnboarding" -> Some OrgOnboarding
      | "EmployeeOnboarding" -> Some EmployeeOnboarding
      | "CardSetup" -> Some CardSetup
      | "Purchase" -> Some Purchase
      | "DomesticTransfer" -> Some DomesticTransfer
      | "PlatformTransfer" -> Some PlatformTransfer
      | "PaymentRequest" -> Some PaymentRequest
      | "BillingStatement" -> Some BillingStatement
      | _ -> None

   static member fromQueryString: string -> SagaKind list option =
      listFromQueryString SagaKind.fromString

   static member listToDisplay(items: SagaKind list) =
      List.fold
         (fun acc (filter: SagaKind) ->
            if acc = "" then
               filter.Display
            else
               $"{acc}, {filter.Display}")
         ""
         items

[<RequireQualifiedAccess>]
type SagaActivityDTOStatus =
   | InProgress
   | Completed
   | Failed
   | Aborted

type SagaActivityDTO = {
   Start: DateTime
   End: DateTime option
   Name: string
   Attempts: int
   MaxAttempts: int
   Status: SagaActivityDTOStatus
}

[<RequireQualifiedAccess>]
type ActivityRecoverableByHumanInTheLoop =
   | DomesticTransferServiceDevelopmentFix

   static member fromString(str: string) =
      match str with
      | "DomesticTransferServiceDevelopmentFix" ->
         Some DomesticTransferServiceDevelopmentFix
      | _ -> None

type SagaDTO = {
   SagaKind: SagaKind
   Name: string
   LifeCycle: SagaActivityDTO list
   StartedAt: DateTime
   Id: CorrelationId
   Status: SagaDTOStatus
   StatusDetail: string
   Events: string
   RecoverableActivity: ActivityRecoverableByHumanInTheLoop option
   Amount: decimal option
}

type SagaCursor = {
   CreatedAt: DateTime
   SagaId: CorrelationId
}

type SagaQuery = {
   PageLimit: int
   Cursor: SagaCursor option
   DateRange: (DateTime * DateTime) option
   Status: SagaDTOStatus list option
   SagaKind: SagaKind list option
}
