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

type SagaDTO = {
   Name: string
   LifeCycle: SagaActivityDTO list
   CreatedAt: DateTime
   Id: CorrelationId
   Status: SagaDTOStatus
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
}
