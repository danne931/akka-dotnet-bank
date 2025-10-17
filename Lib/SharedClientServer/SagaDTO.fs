module SagaDTO

open System

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
}
