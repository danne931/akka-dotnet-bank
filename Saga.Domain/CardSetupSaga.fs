module CardSetupSaga

open System

open Lib.SharedTypes
open Bank.Employee.Domain
open Lib.Saga

[<RequireQualifiedAccess>]
type CardSetupFailureReason = | CardProviderCardCreateFail

[<RequireQualifiedAccess>]
type CardSetupSagaStatus =
   | InProgress
   | Completed
   | Failed of CardSetupFailureReason

[<RequireQualifiedAccess>]
type CardSetupSagaStartEvent = {
   EmployeeName: string
   EmployeeEmail: Email
   Event: BankEvent<CreatedCard>
}

[<RequireQualifiedAccess>]
type CardSetupSagaEvent =
   | CardSetupSuccessNotificationSent
   | CardSetupFailNotificationSent
   | CardCreateResponse of Result<ThirdPartyProviderCardId, string>
   | ProviderCardIdLinked
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess>]
type Activity =
   | InitializeCard
   | SendCardSetupSuccessNotification
   | SendCardSetupFailNotification
   | CreateCardViaThirdPartyProvider
   | LinkProviderCardId

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | InitializeCard -> 1
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | InitializeCard -> None
         | CreateCardViaThirdPartyProvider -> Some(TimeSpan.FromMinutes 2.)
         | SendCardSetupSuccessNotification
         | SendCardSetupFailNotification -> Some(TimeSpan.FromMinutes 4.)
         | LinkProviderCardId -> Some(TimeSpan.FromSeconds 5.)

type CardSetupSaga = {
   CardId: CardId
   CardNumberLast4: string
   EmployeeId: EmployeeId
   OrgId: OrgId
   CorrelationId: CorrelationId
   InitiatedBy: Initiator
   EmployeeName: string
   EmployeeEmail: Email
   CardType: CardType
   StartEvent: CardSetupSagaStartEvent
   Events: CardSetupSagaEvent list
   Status: CardSetupSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
   ProviderCardId: ThirdPartyProviderCardId option
}
