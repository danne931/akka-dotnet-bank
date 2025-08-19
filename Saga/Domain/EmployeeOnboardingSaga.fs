module EmployeeOnboardingSaga

open System

open Lib.SharedTypes
open Bank.Employee.Domain
open Lib.Saga
open Email

[<RequireQualifiedAccess>]
type OnboardingFailureReason = | CardProviderCardCreateFail

[<RequireQualifiedAccess>]
type EmployeeOnboardingSagaStatus =
   | InProgress
   | Completed
   | Failed of OnboardingFailureReason
   | Aborted of reason: string option

[<RequireQualifiedAccess>]
type EmployeeOnboardingSagaStartEvent =
   | AccountOwnerCreated of BankEvent<CreatedAccountOwner>
   | EmployeeCreated of BankEvent<CreatedEmployee>
   | EmployeeAccessRestored of
      {|
         Event: BankEvent<AccessRestored>
         EmployeeName: string
         EmployeeEmail: Email
         InviteToken: InviteToken
      |}

[<RequireQualifiedAccess>]
type EmployeeOnboardingSagaEvent =
   | AccessRequestPending
   | AccessApproved
   | InviteNotificationSent
   | InviteTokenRefreshed of InviteToken
   | OnboardingFailNotificationSent
   | InviteConfirmed
   | InviteCancelled of reason: string option
   | CardCreateResponse of Result<ThirdPartyProviderCardId, string>
   | CardAssociatedWithEmployee
   | EvaluateRemainingWork
   | ResetInProgressActivityAttempts

[<RequireQualifiedAccess>]
type Activity =
   | CreateEmployee
   | RestoreEmployeeAccess
   | RequestAccessApproval
   | WaitForAccessApproval
   | SendEmployeeInviteNotification
   | WaitForInviteConfirmation
   | CreateCardViaThirdPartyProvider
   | AssociateCardWithEmployee
   | SendEmployeeOnboardingFailNotification

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForAccessApproval
         | WaitForInviteConfirmation -> 0
         | CreateEmployee -> 1
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | CreateEmployee
         | RestoreEmployeeAccess
         | WaitForAccessApproval
         | WaitForInviteConfirmation -> None
         | CreateCardViaThirdPartyProvider -> Some(TimeSpan.FromMinutes 2.)
         | SendEmployeeInviteNotification
         | SendEmployeeOnboardingFailNotification ->
            Some(TimeSpan.FromMinutes 4.)
         | RequestAccessApproval
         | AssociateCardWithEmployee -> Some(TimeSpan.FromSeconds 5.)

type EmployeeOnboardingSaga = {
   EmployeeId: EmployeeId
   OrgId: OrgId
   CorrelationId: CorrelationId
   InitiatedBy: Initiator
   EmployeeName: string
   EmployeeEmail: Email
   CardInfo: EmployeeInviteSupplementaryCardInfo option
   InviteToken: InviteToken
   StartEvent: EmployeeOnboardingSagaStartEvent
   Events: EmployeeOnboardingSagaEvent list
   Status: EmployeeOnboardingSagaStatus
   LifeCycle: SagaLifeCycle<Activity>
   ProviderCardId: ThirdPartyProviderCardId option
} with

   member x.IsWaitingForInviteConfirmation =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsWaitForInviteConfirmation
