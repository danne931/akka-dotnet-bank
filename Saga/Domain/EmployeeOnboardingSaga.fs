module EmployeeOnboardingSaga

open System

open Lib.SharedTypes
open Bank.Employee.Domain
open Lib.Saga
open Email

[<RequireQualifiedAccess>]
type EmployeeOnboardingSagaStatus =
   | InProgress
   | Completed
   | Failed
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

type CardSetupSagaId = CardSetupSagaId of CorrelationId

[<RequireQualifiedAccess>]
type EmployeeOnboardingSagaEvent =
   | AccessRequestPending
   | AccessApproved
   | InviteNotificationSent
   | InviteTokenRefreshed of InviteToken
   | InviteConfirmed
   | InviteCancelled of reason: string option
   | CardSetupSagaCompleted of CardSetupSagaId
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
   | CardSetup

   interface IActivity with
      member x.MaxAttempts =
         match x with
         | WaitForAccessApproval
         | WaitForInviteConfirmation -> 0
         | CardSetup
         | CreateEmployee -> 1
         | _ -> 3

      member x.InactivityTimeout =
         match x with
         | CreateEmployee
         | RestoreEmployeeAccess
         | WaitForAccessApproval
         | WaitForInviteConfirmation -> None
         | CardSetup -> Some(TimeSpan.FromMinutes 30.)
         | SendEmployeeInviteNotification
         | RequestAccessApproval -> Some(TimeSpan.FromSeconds 5.)

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
} with

   member x.IsWaitingForInviteConfirmation =
      x.LifeCycle.InProgress
      |> List.exists _.Activity.IsWaitForInviteConfirmation
