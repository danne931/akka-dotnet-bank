namespace Bank.Employee.Domain

open System

open Lib.SharedTypes

type EmployeeCommand =
   | CreateAccountOwner of CreateAccountOwnerCommand
   | CreateEmployee of CreateEmployeeCommand
   | CreateCard of CreateCardCommand
   | DebitRequest of DebitRequestCommand
   | ApproveDebit of ApproveDebitCommand
   | DeclineDebit of DeclineDebitCommand
   | LimitDailyDebits of LimitDailyDebitsCommand
   | LimitMonthlyDebits of LimitMonthlyDebitsCommand
   | LockCard of LockCardCommand
   | UnlockCard of UnlockCardCommand
   | UpdateRole of UpdateRoleCommand
   | EditCardNickname of EditCardNicknameCommand
   | CancelInvitation of CancelInvitationCommand
   | RefreshInvitationToken of RefreshInvitationTokenCommand
   | ConfirmInvitation of ConfirmInvitationCommand
   | RestoreAccess of RestoreAccessCommand
   | ApproveAccess of ApproveAccessCommand
   (*
   | RequestPlatformPayment of RequestPlatformPaymentCommand
   | ConfirmPlatformPayment of ConfirmPlatformPaymentCommand
   | DeclinePlatformPayment of DeclinePlatformPaymentCommand
   | RequestInternalTransferBetweenOrgs of RequestInternalTransferBetweenOrgsCommand
   | ConfirmInternalTransferBetweenOrgs of ConfirmInternalTransferBetweenOrgsCommand
   | DeclineInternalTransferBetweenOrgs of DeclineInternalTransferBetweenOrgsCommand
   *)
   | RequestDomesticTransfer of RequestDomesticTransferCommand
   | ConfirmDomesticTransfer of ConfirmDomesticTransferCommand
   //| DeclineDomesticTransfer of DeclineDomesticTransferCommand
   | ConfigureApprovalRule of CommandApprovalRule.ConfigureApprovalRuleCommand
   | RequestCommandApproval of CommandApprovalProgress.RequestCommandApproval
   | AcquireCommandApproval of CommandApprovalProgress.AcquireCommandApproval
   | DeclineCommandApproval of CommandApprovalProgress.DeclineCommandApproval

type EmployeeEvent =
   | CreatedAccountOwner of BankEvent<CreatedAccountOwner>
   | CreatedEmployee of BankEvent<CreatedEmployee>
   | CreatedCard of BankEvent<CreatedCard>
   | DebitRequested of BankEvent<DebitRequested>
   | DebitApproved of BankEvent<DebitApproved>
   | DebitDeclined of BankEvent<DebitDeclined>
   | DomesticTransferRequested of BankEvent<DomesticTransferRequested>
   | DomesticTransferConfirmed of BankEvent<DomesticTransferConfirmed>
   | DailyDebitLimitUpdated of BankEvent<DailyDebitLimitUpdated>
   | MonthlyDebitLimitUpdated of BankEvent<MonthlyDebitLimitUpdated>
   | LockedCard of BankEvent<LockedCard>
   | UnlockedCard of BankEvent<UnlockedCard>
   | UpdatedRole of BankEvent<RoleUpdated>
   | CardNicknamed of BankEvent<CardNicknamed>
   | InvitationCancelled of BankEvent<InvitationCancelled>
   | InvitationTokenRefreshed of BankEvent<InvitationTokenRefreshed>
   | InvitationConfirmed of BankEvent<InvitationConfirmed>
   | AccessRestored of BankEvent<AccessRestored>
   | AccessApproved of BankEvent<AccessApproved>
   | CommandApprovalRuleConfigured of BankEvent<CommandApprovalRule.T>
   | CommandApprovalRequested of
      BankEvent<CommandApprovalProgress.CommandApprovalRequested>
   | CommandApprovalAcquired of
      BankEvent<CommandApprovalProgress.CommandApprovalAcquired>
   | CommandApprovalDeclined of
      BankEvent<CommandApprovalProgress.CommandApprovalDeclined>

type OpenEmployeeEventEnvelope = EmployeeEvent * Envelope

[<RequireQualifiedAccess>]
module EmployeeEnvelope =
   let get (evt: BankEvent<'E>) : Envelope = {
      Id = evt.Id
      EntityId = evt.EntityId
      OrgId = evt.OrgId
      CorrelationId = evt.CorrelationId
      InitiatedById = evt.InitiatedById
      Timestamp = evt.Timestamp
      EventName = evt.EventName
   }

   let wrap (o: BankEvent<_>) : EmployeeEvent =
      match box o with
      | :? BankEvent<CreatedAccountOwner> as evt -> CreatedAccountOwner evt
      | :? BankEvent<CreatedEmployee> as evt -> CreatedEmployee evt
      | :? BankEvent<CreatedCard> as evt -> CreatedCard evt
      | :? BankEvent<DebitRequested> as evt -> DebitRequested evt
      | :? BankEvent<DebitApproved> as evt -> DebitApproved evt
      | :? BankEvent<DebitDeclined> as evt -> DebitDeclined evt
      | :? BankEvent<DomesticTransferRequested> as evt ->
         DomesticTransferRequested evt
      | :? BankEvent<DomesticTransferConfirmed> as evt ->
         DomesticTransferConfirmed evt
      | :? BankEvent<DailyDebitLimitUpdated> as evt ->
         DailyDebitLimitUpdated evt
      | :? BankEvent<MonthlyDebitLimitUpdated> as evt ->
         MonthlyDebitLimitUpdated evt
      | :? BankEvent<LockedCard> as evt -> LockedCard evt
      | :? BankEvent<UnlockedCard> as evt -> UnlockedCard evt
      | :? BankEvent<RoleUpdated> as evt -> UpdatedRole evt
      | :? BankEvent<CardNicknamed> as evt -> CardNicknamed evt
      | :? BankEvent<InvitationCancelled> as evt -> InvitationCancelled evt
      | :? BankEvent<InvitationTokenRefreshed> as evt ->
         InvitationTokenRefreshed evt
      | :? BankEvent<InvitationConfirmed> as evt -> InvitationConfirmed evt
      | :? BankEvent<AccessRestored> as evt -> AccessRestored evt
      | :? BankEvent<AccessApproved> as evt -> AccessApproved evt
      | :? BankEvent<CommandApprovalRule.T> as evt ->
         CommandApprovalRuleConfigured evt
      | :? BankEvent<CommandApprovalProgress.CommandApprovalRequested> as evt ->
         CommandApprovalRequested evt
      | :? BankEvent<CommandApprovalProgress.CommandApprovalAcquired> as evt ->
         CommandApprovalAcquired evt
      | :? BankEvent<CommandApprovalProgress.CommandApprovalDeclined> as evt ->
         CommandApprovalDeclined evt
      | _ -> failwith "Missing definition for EmployeeEvent message"

   let unwrap (o: EmployeeEvent) : OpenEmployeeEventEnvelope =
      match o with
      | CreatedAccountOwner evt -> wrap evt, get evt
      | CreatedEmployee evt -> wrap evt, get evt
      | CreatedCard evt -> wrap evt, get evt
      | DebitRequested evt -> wrap evt, get evt
      | DebitApproved evt -> wrap evt, get evt
      | DebitDeclined evt -> wrap evt, get evt
      | DomesticTransferRequested evt -> wrap evt, get evt
      | DomesticTransferConfirmed evt -> wrap evt, get evt
      | DailyDebitLimitUpdated evt -> wrap evt, get evt
      | MonthlyDebitLimitUpdated evt -> wrap evt, get evt
      | LockedCard evt -> wrap evt, get evt
      | UnlockedCard evt -> wrap evt, get evt
      | UpdatedRole evt -> wrap evt, get evt
      | CardNicknamed evt -> wrap evt, get evt
      | InvitationCancelled evt -> wrap evt, get evt
      | InvitationTokenRefreshed evt -> wrap evt, get evt
      | InvitationConfirmed evt -> wrap evt, get evt
      | AccessRestored evt -> wrap evt, get evt
      | AccessApproved evt -> wrap evt, get evt
      | CommandApprovalRuleConfigured evt -> wrap evt, get evt
      | CommandApprovalRequested evt -> wrap evt, get evt
      | CommandApprovalAcquired evt -> wrap evt, get evt
      | CommandApprovalDeclined evt -> wrap evt, get evt

type EmployeeMessage =
   | GetEmployee
   | ApprovableStateChange of ApprovableCommand
   | StateChange of EmployeeCommand
   | Event of EmployeeEvent
   | Delete

type EmployeeHistory = {
   InitiatedByName: string
   EmployeeName: string
   Event: EmployeeEvent
}

type Employee = {
   EmployeeId: EmployeeId
   OrgId: OrgId
   Role: Role
   Email: Email
   FirstName: string
   LastName: string
   Cards: Map<CardId, Card>
   Status: EmployeeStatus
   PendingPurchases: Map<CorrelationId, DebitInfo>
   OnboardingTasks: EmployeeOnboardingTask list
   AuthProviderUserId: Guid option
} with

   member x.Name = $"{x.FirstName} {x.LastName}"

   member x.CompositeId = x.EmployeeId, x.OrgId

   member x.PendingAccessApproval =
      match x.Status with
      | EmployeeStatus.PendingInviteApproval _ -> true
      | EmployeeStatus.PendingRestoreAccessApproval -> true
      | _ -> false

   member x.HasCard =
      x.Cards.Values
      |> Seq.exists (fun card ->
         not (card.IsExpired()) && card.Status <> CardStatus.Closed)

type EmployeeWithEvents = {
   Info: Employee
   Events: EmployeeEvent list
}

type CardWithMetrics = {
   Card: Card
   DailyPurchaseAccrued: decimal
   MonthlyPurchaseAccrued: decimal
   Employee: Employee
}
