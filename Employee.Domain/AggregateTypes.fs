namespace Bank.Employee.Domain

open System

open Lib.SharedTypes

[<RequireQualifiedAccess>]
type EmployeeCommand =
   | CreateAccountOwner of CreateAccountOwnerCommand
   | CreateEmployee of CreateEmployeeCommand
   | CreateCard of CreateCardCommand
   | PurchasePending of PurchasePendingCommand
   | AccountConfirmsPurchase of AccountConfirmsPurchaseCommand
   | AccountRejectsPurchase of AccountRejectsPurchaseCommand
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

type EmployeeEvent =
   | CreatedAccountOwner of BankEvent<CreatedAccountOwner>
   | CreatedEmployee of BankEvent<CreatedEmployee>
   | CreatedCard of BankEvent<CreatedCard>
   | PurchasePending of BankEvent<PurchasePending>
   | PurchaseConfirmedByAccount of BankEvent<PurchaseConfirmedByAccount>
   | PurchaseRejectedByAccount of BankEvent<PurchaseRejectedByAccount>
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
      | :? BankEvent<PurchasePending> as evt -> PurchasePending evt
      | :? BankEvent<PurchaseConfirmedByAccount> as evt ->
         PurchaseConfirmedByAccount evt
      | :? BankEvent<PurchaseRejectedByAccount> as evt ->
         PurchaseRejectedByAccount evt
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
      | _ -> failwith "Missing definition for EmployeeEvent message"

   let unwrap (o: EmployeeEvent) : OpenEmployeeEventEnvelope =
      match o with
      | CreatedAccountOwner evt -> wrap evt, get evt
      | CreatedEmployee evt -> wrap evt, get evt
      | CreatedCard evt -> wrap evt, get evt
      | PurchasePending evt -> wrap evt, get evt
      | PurchaseConfirmedByAccount evt -> wrap evt, get evt
      | PurchaseRejectedByAccount evt -> wrap evt, get evt
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

[<RequireQualifiedAccess>]
type EmployeeMessage =
   | GetEmployee
   | StateChange of EmployeeCommand
   | Event of EmployeeEvent
   | Delete

type Employee = {
   EmployeeId: EmployeeId
   OrgId: OrgId
   Role: Role
   Email: Email
   FirstName: string
   LastName: string
   Cards: Map<CardId, Card>
   Status: EmployeeStatus
   PendingPurchases: Map<CorrelationId, PurchaseInfo>
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

type EmployeeSnapshot = {
   Info: Employee
   Events: EmployeeEvent list
}

type CardWithMetrics = {
   Card: Card
   DailyPurchaseAccrued: decimal
   MonthlyPurchaseAccrued: decimal
   Employee: Employee
}
