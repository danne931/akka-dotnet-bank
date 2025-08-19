namespace Bank.Employee.Domain

open System

open Lib.SharedTypes
open Email

[<RequireQualifiedAccess>]
type EmployeeCommand =
   | CreateAccountOwner of CreateAccountOwnerCommand
   | CreateEmployee of CreateEmployeeCommand
   | CreateCard of CreateCardCommand
   | LinkThirdPartyProviderCard of LinkThirdPartyProviderCardCommand
   | PurchaseIntent of PurchaseIntentCommand
   | SettlePurchase of SettlePurchaseWithCardCommand
   | FailPurchase of FailPurchaseCommand
   | RefundPurchase of RefundPurchaseCommand
   | ConfigureRollingPurchaseLimit of ConfigureRollingPurchaseLimitCommand
   | LockCard of LockCardCommand
   | UnlockCard of UnlockCardCommand
   | UpdateRole of UpdateRoleCommand
   | EditCardNickname of EditCardNicknameCommand
   | CancelInvitation of CancelInvitationCommand
   | RefreshInvitationToken of RefreshInvitationTokenCommand
   | ConfirmInvitation of ConfirmInvitationCommand
   | RestoreAccess of RestoreAccessCommand
   | ApproveAccess of ApproveAccessCommand

   member x.Envelope: Envelope =
      match x with
      | CreateAccountOwner cmd -> Command.envelope cmd
      | CreateEmployee cmd -> Command.envelope cmd
      | CreateCard cmd -> Command.envelope cmd
      | LinkThirdPartyProviderCard cmd -> Command.envelope cmd
      | PurchaseIntent cmd -> Command.envelope cmd
      | SettlePurchase cmd -> Command.envelope cmd
      | FailPurchase cmd -> Command.envelope cmd
      | RefundPurchase cmd -> Command.envelope cmd
      | ConfigureRollingPurchaseLimit cmd -> Command.envelope cmd
      | LockCard cmd -> Command.envelope cmd
      | UnlockCard cmd -> Command.envelope cmd
      | UpdateRole cmd -> Command.envelope cmd
      | EditCardNickname cmd -> Command.envelope cmd
      | CancelInvitation cmd -> Command.envelope cmd
      | RefreshInvitationToken cmd -> Command.envelope cmd
      | ConfirmInvitation cmd -> Command.envelope cmd
      | RestoreAccess cmd -> Command.envelope cmd
      | ApproveAccess cmd -> Command.envelope cmd

type EmployeeEvent =
   | CreatedAccountOwner of BankEvent<CreatedAccountOwner>
   | CreatedEmployee of BankEvent<CreatedEmployee>
   | CreatedCard of BankEvent<CreatedCard>
   | ThirdPartyProviderCardLinked of BankEvent<ThirdPartyProviderCardLinked>
   | PurchasePending of BankEvent<CardPurchasePending>
   | PurchaseSettled of BankEvent<CardPurchaseSettled>
   | PurchaseFailed of BankEvent<CardPurchaseFailed>
   | PurchaseRefunded of BankEvent<CardPurchaseRefunded>
   | ConfiguredRollingPurchaseLimit of BankEvent<ConfiguredRollingPurchaseLimit>
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
      InitiatedBy = evt.InitiatedBy
      Timestamp = evt.Timestamp
      EventName = evt.EventName
   }

   let wrap (o: BankEvent<_>) : EmployeeEvent =
      match box o with
      | :? BankEvent<CreatedAccountOwner> as evt -> CreatedAccountOwner evt
      | :? BankEvent<CreatedEmployee> as evt -> CreatedEmployee evt
      | :? BankEvent<CreatedCard> as evt -> CreatedCard evt
      | :? BankEvent<ThirdPartyProviderCardLinked> as evt ->
         ThirdPartyProviderCardLinked evt
      | :? BankEvent<CardPurchasePending> as evt -> PurchasePending evt
      | :? BankEvent<CardPurchaseSettled> as evt -> PurchaseSettled evt
      | :? BankEvent<CardPurchaseFailed> as evt -> PurchaseFailed evt
      | :? BankEvent<CardPurchaseRefunded> as evt -> PurchaseRefunded evt
      | :? BankEvent<ConfiguredRollingPurchaseLimit> as evt ->
         ConfiguredRollingPurchaseLimit evt
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
      | ThirdPartyProviderCardLinked evt -> wrap evt, get evt
      | PurchasePending evt -> wrap evt, get evt
      | PurchaseSettled evt -> wrap evt, get evt
      | PurchaseFailed evt -> wrap evt, get evt
      | PurchaseRefunded evt -> wrap evt, get evt
      | ConfiguredRollingPurchaseLimit evt -> wrap evt, get evt
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

   static member Empty = {
      EmployeeId = EmployeeId System.Guid.Empty
      OrgId = OrgId System.Guid.Empty
      Role = Role.Scholar
      Email = Email.empty
      FirstName = ""
      LastName = ""
      Status = EmployeeStatus.InitialEmptyState
      Cards = Map.empty
      AuthProviderUserId = None
   }

type EmployeeSnapshot = {
   Info: Employee
   Events: EmployeeEvent list
   PendingPurchaseDeductions: PendingDeductions
} with

   static member Empty = {
      Info = Employee.Empty
      Events = []
      PendingPurchaseDeductions = PendingDeductions.Zero
   }

type CardWithMetrics = {
   Card: Card
   DailyPurchaseAccrued: decimal
   MonthlyPurchaseAccrued: decimal
   Employee: Employee
}
