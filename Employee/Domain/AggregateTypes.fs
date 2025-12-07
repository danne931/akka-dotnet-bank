namespace Bank.Employee.Domain

open System

open Lib.SharedTypes
open Email
open Bank.Purchase.Domain

[<RequireQualifiedAccess>]
type EmployeeCommand =
   | CreateAccountOwner of CreateAccountOwnerCommand
   | CreateEmployee of CreateEmployeeCommand
   | CreateCard of CreateCardCommand
   | LinkCard of LinkCardCommand
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
      | LinkCard cmd -> Command.envelope cmd
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
   | CardLinked of BankEvent<CardLinked>
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
      | :? BankEvent<CardLinked> as evt -> CardLinked evt
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
      | CardLinked evt -> wrap evt, get evt
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
   | AuthorizePurchase of PurchaseAuthorization
   | PurchaseProgress of CardIssuerPurchaseProgress * CardId
   | Delete
   | PruneIdempotencyChecker
   | PruneOutbox

type Employee = {
   EmployeeId: EmployeeId
   OrgId: OrgId
   ParentAccountId: ParentAccountId
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
         match card.IsExpired(), card.Status with
         | true, _ -> false
         | false, CardStatus.Closed _ -> false
         | _ -> true)

   static member Empty = {
      EmployeeId = EmployeeId Guid.Empty
      OrgId = OrgId Guid.Empty
      ParentAccountId = ParentAccountId Guid.Empty
      Role = Role.Scholar
      Email = Email.empty
      FirstName = ""
      LastName = ""
      Status = EmployeeStatus.InitialEmptyState
      Cards = Map.empty
      AuthProviderUserId = None
   }

/// If the employee actor receives a message with an ID equivalent
/// to an event we already persisted then we ignore it.  This ignores
/// messages that may be duplicated due to AtLeastOnceDelivery.
/// However, we need to have slightly different behavior if the
/// message comes from a saga actor since it has retry capabilities.
/// If the saga actor has an in-progress activity which has reached its
/// InactivityTimeout (it did not complete within the expected time),
/// then it will resend the message to the employee actor using
/// the same ID. So, in that case the saga actor wants to receive an
/// ACK that the employee actor indeed processed the message so it can
/// complete it's activity.
/// Scenario:
///    1. Employee event persisted
///    2. message sent from employee actor to saga actor
///    3. message lost in translation instead of delivered
///    (probably will be a rare situation since I am delivering most
///     messages to saga actors via RabbitMQ but better safe than sorry)
/// Reconciliation:
///    1. saga actor receives EvaluateRemainingWork message a few
///       seconds/minutes into the future due to having an in-progress activity
///       with an expired InactivityTimeout
///    2. saga actor reattempts delivery of the message to the employee actor
///    3. employee actor receives message and checks Outbox to see
///       if it has already associated an outbox message with the same ID
///    4. since an outgoing saga message exists in the Outbox, it will
///       resend that saga message
///    5. saga actor will receive the message, complete the activity, and resume
///       processing next steps
type EmployeeOutboxMessage =
   // NOTE:
   // AppSaga.AppSagaMessage in Saga/Domain directory depends
   // on Employee/Domain directory so can not reference it here.
   // Instead will use obj here and interpret it in the employee actor.
   // Good enough for now.
   | Saga of obj

type EmployeeSnapshot = {
   Info: Employee
   Events: EmployeeEvent list
   CardIssuerLinks: Map<CardId, CardIssuerLink>
   PendingPurchaseDeductions: Map<CardId, PendingFunds>
   PendingPurchases: Map<CardIssuerTransactionId, Purchase>
   ProcessedCommands: Map<EventId, DateTime>
   Outbox: Map<EventId, DateTime * EmployeeOutboxMessage>
} with

   static member Empty = {
      Info = Employee.Empty
      Events = []
      CardIssuerLinks = Map.empty
      PendingPurchaseDeductions = Map.empty
      PendingPurchases = Map.empty
      ProcessedCommands = Map.empty
      Outbox = Map.empty
   }

type CardWithMetrics = {
   Card: Card
   DailyPurchaseAccrued: decimal
   MonthlyPurchaseAccrued: decimal
   Employee: Employee
}
