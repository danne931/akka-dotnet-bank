[<RequireQualifiedAccess>]
module Employee

open Validus
open System

open Bank.Employee.Domain
open Lib.SharedTypes
open Lib.Time

let private purchaseAccrued
   (satisfiesDate: DateTime -> bool)
   (events: EmployeeEvent list)
   (cardId: CardId)
   : decimal
   =
   List.fold
      (fun acc evt ->
         match evt with
         | PurchaseConfirmedByAccount e ->
            let e = e.Data.Info

            if e.CardId = cardId && satisfiesDate e.Date then
               acc + e.Amount
            else
               acc
         | _ -> acc)
      0m
      events

let dailyPurchaseAccrued = purchaseAccrued DateTime.isToday
let monthlyPurchaseAccrued = purchaseAccrued DateTime.isThisMonth

let applyEvent
   (state: EmployeeSnapshot)
   (evt: EmployeeEvent)
   : EmployeeSnapshot
   =
   let em = state.Info

   let updatedEmployee =
      match evt with
      | CreatedAccountOwner e -> {
         EmployeeId = EmployeeId.fromEntityId e.EntityId
         Role = Role.Admin
         OrgId = e.OrgId
         Email = e.Data.Email
         FirstName = e.Data.FirstName
         LastName = e.Data.LastName
         Cards = Map.empty
         Status = EmployeeStatus.PendingInviteConfirmation e.Data.InviteToken
         PendingPurchases = Map.empty
         OnboardingTasks = []
         AuthProviderUserId = None
        }
      | CreatedEmployee e -> {
         EmployeeId = EmployeeId.fromEntityId e.EntityId
         Role = e.Data.Role
         OrgId = e.OrgId
         Email = e.Data.Email
         FirstName = e.Data.FirstName
         LastName = e.Data.LastName
         Cards = Map.empty
         Status =
            match e.Data.OrgRequiresEmployeeInviteApproval with
            | Some ruleId ->
               EmployeeStatus.PendingInviteApproval(
                  {
                     RuleId = ruleId
                     ProgressId = CommandApprovalProgressId e.CorrelationId
                  }
               )
            | None ->
               EmployeeStatus.PendingInviteConfirmation(InviteToken.generate ())
         PendingPurchases = Map.empty
         OnboardingTasks =
            match e.Data.CardInfo with
            | Some cardInfo -> [ EmployeeOnboardingTask.CreateCard cardInfo ]
            | None -> []
         AuthProviderUserId = None
        }
      | CreatedCard e ->
         let info = e.Data.Card

         {
            em with
               Cards = em.Cards |> Map.add info.CardId info
               OnboardingTasks =
                  em.OnboardingTasks
                  |> List.filter (function
                     | EmployeeOnboardingTask.CreateCard _ -> false)
         }
      | PurchasePending e -> {
         em with
            PendingPurchases =
               em.PendingPurchases |> Map.add e.CorrelationId e.Data.Info
        }
      | PurchaseConfirmedByAccount e -> {
         em with
            PendingPurchases = em.PendingPurchases |> Map.remove e.CorrelationId
            Cards =
               Map.change
                  e.Data.Info.CardId
                  (Option.map (fun card -> {
                     card with
                        LastPurchaseAt = Some DateTime.UtcNow
                  }))
                  em.Cards

        }
      | PurchaseRejectedByAccount e -> {
         em with
            PendingPurchases = em.PendingPurchases |> Map.remove e.CorrelationId
        }
      | DailyDebitLimitUpdated e -> {
         em with
            Cards =
               Map.change
                  e.Data.CardId
                  (Option.map (fun card -> {
                     card with
                        DailyPurchaseLimit = e.Data.DebitLimit
                  }))
                  em.Cards
        }
      | MonthlyDebitLimitUpdated e -> {
         em with
            Cards =
               Map.change
                  e.Data.CardId
                  (Option.map (fun card -> {
                     card with
                        MonthlyPurchaseLimit = e.Data.DebitLimit
                  }))
                  em.Cards
        }
      | LockedCard e -> {
         em with
            Cards =
               Map.change
                  e.Data.CardId
                  (Option.map (fun card -> {
                     card with
                        Status = CardStatus.Frozen
                  }))
                  em.Cards
        }
      | UnlockedCard e -> {
         em with
            Cards =
               Map.change
                  e.Data.CardId
                  (Option.map (fun card -> {
                     card with
                        Status = CardStatus.Active
                  }))
                  em.Cards
        }
      | UpdatedRole e -> { em with Role = e.Data.Role }
      | CardNicknamed e -> {
         em with
            Cards =
               Map.change
                  e.Data.CardId
                  (Option.map (fun card -> {
                     card with
                        CardNickname = Some e.Data.Name
                  }))
                  em.Cards
        }
      | InvitationCancelled _ -> {
         em with
            Status = EmployeeStatus.Closed
        }
      | InvitationTokenRefreshed e -> {
         em with
            Status = EmployeeStatus.PendingInviteConfirmation e.Data.InviteToken
        }
      | InvitationConfirmed e -> {
         em with
            Status = EmployeeStatus.Active
            Email = e.Data.Email
            AuthProviderUserId = Some e.Data.AuthProviderUserId
        }
      | AccessApproved e -> {
         em with
            Status = EmployeeStatus.PendingInviteConfirmation e.Data.InviteToken
        }
      | AccessRestored _ -> {
         em with
            Status =
               match em.AuthProviderUserId with
               | Some _ -> EmployeeStatus.Active
               | None ->
                  InviteToken.generate ()
                  |> EmployeeStatus.PendingInviteConfirmation
        }

   {
      Info = updatedEmployee
      Events = evt :: state.Events
   }

module private StateTransition =
   let transitionErr (err: EmployeeStateTransitionError) =
      Error <| EmployeeStateTransitionError err

   let map
      (eventTransform: BankEvent<'t> -> EmployeeEvent)
      (state: EmployeeSnapshot)
      (eventValidation: ValidationResult<BankEvent<'t>>)
      =
      eventValidation
      |> Result.mapError ValidationError
      |> Result.map (fun evt ->
         let evt = eventTransform evt
         (evt, applyEvent state evt))

   let createAccountOwner
      (state: EmployeeSnapshot)
      (cmd: CreateAccountOwnerCommand)
      =
      if state.Info.Status <> EmployeeStatus.InitialEmptyState then
         transitionErr EmployeeNotReadyToActivate
      else
         map CreatedAccountOwner state (CreateAccountOwnerCommand.toEvent cmd)

   let create (state: EmployeeSnapshot) (cmd: CreateEmployeeCommand) =
      if state.Info.Status <> EmployeeStatus.InitialEmptyState then
         transitionErr EmployeeNotReadyToActivate
      else
         map CreatedEmployee state (CreateEmployeeCommand.toEvent cmd)

   let createCard (state: EmployeeSnapshot) (cmd: CreateCardCommand) =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map CreatedCard state (CreateCardCommand.toEvent cmd)

   let limitDailyDebits
      (state: EmployeeSnapshot)
      (cmd: LimitDailyDebitsCommand)
      =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map DailyDebitLimitUpdated state (LimitDailyDebitsCommand.toEvent cmd)

   let limitMonthlyDebits
      (state: EmployeeSnapshot)
      (cmd: LimitMonthlyDebitsCommand)
      =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map
            MonthlyDebitLimitUpdated
            state
            (LimitMonthlyDebitsCommand.toEvent cmd)

   let lockCard (state: EmployeeSnapshot) (cmd: LockCardCommand) =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map LockedCard state (LockCardCommand.toEvent cmd)

   let unlockCard (state: EmployeeSnapshot) (cmd: UnlockCardCommand) =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map UnlockedCard state (UnlockCardCommand.toEvent cmd)

   let purchasePending (state: EmployeeSnapshot) (cmd: PurchasePendingCommand) =
      let em = state.Info
      let info = cmd.Data

      if em.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         match Map.tryFind info.CardId em.Cards with
         | None -> transitionErr CardNotFound
         | Some card ->
            let dpa = dailyPurchaseAccrued state.Events card.CardId
            let mpa = monthlyPurchaseAccrued state.Events card.CardId

            if card.Status = CardStatus.Frozen then
               transitionErr CardLocked
            elif card.IsExpired() then
               transitionErr CardExpired
            elif
               DateTime.isToday info.Date
               && dpa + info.Amount > card.DailyPurchaseLimit
            then
               transitionErr <| ExceededDailyDebit(card.DailyPurchaseLimit, dpa)
            elif
               DateTime.isThisMonth info.Date
               && mpa + info.Amount > card.MonthlyPurchaseLimit
            then
               transitionErr
               <| ExceededMonthlyDebit(card.DailyPurchaseLimit, dpa)
            else
               map PurchasePending state (PurchasePendingCommand.toEvent cmd)

   let accountConfirmsPurchase
      (state: EmployeeSnapshot)
      (cmd: AccountConfirmsPurchaseCommand)
      =
      let em = state.Info

      if em.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      elif
         Option.isNone <| Map.tryFind cmd.CorrelationId em.PendingPurchases
      then
         transitionErr DebitAlreadyProgressedToCompletedOrFailed
      else
         map
            PurchaseConfirmedByAccount
            state
            (AccountConfirmsPurchaseCommand.toEvent cmd)

   let accountRejectsPurchase
      (state: EmployeeSnapshot)
      (cmd: AccountRejectsPurchaseCommand)
      =
      let em = state.Info

      if em.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      elif
         Option.isNone <| Map.tryFind cmd.CorrelationId em.PendingPurchases
      then
         transitionErr DebitAlreadyProgressedToCompletedOrFailed
      else
         map
            PurchaseRejectedByAccount
            state
            (AccountRejectsPurchaseCommand.toEvent cmd)

   let updateRole (state: EmployeeSnapshot) (cmd: UpdateRoleCommand) =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map UpdatedRole state (UpdateRoleCommand.toEvent cmd)

   let nicknameCard (state: EmployeeSnapshot) (cmd: EditCardNicknameCommand) =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map CardNicknamed state (EditCardNicknameCommand.toEvent cmd)

   let refreshEmployeeInvitationToken
      (state: EmployeeSnapshot)
      (cmd: RefreshInvitationTokenCommand)
      =
      let em = state.Info

      match em.Status with
      | EmployeeStatus.PendingInviteConfirmation _ ->
         map
            InvitationTokenRefreshed
            state
            (RefreshInvitationTokenCommand.toEvent cmd)
      | _ ->
         transitionErr
         <| EmployeeStatusDisallowsInviteProgression(string em.Status)

   let cancelEmployeeInvitation
      (state: EmployeeSnapshot)
      (cmd: CancelInvitationCommand)
      =
      let em = state.Info

      match em.Status with
      | EmployeeStatus.PendingInviteApproval _
      | EmployeeStatus.PendingInviteConfirmation _ ->
         map InvitationCancelled state (CancelInvitationCommand.toEvent cmd)
      | _ ->
         transitionErr
         <| EmployeeStatusDisallowsInviteProgression(string em.Status)

   let confirmEmployeeInvitation
      (state: EmployeeSnapshot)
      (cmd: ConfirmInvitationCommand)
      =
      let em = state.Info

      match em.Status with
      | EmployeeStatus.PendingInviteConfirmation _ ->
         map InvitationConfirmed state (ConfirmInvitationCommand.toEvent cmd)
      | _ ->
         transitionErr
         <| EmployeeStatusDisallowsInviteProgression(string em.Status)

   let approveAccess (state: EmployeeSnapshot) (cmd: ApproveAccessCommand) =
      let em = state.Info

      match em.Status with
      | EmployeeStatus.PendingInviteApproval _ ->
         map AccessApproved state (ApproveAccessCommand.toEvent cmd)
      | _ ->
         transitionErr
         <| EmployeeStatusDisallowsInviteProgression(string em.Status)

   let restoreAccess (state: EmployeeSnapshot) (cmd: RestoreAccessCommand) =
      let em = state.Info

      if em.Status <> EmployeeStatus.Closed then
         transitionErr <| EmployeeStatusDisallowsAccessRestore(string em.Status)
      else
         map AccessRestored state (RestoreAccessCommand.toEvent cmd)

let stateTransition (state: EmployeeSnapshot) (command: EmployeeCommand) =
   match command with
   | EmployeeCommand.CreateAccountOwner cmd ->
      StateTransition.createAccountOwner state cmd
   | EmployeeCommand.CreateEmployee cmd -> StateTransition.create state cmd
   | EmployeeCommand.CreateCard cmd -> StateTransition.createCard state cmd
   | EmployeeCommand.PurchasePending cmd ->
      StateTransition.purchasePending state cmd
   | EmployeeCommand.AccountConfirmsPurchase cmd ->
      StateTransition.accountConfirmsPurchase state cmd
   | EmployeeCommand.AccountRejectsPurchase cmd ->
      StateTransition.accountRejectsPurchase state cmd
   | EmployeeCommand.LimitDailyDebits cmd ->
      StateTransition.limitDailyDebits state cmd
   | EmployeeCommand.LimitMonthlyDebits cmd ->
      StateTransition.limitMonthlyDebits state cmd
   | EmployeeCommand.LockCard cmd -> StateTransition.lockCard state cmd
   | EmployeeCommand.UnlockCard cmd -> StateTransition.unlockCard state cmd
   | EmployeeCommand.UpdateRole cmd -> StateTransition.updateRole state cmd
   | EmployeeCommand.EditCardNickname cmd ->
      StateTransition.nicknameCard state cmd
   | EmployeeCommand.CancelInvitation cmd ->
      StateTransition.cancelEmployeeInvitation state cmd
   | EmployeeCommand.RefreshInvitationToken cmd ->
      StateTransition.refreshEmployeeInvitationToken state cmd
   | EmployeeCommand.ConfirmInvitation cmd ->
      StateTransition.confirmEmployeeInvitation state cmd
   | EmployeeCommand.ApproveAccess cmd ->
      StateTransition.approveAccess state cmd
   | EmployeeCommand.RestoreAccess cmd ->
      StateTransition.restoreAccess state cmd

let empty: Employee = {
   EmployeeId = EmployeeId System.Guid.Empty
   OrgId = OrgId System.Guid.Empty
   Role = Role.Scholar
   Email = Email.empty
   FirstName = ""
   LastName = ""
   Status = EmployeeStatus.InitialEmptyState
   Cards = Map.empty
   PendingPurchases = Map.empty
   OnboardingTasks = []
   AuthProviderUserId = None
}
