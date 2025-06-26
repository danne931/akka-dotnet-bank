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
         | PurchaseSettled e ->
            let e = e.Data.Info

            if e.CardId = cardId && satisfiesDate e.Date then
               acc + e.Amount
            else
               acc
         | PurchaseFailed e ->
            let e = e.Data.Info

            if e.CardId = cardId && satisfiesDate e.Date then
               acc - e.Amount
            else
               acc
         | PurchaseRefunded e ->
            let e = e.Data.Info

            if e.CardId = cardId && satisfiesDate e.Date then
               acc - e.Amount
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
         Status =
            EmployeeStatus.PendingInviteConfirmation {
               Token = e.Data.InviteToken
               CorrelationId = e.CorrelationId
            }
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
               EmployeeStatus.PendingInviteApproval {
                  RuleId = ruleId
                  ProgressId = CommandApprovalProgressId e.CorrelationId
               }
            | None ->
               EmployeeStatus.PendingInviteConfirmation {
                  Token = e.Data.InviteToken
                  CorrelationId = e.CorrelationId
               }
         AuthProviderUserId = None
        }
      | CreatedCard e ->
         let info = e.Data.Card

         {
            em with
               Cards = em.Cards |> Map.add info.CardId info
         }
      | ThirdPartyProviderCardLinked e -> {
         em with
            Cards =
               Map.change
                  e.Data.CardId
                  (Option.map (fun card -> {
                     card with
                        Status =
                           CardStatus.Active {
                              ThirdPartyProviderCardId = e.Data.ProviderCardId
                           }
                  }))
                  em.Cards

        }
      | PurchaseSettled e -> {
         em with
            Cards =
               Map.change
                  e.Data.Info.CardId
                  (Option.map (fun card -> {
                     card with
                        LastPurchaseAt = Some DateTime.UtcNow
                  }))
                  em.Cards

        }
      | PurchasePending _ -> em
      | PurchaseFailed _ -> em
      | PurchaseRefunded _ -> em
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
                  (Option.map (fun card ->
                     match card.Status with
                     | CardStatus.Active detail -> {
                        card with
                           Status =
                              CardStatus.Frozen {
                                 Reason = CardFrozenReason.UserRequested
                                 ThirdPartyProviderCardId =
                                    detail.ThirdPartyProviderCardId
                              }
                       }
                     | _ -> card))
                  em.Cards
        }
      | UnlockedCard e -> {
         em with
            Cards =
               Map.change
                  e.Data.CardId
                  (Option.map (fun card ->
                     match card.Status with
                     | CardStatus.Frozen detail -> {
                        card with
                           Status =
                              CardStatus.Active {
                                 ThirdPartyProviderCardId =
                                    detail.ThirdPartyProviderCardId
                              }
                       }
                     | _ -> card))
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
            Status =
               EmployeeStatus.PendingInviteConfirmation {
                  Token = e.Data.InviteToken
                  CorrelationId = e.CorrelationId
               }
        }
      | InvitationConfirmed e -> {
         em with
            Status = EmployeeStatus.Active
            Email = e.Data.Email
            AuthProviderUserId = Some e.Data.AuthProviderUserId
        }
      | AccessApproved e -> {
         em with
            Status =
               EmployeeStatus.PendingInviteConfirmation {
                  Token = e.Data.InviteToken
                  CorrelationId = e.CorrelationId
               }
        }
      | AccessRestored e -> {
         em with
            Status =
               match em.AuthProviderUserId with
               | Some _ -> EmployeeStatus.Active
               | None ->
                  EmployeeStatus.PendingInviteConfirmation {
                     Token = e.Data.InviteToken
                     CorrelationId = e.CorrelationId
                  }
        }

   let updatedPendingDeductions =
      match evt with
      | PurchasePending e ->
         state.PendingPurchaseDeductions.Add e.Data.Info.Amount
      | PurchaseSettled e ->
         state.PendingPurchaseDeductions.Remove e.Data.Info.Amount
      | PurchaseFailed e ->
         state.PendingPurchaseDeductions.Remove e.Data.Info.Amount
      | _ -> state.PendingPurchaseDeductions

   {
      Info = updatedEmployee
      Events = evt :: state.Events
      PendingPurchaseDeductions = updatedPendingDeductions
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
         evt, applyEvent state evt)

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

   let linkThirdPartyProviderCard
      (state: EmployeeSnapshot)
      (cmd: LinkThirdPartyProviderCardCommand)
      =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotReadyToActivate
      else
         map
            ThirdPartyProviderCardLinked
            state
            (LinkThirdPartyProviderCardCommand.toEvent cmd)

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

   let purchaseIntent (state: EmployeeSnapshot) (cmd: PurchaseIntentCommand) =
      let em = state.Info
      let info = cmd.Data

      if em.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         match Map.tryFind info.CardId em.Cards with
         | None -> transitionErr CardNotFound
         | Some card ->
            let dpa =
               dailyPurchaseAccrued state.Events card.CardId
               + state.PendingPurchaseDeductions.Money

            let mpa =
               monthlyPurchaseAccrued state.Events card.CardId
               + state.PendingPurchaseDeductions.Money

            if card.IsPending then
               transitionErr CardPending
            elif card.IsFrozen.IsSome then
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
               map PurchasePending state (PurchaseIntentCommand.toEvent cmd)

   let settlePurchase
      (state: EmployeeSnapshot)
      (cmd: SettlePurchaseWithCardCommand)
      =
      map PurchaseSettled state (SettlePurchaseWithCardCommand.toEvent cmd)

   let failPurchase (state: EmployeeSnapshot) (cmd: FailPurchaseCommand) =
      map PurchaseFailed state (FailPurchaseCommand.toEvent cmd)

   let refundPurchase (state: EmployeeSnapshot) (cmd: RefundPurchaseCommand) =
      let em = state.Info

      if em.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map PurchaseRefunded state (RefundPurchaseCommand.toEvent cmd)

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
   | EmployeeCommand.LinkThirdPartyProviderCard cmd ->
      StateTransition.linkThirdPartyProviderCard state cmd
   | EmployeeCommand.PurchaseIntent cmd ->
      StateTransition.purchaseIntent state cmd
   | EmployeeCommand.SettlePurchase cmd ->
      StateTransition.settlePurchase state cmd
   | EmployeeCommand.FailPurchase cmd -> StateTransition.failPurchase state cmd
   | EmployeeCommand.RefundPurchase cmd ->
      StateTransition.refundPurchase state cmd
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
