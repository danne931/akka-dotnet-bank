[<RequireQualifiedAccess>]
module Employee

open Validus
open System

open Bank.Employee.Domain
open Bank.Purchase.Domain
open Lib.SharedTypes
open Lib.Time

let private purchaseAccrued
   (satisfiesDate: DateTime -> bool)
   (events: EmployeeEvent list)
   (pendingPurchaseDeductions: Map<CardId, PendingFunds>)
   (cardId: CardId)
   : decimal
   =
   let pendingAmt =
      pendingPurchaseDeductions
      |> Map.tryFind cardId
      |> Option.map (PendingFunds.amount >> _.Out)
      |> Option.defaultValue 0m

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
      pendingAmt
      events

let dailyPurchaseAccrued = purchaseAccrued DateTime.isToday
let monthlyPurchaseAccrued = purchaseAccrued DateTime.isThisMonth

let addPendingPurchase (state: EmployeeSnapshot) (info: PurchaseInfo) =
   let txnId = info.CardIssuerTransactionId

   let fund = {
      Amount = info.Amount
      Flow = MoneyFlow.Out
   }

   {
      state with
         PendingPurchases =
            state.PendingPurchases
            |> Map.add txnId {
               Info = info
               Events = []
               Status = PurchaseStatus.Pending
            }
         PendingPurchaseDeductions =
            state.PendingPurchaseDeductions
            |> Map.change
                  info.CardId
                  (Option.map (PendingFunds.add txnId.Value fund))
   }

let removePendingPurchase (state: EmployeeSnapshot) (info: PurchaseInfo) =
   let txnId = info.CardIssuerTransactionId

   {
      state with
         PendingPurchases = Map.remove txnId state.PendingPurchases
         PendingPurchaseDeductions =
            state.PendingPurchaseDeductions
            |> Map.change
                  info.CardId
                  (Option.map (PendingFunds.remove txnId.Value))
   }

let updatePendingPurchase
   (state: EmployeeSnapshot)
   (progress: CardIssuerPurchaseProgress)
   =
   let txnId = progress.PurchaseId

   let updateProgress (purchase: Purchase) =
      // Purchase amount may change when receiving AuthAdvice events
      let amount =
         match progress.Status with
         | PurchaseStatus.Pending -> progress.Amounts.Hold.Amount
         | PurchaseStatus.Settled -> progress.Amounts.Settlement.Amount
         | PurchaseStatus.Declined
         | PurchaseStatus.Voided
         | PurchaseStatus.Expired -> purchase.Info.Amount

      {
         purchase with
            Status = progress.Status
            Events = purchase.Events @ progress.Events
            Info.Amount = amount
      }

   {
      state with
         PendingPurchases =
            state.PendingPurchases
            |> Map.change txnId (Option.map updateProgress)
   }

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
         ParentAccountId = e.Data.ParentAccountId
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
         ParentAccountId = e.Data.ParentAccountId
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
      | CardLinked e -> {
         em with
            Cards =
               Map.change
                  e.Data.Link.CardId
                  (Option.map (fun card -> {
                     card with
                        Status = CardStatus.Active
                        CardNumberLast4 = e.Data.CardNumberLast4
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
                        LastPurchaseAt = Some e.Timestamp
                  }))
                  em.Cards

        }
      | PurchasePending _ -> em
      | PurchaseProgress _ -> em
      | PurchaseFailed _ -> em
      | PurchaseRefunded _ -> em
      | ConfiguredRollingPurchaseLimit e -> {
         em with
            Cards =
               Map.change
                  e.Data.CardId
                  (Option.map (fun card -> {
                     card with
                        DailyPurchaseLimit = e.Data.DailyLimit
                        MonthlyPurchaseLimit = e.Data.MonthlyLimit
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
                        Status =
                           CardStatus.Frozen CardFrozenReason.UserRequested
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
      | UpdatedRole e -> {
         em with
            Role = e.Data.Role
            Cards =
               match e.Data.Role with
               | Role.Scholar ->
                  em.Cards
                  |> Map.map (fun _ card -> {
                     card with
                        Status =
                           CardStatus.Closed CardClosedReason.EndUserRequest
                  })
               | _ -> em.Cards
        }
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
               EmployeeStatus.PendingInviteConfirmation {
                  Token = e.Data.InviteToken
                  CorrelationId = e.CorrelationId
               }
        }

   let state =
      match evt with
      | CreatedCard e -> {
         state with
            PendingPurchaseDeductions =
               state.PendingPurchaseDeductions
               |> Map.add e.Data.Card.CardId PendingFunds.zero
        }
      | PurchasePending e -> addPendingPurchase state e.Data.Info
      | PurchaseSettled e -> removePendingPurchase state e.Data.Info
      | PurchaseFailed e -> removePendingPurchase state e.Data.Info
      | PurchaseProgress e -> updatePendingPurchase state e.Data.Info
      | _ -> state

   let updatedCardIssuerLinks =
      match evt with
      | CardLinked e ->
         state.CardIssuerLinks |> Map.add e.Data.Link.CardId e.Data.Link
      | _ -> state.CardIssuerLinks

   {
      Info = updatedEmployee
      Events = evt :: state.Events
      PendingPurchaseDeductions = state.PendingPurchaseDeductions
      PendingPurchases = state.PendingPurchases
      CardIssuerLinks = updatedCardIssuerLinks
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

   let linkCard (state: EmployeeSnapshot) (cmd: LinkCardCommand) =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotReadyToActivate
      else
         map CardLinked state (LinkCardCommand.toEvent cmd)

   let configureRollingPurchaseLimit
      (state: EmployeeSnapshot)
      (cmd: ConfigureRollingPurchaseLimitCommand)
      =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map
            ConfiguredRollingPurchaseLimit
            state
            (ConfigureRollingPurchaseLimitCommand.toEvent cmd)

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
               dailyPurchaseAccrued
                  state.Events
                  state.PendingPurchaseDeductions
                  card.CardId

            let mpa =
               monthlyPurchaseAccrued
                  state.Events
                  state.PendingPurchaseDeductions
                  card.CardId

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

   let purchaseProgress
      (state: EmployeeSnapshot)
      (cmd: PurchaseProgressCommand)
      =
      let update = cmd.Data

      match state.PendingPurchases.TryFind update.PurchaseId with
      | None -> transitionErr PurchaseProgressPurchaseNotFound
      | Some purchase ->
         let existingEvtIds =
            purchase.Events |> List.map _.EventId |> Set.ofList

         let evtsToAdd =
            update.Events
            |> List.filter (_.EventId >> existingEvtIds.Contains >> not)
            |> List.sortBy _.CreatedAt

         if evtsToAdd.Length = 0 then
            transitionErr PurchaseProgressNoAdditionalEvents
         else
            let cmd = { cmd with Data.Events = evtsToAdd }
            map PurchaseProgress state (PurchaseProgressCommand.toEvent cmd)

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
   | EmployeeCommand.LinkCard cmd -> StateTransition.linkCard state cmd
   | EmployeeCommand.PurchaseIntent cmd ->
      StateTransition.purchaseIntent state cmd
   | EmployeeCommand.PurchaseProgress cmd ->
      StateTransition.purchaseProgress state cmd
   | EmployeeCommand.SettlePurchase cmd ->
      StateTransition.settlePurchase state cmd
   | EmployeeCommand.FailPurchase cmd -> StateTransition.failPurchase state cmd
   | EmployeeCommand.RefundPurchase cmd ->
      StateTransition.refundPurchase state cmd
   | EmployeeCommand.ConfigureRollingPurchaseLimit cmd ->
      StateTransition.configureRollingPurchaseLimit state cmd
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
