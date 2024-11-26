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
         | DebitApproved e ->
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

let dailyAccrual (events: EmployeeEvent list) : EmployeeDailyAccrual =
   List.fold
      (fun acc evt ->
         match evt with
         | DomesticTransferConfirmed e ->
            let info = e.Data.Info

            if DateTime.isToday info.ScheduledDate then
               {
                  acc with
                     DomesticTransfer = acc.DomesticTransfer + info.Amount
               }
            else
               acc
         | _ -> acc)
      {
         PaymentsPaid = 0m
         InternalTransferBetweenOrgs = 0m
         DomesticTransfer = 0m
      }
      events

let applyEvent
   (state: EmployeeWithEvents)
   (evt: EmployeeEvent)
   : EmployeeWithEvents
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
            | Some _ -> EmployeeStatus.PendingInviteApproval
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
      | DebitRequested e -> {
         em with
            PendingPurchases =
               em.PendingPurchases |> Map.add e.CorrelationId e.Data.Info
        }
      | DebitApproved e -> {
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
      | DebitDeclined e -> {
         em with
            PendingPurchases = em.PendingPurchases |> Map.remove e.CorrelationId
        }
      | DomesticTransferRequested _ -> em
      | DomesticTransferConfirmed _ -> em
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
      | AccessApproved _
      | AccessRestored _ -> {
         em with
            Status = EmployeeStatus.Active
        }
      | CommandApprovalRuleConfigured _ -> em
      | CommandApprovalRequested _ -> em
      | CommandApprovalAcquired _ -> em
      | CommandApprovalDeclined e ->
         match e.Data.CommandType with
         | ApprovableCommandType.InviteEmployee -> {
            em with
               Status = EmployeeStatus.Closed
           }
         | _ -> em

   {
      Info = updatedEmployee
      Events = evt :: state.Events
   }

module private StateTransition =
   let transitionErr (err: EmployeeStateTransitionError) =
      Error <| EmployeeStateTransitionError err

   let map
      (eventTransform: BankEvent<'t> -> EmployeeEvent)
      (state: EmployeeWithEvents)
      (eventValidation: ValidationResult<BankEvent<'t>>)
      =
      eventValidation
      |> Result.mapError ValidationError
      |> Result.map (fun evt ->
         let evt = eventTransform evt
         (evt, applyEvent state evt))

   let createAccountOwner
      (state: EmployeeWithEvents)
      (cmd: CreateAccountOwnerCommand)
      =
      if state.Info.Status <> EmployeeStatus.InitialEmptyState then
         transitionErr EmployeeNotReadyToActivate
      else
         map CreatedAccountOwner state (CreateAccountOwnerCommand.toEvent cmd)

   let create (state: EmployeeWithEvents) (cmd: CreateEmployeeCommand) =
      if state.Info.Status <> EmployeeStatus.InitialEmptyState then
         transitionErr EmployeeNotReadyToActivate
      else
         map CreatedEmployee state (CreateEmployeeCommand.toEvent cmd)

   let createCard (state: EmployeeWithEvents) (cmd: CreateCardCommand) =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map CreatedCard state (CreateCardCommand.toEvent cmd)

   let limitDailyDebits
      (state: EmployeeWithEvents)
      (cmd: LimitDailyDebitsCommand)
      =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map DailyDebitLimitUpdated state (LimitDailyDebitsCommand.toEvent cmd)

   let limitMonthlyDebits
      (state: EmployeeWithEvents)
      (cmd: LimitMonthlyDebitsCommand)
      =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map
            MonthlyDebitLimitUpdated
            state
            (LimitMonthlyDebitsCommand.toEvent cmd)

   let lockCard (state: EmployeeWithEvents) (cmd: LockCardCommand) =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map LockedCard state (LockCardCommand.toEvent cmd)

   let unlockCard (state: EmployeeWithEvents) (cmd: UnlockCardCommand) =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map UnlockedCard state (UnlockCardCommand.toEvent cmd)

   let debitRequest (state: EmployeeWithEvents) (cmd: DebitRequestCommand) =
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
               map DebitRequested state (DebitRequestCommand.toEvent cmd)

   let approveDebit (state: EmployeeWithEvents) (cmd: ApproveDebitCommand) =
      let em = state.Info

      if em.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      elif
         Option.isNone <| Map.tryFind cmd.CorrelationId em.PendingPurchases
      then
         transitionErr DebitAlreadyProgressedToApprovedOrDeclined
      else
         map DebitApproved state (ApproveDebitCommand.toEvent cmd)

   let declineDebit (state: EmployeeWithEvents) (cmd: DeclineDebitCommand) =
      let em = state.Info

      if em.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      elif
         Option.isNone <| Map.tryFind cmd.CorrelationId em.PendingPurchases
      then
         transitionErr DebitAlreadyProgressedToApprovedOrDeclined
      else
         map DebitDeclined state (DeclineDebitCommand.toEvent cmd)

   let requestDomesticTransfer
      (state: EmployeeWithEvents)
      (cmd: RequestDomesticTransferCommand)
      =
      let em = state.Info

      if em.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map
            DomesticTransferRequested
            state
            (RequestDomesticTransferCommand.toEvent cmd)

   let confirmDomesticTransfer
      (state: EmployeeWithEvents)
      (cmd: ConfirmDomesticTransferCommand)
      =
      let em = state.Info

      if em.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map
            DomesticTransferConfirmed
            state
            (ConfirmDomesticTransferCommand.toEvent cmd)

   let updateRole (state: EmployeeWithEvents) (cmd: UpdateRoleCommand) =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map UpdatedRole state (UpdateRoleCommand.toEvent cmd)

   let nicknameCard (state: EmployeeWithEvents) (cmd: EditCardNicknameCommand) =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map CardNicknamed state (EditCardNicknameCommand.toEvent cmd)

   let refreshEmployeeInvitationToken
      (state: EmployeeWithEvents)
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
      (state: EmployeeWithEvents)
      (cmd: CancelInvitationCommand)
      =
      let em = state.Info

      match em.Status with
      | EmployeeStatus.PendingInviteConfirmation _ ->
         map InvitationCancelled state (CancelInvitationCommand.toEvent cmd)
      | _ ->
         transitionErr
         <| EmployeeStatusDisallowsInviteProgression(string em.Status)

   let confirmEmployeeInvitation
      (state: EmployeeWithEvents)
      (cmd: ConfirmInvitationCommand)
      =
      let em = state.Info

      match em.Status with
      | EmployeeStatus.PendingInviteConfirmation _ ->
         map InvitationConfirmed state (ConfirmInvitationCommand.toEvent cmd)
      | _ ->
         transitionErr
         <| EmployeeStatusDisallowsInviteProgression(string em.Status)

   let approveAccess (state: EmployeeWithEvents) (cmd: ApproveAccessCommand) =
      let em = state.Info

      if em.Status <> EmployeeStatus.PendingInviteApproval then
         transitionErr
         <| EmployeeStatusDisallowsInviteProgression(string em.Status)
      else
         map AccessApproved state (ApproveAccessCommand.toEvent cmd)

   let restoreAccess (state: EmployeeWithEvents) (cmd: RestoreAccessCommand) =
      let em = state.Info

      if em.Status <> EmployeeStatus.Closed then
         transitionErr <| EmployeeStatusDisallowsAccessRestore(string em.Status)
      else
         map AccessRestored state (RestoreAccessCommand.toEvent cmd)

   let configureCommandApprovalRule
      (state: EmployeeWithEvents)
      (cmd: CommandApprovalRule.ConfigureApprovalRuleCommand)
      =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map
            CommandApprovalRuleConfigured
            state
            (CommandApprovalRule.ConfigureApprovalRuleCommand.toEvent cmd)

   let requestCommandApproval
      (state: EmployeeWithEvents)
      (cmd: CommandApprovalProgress.RequestCommandApproval)
      =
      let em = state.Info

      let applyApprovalRequest () =
         map
            CommandApprovalRequested
            state
            (CommandApprovalProgress.RequestCommandApproval.toEvent cmd)

      match cmd.Data.Command with
      | ApprovableCommand.InviteEmployee _ ->
         if em.Status <> EmployeeStatus.PendingInviteApproval then
            transitionErr
            <| EmployeeStatusDisallowsInviteProgression(string em.Status)
         else
            applyApprovalRequest ()
      | _ ->
         if em.Status <> EmployeeStatus.Active then
            transitionErr EmployeeNotActive
         else
            map
               CommandApprovalRequested
               state
               (CommandApprovalProgress.RequestCommandApproval.toEvent cmd)

   let acquireCommandApproval
      (state: EmployeeWithEvents)
      (cmd: CommandApprovalProgress.AcquireCommandApproval)
      =
      let em = state.Info

      let applyApprovalAcquired () =
         map
            CommandApprovalAcquired
            state
            (CommandApprovalProgress.AcquireCommandApproval.toEvent cmd)

      match cmd.Data.CommandType with
      | ApprovableCommandType.InviteEmployee ->
         if em.Status <> EmployeeStatus.PendingInviteApproval then
            transitionErr
            <| EmployeeStatusDisallowsInviteProgression(string em.Status)
         else
            applyApprovalAcquired ()
      | _ ->
         if state.Info.Status <> EmployeeStatus.Active then
            transitionErr EmployeeNotActive
         else
            applyApprovalAcquired ()

   let declineCommandApproval
      (state: EmployeeWithEvents)
      (cmd: CommandApprovalProgress.DeclineCommandApproval)
      =
      if state.Info.Status <> EmployeeStatus.Active then
         transitionErr EmployeeNotActive
      else
         map
            CommandApprovalDeclined
            state
            (CommandApprovalProgress.DeclineCommandApproval.toEvent cmd)

let stateTransition (state: EmployeeWithEvents) (command: EmployeeCommand) =
   match command with
   | CreateAccountOwner cmd -> StateTransition.createAccountOwner state cmd
   | CreateEmployee cmd -> StateTransition.create state cmd
   | CreateCard cmd -> StateTransition.createCard state cmd
   | DebitRequest cmd -> StateTransition.debitRequest state cmd
   | ApproveDebit cmd -> StateTransition.approveDebit state cmd
   | DeclineDebit cmd -> StateTransition.declineDebit state cmd
   | RequestDomesticTransfer cmd ->
      StateTransition.requestDomesticTransfer state cmd
   | ConfirmDomesticTransfer cmd ->
      StateTransition.confirmDomesticTransfer state cmd
   | LimitDailyDebits cmd -> StateTransition.limitDailyDebits state cmd
   | LimitMonthlyDebits cmd -> StateTransition.limitMonthlyDebits state cmd
   | LockCard cmd -> StateTransition.lockCard state cmd
   | UnlockCard cmd -> StateTransition.unlockCard state cmd
   | UpdateRole cmd -> StateTransition.updateRole state cmd
   | EditCardNickname cmd -> StateTransition.nicknameCard state cmd
   | CancelInvitation cmd -> StateTransition.cancelEmployeeInvitation state cmd
   | RefreshInvitationToken cmd ->
      StateTransition.refreshEmployeeInvitationToken state cmd
   | ConfirmInvitation cmd ->
      StateTransition.confirmEmployeeInvitation state cmd
   | ApproveAccess cmd -> StateTransition.approveAccess state cmd
   | RestoreAccess cmd -> StateTransition.restoreAccess state cmd
   | ConfigureApprovalRule cmd ->
      StateTransition.configureCommandApprovalRule state cmd
   | RequestCommandApproval cmd ->
      StateTransition.requestCommandApproval state cmd
   | AcquireCommandApproval cmd ->
      StateTransition.acquireCommandApproval state cmd
   | DeclineCommandApproval cmd ->
      StateTransition.declineCommandApproval state cmd

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
