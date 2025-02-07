namespace Bank.Employee.Domain

open Validus
open System

open Lib.SharedTypes
open Lib.Validators

type CreateAccountOwnerInput = {
   Email: string
   FirstName: string
   LastName: string
   OrgId: OrgId
   EmployeeId: EmployeeId
}

type CreateAccountOwnerCommand = Command<CreateAccountOwnerInput>

module CreateAccountOwnerCommand =
   let create (data: CreateAccountOwnerInput) =
      Command.create
         (EmployeeId.toEntityId data.EmployeeId)
         data.OrgId
         (CorrelationId.create ())
         (InitiatedById data.EmployeeId)
         data

   let toEvent
      (cmd: CreateAccountOwnerCommand)
      : ValidationResult<BankEvent<CreatedAccountOwner>>
      =
      validate {
         let input = cmd.Data
         let! firstName = firstNameValidator input.FirstName
         and! lastName = lastNameValidator input.LastName
         and! email = Email.ofString "Create employee email" input.Email

         return
            BankEvent.create2<CreateAccountOwnerInput, CreatedAccountOwner> cmd {
               Email = email
               FirstName = firstName
               LastName = lastName
               InviteToken = InviteToken.generate ()
            }
      }

type CreateEmployeeInput = {
   Email: string
   FirstName: string
   LastName: string
   OrgId: OrgId
   Role: Role
   OrgRequiresEmployeeInviteApproval: CommandApprovalRuleId option
   CardInfo: EmployeeInviteSupplementaryCardInfo option
}

type CreateEmployeeCommand = Command<CreateEmployeeInput>

module CreateEmployeeCommand =
   // Remaining employees are created by someone other than self.
   let create (initiatedBy: InitiatedById) (data: CreateEmployeeInput) =
      let employeeId = Guid.NewGuid() |> EmployeeId

      Command.create
         (EmployeeId.toEntityId employeeId)
         data.OrgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: CreateEmployeeCommand)
      : ValidationResult<BankEvent<CreatedEmployee>>
      =
      validate {
         let input = cmd.Data
         let! firstName = firstNameValidator input.FirstName
         and! lastName = lastNameValidator input.LastName
         and! email = Email.ofString "Create employee email" input.Email

         and! _ =
            match input.CardInfo with
            | Some card ->
               amountValidator "Daily purchase limit" card.DailyPurchaseLimit
            | None -> ValidationResult.Ok(0m)

         return
            BankEvent.create2<CreateEmployeeInput, CreatedEmployee> cmd {
               Email = email
               FirstName = firstName
               LastName = lastName
               Role = input.Role
               OrgRequiresEmployeeInviteApproval =
                  input.OrgRequiresEmployeeInviteApproval
               CardInfo = input.CardInfo
            }
      }

type RefreshInvitationTokenInput = { Reason: string option }

type RefreshInvitationTokenCommand = Command<RefreshInvitationTokenInput>

module RefreshInvitationTokenCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: InitiatedById)
      (data: RefreshInvitationTokenInput)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: RefreshInvitationTokenCommand)
      : ValidationResult<BankEvent<InvitationTokenRefreshed>>
      =
      BankEvent.create2<RefreshInvitationTokenInput, InvitationTokenRefreshed>
         cmd
         {
            InviteToken = InviteToken.generate ()
            Reason = cmd.Data.Reason
         }
      |> Ok

type CreateCardInput = {
   PersonName: string
   CardNickname: string option
   Virtual: bool
   CardType: CardType
   InitiatedBy: InitiatedById
   DailyPurchaseLimit: decimal option
   MonthlyPurchaseLimit: decimal option
   AccountId: AccountId
   OrgId: OrgId
   EmployeeId: EmployeeId
   CardId: CardId
}

type CreateCardCommand = Command<CreateCardInput>

module CreateCardCommand =
   let create (data: CreateCardInput) =
      Command.create
         (EmployeeId.toEntityId data.EmployeeId)
         data.OrgId
         (CorrelationId.create ())
         data.InitiatedBy
         data

   let toEvent
      (cmd: CreateCardCommand)
      : ValidationResult<BankEvent<CreatedCard>>
      =
      validate {
         let input = cmd.Data
         let random = System.Random()

         let last4 =
            List.init 4 (fun _ -> random.Next(1, 9) |> string)
            |> String.concat ""

         let! dailyPurchaseLimit =
            match input.DailyPurchaseLimit with
            | Some limit ->
               Card.dailyPurchaseLimitValidator "Daily purchase limit" limit
            | None -> ValidationResult.Ok Constants.DAILY_PURCHASE_LIMIT_DEFAULT

         let! monthlyPurchaseLimit =
            match input.MonthlyPurchaseLimit with
            | Some limit ->
               Card.monthlyPurchaseLimitValidator "Monthly purchase limit" limit
            | None ->
               ValidationResult.Ok Constants.MONTHLY_PURCHASE_LIMIT_DEFAULT

         return
            BankEvent.create2<CreateCardInput, CreatedCard> cmd {
               PersonName = input.PersonName
               Card = {
                  CardNumberLast4 = last4
                  CardNickname = input.CardNickname
                  CardId = cmd.Data.CardId
                  AccountId = cmd.Data.AccountId
                  DailyPurchaseLimit = dailyPurchaseLimit
                  MonthlyPurchaseLimit = monthlyPurchaseLimit
                  Virtual = input.Virtual
                  Status = CardStatus.Active
                  CardType = input.CardType
                  Expiration = CardExpiration.create ()
                  LastPurchaseAt = None
               }
            }
      }

type PurchasePendingInput = {
   CardId: CardId
   CardNumberLast4: string
   AccountId: AccountId
   Amount: decimal
   Merchant: string
   Reference: string option
   Date: DateTime
}

type PurchasePendingCommand = Command<PurchasePendingInput>

// NOTE: CorrelationId traced from
// EmployeeCommand.PurchasePending
// -> AccountCommand.Debit
// -> EmployeeCommand.AccountConfirmsPurchase/AccountRejectsPurchase
module PurchasePendingCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (data: PurchasePendingInput)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         (CorrelationId.create ())
         (InitiatedById employeeId)
         data

   let toEvent
      (cmd: PurchasePendingCommand)
      : ValidationResult<BankEvent<PurchasePending>>
      =
      validate {
         let input = cmd.Data
         let! amount = amountValidator "Debit amount" input.Amount
         let! date = dateNotDefaultValidator "Date" input.Date
         let! merchant = merchantValidator input.Merchant

         return
            BankEvent.create2<PurchasePendingInput, PurchasePending> cmd {
               Info = {
                  EmployeeId = EmployeeId.fromEntityId cmd.EntityId
                  CorrelationId = cmd.CorrelationId
                  CardId = input.CardId
                  CardNumberLast4 = input.CardNumberLast4
                  AccountId = input.AccountId
                  Amount = amount
                  Merchant = merchant
                  Reference = cmd.Data.Reference
                  Date = date
               }
            }
      }

type AccountConfirmsPurchaseCommand = Command<PurchaseConfirmedByAccount>

module AccountConfirmsPurchaseCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (data: PurchaseConfirmedByAccount)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         data.Info.CorrelationId
         (InitiatedById employeeId)
         data

   let toEvent
      (cmd: AccountConfirmsPurchaseCommand)
      : ValidationResult<BankEvent<PurchaseConfirmedByAccount>>
      =
      BankEvent.create<PurchaseConfirmedByAccount> cmd |> Ok

type AccountRejectsPurchaseCommand = Command<PurchaseRejectedByAccount>

module AccountRejectsPurchaseCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (data: PurchaseRejectedByAccount)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         data.Info.CorrelationId
         (InitiatedById employeeId)
         data

   let toEvent
      (cmd: AccountRejectsPurchaseCommand)
      : ValidationResult<BankEvent<PurchaseRejectedByAccount>>
      =
      BankEvent.create<PurchaseRejectedByAccount> cmd |> Ok

type LimitDailyDebitsCommand = Command<DailyDebitLimitUpdated>

module LimitDailyDebitsCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: InitiatedById)
      (data: DailyDebitLimitUpdated)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: LimitDailyDebitsCommand)
      : ValidationResult<BankEvent<DailyDebitLimitUpdated>>
      =
      validate {
         let! _ =
            Card.dailyPurchaseLimitValidator
               "Daily purchase limit"
               cmd.Data.DebitLimit

         return BankEvent.create<DailyDebitLimitUpdated> cmd
      }

type LimitMonthlyDebitsCommand = Command<MonthlyDebitLimitUpdated>

module LimitMonthlyDebitsCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: InitiatedById)
      (data: MonthlyDebitLimitUpdated)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: LimitMonthlyDebitsCommand)
      : ValidationResult<BankEvent<MonthlyDebitLimitUpdated>>
      =
      validate {
         let! _ =
            Card.monthlyPurchaseLimitValidator
               "Monthly purchase limit"
               cmd.Data.DebitLimit

         return BankEvent.create<MonthlyDebitLimitUpdated> cmd
      }

type LockCardCommand = Command<LockedCard>

module LockCardCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: InitiatedById)
      (data: LockedCard)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: LockCardCommand)
      : ValidationResult<BankEvent<LockedCard>>
      =
      Ok <| BankEvent.create<LockedCard> cmd

type UnlockCardCommand = Command<UnlockedCard>

module UnlockCardCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: InitiatedById)
      (data: UnlockedCard)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: UnlockCardCommand)
      : ValidationResult<BankEvent<UnlockedCard>>
      =
      Ok <| BankEvent.create<UnlockedCard> cmd

type UpdateRoleCommand = Command<RoleUpdated>

module UpdateRoleCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: InitiatedById)
      (data: RoleUpdated)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: UpdateRoleCommand)
      : ValidationResult<BankEvent<RoleUpdated>>
      =
      validate {
         let! _ =
            match cmd.Data.CardInfo with
            | Some card ->
               amountValidator "Daily purchase limit" card.DailyPurchaseLimit
            | None -> ValidationResult.Ok(0m)

         return BankEvent.create<RoleUpdated> cmd
      }

type EditCardNicknameCommand = Command<CardNicknamed>

module EditCardNicknameCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: InitiatedById)
      (data: CardNicknamed)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: EditCardNicknameCommand)
      : ValidationResult<BankEvent<CardNicknamed>>
      =
      Ok <| BankEvent.create<CardNicknamed> cmd


type CancelInvitationCommand = Command<InvitationCancelled>

module CancelInvitationCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: InitiatedById)
      (data: InvitationCancelled)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: CancelInvitationCommand)
      : ValidationResult<BankEvent<InvitationCancelled>>
      =
      Ok <| BankEvent.create<InvitationCancelled> cmd

type ConfirmInvitationCommand = Command<InvitationConfirmed>

module ConfirmInvitationCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (data: InvitationConfirmed)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         (CorrelationId.create ())
         (InitiatedById employeeId)
         data

   let toEvent
      (cmd: ConfirmInvitationCommand)
      : ValidationResult<BankEvent<InvitationConfirmed>>
      =
      Ok <| BankEvent.create<InvitationConfirmed> cmd

type RestoreAccessCommand = Command<AccessRestored>

module RestoreAccessCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: InitiatedById)
      (data: AccessRestored)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: RestoreAccessCommand)
      : ValidationResult<BankEvent<AccessRestored>>
      =
      Ok <| BankEvent.create<AccessRestored> cmd

type ApproveAccessInput = {
   Name: string
   Reference: string option
}

type ApproveAccessCommand = Command<ApproveAccessInput>

module ApproveAccessCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: InitiatedById)
      (correlationId: CorrelationId)
      (data: ApproveAccessInput)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: ApproveAccessCommand)
      : ValidationResult<BankEvent<AccessApproved>>
      =
      BankEvent.create2<ApproveAccessInput, AccessApproved> cmd {
         InviteToken = InviteToken.generate ()
         Name = cmd.Data.Name
         Reference = cmd.Data.Reference
      }
      |> Ok
