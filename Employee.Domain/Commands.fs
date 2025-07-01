namespace Bank.Employee.Domain

open Validus
open System

open Lib.SharedTypes
open Lib.Validators
open Bank.Account.Domain

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
         {
            Name = $"{data.FirstName} {data.LastName}"
            Id = InitiatedById data.EmployeeId
         }
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
   let create (initiator: Initiator) (data: CreateEmployeeInput) =
      let employeeId = Guid.NewGuid() |> EmployeeId

      Command.create
         (EmployeeId.toEntityId employeeId)
         data.OrgId
         (CorrelationId.create ())
         initiator
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
               InviteToken = InviteToken.generate ()
            }
      }

type RefreshInvitationTokenInput = { Reason: string option }

type RefreshInvitationTokenCommand = Command<RefreshInvitationTokenInput>

module RefreshInvitationTokenCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: Initiator)
      (correlationId: CorrelationId)
      (data: RefreshInvitationTokenInput)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         correlationId
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
   InitiatedBy: Initiator
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
                  ThirdPartyProviderCardId = None
                  DailyPurchaseLimit = dailyPurchaseLimit
                  MonthlyPurchaseLimit = monthlyPurchaseLimit
                  Virtual = input.Virtual
                  Status = CardStatus.Pending
                  CardType = input.CardType
                  Expiration = CardExpiration.create ()
                  LastPurchaseAt = None
               }
            }
      }

type LinkThirdPartyProviderCardInput = {
   CardId: CardId
   ProviderCardId: ThirdPartyProviderCardId
   CardNumberLast4: string
   OrgId: OrgId
   EmployeeId: EmployeeId
   InitiatedBy: Initiator
   CorrelationId: CorrelationId
}

type LinkThirdPartyProviderCardCommand =
   Command<LinkThirdPartyProviderCardInput>

module LinkThirdPartyProviderCardCommand =
   let create (data: LinkThirdPartyProviderCardInput) =
      Command.create
         (EmployeeId.toEntityId data.EmployeeId)
         data.OrgId
         data.CorrelationId
         data.InitiatedBy
         data

   let toEvent
      (cmd: LinkThirdPartyProviderCardCommand)
      : ValidationResult<BankEvent<ThirdPartyProviderCardLinked>>
      =
      BankEvent.create2<
         LinkThirdPartyProviderCardInput,
         ThirdPartyProviderCardLinked
       >
         cmd
         {
            CardId = cmd.Data.CardId
            ProviderCardId = cmd.Data.ProviderCardId
            CardNumberLast4 = cmd.Data.CardNumberLast4
         }
      |> Ok

type PurchaseIntentCommand = Command<PurchaseInfo>

module PurchaseIntentCommand =
   let create (data: PurchaseInfo) =
      Command.create
         (data.InitiatedBy.Id |> InitiatedById.get |> EntityId)
         data.OrgId
         data.CorrelationId
         data.InitiatedBy
         data

   let toEvent
      (cmd: PurchaseIntentCommand)
      : ValidationResult<BankEvent<CardPurchasePending>>
      =
      validate {
         let input = cmd.Data
         let! _ = amountValidator "Debit amount" input.Amount
         let! _ = dateNotDefaultValidator "Date" input.Date
         let! _ = merchantValidator input.Merchant

         return
            BankEvent.create2<PurchaseInfo, CardPurchasePending> cmd {
               Info = cmd.Data
            }
      }

type SettlePurchaseWithCardCommand = Command<CardPurchaseSettled>

module SettlePurchaseWithCardCommand =
   let create (data: CardPurchaseSettled) =
      Command.create
         (data.Info.InitiatedBy.Id |> InitiatedById.get |> EntityId)
         data.Info.OrgId
         data.Info.CorrelationId
         data.Info.InitiatedBy
         data

   let toEvent
      (cmd: SettlePurchaseWithCardCommand)
      : ValidationResult<BankEvent<CardPurchaseSettled>>
      =
      BankEvent.create<CardPurchaseSettled> cmd |> Ok

type FailPurchaseCommand = Command<CardPurchaseFailed>

module FailPurchaseCommand =
   let create (data: CardPurchaseFailed) =
      Command.create
         (data.Info.InitiatedBy.Id |> InitiatedById.get |> EntityId)
         data.Info.OrgId
         data.Info.CorrelationId
         data.Info.InitiatedBy
         data

   let toEvent
      (cmd: FailPurchaseCommand)
      : ValidationResult<BankEvent<CardPurchaseFailed>>
      =
      BankEvent.create<CardPurchaseFailed> cmd |> Ok

type RefundPurchaseCommand = Command<CardPurchaseRefunded>

module RefundPurchaseCommand =
   let create (data: CardPurchaseRefunded) =
      Command.create
         (data.Info.InitiatedBy.Id |> InitiatedById.get |> EntityId)
         data.Info.OrgId
         data.Info.CorrelationId
         data.Info.InitiatedBy
         data

   let toEvent
      (cmd: RefundPurchaseCommand)
      : ValidationResult<BankEvent<CardPurchaseRefunded>>
      =
      BankEvent.create<CardPurchaseRefunded> cmd |> Ok

type ConfigureRollingPurchaseLimitCommand =
   Command<ConfiguredRollingPurchaseLimit>

module ConfigureRollingPurchaseLimitCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: Initiator)
      (data: ConfiguredRollingPurchaseLimit)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: ConfigureRollingPurchaseLimitCommand)
      : ValidationResult<BankEvent<ConfiguredRollingPurchaseLimit>>
      =
      validate {
         let! _ =
            Card.dailyPurchaseLimitValidator
               "Daily purchase limit"
               cmd.Data.DailyLimit

         and! _ =
            Card.monthlyPurchaseLimitValidator
               "Monthly purchase limit"
               cmd.Data.MonthlyLimit

         return BankEvent.create<ConfiguredRollingPurchaseLimit> cmd
      }

type LockCardCommand = Command<LockedCard>

module LockCardCommand =
   let create
      (employeeId: EmployeeId, orgId: OrgId)
      (initiatedBy: Initiator)
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
      (initiatedBy: Initiator)
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
      (initiatedBy: Initiator)
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
      (initiatedBy: Initiator)
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
      (initiatedBy: Initiator)
      (corrId: CorrelationId)
      (data: InvitationCancelled)
      =
      Command.create
         (EmployeeId.toEntityId employeeId)
         orgId
         corrId
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
      (initiatedBy: Initiator)
      (orgId: OrgId)
      (correlationId: CorrelationId)
      (data: InvitationConfirmed)
      =
      Command.create
         (initiatedBy.Id |> InitiatedById.toEmployeeId |> EmployeeId.toEntityId)
         orgId
         correlationId
         initiatedBy
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
      (initiatedBy: Initiator)
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
      (initiatedBy: Initiator)
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
