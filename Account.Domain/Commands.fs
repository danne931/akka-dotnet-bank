namespace Bank.Account.Domain

open Validus

open Lib.SharedTypes
open Lib.Validators

type CreateAccountInput = {
   Name: string
   AccountNumber: string
   Depository: AccountDepository
   Currency: Currency
   AccountId: AccountId
   ParentAccountId: ParentAccountId
   OrgId: OrgId
   InitiatedBy: Initiator
}

type CreateAccountCommand = Command<CreateAccountInput>

module CreateAccountCommand =
   let create (data: CreateAccountInput) =
      Command.create
         (ParentAccountId.toEntityId data.ParentAccountId)
         data.OrgId
         (CorrelationId.create ())
         data.InitiatedBy
         data

   let toEvent
      (cmd: CreateAccountCommand)
      : ValidationResult<BankEvent<CreatedAccount>>
      =
      validate {
         let input = cmd.Data

         let! accountName = accountNameValidator input.Name

         and! accountNumber =
            AccountNumber.fromString "Account Number" input.AccountNumber

         and! routingNumber =
            RoutingNumber.fromString "Routing Number" "123456789"

         return
            BankEvent.create2<CreateAccountInput, CreatedAccount> cmd {
               Name = accountName
               Depository = input.Depository
               Balance = 0m
               Currency = input.Currency
               RoutingNumber = routingNumber
               AccountNumber = accountNumber
               AccountId = input.AccountId
            }
      }

type DepositCashInput = {
   AccountId: AccountId
   Amount: decimal
   Origin: string option
}

type DepositCashCommand = Command<DepositCashInput>

module DepositCashCommand =
   let create
      (parentAccountId: ParentAccountId, orgId: OrgId)
      (initiator: Initiator)
      (data: DepositCashInput)
      =
      Command.create
         (ParentAccountId.toEntityId parentAccountId)
         orgId
         (CorrelationId.create ())
         initiator
         data

   let toEvent
      (cmd: DepositCashCommand)
      : ValidationResult<BankEvent<DepositedCash>>
      =
      validate {
         let input = cmd.Data
         let! _ = amountValidator "Deposit amount" input.Amount

         return
            BankEvent.create2<DepositCashInput, DepositedCash> cmd {
               AccountId = input.AccountId
               Amount = input.Amount
               Origin = input.Origin |> Option.defaultValue "ATM"
            }
      }

type DebitCommand = Command<DebitedAccount>

module DebitCommand =
   let create
      (parentAccountId: ParentAccountId, orgId: OrgId)
      (correlationId: CorrelationId)
      (initiator: Initiator)
      (data: DebitedAccount)
      =
      Command.create
         (ParentAccountId.toEntityId parentAccountId)
         orgId
         correlationId
         initiator
         data

   let fromPurchase (info: PurchaseInfo) =
      create
         (info.ParentAccountId, info.OrgId)
         info.CorrelationId
         info.InitiatedBy
         {
            AccountId = info.AccountId
            Date = info.Date
            Amount = info.Amount
            Merchant = info.Merchant
            Reference = info.Reference
            EmployeePurchaseReference = {
               EmployeeName = info.EmployeeName
               EmployeeCardNumberLast4 = info.CardNumberLast4
               EmployeeId = InitiatedById.toEmployeeId info.InitiatedBy.Id
               CardId = info.CardId
            }
         }

   let toEvent
      (cmd: DebitCommand)
      : ValidationResult<BankEvent<DebitedAccount>>
      =
      validate {
         let input = cmd.Data
         let! _ = amountValidator "Debit amount" input.Amount
         let! _ = dateNotDefaultValidator "Date" input.Date
         let! _ = merchantValidator input.Merchant

         return BankEvent.create<DebitedAccount> cmd
      }

type RefundDebitCommand = Command<RefundedDebit>

module RefundDebitCommand =
   let create
      (parentAccountId: ParentAccountId, orgId: OrgId)
      (correlationId: CorrelationId)
      (initiator: Initiator)
      (data: RefundedDebit)
      =
      Command.create
         (ParentAccountId.toEntityId parentAccountId)
         orgId
         correlationId
         initiator
         data

   let fromPurchase
      (purchaseInfo: PurchaseInfo)
      (reason: PurchaseRefundReason)
      =
      create
         (purchaseInfo.ParentAccountId, purchaseInfo.OrgId)
         purchaseInfo.CorrelationId
         purchaseInfo.InitiatedBy
         {
            AccountId = purchaseInfo.AccountId
            EmployeePurchaseReference = {
               EmployeeName = purchaseInfo.EmployeeName
               EmployeeId = purchaseInfo.EmployeeId
               EmployeeCardNumberLast4 = purchaseInfo.CardNumberLast4
               CardId = purchaseInfo.CardId
            }
            Merchant = purchaseInfo.Merchant
            Amount = purchaseInfo.Amount
            Reason = reason
         }

   let toEvent
      (cmd: RefundDebitCommand)
      : ValidationResult<BankEvent<RefundedDebit>>
      =
      BankEvent.create<RefundedDebit> cmd |> Ok

type MaintenanceFeeCommand = Command<MaintenanceFeeDebited>

module MaintenanceFeeCommand =
   let create
      (accountId: AccountId, parentAccountId: ParentAccountId, orgId: OrgId)
      =
      Command.create
         (ParentAccountId.toEntityId parentAccountId)
         orgId
         (CorrelationId.create ())
         Initiator.System
         {
            AccountId = accountId
            Amount = MaintenanceFee.RecurringDebitAmount
         }

   let toEvent
      (cmd: MaintenanceFeeCommand)
      : ValidationResult<BankEvent<MaintenanceFeeDebited>>
      =
      Ok <| BankEvent.create<MaintenanceFeeDebited> cmd

type SkipMaintenanceFeeCommand = Command<MaintenanceFeeSkipped>

module SkipMaintenanceFeeCommand =
   let create
      (accountId: AccountId, parentAccountId: ParentAccountId, orgId: OrgId)
      (criteria: MaintenanceFee.MaintenanceFeeCriteria)
      =
      Command.create
         (ParentAccountId.toEntityId parentAccountId)
         orgId
         (CorrelationId.create ())
         Initiator.System
         {
            AccountId = accountId
            Reason = criteria
         }

   let toEvent
      (cmd: SkipMaintenanceFeeCommand)
      : ValidationResult<BankEvent<MaintenanceFeeSkipped>>
      =
      Ok <| BankEvent.create<MaintenanceFeeSkipped> cmd

type CloseAccountCommand = Command<AccountClosed>

module CloseAccountCommand =
   let create
      (parentAccountId: ParentAccountId, orgId: OrgId)
      (initiator: Initiator)
      (data: AccountClosed)
      =
      Command.create
         (ParentAccountId.toEntityId parentAccountId)
         orgId
         (CorrelationId.create ())
         initiator
         data

   let toEvent
      (cmd: CloseAccountCommand)
      : ValidationResult<BankEvent<AccountClosed>>
      =
      Ok <| BankEvent.create<AccountClosed> cmd

type StartBillingCycleCommand = Command<BillingCycleStarted>

module StartBillingCycleCommand =
   let create
      (parentAccountId: ParentAccountId, orgId: OrgId)
      (data: BillingCycleStarted)
      =
      Command.create
         (ParentAccountId.toEntityId parentAccountId)
         orgId
         (CorrelationId.create ())
         Initiator.System
         data

   let toEvent
      (cmd: StartBillingCycleCommand)
      : ValidationResult<BankEvent<BillingCycleStarted>>
      =
      Ok <| BankEvent.create<BillingCycleStarted> cmd
