namespace Bank.Account.Domain

open Validus
open System

open Lib.SharedTypes
open Lib.Validators
open Bank.Purchase.Domain

type InitializePrimaryCheckingAccountInput = {
   OrgId: OrgId
   CorrelationId: CorrelationId
   ParentAccountId: ParentAccountId
   PartnerBankLink: PartnerBankInternalAccountLink
}

type InitializePrimaryCheckingAccountCommand =
   Command<InitializePrimaryCheckingAccountInput>

module InitializePrimaryCheckingAccountCommand =
   let create (data: InitializePrimaryCheckingAccountInput) =
      Command.create
         data.ParentAccountId.AsEntityId
         data.OrgId
         data.CorrelationId
         Initiator.System
         data

   let toEvent
      (cmd: InitializePrimaryCheckingAccountCommand)
      : ValidationResult<BankEvent<InitializedPrimaryCheckingAccount>>
      =
      validate {
         let input = cmd.Data

         return
            BankEvent.create2<
               InitializePrimaryCheckingAccountInput,
               InitializedPrimaryCheckingAccount
             >
               cmd
               {
                  OrgId = input.OrgId
                  ParentAccountId = input.ParentAccountId
                  PartnerBankLink = input.PartnerBankLink
                  PrimaryChecking = {
                     Name = "Checking"
                     AccountId = Guid.NewGuid() |> AccountId
                     AccountNumber = AccountNumber.generate ()
                     RoutingNumber = RoutingNumber.Empty
                  }
               }
      }

type CreateVirtualAccountInput = {
   Name: string
   AccountNumber: string
   Depository: AccountDepository
   Currency: Currency
   AccountId: AccountId
   ParentAccountId: ParentAccountId
   OrgId: OrgId
   InitiatedBy: Initiator
}

type CreateVirtualAccountCommand = Command<CreateVirtualAccountInput>

module CreateVirtualAccountCommand =
   let create (data: CreateVirtualAccountInput) =
      Command.create
         data.ParentAccountId.AsEntityId
         data.OrgId
         (CorrelationId.create ())
         data.InitiatedBy
         data

   let toEvent
      (cmd: CreateVirtualAccountCommand)
      : ValidationResult<BankEvent<CreatedVirtualAccount>>
      =
      validate {
         let input = cmd.Data

         let! accountName = accountNameValidator input.Name

         and! accountNumber =
            AccountNumber.fromString "Account Number" input.AccountNumber

         and! routingNumber =
            RoutingNumber.fromString "Routing Number" "123456789"

         return
            BankEvent.create2<CreateVirtualAccountInput, CreatedVirtualAccount>
               cmd
               {
                  Name = accountName
                  Depository = input.Depository
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
         parentAccountId.AsEntityId
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

type DebitCommand = Command<DebitPending>

module DebitCommand =
   let create
      (parentAccountId: ParentAccountId, orgId: OrgId)
      (correlationId: CorrelationId)
      (initiator: Initiator)
      (data: DebitPending)
      =
      Command.create
         parentAccountId.AsEntityId
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
               EmployeeId = info.InitiatedBy.Id.AsEmployeeId
               CardIssuerTransactionId = info.CardIssuerTransactionId
               CardId = info.CardId
               CardNickname = info.CardNickname
               PurchaseAuthType = info.AuthorizationType
            }
         }

   let toEvent (cmd: DebitCommand) : ValidationResult<BankEvent<DebitPending>> = validate {
      let input = cmd.Data
      let! _ = amountValidator "Debit amount" input.Amount
      let! _ = dateNotDefaultValidator "Date" input.Date
      let! _ = merchantValidator input.Merchant

      return BankEvent.create<DebitPending> cmd
   }

   let toEventWithAuthBypass
      (cmd: DebitCommand)
      : ValidationResult<BankEvent<DebitPending>>
      =
      BankEvent.create<DebitPending> cmd |> Ok

type FailDebitCommand = Command<DebitFailed>

module FailDebitCommand =
   let create
      (parentAccountId: ParentAccountId, orgId: OrgId)
      (correlationId: CorrelationId)
      (initiator: Initiator)
      (data: DebitFailed)
      =
      Command.create
         parentAccountId.AsEntityId
         orgId
         correlationId
         initiator
         data

   let fromPurchase (purchaseInfo: PurchaseInfo) (reason: PurchaseFailReason) =
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
               CardIssuerTransactionId = purchaseInfo.CardIssuerTransactionId
               CardId = purchaseInfo.CardId
               CardNickname = purchaseInfo.CardNickname
               PurchaseAuthType = purchaseInfo.AuthorizationType
            }
            Merchant = purchaseInfo.Merchant
            Amount = purchaseInfo.Amount
            Reason = reason
         }

   let toEvent
      (cmd: FailDebitCommand)
      : ValidationResult<BankEvent<DebitFailed>>
      =
      BankEvent.create<DebitFailed> cmd |> Ok

type RefundDebitCommand = Command<DebitRefunded>

module RefundDebitCommand =
   let create
      (parentAccountId: ParentAccountId, orgId: OrgId)
      (correlationId: CorrelationId)
      (initiator: Initiator)
      (data: DebitRefunded)
      =
      Command.create
         parentAccountId.AsEntityId
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
               CardIssuerTransactionId = purchaseInfo.CardIssuerTransactionId
               CardId = purchaseInfo.CardId
               CardNickname = purchaseInfo.CardNickname
               PurchaseAuthType = purchaseInfo.AuthorizationType
            }
            Merchant = purchaseInfo.Merchant
            Amount = purchaseInfo.Amount
            Reason = reason
         }

   let toEvent
      (cmd: RefundDebitCommand)
      : ValidationResult<BankEvent<DebitRefunded>>
      =
      BankEvent.create<DebitRefunded> cmd |> Ok

type MaintenanceFeeCommand = Command<MaintenanceFeeDebited>

module MaintenanceFeeCommand =
   let create
      (accountId: AccountId, parentAccountId: ParentAccountId, orgId: OrgId)
      (corrId: CorrelationId)
      (billingDate: DateTime)
      =
      Command.create parentAccountId.AsEntityId orgId corrId Initiator.System {
         AccountId = accountId
         Amount = MaintenanceFee.RecurringDebitAmount
         BillingDate = billingDate
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
      (corrId: CorrelationId)
      (criteria: MaintenanceFee.MaintenanceFeeCriteria)
      (billingDate: DateTime)
      =
      Command.create parentAccountId.AsEntityId orgId corrId Initiator.System {
         AccountId = accountId
         Reason = criteria
         BillingDate = billingDate
      }

   let toEvent
      (cmd: SkipMaintenanceFeeCommand)
      : ValidationResult<BankEvent<MaintenanceFeeSkipped>>
      =
      Ok <| BankEvent.create<MaintenanceFeeSkipped> cmd

type SettleDebitCommand = Command<DebitSettled>

module SettleDebitCommand =
   let create (purchaseInfo: PurchaseInfo) (clearing: PurchaseClearing) =
      Command.create
         purchaseInfo.ParentAccountId.AsEntityId
         purchaseInfo.OrgId
         purchaseInfo.CorrelationId
         purchaseInfo.InitiatedBy
         {
            AccountId = purchaseInfo.AccountId
            EmployeePurchaseReference = {
               EmployeeName = purchaseInfo.EmployeeName
               EmployeeId = purchaseInfo.EmployeeId
               EmployeeCardNumberLast4 = purchaseInfo.CardNumberLast4
               CardIssuerTransactionId = purchaseInfo.CardIssuerTransactionId
               CardId = purchaseInfo.CardId
               CardNickname = purchaseInfo.CardNickname
               PurchaseAuthType = purchaseInfo.AuthorizationType
            }
            Merchant = purchaseInfo.Merchant
            // TODO: see about removing "amount" or changing to "total amount"
            Amount = purchaseInfo.Amount
            Clearing = clearing
         }

   let toEvent
      (cmd: SettleDebitCommand)
      : ValidationResult<BankEvent<DebitSettled>>
      =
      Ok <| BankEvent.create<DebitSettled> cmd

type CloseAccountCommand = Command<AccountClosed>

module CloseAccountCommand =
   let create
      (parentAccountId: ParentAccountId, orgId: OrgId)
      (initiator: Initiator)
      (data: AccountClosed)
      =
      Command.create
         parentAccountId.AsEntityId
         orgId
         (CorrelationId.create ())
         initiator
         data

   let toEvent
      (cmd: CloseAccountCommand)
      : ValidationResult<BankEvent<AccountClosed>>
      =
      Ok <| BankEvent.create<AccountClosed> cmd
