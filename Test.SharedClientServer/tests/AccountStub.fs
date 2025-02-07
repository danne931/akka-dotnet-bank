[<RequireQualifiedAccess>]
module AccountStub

open System

open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Transfer.Domain
open BillingStatement

let accountId = Guid.NewGuid() |> AccountId
let accountNumber = AccountNumber.generate () |> int64 |> AccountNumber
let orgId = Guid.NewGuid() |> OrgId
let compositeId = accountId, orgId
let correlationId = Guid.NewGuid() |> CorrelationId
let initiatedById = Guid.NewGuid() |> EmployeeId |> InitiatedById

let internalRecipient: InternalTransferRecipient = {
   Name = "Savings"
   AccountId = Guid.NewGuid() |> AccountId
   OrgId = orgId
}

let internalSender: InternalTransferSender = {
   Name = "Operations"
   AccountId = accountId
   OrgId = orgId
}

let internalTransferWithinOrgBaseInfo = {
   Sender = internalSender
   InitiatedBy = initiatedById
   TransferId = correlationId |> CorrelationId.get |> TransferId
   Recipient = internalRecipient
   Amount = 33m
   ScheduledDate = DateTime.UtcNow
}

let internalTransferBetweenOrgsBaseInfo = {
   Sender = internalSender
   InitiatedBy = initiatedById
   TransferId = correlationId |> CorrelationId.get |> TransferId
   Recipient = {
      Name = "Linear"
      AccountId = Guid.NewGuid() |> AccountId
      OrgId = Guid.NewGuid() |> OrgId
   }
   Amount = 100m
   ScheduledDate = DateTime.UtcNow
}

let domesticSender: DomesticTransferSender = {
   Name = "Operations"
   AccountNumber = AccountNumber <| Int64.Parse "987654321123456"
   RoutingNumber = RoutingNumber 123456789
   OrgId = orgId
   AccountId = accountId
}

let command = {|
   createAccount =
      CreateAccountCommand.create {
         Name = "Operations"
         Currency = Currency.VND
         AccountId = accountId
         AccountNumber = string accountNumber
         Depository = AccountDepository.Checking
         OrgId = orgId
         InitiatedBy = initiatedById
      }
   closeAccount =
      CloseAccountCommand.create compositeId initiatedById { Reference = None }
   debit =
      fun amount ->
         DebitCommand.create compositeId correlationId initiatedById {
            Date = DateTime.UtcNow
            Amount = amount
            Merchant = "Groceries"
            Reference = None
            EmployeePurchaseReference = {
               EmployeeName = "Dan Eis"
               EmployeeCardNumberLast4 = EmployeeStub.cardNumberLast4
               EmployeeId = EmployeeStub.employeeId
               CardId = EmployeeStub.cardId
            }
         }
   depositCash =
      fun amount ->
         DepositCashCommand.create compositeId initiatedById {
            Amount = amount
            Origin = None
         }
   domesticTransfer =
      fun amount ->
         DomesticTransferCommand.create compositeId correlationId initiatedById {
            Sender = domesticSender
            Recipient = OrganizationStub.domesticRecipient
            Amount = amount
            Memo = None
            ScheduledDateSeedOverride = None
         }
   internalTransfer =
      fun amount ->
         let transferCmd =
            InternalTransferWithinOrgCommand.create compositeId initiatedById {
               Recipient = internalRecipient
               Sender = internalSender
               Amount = amount
               Memo = None
               ScheduledDateSeedOverride = None
            }

         {
            transferCmd with
               CorrelationId = correlationId
         }
   completeInternalTransfer =
      CompleteInternalTransferWithinOrgCommand.create
         compositeId
         correlationId
         initiatedById
         {
            BaseInfo = internalTransferWithinOrgBaseInfo
         }
   failInternalTransfer =
      fun amount ->
         FailInternalTransferWithinOrgCommand.create
            compositeId
            correlationId
            initiatedById
            {
               BaseInfo = {
                  internalTransferWithinOrgBaseInfo with
                     Amount = amount
               }
               Reason = InternalTransferFailReason.AccountClosed
            }
   depositTransfer =
      fun amount ->
         DepositInternalTransferWithinOrgCommand.create
            compositeId
            correlationId
            initiatedById
            {
               BaseInfo = {
                  internalTransferWithinOrgBaseInfo with
                     Amount = amount
               }
            }
   depositTransferBetweenOrgs =
      fun amount ->
         DepositInternalTransferBetweenOrgsCommand.create
            compositeId
            correlationId
            initiatedById
            {
               BaseInfo = {
                  internalTransferBetweenOrgsBaseInfo with
                     Amount = amount
               }
            }
   maintenanceFee = MaintenanceFeeCommand.create compositeId
   skipMaintenanceFee =
      SkipMaintenanceFeeCommand.create compositeId {
         Reason = {
            DailyBalanceThreshold = true
            QualifyingDepositFound = false
         }
      }
|}

type EventIndex = {
   createdAccount: BankEvent<CreatedAccount>
   depositedCash: BankEvent<DepositedCash>
   debitedAccount: BankEvent<DebitedAccount>
   maintenanceFeeDebited: BankEvent<MaintenanceFeeDebited>
   internalTransferPending: BankEvent<InternalTransferWithinOrgPending>
   domesticTransferPending: BankEvent<DomesticTransferPending>
   internalTransferFailed: BankEvent<InternalTransferWithinOrgFailed>
   transferDeposited: BankEvent<InternalTransferWithinOrgDeposited>
}

let event: EventIndex = {
   createdAccount =
      command.createAccount
      |> CreateAccountCommand.toEvent
      |> Result.toValueOption
      |> _.Value
   depositedCash =
      command.depositCash 300m
      |> DepositCashCommand.toEvent
      |> Result.toValueOption
      |> _.Value
   debitedAccount =
      command.debit 150m
      |> DebitCommand.toEvent
      |> Result.toValueOption
      |> _.Value
   maintenanceFeeDebited =
      command.maintenanceFee
      |> MaintenanceFeeCommand.toEvent
      |> Result.toValueOption
      |> _.Value
   internalTransferPending =
      command.internalTransfer 20m
      |> InternalTransferWithinOrgCommand.toEvent
      |> Result.toValueOption
      |> _.Value
   domesticTransferPending =
      command.domesticTransfer 20m
      |> DomesticTransferCommand.toEvent
      |> Result.toValueOption
      |> _.Value
   internalTransferFailed =
      command.failInternalTransfer 20m
      |> FailInternalTransferWithinOrgCommand.toEvent
      |> Result.toValueOption
      |> _.Value
   transferDeposited =
      command.depositTransfer 100m
      |> DepositInternalTransferWithinOrgCommand.toEvent
      |> Result.toValueOption
      |> _.Value
}

let commands: AccountCommand list = [
   AccountCommand.CreateAccount command.createAccount

   AccountCommand.DepositCash
   <| command.depositCash event.depositedCash.Data.Amount

   AccountCommand.Debit <| command.debit event.debitedAccount.Data.Amount

   AccountCommand.MaintenanceFee command.maintenanceFee

   AccountCommand.InternalTransfer
   <| command.internalTransfer
         event.internalTransferPending.Data.BaseInfo.Amount

   AccountCommand.FailInternalTransfer
   <| command.failInternalTransfer
         event.internalTransferFailed.Data.BaseInfo.Amount
]

let accountEvents = [
   AccountEnvelope.wrap event.createdAccount
   AccountEnvelope.wrap event.depositedCash
   AccountEnvelope.wrap event.debitedAccount
   AccountEnvelope.wrap event.maintenanceFeeDebited
   AccountEnvelope.wrap event.internalTransferPending
   AccountEnvelope.wrap event.internalTransferFailed
]

let accountState = {
   Account.empty with
      AccountId = command.createAccount.Data.AccountId
      OrgId = command.createAccount.OrgId
      AccountNumber = accountNumber
      Name = command.createAccount.Data.Name
      Status = AccountStatus.Active
      Balance = 300m
}

let accountStateWithEvents: AccountSnapshot = {
   AccountSnapshot.empty with
      Info = accountState
      Events = [
         AccountEnvelope.wrap event.createdAccount
         AccountEnvelope.wrap event.depositedCash
      ]
}

let accountStateAfterCreate = {
   Account.empty with
      AccountId = command.createAccount.Data.AccountId
      OrgId = command.createAccount.OrgId
      AccountNumber = accountNumber
      Name = command.createAccount.Data.Name
      Status = AccountStatus.Active
      Balance = 0m
      Currency = command.createAccount.Data.Currency
      MaintenanceFeeCriteria = {
         QualifyingDepositFound = false
         DailyBalanceThreshold = false
      }
}

let accountStateAfterCreateWithEvents: AccountSnapshot = {
   AccountSnapshot.empty with
      Info = accountStateAfterCreate
      Events = [ AccountEnvelope.wrap event.createdAccount ]
}

let billingPeriod: BillingPeriod =
   let d = DateTime.UtcNow
   { Month = d.Month; Year = d.Year }

let createBillingTxn = BillingTransaction.create billingPeriod

let billingTransactions: BillingTransaction list = [
   event.createdAccount
   |> AccountEvent.CreatedAccount
   |> createBillingTxn
   |> _.Value

   event.depositedCash
   |> AccountEvent.DepositedCash
   |> createBillingTxn
   |> _.Value

   event.debitedAccount
   |> AccountEvent.DebitedAccount
   |> createBillingTxn
   |> _.Value

   event.maintenanceFeeDebited
   |> AccountEvent.MaintenanceFeeDebited
   |> createBillingTxn
   |> _.Value

   event.internalTransferPending
   |> AccountEvent.InternalTransferWithinOrgPending
   |> createBillingTxn
   |> _.Value

   event.internalTransferFailed
   |> AccountEvent.InternalTransferWithinOrgFailed
   |> createBillingTxn
   |> _.Value
]
