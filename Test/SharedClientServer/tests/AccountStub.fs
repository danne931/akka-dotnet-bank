[<RequireQualifiedAccess>]
module AccountStub

open System

open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Purchase.Domain
open BillingStatement

let accountId = Guid.NewGuid() |> AccountId
let parentAccountId = Guid.NewGuid() |> ParentAccountId
let accountNumber = AccountNumber.generate ()
let orgId = Guid.NewGuid() |> OrgId
let compositeId = accountId, orgId
let parentCompositeId = parentAccountId, orgId
let correlationId = Guid.NewGuid() |> CorrelationId

let initiator: Initiator = {
   Id = Guid.NewGuid() |> EmployeeId |> InitiatedById
   Name = "Devon E"
}

let internalRecipient: InternalTransferRecipient = {
   Name = "Savings"
   AccountId = Guid.NewGuid() |> AccountId
   ParentAccountId = parentAccountId
   OrgId = orgId
}

let internalSender: InternalTransferSender = {
   Name = "Operations"
   AccountId = accountId
   ParentAccountId = parentAccountId
   OrgId = orgId
}

let internalTransferWithinOrgBaseInfo = {
   Sender = internalSender
   InitiatedBy = initiator
   TransferId = TransferId correlationId.Value
   Recipient = internalRecipient
   Amount = 33m
   ScheduledDate = DateTime.UtcNow
   Memo = None
}

let internalTransferBetweenOrgsBaseInfo = {
   Sender = internalSender
   InitiatedBy = initiator
   TransferId = TransferId correlationId.Value
   Recipient = {
      Name = "Linear"
      AccountId = Guid.NewGuid() |> AccountId
      ParentAccountId = Guid.NewGuid() |> ParentAccountId
      OrgId = Guid.NewGuid() |> OrgId
   }
   Amount = 100m
   ScheduledDate = DateTime.UtcNow
   FromPaymentRequest = None
   Memo = None
}

let domesticOriginator: DomesticTransferOriginatorReference = {
   Name = "Operations"
   OrgId = orgId
   ParentAccountId = parentAccountId
   AccountId = accountId
}

let command = {|
   createAccount =
      CreateVirtualAccountCommand.create {
         Name = "Operations"
         Currency = Currency.VND
         AccountId = accountId
         AccountNumber = string accountNumber
         Depository = AccountDepository.Checking
         ParentAccountId = parentAccountId
         OrgId = orgId
         InitiatedBy = initiator
      }
   closeAccount =
      CloseAccountCommand.create parentCompositeId initiator {
         AccountId = accountId
         Reference = None
      }
   debit =
      fun amount ->
         DebitCommand.create parentCompositeId correlationId initiator {
            AccountId = accountId
            Date = DateTime.UtcNow
            Amount = amount
            Merchant = NonEmptyString.deserializeUnsafe "Groceries"
            Reference = None
            EmployeePurchaseReference = {
               EmployeeName = "Dan Eis"
               EmployeeCardNumberLast4 = EmployeeStub.cardNumberLast4
               EmployeeId = EmployeeStub.employeeId
               CardId = EmployeeStub.cardId
               CardNickname = None
               CardIssuerTransactionId =
                  Guid.NewGuid() |> CardIssuerTransactionId
               PurchaseAuthType = PurchaseAuthType.Debit
            }
         }
   depositCash =
      fun amount ->
         DepositCashCommand.create parentCompositeId initiator {
            AccountId = accountId
            Amount = amount
            Origin = None
         }
   domesticTransfer =
      fun amount ->
         DomesticTransferCommand.create correlationId initiator {
            Originator = domesticOriginator
            Counterparty = OrganizationStub.domesticRecipient
            Amount = amount
            MoneyFlow = MoneyFlow.Out
            Memo = None
            ScheduledDateSeedOverride = None
            OriginatedFromSchedule = false
         }
   internalTransfer =
      fun amount ->
         let cmd =
            InternalTransferWithinOrgCommand.create initiator {
               Recipient = internalRecipient
               Sender = internalSender
               Amount = amount
               Memo = None
               ScheduledDateSeedOverride = None
               OriginatedFromSchedule = false
            }

         {
            cmd with
               CorrelationId = correlationId
         }
   internalTransferBetweenOrgs =
      fun amount ->
         InternalTransferBetweenOrgsCommand.create initiator {
            Amount = amount
            Sender = internalTransferBetweenOrgsBaseInfo.Sender
            Recipient = internalTransferBetweenOrgsBaseInfo.Recipient
            Memo = None
            OriginatedFromSchedule = false
            OriginatedFromPaymentRequest = None
            ScheduledDateSeedOverride = None
         }
   depositTransfer =
      fun amount ->
         DepositInternalTransferWithinOrgCommand.create correlationId initiator {
            BaseInfo = {
               internalTransferWithinOrgBaseInfo with
                  Amount = amount
            }
         }
   depositTransferBetweenOrgs =
      fun amount ->
         DepositInternalTransferBetweenOrgsCommand.create
            correlationId
            initiator
            {
               BaseInfo = {
                  internalTransferBetweenOrgsBaseInfo with
                     Amount = amount
               }
            }
   maintenanceFee =
      fun billingDate ->
         MaintenanceFeeCommand.create
            (accountId, parentAccountId, orgId)
            correlationId
            billingDate
   skipMaintenanceFee =
      fun billingDate ->
         SkipMaintenanceFeeCommand.create
            (accountId, parentAccountId, orgId)
            correlationId
            {
               QualifyingDepositFound = false
               DailyBalanceThreshold = true
            }
            billingDate
|}

type EventIndex = {
   createdAccount: BankEvent<CreatedVirtualAccount>
   depositedCash: BankEvent<DepositedCash>
   debitedAccount: BankEvent<DebitPending>
   maintenanceFeeDebited: BankEvent<MaintenanceFeeDebited>
   internalTransferPending: BankEvent<InternalTransferWithinOrgDeducted>
   domesticTransferPending: BankEvent<DomesticTransferPending>
   transferDeposited: BankEvent<InternalTransferWithinOrgDeposited>
}

let event: EventIndex = {
   createdAccount =
      command.createAccount
      |> CreateVirtualAccountCommand.toEvent
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
      command.maintenanceFee DateTime.UtcNow
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
   transferDeposited =
      command.depositTransfer 100m
      |> DepositInternalTransferWithinOrgCommand.toEvent
      |> Result.toValueOption
      |> _.Value
}

let commands: AccountCommand list = [
   AccountCommand.CreateVirtualAccount command.createAccount

   AccountCommand.DepositCash
   <| command.depositCash event.depositedCash.Data.Amount

   AccountCommand.Debit <| command.debit event.debitedAccount.Data.Amount

   AccountCommand.MaintenanceFee(command.maintenanceFee DateTime.UtcNow)

   AccountCommand.InternalTransfer
   <| command.internalTransfer
         event.internalTransferPending.Data.BaseInfo.Amount

   AccountCommand.InternalTransferBetweenOrgs
   <| command.internalTransferBetweenOrgs
         event.internalTransferPending.Data.BaseInfo.Amount
]

let accountEvents = [
   AccountEnvelope.wrap event.createdAccount
   AccountEnvelope.wrap event.depositedCash
   AccountEnvelope.wrap event.debitedAccount
   AccountEnvelope.wrap event.maintenanceFeeDebited
   AccountEnvelope.wrap event.internalTransferPending
]

let accountState = {
   Account.empty with
      AccountId = accountId
      ParentAccountId = parentAccountId
      OrgId = orgId
      AccountNumber = accountNumber
      Name = command.createAccount.Data.Name
      Status = AccountStatus.Active
      Balance = 300m
}

let internalRecipientAccount = {
   Account.empty with
      AccountId = internalRecipient.AccountId
      ParentAccountId = parentAccountId
      OrgId = orgId
      AccountNumber = AccountNumber.generate ()
      Name = internalRecipient.Name
      Status = AccountStatus.Active
      Balance = 1000m
}

let accountStateWithEvents: ParentAccountSnapshot = {
   ParentAccountSnapshot.empty with
      Status = ParentAccountStatus.Active
      ParentAccountId = parentAccountId
      OrgId = orgId
      VirtualAccounts =
         Map [
            accountId, accountState
            internalRecipient.AccountId, internalRecipientAccount
         ]
      Events = [
         AccountEnvelope.wrap event.createdAccount
         AccountEnvelope.wrap event.depositedCash
      ]
}

let accountStateAfterCreate = {
   Account.empty with
      AccountId = command.createAccount.Data.AccountId
      ParentAccountId = parentAccountId
      OrgId = command.createAccount.OrgId
      AccountNumber = accountNumber
      Name = command.createAccount.Data.Name
      Status = AccountStatus.Active
      Balance = 0m
      Currency = command.createAccount.Data.Currency
}

let accountStateAfterCreateWithEvents: ParentAccountSnapshot = {
   ParentAccountSnapshot.empty with
      Status = ParentAccountStatus.Active
      ParentAccountId = parentAccountId
      OrgId = orgId
      VirtualAccounts =
         Map [
            accountId, accountStateAfterCreate
            internalRecipient.AccountId, internalRecipientAccount
         ]
      Events = [ AccountEnvelope.wrap event.createdAccount ]
}

let billingPeriod: BillingPeriod =
   let d = DateTime.UtcNow
   { Month = d.Month; Year = d.Year }

let createBillingTxn = BillingTransaction.create billingPeriod

let billingTransactions: BillingTransaction list = [
   event.depositedCash
   |> AccountEvent.DepositedCash
   |> createBillingTxn
   |> _.Value

   event.maintenanceFeeDebited
   |> AccountEvent.MaintenanceFeeDebited
   |> createBillingTxn
   |> _.Value

   event.internalTransferPending
   |> AccountEvent.InternalTransferWithinOrgDeducted
   |> createBillingTxn
   |> _.Value
]

let settleDebitFromPending (evt: BankEvent<DebitPending>) =
   let debit = evt.Data

   let purchaseInfo = {
      OrgId = evt.OrgId
      ParentAccountId = ParentAccountId.fromEntityId evt.EntityId
      AccountId = debit.AccountId
      EmployeeId = debit.EmployeePurchaseReference.EmployeeId
      CardId = debit.EmployeePurchaseReference.CardId
      InitiatedBy = evt.InitiatedBy
      CorrelationId = evt.CorrelationId
      EmployeeName = debit.EmployeePurchaseReference.EmployeeName
      EmployeeEmail = Email.Email.deserialize "test@example.com"
      CardNumberLast4 = debit.EmployeePurchaseReference.EmployeeCardNumberLast4
      Date = debit.Date
      Amount = debit.Amount
      Merchant = debit.Merchant
      CurrencyMerchant = Currency.USD
      CurrencyCardHolder = Currency.USD
      Reference = debit.Reference
      CardIssuerCardId = Guid.NewGuid() |> CardIssuerCardId
      CardIssuerTransactionId =
         debit.EmployeePurchaseReference.CardIssuerTransactionId
      CardNickname = debit.EmployeePurchaseReference.CardNickname
      AuthorizationType = debit.EmployeePurchaseReference.PurchaseAuthType
   }

   let clearing = {
      PurchaseClearedId = Guid.NewGuid() |> PurchaseClearedId
      ClearedAmount = {
         Amount = debit.Amount
         Flow = MoneyFlow.Out
      }
   }

   SettleDebitCommand.create purchaseInfo clearing

let settlePlatformTransfer
   (evt: BankEvent<InternalTransferBetweenOrgsPending>)
   =
   SettleInternalTransferBetweenOrgsCommand.create
      evt.CorrelationId
      evt.InitiatedBy
      {
         BaseInfo = evt.Data.BaseInfo
         SettlementId = SettlementId(Guid.NewGuid())
      }

let failInternalTransferBetweenOrgs
   (evt: BankEvent<InternalTransferBetweenOrgsPending>)
   =
   FailInternalTransferBetweenOrgsCommand.create
      evt.CorrelationId
      evt.InitiatedBy
      {
         BaseInfo = evt.Data.BaseInfo
         Reason = InternalTransferFailReason.InsufficientFunds
      }

let refundDebitFromSettled (evt: BankEvent<DebitSettled>) =
   let debit = evt.Data

   let purchaseInfo = {
      OrgId = evt.OrgId
      ParentAccountId = ParentAccountId.fromEntityId evt.EntityId
      AccountId = debit.AccountId
      EmployeeId = debit.EmployeePurchaseReference.EmployeeId
      CardId = debit.EmployeePurchaseReference.CardId
      InitiatedBy = evt.InitiatedBy
      CorrelationId = evt.CorrelationId
      EmployeeName = debit.EmployeePurchaseReference.EmployeeName
      EmployeeEmail = Email.Email.deserialize "test@example.com"
      CardNumberLast4 = debit.EmployeePurchaseReference.EmployeeCardNumberLast4
      Date = DateTime.UtcNow
      Amount = debit.Amount
      Merchant = debit.Merchant
      CurrencyMerchant = Currency.USD
      CurrencyCardHolder = Currency.USD
      Reference = None
      CardIssuerCardId = Guid.NewGuid() |> CardIssuerCardId
      CardIssuerTransactionId =
         debit.EmployeePurchaseReference.CardIssuerTransactionId
      CardNickname = debit.EmployeePurchaseReference.CardNickname
      AuthorizationType = debit.EmployeePurchaseReference.PurchaseAuthType
   }

   RefundDebitCommand.fromPurchase
      purchaseInfo
      (PurchaseRefundReason.UserRequested "Test refund")
