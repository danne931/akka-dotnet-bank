[<RequireQualifiedAccess>]
module EmployeeStub

open System

open Lib.SharedTypes
open Bank.Employee.Domain
open Bank.Purchase.Domain
open Email

let parentAccountId = Guid.NewGuid() |> ParentAccountId
let accountId = Guid.NewGuid() |> AccountId

let accountNumber = AccountNumber.generate ()
let employeeId = Guid.NewGuid() |> EmployeeId
let orgId = Guid.NewGuid() |> OrgId
let compositeId = employeeId, orgId
let correlationId = Guid.NewGuid() |> CorrelationId
let cardId = Guid.NewGuid() |> CardId
let cardNumberLast4 = "9310"

let initiator = {
   Id = Guid.NewGuid() |> EmployeeId |> InitiatedById
   Name = "Devon E"
}

let purchaseInfo: PurchaseInfo = {
   CardId = cardId
   AccountId = accountId
   OrgId = orgId
   ParentAccountId = parentAccountId
   EmployeeId = employeeId
   InitiatedBy = initiator
   CorrelationId = correlationId
   EmployeeName = "Dan Eis"
   EmployeeEmail = Email.deserialize "jellyfish@gmail.com"
   CardNumberLast4 = cardNumberLast4
   Date = DateTime.UtcNow
   Amount = 10m
   Merchant = NonEmptyString.deserializeUnsafe "Groceries"
   CurrencyMerchant = Currency.USD
   CurrencyCardHolder = Currency.USD
   Reference = None
   CardIssuerCardId = Guid.NewGuid() |> CardIssuerCardId
   CardIssuerTransactionId = Guid.NewGuid() |> CardIssuerTransactionId
   CardNickname = Some "Travel"
   AuthorizationType = PurchaseAuthType.Debit
}

let command =
   let dailyPurchaseLimit = 2000m
   let monthlyPurchaseLimit = 20_000m

   {|
      createAccountOwner =
         CreateAccountOwnerCommand.create {
            OrgId = orgId
            Email = "smartfish@gmail.com"
            FirstName = "Smart"
            LastName = "Fish"
            ParentAccountId = parentAccountId
            EmployeeId = Guid.NewGuid() |> EmployeeId
         }
      createEmployee =
         CreateEmployeeCommand.create initiator {
            OrgId = orgId
            OrgRequiresEmployeeInviteApproval = None
            Role = Role.Admin
            FirstName = "Dan"
            LastName = "Eis"
            Email = "jellyfish@gmail.com"
            ParentAccountId = parentAccountId
            CardInfo = None
         }
      confirmInvite =
         ConfirmInvitationCommand.create initiator orgId correlationId {
            AuthProviderUserId = Guid.NewGuid()
            Email = Email.deserialize "jellyfish@gmail.com"
            Reference = None
         }
      refreshInviteToken =
         RefreshInvitationTokenCommand.create
            compositeId
            initiator
            correlationId
            { Reason = None }
      updateRoleWithSupplementaryCardInfo =
         UpdateRoleCommand.create compositeId initiator {
            EmployeeName = "Dan Eis"
            PriorRole = Role.Admin
            Role = Role.CardOnly
            CardInfo =
               Some {
                  DailyPurchaseLimit = 1300m
                  MonthlyPurchaseLimit = 13_000m
                  LinkedAccountId = accountId
                  CardType = CardType.Debit
               }
         }
      createCard =
         CreateCardCommand.create {
            Virtual = true
            PersonName = "Dan Eis"
            CardNickname = Some "Travel"
            CardType = CardType.Debit
            InitiatedBy = initiator
            DailyPurchaseLimit = Some dailyPurchaseLimit
            MonthlyPurchaseLimit = Some monthlyPurchaseLimit
            AccountId = accountId
            OrgId = orgId
            EmployeeId = employeeId
            CardId = cardId
            OriginatedFromEmployeeOnboarding = None
         }
      linkCard =
         LinkCardCommand.create {
            Link = {
               CardId = cardId
               CardIssuerCardId = Guid.NewGuid() |> CardIssuerCardId
               CardIssuerName = CardIssuerName.Lithic
            }
            CardNumberLast4 = cardNumberLast4
            OrgId = orgId
            EmployeeId = employeeId
            InitiatedBy = initiator
            CorrelationId = correlationId
         }
      debit =
         fun amount ->
            PurchaseIntentCommand.create { purchaseInfo with Amount = amount }
      approveDebit =
         SettlePurchaseWithCardCommand.create {
            Info = purchaseInfo
            Clearing = {
               PurchaseClearedId = Guid.NewGuid() |> PurchaseClearedId
               ClearedAmount = {
                  Amount = purchaseInfo.Amount
                  Flow = MoneyFlow.Out
               }
            }
         }
      limitDailyDebits =
         fun amount ->
            ConfigureRollingPurchaseLimitCommand.create compositeId initiator {
               CardId = cardId
               CardNumberLast4 = cardNumberLast4
               PriorDailyLimit = dailyPurchaseLimit
               PriorMonthlyLimit = monthlyPurchaseLimit
               DailyLimit = amount
               MonthlyLimit = monthlyPurchaseLimit
            }
      limitMonthlyDebits =
         fun amount ->
            ConfigureRollingPurchaseLimitCommand.create compositeId initiator {
               CardId = cardId
               CardNumberLast4 = cardNumberLast4
               PriorDailyLimit = dailyPurchaseLimit
               PriorMonthlyLimit = monthlyPurchaseLimit
               DailyLimit = dailyPurchaseLimit
               MonthlyLimit = amount
            }
      lockCard =
         LockCardCommand.create compositeId initiator {
            CardName = ""
            EmployeeName = ""
            CardId = cardId
            Reference = None
            CardNumberLast4 = cardNumberLast4
         }
      unlockCard =
         UnlockCardCommand.create compositeId initiator {
            CardName = ""
            EmployeeName = ""
            CardId = cardId
            Reference = None
            CardNumberLast4 = cardNumberLast4
         }
   |}

let initCommands: EmployeeCommand list = [
   EmployeeCommand.CreateEmployee command.createEmployee
   EmployeeCommand.ConfirmInvitation command.confirmInvite
   EmployeeCommand.CreateCard command.createCard
   EmployeeCommand.LinkCard command.linkCard
]

type EventIndex = {
   employeeCreated: BankEvent<CreatedEmployee>
   cardCreated: BankEvent<CreatedCard>
   cardLinked: BankEvent<CardLinked>
}

let event: EventIndex = {
   employeeCreated =
      command.createEmployee
      |> CreateEmployeeCommand.toEvent
      |> Result.toValueOption
      |> _.Value
   cardCreated =
      command.createCard
      |> CreateCardCommand.toEvent
      |> Result.toValueOption
      |> _.Value
   cardLinked =
      command.linkCard
      |> LinkCardCommand.toEvent
      |> Result.toValueOption
      |> _.Value
}

let employeeState =
   let e = event.employeeCreated

   {
      Employee.Empty with
         EmployeeId = EmployeeId.fromEntityId e.EntityId
         OrgId = e.OrgId
         FirstName = e.Data.FirstName
         LastName = e.Data.LastName
         Email = e.Data.Email
         Role = e.Data.Role
         Status = EmployeeStatus.Active
         Cards =
            Map [
               event.cardCreated.Data.Card.CardId,
               {
                  event.cardCreated.Data.Card with
                     Status = CardStatus.Active
               }
            ]
   }

let employeeStateWithEvents: EmployeeSnapshot = {
   Info = employeeState
   Events = [
      EmployeeEnvelope.wrap event.employeeCreated
      EmployeeEnvelope.wrap event.cardCreated
      EmployeeEnvelope.wrap event.cardLinked
   ]
   CardIssuerLinks = Map.empty
   PendingPurchaseDeductions = Map.empty
   PendingPurchases = Map.empty
   ProcessedCommands = Map.empty
   Outbox = Map.empty
}
