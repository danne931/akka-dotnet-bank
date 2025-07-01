module RoutePaths

open Lib.SharedTypes

let private API = "/api"

module OrgPath =
   let Base = $"{API}/org"
   let Get = Base + "/{orgId}"
   let get (orgId: OrgId) = $"{Base}/{orgId}"

   let Search = Base + "/search/{orgId}/{searchQuery}"

   let search (orgId: OrgId) (searchQuery: string) =
      $"{Base}/search/{orgId}/{searchQuery}"

   let Merchants = Base + "/merchants/{orgId}"
   let merchants (orgId: OrgId) = $"{Base}/merchants/{orgId}"

   let ConfigureCommandApprovalRule = Base + "/configure-command-approval-rule"
   let DeleteCommandApprovalRule = Base + "/delete-command-approval-rule"
   let GetCommandApprovalRules = Get + "/command-approval-rule"

   let getCommandApprovalRules (orgId: OrgId) =
      get orgId + "/command-approval-rule"

   let RequestCommandApproval = Base + "/request-command-approval"

   let AcquireCommandApproval = Base + "/acquire-command-approval"

   let DeclineCommandApproval = Base + "/decline-command-approval"

   let GetCommandApprovals = Base + "/command-approvals/{orgId}"

   let getCommandApprovals (orgId: OrgId) = Base + $"/command-approvals/{orgId}"

   let CommandApprovalDailyAccrual =
      Base + "/command-approval-daily-accrual/{orgId}/{initiatedByid}"

   let commandApprovalDailyAccrual
      (orgId: OrgId)
      (initiatedById: InitiatedById)
      =
      $"{Base}/command-approval-daily-accrual/{orgId}/{initiatedById}"

module AccountPath =
   let Base = $"{API}/accounts"
   let account (id: AccountId) = $"{Base}/{id}"
   let Account = Base + "/{id}"
   let Deposit = Base + "/deposit"
   let CloseAccount = Base + "/close-account"
   let BillingStatement = Base + "/billing-statement/{accountId}/{page}"

module DiagnosticPath =
   let Base = $"{API}/diagnostic"
   let Account = Base + "/account/{id}"
   let AccountEvents = Base + "/events/{id}"
   let LoadTest = Base + "/load-test"
   let LoadTestProgress = LoadTest + "/progress"
   let LoadTestTeardown = LoadTest + "/teardown"
   let CircuitBreaker = Base + "/circuit-breaker"

module TransferPath =
   let Base = $"{API}/transfers"
   let InternalWithinOrg = Base + "/internal-within-org"
   let InternalBetweenOrgs = Base + "/internal-between-orgs"
   let ScheduleInternalBetweenOrgs = InternalBetweenOrgs + "/schedule"
   let Domestic = Base + "/domestic"
   let ScheduleDomestic = Domestic + "/domestic"
   let DomesticTransferRecipient = Domestic + "/register-recipient"
   let DomesticTransferRecipientEdit = Domestic + "/edit-recipient"
   let NicknameRecipient = Base + "/recipient-nickname"
   let ConfigureAutoTransferRule = Base + "/configure-auto-transfer-rule"
   let DeleteAutoTransferRule = Base + "/delete-auto-transfer-rule"

   let RetryableDomesticTransfersUponRecipientCorrection =
      Base
      + "/retryable-domestic-transfers-upon-recipient-correction/{recipientAccountId}"

   let retryableDomesticTransfersUponRecipientCorrection
      (recipientAccountId: AccountId)
      =
      Base
      + $"/retryable-domestic-transfers-upon-recipient-correction/{recipientAccountId}"

module PaymentPath =
   let Base = $"{API}/payments"
   let Payments = Base + "/{orgId}"
   let payments (orgId: OrgId) = Base + $"/{orgId}"
   let RequestPayment = Base + "/request"
   let CancelPayment = Base + "/cancel"
   let DeclinePayment = Base + "/decline"
   let FulfillPayment = Base + "/fulfill"

module TransactionPath =
   let Base = $"{API}/transactions"
   let Transactions = Base + "/{orgId}"
   let transactions (orgId: OrgId) = $"{Base}/{orgId}"
   let Categories = Base + "/categories"

   let History = Transactions + "/history"
   let history (orgId: OrgId) = transactions orgId + "/history"

   let TransactionInfo = Base + "/transaction/{txnId}"

   let transactionInfo (id: TransactionId) = $"{Base}/transaction/{id}"

   let TransactionConfirmation =
      Base + "/transaction-confirmation/{correlationId}"

   let transactionConfirmation (correlationId: CorrelationId) =
      $"{Base}/transaction-confirmation/{correlationId}"

   let Category = TransactionInfo + "/category/{categoryId}"
   let CategoryDelete = TransactionInfo + "/category"

   let category (txnId: TransactionId) (categoryId: int) =
      transactionInfo txnId + $"/category/{categoryId}"

   let categoryDelete (txnId: TransactionId) =
      transactionInfo txnId + "/category"

   let Note = TransactionInfo + "/note"
   let note (txnId: TransactionId) = transactionInfo txnId + "/note"

module EmployeePath =
   let Base = $"{API}/employees"
   let Get = Base + "/{orgId}"
   let get (orgId: OrgId) = $"{Base}/{orgId}"
   let Search = Base + "/search/{orgId}/{searchQuery}"
   let GetEmployee = Get + "/{employeeId}"

   let getEmployee (orgId: OrgId) (employeeId: EmployeeId) =
      $"{get orgId}/{employeeId}"

   let search (orgId: OrgId) (searchQuery: string) =
      $"{Base}/search/{orgId}/{searchQuery}"

   let UpdateRole = Base + "/role"
   let CancelEmployeeInvitation = Base + "/cancel-employee-invitation"
   let ResendInviteNotification = Base + "/resend-invite-notification"
   let RestoreAccess = Base + "/restore-access"

module CardPath =
   let Base = $"{API}/cards"
   let Get = Base + "/{orgId}"
   let get (orgId: OrgId) = $"{Base}/{orgId}"
   let PurchaseLimit = Base + "/purchase-limit"
   let LockCard = Base + "/lock"
   let UnlockCard = Base + "/unlock"
   let Purchase = Base + "/purchase"
   let UpdateNickname = Base + "/nickname"

module UserSessionPath =
   let Login = "/login"
   let GetSession = "/session"
   let AuthorizeInvite = "/auth/invite"
   let AuthorizationCallback = "/auth/callback"
   let GetDemoUserSessions = GetSession + "/{orgId}"
   let getDemoUserSessions (orgId: OrgId) = $"{GetSession}/{orgId}"
   let OverrideDemoUserSession = "/session-override/{employeeId}"

   let overrideDemoUserSession (employeeId: EmployeeId) =
      $"/session-override/{employeeId}"

module AnalyticsPath =
   let Base = $"{API}/analytics"
   let Get = Base + "/{orgId}"
   let get (orgId: OrgId) = $"{Base}/{orgId}"

   let MoneyFlowMonthlyTimeSeriesForOrg =
      Base + "/money-flow-monthly-time-series-for-org/{orgId}"

   let moneyFlowMonthlyTimeSeriesForOrg (orgId: OrgId) =
      $"{Base}/money-flow-monthly-time-series-for-org/{orgId}"
