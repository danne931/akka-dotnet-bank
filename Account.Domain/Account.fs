[<RequireQualifiedAccess>]
module Account

open Validus

open Bank.Account.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes

let private applyInternalTransferPending
   (info: BaseInternalTransferInfo)
   (account: Account)
   =
   let balance = account.Balance - info.Amount

   {
      account with
         Balance = balance
         MaintenanceFeeCriteria =
            MaintenanceFee.fromDebit account.MaintenanceFeeCriteria balance
   }

let private applyInternalTransferFailed
   (info: BaseInternalTransferInfo)
   (account: Account)
   =
   let balance = account.Balance + info.Amount

   {
      account with
         Balance = balance
         MaintenanceFeeCriteria =
            MaintenanceFee.fromDebitReversal
               account.MaintenanceFeeCriteria
               balance
   }

let private applyTransferDeposit (account: Account) (amount: decimal) = {
   account with
      Balance = account.Balance + amount
      MaintenanceFeeCriteria =
         MaintenanceFee.fromDeposit account.MaintenanceFeeCriteria amount
}

let applyEvent (account: Account) (evt: AccountEvent) =
   match evt with
   | InitializedPrimaryCheckingAccount e -> {
      AccountId = e.Data.PrimaryChecking.AccountId
      AccountNumber = e.Data.PrimaryChecking.AccountNumber
      RoutingNumber = e.Data.PrimaryChecking.RoutingNumber
      OrgId = e.OrgId
      ParentAccountId = ParentAccountId.fromEntityId e.EntityId
      Name = e.Data.PrimaryChecking.Name
      Depository = AccountDepository.Checking
      Currency = Currency.USD
      Balance = 0m
      Status = AccountStatus.Active
      LastBillingCycleDate = None
      MaintenanceFeeCriteria = {
         QualifyingDepositFound = false
         DailyBalanceThreshold = false
      }
      AutoTransferRule = None
     }
   | BillingCycleStarted e -> {
      account with
         LastBillingCycleDate = Some e.Timestamp
     }
   | CreatedAccount e -> {
      AccountId = e.Data.AccountId
      AccountNumber = e.Data.AccountNumber
      RoutingNumber = e.Data.RoutingNumber
      OrgId = e.OrgId
      ParentAccountId = ParentAccountId.fromEntityId e.EntityId
      Name = e.Data.Name
      Depository = e.Data.Depository
      Currency = e.Data.Currency
      Balance = e.Data.Balance
      Status = AccountStatus.Active
      LastBillingCycleDate = None
      MaintenanceFeeCriteria = {
         QualifyingDepositFound = false
         DailyBalanceThreshold = false
      }
      AutoTransferRule = None
     }
   | AccountEvent.AccountClosed _ -> {
      account with
         Status = AccountStatus.Closed
     }
   | DepositedCash e -> {
      account with
         Balance = account.Balance + e.Data.Amount
         MaintenanceFeeCriteria =
            MaintenanceFee.fromDeposit
               account.MaintenanceFeeCriteria
               e.Data.Amount
     }
   | DebitedAccount e ->
      let balance = account.Balance - e.Data.Amount

      {
         account with
            Balance = balance
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDebit account.MaintenanceFeeCriteria balance
      }
   | RefundedDebit e ->
      let balance = account.Balance + e.Data.Amount

      {
         account with
            Balance = balance
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDebitReversal
                  account.MaintenanceFeeCriteria
                  balance
      }
   | MaintenanceFeeDebited e ->
      let balance = account.Balance - e.Data.Amount

      {
         account with
            Balance = balance
            MaintenanceFeeCriteria = MaintenanceFee.reset balance
      }
   | MaintenanceFeeSkipped _ -> {
      account with
         MaintenanceFeeCriteria = MaintenanceFee.reset account.Balance
     }
   | DomesticTransferScheduled _ -> account
   | DomesticTransferPending e ->
      let info = e.Data.BaseInfo
      let balance = account.Balance - info.Amount

      {
         account with
            Balance = balance
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDebit account.MaintenanceFeeCriteria balance
      }
   | DomesticTransferProgress e -> account
   | DomesticTransferCompleted e -> account
   | DomesticTransferFailed e ->
      let info = e.Data.BaseInfo
      let balance = account.Balance + info.Amount

      {
         account with
            Balance = balance
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDebitReversal
                  account.MaintenanceFeeCriteria
                  balance
      }
   | InternalTransferWithinOrgPending e ->
      applyInternalTransferPending e.Data.BaseInfo account
   | InternalTransferWithinOrgFailed e ->
      applyInternalTransferFailed e.Data.BaseInfo account
   | InternalTransferBetweenOrgsScheduled _ -> account
   | InternalTransferBetweenOrgsPending e ->
      applyInternalTransferPending e.Data.BaseInfo account
   | InternalTransferBetweenOrgsFailed e ->
      applyInternalTransferFailed e.Data.BaseInfo account
   | InternalTransferWithinOrgDeposited e ->
      applyTransferDeposit account e.Data.BaseInfo.Amount
   | InternalTransferBetweenOrgsDeposited e ->
      applyTransferDeposit account e.Data.BaseInfo.Amount
   | PlatformPaymentRequested _ -> account
   | PlatformPaymentCancelled _ -> account
   | PlatformPaymentDeclined _ -> account
   | PlatformPaymentPaid e ->
      // If payment fulfilled with account funds then deduct the
      // payment amount from the account.
      match e.Data.PaymentMethod with
      | PaymentMethod.Platform _ ->
         let balance = account.Balance - e.Data.BaseInfo.Amount

         {
            account with
               Balance = balance
               MaintenanceFeeCriteria =
                  MaintenanceFee.fromDebit
                     account.MaintenanceFeeCriteria
                     balance
         }
      | PaymentMethod.ThirdParty _ -> account
   | PlatformPaymentRefunded e ->
      let balance = account.Balance + e.Data.BaseInfo.Amount

      {
         account with
            Balance = balance
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDebitReversal
                  account.MaintenanceFeeCriteria
                  balance
      }
   | PlatformPaymentDeposited e ->
      applyTransferDeposit account e.Data.BaseInfo.Amount
   | AutoTransferRuleConfigured e -> {
      account with
         AutoTransferRule = Some e.Data.Config
     }
   | AutoTransferRuleDeleted _ -> { account with AutoTransferRule = None }
   | InternalAutomatedTransferPending e ->
      applyInternalTransferPending e.Data.BaseInfo account
   | InternalAutomatedTransferFailed e ->
      applyInternalTransferFailed e.Data.BaseInfo account
   | InternalAutomatedTransferDeposited e ->
      applyTransferDeposit account e.Data.BaseInfo.Amount

let transitionErr (err: AccountStateTransitionError) =
   Error <| AccountStateTransitionError err

module private StateTransition =
   let accountNotActiveError (account: Account) =
      transitionErr (AccountNotActive account.FullName)

   let map
      (eventTransform: BankEvent<'t> -> AccountEvent)
      (account: Account)
      (eventValidation: ValidationResult<BankEvent<'t>>)
      : Result<(AccountEvent * Account), Err>
      =
      eventValidation
      |> Result.mapError ValidationError
      |> Result.map (fun evt ->
         let evt = eventTransform evt
         (evt, applyEvent account evt))

   let initializePrimaryCheckingAccount
      (account: Account)
      (cmd: InitializePrimaryCheckingAccountCommand)
      =
      if account.Status <> AccountStatus.InitialEmptyState then
         transitionErr AccountNotReadyToActivate
      else
         map
            InitializedPrimaryCheckingAccount
            account
            (InitializePrimaryCheckingAccountCommand.toEvent cmd)

   let create (account: Account) (cmd: CreateAccountCommand) =
      if account.Status <> AccountStatus.InitialEmptyState then
         transitionErr AccountNotReadyToActivate
      else
         map CreatedAccount account (CreateAccountCommand.toEvent cmd)

   let startBillingcycle (account: Account) (cmd: StartBillingCycleCommand) =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      else
         map BillingCycleStarted account (StartBillingCycleCommand.toEvent cmd)

   let deposit (account: Account) (cmd: DepositCashCommand) =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map DepositedCash account (DepositCashCommand.toEvent cmd)

   let debit (account: Account) (cmd: DebitCommand) =
      let input = cmd.Data

      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance(account.Balance, account.FullName)
      else
         map DebitedAccount account (DebitCommand.toEvent cmd)

   let refundDebit (account: Account) (cmd: RefundDebitCommand) =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map RefundedDebit account (RefundDebitCommand.toEvent cmd)

   let maintenanceFee (account: Account) (cmd: MaintenanceFeeCommand) =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      elif account.Balance - cmd.Data.Amount < 0m then
         transitionErr <| InsufficientBalance(account.Balance, account.FullName)
      else
         map MaintenanceFeeDebited account (MaintenanceFeeCommand.toEvent cmd)

   let skipMaintenanceFee (account: Account) (cmd: SkipMaintenanceFeeCommand) =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            MaintenanceFeeSkipped
            account
            (SkipMaintenanceFeeCommand.toEvent cmd)

   let internalTransfer
      (account: Account)
      (cmd: InternalTransferWithinOrgCommand)
      =
      let input = cmd.Data

      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance(account.Balance, account.FullName)
      elif input.Recipient.OrgId <> account.OrgId then
         transitionErr TransferExpectedToOccurWithinOrg
      else
         map
            InternalTransferWithinOrgPending
            account
            (InternalTransferWithinOrgCommand.toEvent cmd)

   let failInternalTransfer
      (account: Account)
      (cmd: FailInternalTransferWithinOrgCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            InternalTransferWithinOrgFailed
            account
            (FailInternalTransferWithinOrgCommand.toEvent cmd)

   let scheduleInternalTransferBetweenOrgs
      (account: Account)
      (cmd: ScheduleInternalTransferBetweenOrgsCommand)
      =
      let input = cmd.Data.TransferInput

      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance(account.Balance, account.FullName)
      else
         map
            InternalTransferBetweenOrgsScheduled
            account
            (ScheduleInternalTransferBetweenOrgsCommand.toEvent cmd)

   let internalTransferBetweenOrgs
      (account: Account)
      (cmd: InternalTransferBetweenOrgsCommand)
      =
      let input = cmd.Data

      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance(account.Balance, account.FullName)
      else
         map
            InternalTransferBetweenOrgsPending
            account
            (InternalTransferBetweenOrgsCommand.toEvent cmd)

   let failInternalTransferBetweenOrgs
      (account: Account)
      (cmd: FailInternalTransferBetweenOrgsCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            InternalTransferBetweenOrgsFailed
            account
            (FailInternalTransferBetweenOrgsCommand.toEvent cmd)

   let scheduleDomesticTransfer
      (account: Account)
      (cmd: ScheduleDomesticTransferCommand)
      =
      let input = cmd.Data.TransferInput

      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance(account.Balance, account.FullName)
      else
         map
            DomesticTransferScheduled
            account
            (ScheduleDomesticTransferCommand.toEvent cmd)

   let domesticTransfer (account: Account) (cmd: DomesticTransferCommand) =
      let input = cmd.Data

      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance(account.Balance, account.FullName)
      else
         map
            DomesticTransferPending
            account
            (DomesticTransferCommand.toEvent cmd)

   let domesticTransferProgress
      (account: Account)
      (cmd: UpdateDomesticTransferProgressCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            DomesticTransferProgress
            account
            (UpdateDomesticTransferProgressCommand.toEvent cmd)

   let completeDomesticTransfer
      (account: Account)
      (cmd: CompleteDomesticTransferCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            DomesticTransferCompleted
            account
            (CompleteDomesticTransferCommand.toEvent cmd)

   let failDomesticTransfer
      (account: Account)
      (cmd: FailDomesticTransferCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            DomesticTransferFailed
            account
            (FailDomesticTransferCommand.toEvent cmd)

   let depositTransferWithinOrg
      (account: Account)
      (cmd: DepositInternalTransferWithinOrgCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map InternalTransferWithinOrgDeposited account
         <| (DepositInternalTransferWithinOrgCommand.toEvent cmd)

   let depositTransferBetweenOrgs
      (account: Account)
      (cmd: DepositInternalTransferBetweenOrgsCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map InternalTransferBetweenOrgsDeposited account
         <| (DepositInternalTransferBetweenOrgsCommand.toEvent cmd)

   let platformPaymentRequested
      (account: Account)
      (cmd: RequestPlatformPaymentCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            PlatformPaymentRequested
            account
            (RequestPlatformPaymentCommand.toEvent cmd)

   let platformPaymentCancelled
      (account: Account)
      (cmd: CancelPlatformPaymentCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            PlatformPaymentCancelled
            account
            (CancelPlatformPaymentCommand.toEvent cmd)

   let platformPaymentDeclined
      (account: Account)
      (cmd: DeclinePlatformPaymentCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            PlatformPaymentDeclined
            account
            (DeclinePlatformPaymentCommand.toEvent cmd)

   let platformPaymentPaid
      (account: Account)
      (cmd: FulfillPlatformPaymentCommand)
      =
      let input = cmd.Data.RequestedPayment.BaseInfo

      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      elif account.Balance - input.Amount < 0m then
         transitionErr <| InsufficientBalance(account.Balance, account.FullName)
      else
         map
            PlatformPaymentPaid
            account
            (FulfillPlatformPaymentCommand.toEvent cmd)

   let platformPaymentRefunded
      (account: Account)
      (cmd: RefundPlatformPaymentCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            PlatformPaymentRefunded
            account
            (RefundPlatformPaymentCommand.toEvent cmd)

   let platformPaymentDeposited
      (account: Account)
      (cmd: DepositPlatformPaymentCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            PlatformPaymentDeposited
            account
            (DepositPlatformPaymentCommand.toEvent cmd)

   let closeAccount (account: Account) (cmd: CloseAccountCommand) =
      map AccountEvent.AccountClosed account (CloseAccountCommand.toEvent cmd)

   let configureAutoTransferRule
      (account: Account)
      (cmd: ConfigureAutoTransferRuleCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         match account.AutoTransferRule, cmd.Data.RuleIdToUpdate with
         | Some _, None -> transitionErr OnlyOneAutoTransferRuleMayExistAtATime
         | Some r, Some existingId when r.Id <> existingId ->
            transitionErr AutoTransferRuleDoesNotExist
         | None, Some _ -> transitionErr AutoTransferRuleDoesNotExist
         | _ ->
            map
               AutoTransferRuleConfigured
               account
               (ConfigureAutoTransferRuleCommand.toEvent cmd)

   let deleteAutoTransferRule
      (account: Account)
      (cmd: DeleteAutoTransferRuleCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            AutoTransferRuleDeleted
            account
            (DeleteAutoTransferRuleCommand.toEvent cmd)

   let internalAutoTransfer
      (account: Account)
      (cmd: InternalAutoTransferCommand)
      =
      let input = cmd.Data.Transfer
      let amount = PositiveAmount.get input.Amount

      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      elif account.Balance - amount < 0m then
         transitionErr <| InsufficientBalance(account.Balance, account.FullName)
      elif input.Recipient.OrgId <> account.OrgId then
         transitionErr TransferExpectedToOccurWithinOrg
      else
         map
            InternalAutomatedTransferPending
            account
            (InternalAutoTransferCommand.toEvent cmd)

   let failInternalAutoTransfer
      (account: Account)
      (cmd: FailInternalAutoTransferCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map
            InternalAutomatedTransferFailed
            account
            (FailInternalAutoTransferCommand.toEvent cmd)

   let depositInternalAutoTransfer
      (account: Account)
      (cmd: DepositInternalAutoTransferCommand)
      =
      if account.Status <> AccountStatus.Active then
         transitionErr (AccountNotActive account.FullName)
      else
         map InternalAutomatedTransferDeposited account
         <| (DepositInternalAutoTransferCommand.toEvent cmd)

let stateTransition (account: Account) (command: AccountCommand) =
   match command with
   | AccountCommand.InitializePrimaryCheckingAccount cmd ->
      StateTransition.initializePrimaryCheckingAccount account cmd
   | AccountCommand.CreateAccount cmd -> StateTransition.create account cmd
   | AccountCommand.StartBillingCycle cmd ->
      StateTransition.startBillingcycle account cmd
   | AccountCommand.DepositCash cmd -> StateTransition.deposit account cmd
   | AccountCommand.Debit cmd -> StateTransition.debit account cmd
   | AccountCommand.RefundDebit cmd -> StateTransition.refundDebit account cmd
   | AccountCommand.MaintenanceFee cmd ->
      StateTransition.maintenanceFee account cmd
   | AccountCommand.SkipMaintenanceFee cmd ->
      StateTransition.skipMaintenanceFee account cmd
   | AccountCommand.InternalTransfer cmd ->
      StateTransition.internalTransfer account cmd
   | AccountCommand.FailInternalTransfer cmd ->
      StateTransition.failInternalTransfer account cmd
   | AccountCommand.ScheduleInternalTransferBetweenOrgs cmd ->
      StateTransition.scheduleInternalTransferBetweenOrgs account cmd
   | AccountCommand.InternalTransferBetweenOrgs cmd ->
      StateTransition.internalTransferBetweenOrgs account cmd
   | AccountCommand.FailInternalTransferBetweenOrgs cmd ->
      StateTransition.failInternalTransferBetweenOrgs account cmd
   | AccountCommand.DepositTransferWithinOrg cmd ->
      StateTransition.depositTransferWithinOrg account cmd
   | AccountCommand.DepositTransferBetweenOrgs cmd ->
      StateTransition.depositTransferBetweenOrgs account cmd
   | AccountCommand.ScheduleDomesticTransfer cmd ->
      StateTransition.scheduleDomesticTransfer account cmd
   | AccountCommand.DomesticTransfer cmd ->
      StateTransition.domesticTransfer account cmd
   | AccountCommand.CompleteDomesticTransfer cmd ->
      StateTransition.completeDomesticTransfer account cmd
   | AccountCommand.FailDomesticTransfer cmd ->
      StateTransition.failDomesticTransfer account cmd
   | AccountCommand.UpdateDomesticTransferProgress cmd ->
      StateTransition.domesticTransferProgress account cmd
   | AccountCommand.CloseAccount cmd -> StateTransition.closeAccount account cmd
   | AccountCommand.RequestPlatformPayment cmd ->
      StateTransition.platformPaymentRequested account cmd
   | AccountCommand.FulfillPlatformPayment cmd ->
      StateTransition.platformPaymentPaid account cmd
   | AccountCommand.RefundPlatformPayment cmd ->
      StateTransition.platformPaymentRefunded account cmd
   | AccountCommand.DepositPlatformPayment cmd ->
      StateTransition.platformPaymentDeposited account cmd
   | AccountCommand.CancelPlatformPayment cmd ->
      StateTransition.platformPaymentCancelled account cmd
   | AccountCommand.DeclinePlatformPayment cmd ->
      StateTransition.platformPaymentDeclined account cmd
   | AccountCommand.ConfigureAutoTransferRule cmd ->
      StateTransition.configureAutoTransferRule account cmd
   | AccountCommand.DeleteAutoTransferRule cmd ->
      StateTransition.deleteAutoTransferRule account cmd
   | AccountCommand.InternalAutoTransfer cmd ->
      StateTransition.internalAutoTransfer account cmd
   | AccountCommand.FailInternalAutoTransfer cmd ->
      StateTransition.failInternalAutoTransfer account cmd
   | AccountCommand.DepositInternalAutoTransfer cmd ->
      StateTransition.depositInternalAutoTransfer account cmd
