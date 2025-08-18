[<RequireQualifiedAccess>]
module Account

open Validus

open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Payment.Domain
open Lib.SharedTypes

let applyEvent (account: Account) (evt: AccountEvent) =
   match evt with
   | ParentAccount _ -> account
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
      PendingDeductions = PendingDeductions.Zero
      Status = AccountStatus.Active
      AutoTransferRule = None
     }
   | CreatedVirtualAccount e -> {
      AccountId = e.Data.AccountId
      AccountNumber = e.Data.AccountNumber
      RoutingNumber = e.Data.RoutingNumber
      OrgId = e.OrgId
      ParentAccountId = ParentAccountId.fromEntityId e.EntityId
      Name = e.Data.Name
      Depository = e.Data.Depository
      Currency = e.Data.Currency
      Balance = 0m
      PendingDeductions = PendingDeductions.Zero
      Status = AccountStatus.Active
      AutoTransferRule = None
     }
   | AccountEvent.AccountClosed _ -> {
      account with
         Status = AccountStatus.Closed
     }
   | DepositedCash e -> {
      account with
         Balance = account.Balance + e.Data.Amount
     }
   | DebitPending e -> {
      account with
         PendingDeductions = account.PendingDeductions.Add e.Data.Amount
     }
   | DebitSettled e -> {
      account with
         Balance = account.Balance - e.Data.Amount
         PendingDeductions = account.PendingDeductions.Remove e.Data.Amount
     }
   | DebitFailed e -> {
      account with
         PendingDeductions = account.PendingDeductions.Remove e.Data.Amount
     }
   | DebitRefunded e -> {
      account with
         Balance = account.Balance + e.Data.Amount
     }
   | MaintenanceFeeDebited e -> {
      account with
         Balance = account.Balance - e.Data.Amount
     }
   | MaintenanceFeeSkipped _ -> account
   | DomesticTransferScheduled _ -> account
   | DomesticTransferPending e -> {
      account with
         PendingDeductions =
            account.PendingDeductions.Add e.Data.BaseInfo.Amount
     }
   | DomesticTransferProgress _ -> account
   | DomesticTransferSettled e -> {
      account with
         Balance = account.Balance - e.Data.BaseInfo.Amount
         PendingDeductions =
            account.PendingDeductions.Remove e.Data.BaseInfo.Amount
     }
   | DomesticTransferFailed e -> {
      account with
         PendingDeductions =
            account.PendingDeductions.Remove e.Data.BaseInfo.Amount
     }
   | InternalTransferWithinOrgDeducted e -> {
      account with
         Balance = account.Balance - e.Data.BaseInfo.Amount
     }
   | InternalTransferWithinOrgDeposited e -> {
      account with
         Balance = account.Balance + e.Data.BaseInfo.Amount
     }
   | InternalTransferBetweenOrgsScheduled _ -> account
   | InternalTransferBetweenOrgsPending e -> {
      account with
         PendingDeductions =
            account.PendingDeductions.Add e.Data.BaseInfo.Amount
     }
   | InternalTransferBetweenOrgsFailed e -> {
      account with
         PendingDeductions =
            account.PendingDeductions.Remove e.Data.BaseInfo.Amount
     }
   | InternalTransferBetweenOrgsDeposited e -> {
      account with
         Balance = account.Balance + e.Data.BaseInfo.Amount
     }
   | InternalTransferBetweenOrgsSettled e -> {
      account with
         Balance = account.Balance - e.Data.BaseInfo.Amount
         PendingDeductions =
            account.PendingDeductions.Remove e.Data.BaseInfo.Amount
     }
   | PaymentRequested _ -> account
   | PaymentRequestCancelled _ -> account
   | PaymentRequestDeclined _ -> account
   | AutoTransferRuleConfigured e -> {
      account with
         AutoTransferRule = Some e.Data.Config
     }
   | AutoTransferRuleDeleted _ -> { account with AutoTransferRule = None }
   | InternalAutomatedTransferDeducted e -> {
      account with
         Balance = account.Balance - e.Data.BaseInfo.Amount
     }
   | InternalAutomatedTransferDeposited e -> {
      account with
         Balance = account.Balance + e.Data.BaseInfo.Amount
     }

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
         evt, applyEvent account evt)

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

   let create (account: Account) (cmd: CreateVirtualAccountCommand) =
      if account.Status <> AccountStatus.InitialEmptyState then
         transitionErr AccountNotReadyToActivate
      else
         map
            CreatedVirtualAccount
            account
            (CreateVirtualAccountCommand.toEvent cmd)

   let deposit (account: Account) (cmd: DepositCashCommand) =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      else
         map DepositedCash account (DepositCashCommand.toEvent cmd)

   let debit (account: Account) (cmd: DebitCommand) =
      let input = cmd.Data

      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      elif account.AvailableBalance - input.Amount < 0m then
         transitionErr
         <| InsufficientBalance(account.AvailableBalance, account.FullName)
      else
         map DebitPending account (DebitCommand.toEvent cmd)

   let settleDebit (account: Account) (cmd: SettleDebitCommand) =
      map DebitSettled account (SettleDebitCommand.toEvent cmd)

   let failDebit (account: Account) (cmd: FailDebitCommand) =
      map DebitFailed account (FailDebitCommand.toEvent cmd)

   let refundDebit (account: Account) (cmd: RefundDebitCommand) =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      else
         map DebitRefunded account (RefundDebitCommand.toEvent cmd)

   let maintenanceFee (account: Account) (cmd: MaintenanceFeeCommand) =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      elif account.AvailableBalance - cmd.Data.Amount < 0m then
         transitionErr
         <| InsufficientBalance(account.AvailableBalance, account.FullName)
      else
         map MaintenanceFeeDebited account (MaintenanceFeeCommand.toEvent cmd)

   let skipMaintenanceFee (account: Account) (cmd: SkipMaintenanceFeeCommand) =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
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
         accountNotActiveError account
      elif account.AvailableBalance - input.Amount < 0m then
         transitionErr
         <| InsufficientBalance(account.AvailableBalance, account.FullName)
      elif input.Recipient.OrgId <> account.OrgId then
         transitionErr TransferExpectedToOccurWithinOrg
      else
         map
            InternalTransferWithinOrgDeducted
            account
            (InternalTransferWithinOrgCommand.toEvent cmd)

   let depositTransferWithinOrg
      (account: Account)
      (cmd: DepositInternalTransferWithinOrgCommand)
      =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      else
         map
            InternalTransferWithinOrgDeposited
            account
            (DepositInternalTransferWithinOrgCommand.toEvent cmd)

   let scheduleInternalTransferBetweenOrgs
      (account: Account)
      (cmd: ScheduleInternalTransferBetweenOrgsCommand)
      =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      else
         map
            InternalTransferBetweenOrgsScheduled
            account
            (ScheduleInternalTransferBetweenOrgsCommand.toEvent cmd)

   let internalTransferBetweenOrgs
      (account: Account)
      (cmd: InternalTransferBetweenOrgsCommand)
      =
      let info = cmd.Data

      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      elif account.AvailableBalance - info.Amount < 0m then
         transitionErr
         <| InsufficientBalance(account.AvailableBalance, account.FullName)
      else
         map
            InternalTransferBetweenOrgsPending
            account
            (InternalTransferBetweenOrgsCommand.toEvent cmd)

   let settleInternalTransferBetweenOrgs
      (account: Account)
      (cmd: SettleInternalTransferBetweenOrgsCommand)
      =
      map
         InternalTransferBetweenOrgsSettled
         account
         (SettleInternalTransferBetweenOrgsCommand.toEvent cmd)

   let failInternalTransferBetweenOrgs
      (account: Account)
      (cmd: FailInternalTransferBetweenOrgsCommand)
      =
      map
         InternalTransferBetweenOrgsFailed
         account
         (FailInternalTransferBetweenOrgsCommand.toEvent cmd)

   let scheduleDomesticTransfer
      (account: Account)
      (cmd: ScheduleDomesticTransferCommand)
      =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      else
         map
            DomesticTransferScheduled
            account
            (ScheduleDomesticTransferCommand.toEvent cmd)

   let domesticTransfer (account: Account) (cmd: DomesticTransferCommand) =
      let input = cmd.Data

      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      elif account.AvailableBalance - input.Amount < 0m then
         transitionErr
         <| InsufficientBalance(account.AvailableBalance, account.FullName)
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
         accountNotActiveError account
      else
         map
            DomesticTransferProgress
            account
            (UpdateDomesticTransferProgressCommand.toEvent cmd)

   let settleDomesticTransfer
      (account: Account)
      (cmd: SettleDomesticTransferCommand)
      =
      map
         DomesticTransferSettled
         account
         (SettleDomesticTransferCommand.toEvent cmd)

   let failDomesticTransfer
      (account: Account)
      (cmd: FailDomesticTransferCommand)
      =
      map
         DomesticTransferFailed
         account
         (FailDomesticTransferCommand.toEvent cmd)

   let depositTransferBetweenOrgs
      (account: Account)
      (cmd: DepositInternalTransferBetweenOrgsCommand)
      =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      else
         map InternalTransferBetweenOrgsDeposited account
         <| DepositInternalTransferBetweenOrgsCommand.toEvent cmd

   let requestPayment (account: Account) (cmd: RequestPaymentCommand) =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      else
         map PaymentRequested account (RequestPaymentCommand.toEvent cmd)

   let cancelPaymentRequest
      (account: Account)
      (cmd: CancelPaymentRequestCommand)
      =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      else
         map
            PaymentRequestCancelled
            account
            (CancelPaymentRequestCommand.toEvent cmd)

   let declinePaymentRequest
      (account: Account)
      (cmd: DeclinePaymentRequestCommand)
      =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      else
         map
            PaymentRequestDeclined
            account
            (DeclinePaymentRequestCommand.toEvent cmd)

   let closeAccount (account: Account) (cmd: CloseAccountCommand) =
      map AccountEvent.AccountClosed account (CloseAccountCommand.toEvent cmd)

   let configureAutoTransferRule
      (account: Account)
      (cmd: ConfigureAutoTransferRuleCommand)
      =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      else
         match account.AutoTransferRule, cmd.Data.RuleIdToUpdate with
         | Some _, None -> transitionErr AutoTransferOnlyOneRuleMayExistAtATime
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
         accountNotActiveError account
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
      let amount = input.Amount.Value

      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      elif account.AvailableBalance - amount < 0m then
         transitionErr
         <| InsufficientBalance(account.AvailableBalance, account.FullName)
      elif input.Recipient.OrgId <> account.OrgId then
         transitionErr TransferExpectedToOccurWithinOrg
      else
         map
            InternalAutomatedTransferDeducted
            account
            (InternalAutoTransferCommand.toEvent cmd)

   let depositInternalAutoTransfer
      (account: Account)
      (cmd: DepositInternalAutoTransferCommand)
      =
      if account.Status <> AccountStatus.Active then
         accountNotActiveError account
      else
         map InternalAutomatedTransferDeposited account
         <| DepositInternalAutoTransferCommand.toEvent cmd

let stateTransition (account: Account) (command: AccountCommand) =
   match command with
   | AccountCommand.InitializePrimaryCheckingAccount cmd ->
      StateTransition.initializePrimaryCheckingAccount account cmd
   | AccountCommand.CreateVirtualAccount cmd ->
      StateTransition.create account cmd
   | AccountCommand.DepositCash cmd -> StateTransition.deposit account cmd
   | AccountCommand.Debit cmd -> StateTransition.debit account cmd
   | AccountCommand.SettleDebit cmd -> StateTransition.settleDebit account cmd
   | AccountCommand.FailDebit cmd -> StateTransition.failDebit account cmd
   | AccountCommand.RefundDebit cmd -> StateTransition.refundDebit account cmd
   | AccountCommand.MaintenanceFee cmd ->
      StateTransition.maintenanceFee account cmd
   | AccountCommand.SkipMaintenanceFee cmd ->
      StateTransition.skipMaintenanceFee account cmd
   | AccountCommand.InternalTransfer cmd ->
      StateTransition.internalTransfer account cmd
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
   | AccountCommand.SettleInternalTransferBetweenOrgs cmd ->
      StateTransition.settleInternalTransferBetweenOrgs account cmd
   | AccountCommand.ScheduleDomesticTransfer cmd ->
      StateTransition.scheduleDomesticTransfer account cmd
   | AccountCommand.DomesticTransfer cmd ->
      StateTransition.domesticTransfer account cmd
   | AccountCommand.SettleDomesticTransfer cmd ->
      StateTransition.settleDomesticTransfer account cmd
   | AccountCommand.FailDomesticTransfer cmd ->
      StateTransition.failDomesticTransfer account cmd
   | AccountCommand.UpdateDomesticTransferProgress cmd ->
      StateTransition.domesticTransferProgress account cmd
   | AccountCommand.CloseAccount cmd -> StateTransition.closeAccount account cmd
   | AccountCommand.RequestPayment cmd ->
      StateTransition.requestPayment account cmd
   | AccountCommand.CancelPaymentRequest cmd ->
      StateTransition.cancelPaymentRequest account cmd
   | AccountCommand.DeclinePaymentRequest cmd ->
      StateTransition.declinePaymentRequest account cmd
   | AccountCommand.ConfigureAutoTransferRule cmd ->
      StateTransition.configureAutoTransferRule account cmd
   | AccountCommand.DeleteAutoTransferRule cmd ->
      StateTransition.deleteAutoTransferRule account cmd
   | AccountCommand.InternalAutoTransfer cmd ->
      StateTransition.internalAutoTransfer account cmd
   | AccountCommand.DepositInternalAutoTransfer cmd ->
      StateTransition.depositInternalAutoTransfer account cmd
   | AccountCommand.ParentAccount _ ->
      account.AccountId
      |> AccountStateTransitionError.AccountNotFound
      |> transitionErr
