[<RequireQualifiedAccess>]
module ParentAccount

open System
open Validus

open Bank.Account.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes
open Lib.Time
open AutomaticTransfer

module AutoTransfer =
   let hasCycle
      (accounts: Map<AccountId, Account>)
      (newRule: AutomaticTransferConfig)
      =
      accounts.Values
      |> Seq.toList
      |> List.choose _.AutoTransferRule
      |> CycleDetection.cycleDetected newRule

/// Transfer amounts accrued include settled and in flight transactions.
module TransferLimits =
   /// Transfers & payments between orgs on the platform
   let DailyPlatformLimit = 999_999_999m

   /// Transfers to entities outside the platform (ex: via ACH)
   let DailyDomesticLimit = 100_000m

   let private transferAccrued
      (satisfiesDate: DateTime -> bool)
      (eventToAccrual: AccountEvent -> (DateTime * decimal) option)
      (events: AccountEvent list)
      : decimal
      =
      List.fold
         (fun acc evt ->
            match eventToAccrual evt with
            | Some(date, amount) ->
               if satisfiesDate date then acc + amount else acc
            | None -> acc)
         0m
         events

   let dailyPlatformTransferAccrued =
      transferAccrued DateTime.isToday (function
         | AccountEvent.InternalTransferBetweenOrgsPending e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, info.Amount)
         | AccountEvent.InternalTransferBetweenOrgsFailed e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, -info.Amount)
         | AccountEvent.PlatformPaymentPending e ->
            Some(e.Timestamp, e.Data.BaseInfo.Amount)
         | AccountEvent.PlatformPaymentFailed e ->
            Some(e.Timestamp, -e.Data.BaseInfo.Amount)
         | _ -> None)

   let dailyDomesticTransferAccrued =
      transferAccrued DateTime.isToday (function
         | AccountEvent.DomesticTransferPending e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, info.Amount)
         | AccountEvent.DomesticTransferFailed e ->
            let info = e.Data.BaseInfo
            Some(info.ScheduledDate, -info.Amount)
         | _ -> None)

   let exceedsDailyInternalTransferLimit
      (evts: AccountEvent list)
      (amount: decimal)
      (scheduledDate: DateTime)
      =
      DateTime.isToday scheduledDate
      && dailyPlatformTransferAccrued evts + amount > DailyPlatformLimit

   let exceedsDailyDomesticTransferLimit
      (evts: AccountEvent list)
      (amount: decimal)
      (scheduledDate: DateTime)
      =
      DateTime.isToday scheduledDate
      && dailyDomesticTransferAccrued evts + amount > DailyDomesticLimit

let applyEvent (state: ParentAccountSnapshot) (evt: AccountEvent) =
   let updated =
      match evt with
      | AccountEvent.ParentAccount evt ->
         match evt with
         | ParentAccountEvent.RegisteredDomesticTransferRecipient e ->
            let recipient = e.Data.Recipient

            {
               state with
                  DomesticTransferRecipients =
                     state.DomesticTransferRecipients.Add(
                        recipient.RecipientAccountId,
                        recipient
                     )
            }
         | ParentAccountEvent.EditedDomesticTransferRecipient e ->
            let recipient = e.Data.Recipient

            {
               state with
                  DomesticTransferRecipients =
                     state.DomesticTransferRecipients.Change(
                        recipient.RecipientAccountId,
                        Option.map (fun _ -> recipient)
                     )
            }
         | ParentAccountEvent.NicknamedDomesticTransferRecipient e -> {
            state with
               DomesticTransferRecipients =
                  Map.change
                     e.Data.RecipientId
                     (Option.map (fun acct -> {
                        acct with
                           Nickname = e.Data.Nickname
                     }))
                     state.DomesticTransferRecipients
           }
      | _ -> {
         state with
            VirtualAccounts =
               state.VirtualAccounts
               |> Map.change evt.AccountId (fun accountOpt ->
                  Account.applyEvent
                     (accountOpt |> Option.defaultValue Account.empty)
                     evt
                  |> Some)
        }

   let updated =
      match evt with
      | AccountEvent.InitializedPrimaryCheckingAccount e -> {
         updated with
            OrgId = e.OrgId
            ParentAccountId = e.Data.ParentAccountId
            AccountNumber = e.Data.PartnerBankAccountNumber
            RoutingNumber = e.Data.PartnerBankRoutingNumber
            PrimaryVirtualAccountId = e.Data.PrimaryChecking.AccountId
            Status = ParentAccountStatus.Active
        }
      | AccountEvent.MaintenanceFeeSkipped e -> {
         updated with
            MaintenanceFeeCriteria = MaintenanceFee.reset updated.Balance
            LastBillingCycleDate = Some e.Data.BillingDate
        }
      | AccountEvent.MaintenanceFeeDebited e -> {
         updated with
            MaintenanceFeeCriteria = MaintenanceFee.reset updated.Balance
            LastBillingCycleDate = Some e.Data.BillingDate
        }
      | AccountEvent.DebitSettled _
      | AccountEvent.InternalTransferBetweenOrgsSettled _ -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDebit
                  state.MaintenanceFeeCriteria
                  updated.Balance
        }
      | AccountEvent.PlatformPaymentSettled e ->
         match e.Data.PaymentMethod with
         | PaymentMethod.Platform _ -> {
            updated with
               MaintenanceFeeCriteria =
                  MaintenanceFee.fromDebit
                     state.MaintenanceFeeCriteria
                     updated.Balance
           }
         | PaymentMethod.ThirdParty _ -> updated
      // When a domestic transfer lifecycle is as follows:
      // (fails due to invalid account info -> retries -> settled)
      // then update the recipient status to Confirmed.
      | AccountEvent.DomesticTransferSettled e ->
         let previouslyFailedDueToInvalidRecipient =
            match e.Data.FromRetry with
            | Some(DomesticTransferFailReason.ThirdParty DomesticTransferThirdPartyFailReason.RecipientAccountInvalidInfo) ->
               true
            | _ -> false

         {
            updated with
               MaintenanceFeeCriteria =
                  MaintenanceFee.fromDebit
                     state.MaintenanceFeeCriteria
                     updated.Balance
               DomesticTransferRecipients =
                  if previouslyFailedDueToInvalidRecipient then
                     state.DomesticTransferRecipients.Change(
                        e.Data.BaseInfo.Recipient.RecipientAccountId,
                        Option.map (fun recipient -> {
                           recipient with
                              Status = RecipientRegistrationStatus.Confirmed
                        })
                     )
                  else
                     state.DomesticTransferRecipients
         }
      | AccountEvent.DomesticTransferFailed e ->
         let updateStatusDueToRecipientRelatedFailure =
            match e.Data.Reason with
            | DomesticTransferFailReason.ThirdParty DomesticTransferThirdPartyFailReason.RecipientAccountInvalidInfo ->
               Some RecipientRegistrationStatus.InvalidAccount
            | DomesticTransferFailReason.ThirdParty DomesticTransferThirdPartyFailReason.RecipientAccountNotActive ->
               Some RecipientRegistrationStatus.Closed
            | _ -> None

         {
            updated with
               DomesticTransferRecipients =
                  match updateStatusDueToRecipientRelatedFailure with
                  | Some failStatus ->
                     state.DomesticTransferRecipients.Change(
                        e.Data.BaseInfo.Recipient.RecipientAccountId,
                        Option.map (fun recipient -> {
                           recipient with
                              Status = failStatus
                        })
                     )
                  | None -> state.DomesticTransferRecipients
         }
      | AccountEvent.DebitRefunded _ -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDebitReversal
                  state.MaintenanceFeeCriteria
                  updated.Balance
        }
      | AccountEvent.DepositedCash e -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDeposit
                  state.MaintenanceFeeCriteria
                  e.Data.Amount
        }
      | AccountEvent.InternalTransferWithinOrgDeposited e -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDeposit
                  state.MaintenanceFeeCriteria
                  e.Data.BaseInfo.Amount
        }
      | AccountEvent.InternalTransferBetweenOrgsDeposited e -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDeposit
                  state.MaintenanceFeeCriteria
                  e.Data.BaseInfo.Amount
        }
      | AccountEvent.PlatformPaymentDeposited e -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDeposit
                  state.MaintenanceFeeCriteria
                  e.Data.BaseInfo.Amount
        }
      | AccountEvent.InternalAutomatedTransferDeposited e -> {
         updated with
            MaintenanceFeeCriteria =
               MaintenanceFee.fromDeposit
                  state.MaintenanceFeeCriteria
                  e.Data.BaseInfo.Amount
        }
      | _ -> updated

   let updatedEvents = evt :: state.Events
   let previousBillingDate = state.LastBillingCycleDate

   let updatedEvents =
      match evt, previousBillingDate with
      | MaintenanceFeeDebited _, Some date
      | MaintenanceFeeSkipped _, Some date ->
         // Keep events that occurred after the previous billing statement period.
         let cutoff = DateTime(date.Year, date.Month, 1).ToUniversalTime()

         updatedEvents
         |> List.filter (fun evt ->
            let _, env = AccountEnvelope.unwrap evt
            env.Timestamp >= cutoff)
      | _ -> updatedEvents

   { updated with Events = updatedEvents }

let private virtualAccountTransition
   (account: Account)
   (cmd: AccountCommand)
   (state: ParentAccountSnapshot)
   =
   Account.stateTransition account cmd
   |> Result.map (fun (evt, _) -> evt, applyEvent state evt)

let validatePlatformTransferLimit amount timestamp (evt, state) =
   if
      TransferLimits.exceedsDailyInternalTransferLimit
         state.Events
         amount
         timestamp
   then
      TransferLimits.DailyPlatformLimit
      |> ExceededDailyInternalTransferLimit
      |> Account.transitionErr
   else
      Ok(evt, state)

let validateDomesticTransferLimit amount timestamp (evt, state) =
   if
      TransferLimits.exceedsDailyDomesticTransferLimit
         state.Events
         amount
         timestamp
   then
      TransferLimits.DailyDomesticLimit
      |> ExceededDailyDomesticTransferLimit
      |> Account.transitionErr
   else
      Ok(evt, state)

let validateParentAccountActive (state: ParentAccountSnapshot) =
   if state.Status <> ParentAccountStatus.Active then
      Account.transitionErr AccountStateTransitionError.ParentAccountNotActive
   else
      Ok state

module private StateTransition =
   let mapParent
      (eventTransform: BankEvent<'t> -> ParentAccountEvent)
      (state: ParentAccountSnapshot)
      (eventValidation: ValidationResult<BankEvent<'t>>)
      =
      eventValidation
      |> Result.mapError ValidationError
      |> Result.map (fun evt ->
         let evt = AccountEvent.ParentAccount(eventTransform evt)
         evt, applyEvent state evt)

   let registerDomesticTransferRecipient
      (state: ParentAccountSnapshot)
      (cmd: RegisterDomesticTransferRecipientCommand)
      =
      if state.Status <> ParentAccountStatus.Active then
         Account.transitionErr
            AccountStateTransitionError.ParentAccountNotActive
      elif
         state.DomesticTransferRecipients
         |> Map.exists (fun _ recipient ->
            string recipient.AccountNumber = cmd.Data.AccountNumber
            && string recipient.RoutingNumber = cmd.Data.RoutingNumber)
      then
         Account.transitionErr AccountStateTransitionError.RecipientRegistered
      else
         mapParent
            ParentAccountEvent.RegisteredDomesticTransferRecipient
            state
            (RegisterDomesticTransferRecipientCommand.toEvent cmd)

   let editDomesticTransferRecipient
      (state: ParentAccountSnapshot)
      (cmd: EditDomesticTransferRecipientCommand)
      =
      if state.Status <> ParentAccountStatus.Active then
         Account.transitionErr
            AccountStateTransitionError.ParentAccountNotActive
      elif
         state.DomesticTransferRecipients
         |> Map.tryFind
               cmd.Data.RecipientWithoutAppliedUpdates.RecipientAccountId
         |> Option.bind (fun recipient ->
            if recipient.Status = RecipientRegistrationStatus.Closed then
               Some recipient
            else
               None)
         |> Option.isSome
      then
         Account.transitionErr AccountStateTransitionError.RecipientDeactivated
      else
         mapParent
            ParentAccountEvent.EditedDomesticTransferRecipient
            state
            (EditDomesticTransferRecipientCommand.toEvent cmd)

   let nicknameDomesticTransferRecipient
      (state: ParentAccountSnapshot)
      (cmd: NicknameDomesticTransferRecipientCommand)
      =
      let recipientExists, recipientIsActive =
         match
            state.DomesticTransferRecipients.TryFind cmd.Data.RecipientId
         with
         | None -> false, false
         | Some recipient ->
            true, recipient.Status <> RecipientRegistrationStatus.Closed

      if state.Status <> ParentAccountStatus.Active then
         Account.transitionErr
            AccountStateTransitionError.ParentAccountNotActive
      elif not recipientExists then
         Account.transitionErr AccountStateTransitionError.RecipientNotFound
      else if not recipientIsActive then
         Account.transitionErr AccountStateTransitionError.RecipientDeactivated
      else
         mapParent
            ParentAccountEvent.NicknamedDomesticTransferRecipient
            state
            (NicknameDomesticTransferRecipientCommand.toEvent cmd)

let stateTransition
   (state: ParentAccountSnapshot)
   (command: AccountCommand)
   : Result<(AccountEvent * ParentAccountSnapshot), Err>
   =
   let accountId = command.AccountId
   let virtualAccount = state.VirtualAccounts.TryFind accountId

   match command, virtualAccount with
   | AccountCommand.ParentAccount cmd, _ ->
      match cmd with
      | ParentAccountCommand.RegisterDomesticTransferRecipient cmd ->
         StateTransition.registerDomesticTransferRecipient state cmd
      | ParentAccountCommand.EditDomesticTransferRecipient cmd ->
         StateTransition.editDomesticTransferRecipient state cmd
      | ParentAccountCommand.NicknameDomesticTransferRecipient cmd ->
         StateTransition.nicknameDomesticTransferRecipient state cmd
   | AccountCommand.InitializePrimaryCheckingAccount _, _ when
      not state.VirtualAccounts.IsEmpty
      ->
      Account.transitionErr
         AccountStateTransitionError.ParentAccountAlreadyInitialized
   | AccountCommand.InitializePrimaryCheckingAccount _, None ->
      virtualAccountTransition Account.empty command state
   | AccountCommand.CreateVirtualAccount _, Some _ ->
      Account.transitionErr
         AccountStateTransitionError.AccountNotReadyToActivate
   | AccountCommand.CreateVirtualAccount _, None ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition Account.empty command)
   | AccountCommand.InternalTransferBetweenOrgs cmd, Some account ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)
      |> Result.bind (
         validatePlatformTransferLimit cmd.Data.Amount cmd.Timestamp
      )
   | AccountCommand.PlatformPayment cmd, Some account ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)
      |> Result.bind (
         validatePlatformTransferLimit
            cmd.Data.RequestedPayment.BaseInfo.Amount
            cmd.Timestamp
      )
   | AccountCommand.DomesticTransfer cmd, Some account ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)
      |> Result.bind (
         validateDomesticTransferLimit cmd.Data.Amount cmd.Timestamp
      )
   | AccountCommand.ConfigureAutoTransferRule cmd, Some account ->
      let newRule = {
         Id = cmd.Data.RuleIdToUpdate |> Option.defaultValue (Guid.NewGuid())
         Info = cmd.Data.Rule
      }

      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)
      |> Result.bind (fun res ->
         if AutoTransfer.hasCycle state.VirtualAccounts newRule then
            Account.transitionErr AutoTransferCycleDetected
         else
            Ok res)
   | _, None -> Account.transitionErr (AccountNotFound accountId)
   | _, Some account ->
      validateParentAccountActive state
      |> Result.bind (virtualAccountTransition account command)

/// Compute auto transfer events and updated Parent Account or return a
/// violating auto transfer command with error causing state transition fail.
let computeAutoTransferStateTransitions
   (frequency: AutomaticTransfer.Frequency)
   (accountId: AccountId)
   (parentAccount: ParentAccountSnapshot)
   : Result<ParentAccountSnapshot * AccountEvent list, AccountCommand * Err> option
   =
   parentAccount.VirtualAccounts.TryFind(accountId)
   |> Option.bind (fun account ->
      let transfers =
         match frequency with
         | Frequency.PerTransaction -> account.AutoTransfersPerTransaction
         | Frequency.Schedule CronSchedule.Daily -> account.AutoTransfersDaily
         | Frequency.Schedule CronSchedule.TwiceMonthly ->
            account.AutoTransfersTwiceMonthly

      let transferOutCommands =
         transfers |> List.map InternalAutoTransferCommand.create

      let transferDepositCommands =
         transferOutCommands
         |> List.map (fun transferCmd ->
            let info = transferCmd.Data.Transfer

            DepositInternalAutoTransferCommand.create
               transferCmd.CorrelationId
               transferCmd.InitiatedBy
               {
                  BaseInfo = {
                     TransferId =
                        transferCmd.CorrelationId
                        |> CorrelationId.get
                        |> TransferId
                     InitiatedBy = transferCmd.InitiatedBy
                     Sender = info.Sender
                     Recipient = info.Recipient
                     Amount = PositiveAmount.get info.Amount
                     ScheduledDate = transferCmd.Timestamp
                     Memo = None
                  }
                  Rule = transferCmd.Data.Rule
               }
            |> AccountCommand.DepositInternalAutoTransfer)

      let transferCommands =
         (transferOutCommands |> List.map AccountCommand.InternalAutoTransfer)
         @ transferDepositCommands

      if transferCommands.IsEmpty then
         None
      else
         let validations =
            List.fold
               (fun acc cmd ->
                  match acc with
                  | Ok(accountState, events) ->
                     stateTransition accountState cmd
                     |> Result.map (fun (evt, newState) ->
                        newState, evt :: events)
                     |> Result.mapError (fun err -> cmd, err)
                  | Error err -> Error err)
               (Ok(parentAccount, []))
               transferCommands

         Some validations)
