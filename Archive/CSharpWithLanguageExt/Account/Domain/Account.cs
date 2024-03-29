using LanguageExt;

using Time = Lib.Time;
using Lib.Types;
using Lib.BankTypes;
using Bank.Transfer.Domain;
using MF = Bank.MaintenanceFee.Domain.MaintenanceFee;

namespace Bank.Account.Domain;

using StateTransitionResult = Validation<Err, (Event Event, AccountState NewState)>;

public static class Account {
   public static string StreamName(Guid id) => $"accounts_{id}";

   public static Map<string, Type> EventTypeMapping = (
      (nameof(CreatedAccount), typeof(CreatedAccount)),
      (nameof(TransferPending), typeof(TransferPending)),
      (nameof(TransferApproved), typeof(TransferApproved)),
      (nameof(TransferRejected), typeof(TransferRejected)),
      (nameof(DebitedAccount), typeof(DebitedAccount)),
      (nameof(MaintenanceFeeDebited), typeof(MaintenanceFeeDebited)),
      (nameof(MaintenanceFeeSkipped), typeof(MaintenanceFeeSkipped)),
      (nameof(DailyDebitLimitUpdated), typeof(DailyDebitLimitUpdated)),
      (nameof(DepositedCash), typeof(DepositedCash)),
      (nameof(LockedCard), typeof(LockedCard)),
      (nameof(UnlockedCard), typeof(UnlockedCard)),
      (nameof(RegisteredInternalTransferRecipient), typeof(RegisteredInternalTransferRecipient)),
      (nameof(RegisteredDomesticTransferRecipient), typeof(RegisteredDomesticTransferRecipient)),
      (nameof(RegisteredInternationalTransferRecipient), typeof(RegisteredInternationalTransferRecipient))
   );

   public static AccountState Apply(this AccountState acc, object evt) =>
      evt switch {
         DepositedCash e =>
            MF.FromDeposit(
               acc with { Balance = acc.Balance + e.DepositedAmount },
               e.DepositedAmount),

         TransferPending e =>
            MF.FromDebit(acc with { Balance = acc.Balance - e.DebitedAmount }),

         TransferApproved _ => acc,

         TransferRejected e =>
            MF.FromDebitReversal(acc with { Balance = acc.Balance + e.DebitedAmount }),

         MaintenanceFeeDebited e =>
            MF.Reset(acc with { Balance = acc.Balance - e.DebitedAmount }),

         MaintenanceFeeSkipped _ => MF.Reset(acc),

         DebitedAccount e =>
            MF.FromDebit(acc with {
               Balance = acc.Balance - e.DebitedAmount,
               DailyDebitAccrued = DailyDebitAccrued(acc, e),
               LastDebitDate = e.Timestamp
            }),

         DailyDebitLimitUpdated e =>
            acc with { DailyDebitLimit = e.DebitLimit },

         LockedCard _ =>
            acc with { Status = AccountStatus.ActiveWithLockedCard },

         UnlockedCard _ =>
            acc with { Status = AccountStatus.Active },

         RegisteredInternalTransferRecipient e => acc with {
            TransferRecipients = acc.TransferRecipients.AddOrUpdate(
               e.AccountNumber,
               e.AsTransferRecipient()
            )
         },

         RegisteredDomesticTransferRecipient e => acc with {
            TransferRecipients = acc.TransferRecipients.AddOrUpdate(
               $"{e.RoutingNumber}_{e.AccountNumber}",
               e.AsTransferRecipient()
            )
         },

         RegisteredInternationalTransferRecipient e => acc with {
            TransferRecipients = acc.TransferRecipients.AddOrUpdate(
               e.Identification,
               e.AsTransferRecipient()
            )
         },

         _ => acc
      };

   public static Decimal DailyDebitAccrued(AccountState state, DebitedAccount evt) {
      // When accumulating events into AccountState aggregate...
      // -> Ignore debits older than a day
      if (!Time.IsToday(evt.Timestamp)) return 0;

      // When applying a new event to the cached AccountState & the
      // last debit event did not occur today...
      // -> Ignore the cached DailyDebitAccrued
      if (!Time.IsToday(state.LastDebitDate)) return evt.DebitedAmount;

      return state.DailyDebitAccrued + evt.DebitedAmount;
   }

   public static StateTransitionResult StateTransition(
      this AccountState state,
      Command command
   ) =>
      command switch {
         TransferCmd cmd => state.Transfer(cmd),
         ApproveTransferCmd cmd => state.ApproveTransfer(cmd),
         RejectTransferCmd cmd => state.RejectTransfer(cmd),
         DebitCmd cmd => state.Debit(cmd),
         MaintenanceFeeCmd cmd => state.MaintenanceFee(cmd),
         SkipMaintenanceFeeCmd cmd => state.SkipMaintenanceFee(cmd),
         LimitDailyDebitsCmd cmd => state.LimitDailyDebits(cmd),
         DepositCashCmd cmd => state.Deposit(cmd),
         LockCardCmd cmd => state.LockCard(cmd),
         UnlockCardCmd cmd => state.UnlockCard(cmd),
         RegisterTransferRecipientCmd cmd => state.RegisterTransferRecipient(cmd),
         _ => new Err("State Transition not implemented for command.")
      };

   public static StateTransitionResult Transfer(
      this AccountState state,
      TransferCmd cmd
   ) {
      if (state.Status == AccountStatus.Closed)
         return Errors.AccountNotActive;

      if (state.Balance - cmd.Amount < state.AllowedOverdraft)
         return Errors.InsufficientBalance;

      if (state.TransferRecipients.Find(cmd.Recipient.Identification).IsNone)
         return TransferErr.RecipientRegistrationRequired(cmd);

      var evt = cmd.ToEvent();
      return (evt, state.Apply(evt));
   }

   public static StateTransitionResult ApproveTransfer(
      this AccountState state,
      ApproveTransferCmd cmd
   ) {
      var evt = cmd.ToEvent();
      return (evt, state.Apply(evt));
   }

   public static StateTransitionResult RejectTransfer(
      this AccountState state,
      RejectTransferCmd cmd
   ) {
      var evt = cmd.ToEvent();
      return (evt, state.Apply(evt));
   }

   public static StateTransitionResult MaintenanceFee(
      this AccountState state,
      MaintenanceFeeCmd cmd
   ) {
      if (state.Status == AccountStatus.Closed)
         return Errors.AccountNotActive;
      if (state.Balance - cmd.Amount < state.AllowedOverdraft)
         return Errors.InsufficientBalance;

      var evt = cmd.ToEvent();
      return (evt, state.Apply(evt));
   }

   public static StateTransitionResult SkipMaintenanceFee(
      this AccountState state,
      SkipMaintenanceFeeCmd cmd
   ) {
      var evt = cmd.ToEvent();
      return (evt, state.Apply(evt));
   }

   public static StateTransitionResult Debit(
      this AccountState state,
      DebitCmd cmd
   ) {
      if (state.Status == AccountStatus.Closed)
         return Errors.AccountNotActive;

      if (state.Status == AccountStatus.ActiveWithLockedCard)
         return Errors.AccountCardLocked;

      if (state.Balance - cmd.Amount < state.AllowedOverdraft)
         return Errors.InsufficientBalance;

      if (state.DailyDebitLimit != -1 &&
         Time.IsToday(cmd.Timestamp) &&
         state.DailyDebitAccrued + cmd.Amount > state.DailyDebitLimit
      ) {
         return Errors.ExceededDailyDebitLimit(state.DailyDebitLimit);
      }

      var evt = cmd.ToEvent();
      return (evt, state.Apply(evt));
   }

   public static StateTransitionResult LimitDailyDebits(
      this AccountState state,
      LimitDailyDebitsCmd cmd
   ) {
      var evt = cmd.ToEvent();
      return (evt, state.Apply(evt));
   }

   public static StateTransitionResult Deposit(
      this AccountState state,
      DepositCashCmd cmd
   ) {
      if (state.Status == AccountStatus.Closed)
         return Errors.AccountNotActive;

      if (cmd.Amount <= 0)
         return Errors.InvalidDepositAmount;

      var evt = cmd.ToEvent();
      return (evt, state.Apply(evt));
   }

   public static StateTransitionResult LockCard(
      this AccountState state,
      LockCardCmd cmd
   ) {
      if (state.Status != AccountStatus.Active)
         return Errors.AccountNotActive;

      var evt = cmd.ToEvent();
      return (evt, state.Apply(evt));
   }

   public static StateTransitionResult UnlockCard(
      this AccountState state,
      UnlockCardCmd cmd
   ) {
      if (state.Status != AccountStatus.ActiveWithLockedCard)
         return new Err($"Account card already unlocked: {state.Status}");

      var evt = cmd.ToEvent();
      return (evt, state.Apply(evt));
   }

   public static StateTransitionResult RegisterTransferRecipient(
      this AccountState state,
      RegisterTransferRecipientCmd cmd
   ) {
      if (state.TransferRecipients.Find(cmd.Recipient.Identification).IsSome)
         return TransferErr.RecipientAlreadyRegistered(cmd);

      var evt = cmd.ToEvent().Unwrap();
      return (evt, state.Apply(evt));
   }

   public static AccountState Create(CreatedAccount evt) =>
      MF.Reset(new AccountState(
         FirstName: evt.FirstName,
         LastName: evt.LastName,
         EntityId: evt.EntityId,
         Currency: evt.Currency,
         Balance: evt.Balance,
         MaintenanceFeeCriteria: new()
      ));
}
