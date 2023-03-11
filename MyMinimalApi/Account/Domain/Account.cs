using LanguageExt;
using System.Collections.Immutable;

using Lib.Types;

namespace Account.Domain;

using StateTransitionResult = Validation<Err, (Event Event, AccountState NewState)>;

public enum AccountStatus {
   Active,
   Frozen,
   Closed
}

public sealed record AccountState(
   Guid EntityId,
   string Currency,
   AccountStatus Status = AccountStatus.Active,
   decimal Balance = 0,
   decimal AllowedOverdraft = 0
);

public static class Account {
   public static string StreamName(Guid id) => $"accounts_{id}";

   public static ImmutableDictionary<string, Type> EventTypeMapping =>
      new Dictionary<string, Type> {
         { nameof(CreatedAccount), typeof(CreatedAccount) },
         { nameof(DebitedTransfer), typeof(DebitedTransfer) },
         { nameof(DebitedAccount), typeof(DebitedAccount) },
         { nameof(DepositedCash), typeof(DepositedCash)},
         { nameof(FrozeAccount), typeof(FrozeAccount)}
      }
      .ToImmutableDictionary();

   public static StateTransitionResult StateTransition(
      this AccountState state,
      Command cmd
   ) =>
      cmd switch {
         TransferCmd transfer => state.Transfer(transfer),
         DebitCmd debit => state.Debit(debit),
         DepositCashCmd deposit => state.Deposit(deposit),
         FreezeAccountCmd freeze => state.Freeze(freeze)
      };

   public static StateTransitionResult Transfer(
      this AccountState state,
      TransferCmd cmd
   ) {
      if (state.Status != AccountStatus.Active)
         return Errors.AccountNotActive;

      if (state.Balance - cmd.Amount < state.AllowedOverdraft)
         return Errors.InsufficientBalance;

      var evt = cmd.ToEvent();
      var newState = state.Apply(evt);

      return (evt, newState);
   }

   public static StateTransitionResult Debit(
      this AccountState state,
      DebitCmd cmd
   ) {
      if (state.Status != AccountStatus.Active)
         return Errors.AccountNotActive;

      if (state.Balance - cmd.Amount < state.AllowedOverdraft)
         return Errors.InsufficientBalance;

      var evt = cmd.ToEvent();
      var newState = state.Apply(evt);

      return (evt, newState);
   }

   public static StateTransitionResult Deposit(
      this AccountState state,
      DepositCashCmd cmd
   ) {
      if (state.Status != AccountStatus.Active)
         return Errors.AccountNotActive;

      if (cmd.Amount <= 0)
         return Errors.InvalidDepositAmount;

      var evt = cmd.ToEvent();
      var newState = state.Apply(evt);


      return (evt, newState);
   }

   public static StateTransitionResult Freeze(
      this AccountState state,
      FreezeAccountCmd cmd
   ) {
      if (state.Status == AccountStatus.Frozen)
         return Errors.AccountNotActive;

      var evt = cmd.ToEvent();
      var newState = state.Apply(evt);

      return (evt, newState);
   }

   public static AccountState Create(CreatedAccount evt) =>
      new AccountState(
         EntityId: evt.EntityId,
         Currency: evt.Currency,
         Status: AccountStatus.Active,
         Balance: 0
      );

   public static AccountState Apply(this AccountState acc, object evt) =>
      evt switch {
         DepositedCash e
            => acc with { Balance = acc.Balance + e.DepositedAmount },

         DebitedTransfer e
            => acc with { Balance = acc.Balance - e.DebitedAmount },

         DebitedAccount e
            => acc with { Balance = acc.Balance - e.DebitedAmount },

         FrozeAccount _
            => acc with { Status = AccountStatus.Frozen },

         _ => throw new InvalidOperationException()
      };
}