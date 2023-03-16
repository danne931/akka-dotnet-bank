using LanguageExt;
using OneOf;
using System.Collections.Immutable;

using Lib.Types;
using Bank.Transfer.Domain;

namespace Bank.Account.Domain;

using StateTransitionResult = Validation<Err, (Event Event, AccountState NewState)>;
using TransferRecipient = OneOf<
   RegisteredInternalTransferRecipient,
   RegisteredDomesticTransferRecipient,
   RegisteredInternationalTransferRecipient
>;

public static class Account {
   public static string StreamName(Guid id) => $"accounts_{id}";

   public static ImmutableDictionary<string, Type> EventTypeMapping =>
      new Dictionary<string, Type> {
         { nameof(CreatedAccount), typeof(CreatedAccount) },
         { nameof(DebitedTransfer), typeof(DebitedTransfer) },
         { nameof(DebitedAccount), typeof(DebitedAccount) },
         { nameof(DepositedCash), typeof(DepositedCash)},
         { nameof(LockedCard), typeof(LockedCard)},
         { nameof(UnlockedCard), typeof(UnlockedCard)},
         { nameof(RegisteredInternalTransferRecipient), typeof(RegisteredInternalTransferRecipient) },
         { nameof(RegisteredDomesticTransferRecipient), typeof(RegisteredDomesticTransferRecipient) },
         { nameof(RegisteredInternationalTransferRecipient), typeof(RegisteredInternationalTransferRecipient) }
      }
      .ToImmutableDictionary();

   public static AccountState Apply(this AccountState acc, object evt) =>
      evt switch {
         DepositedCash e
            => acc with { Balance = acc.Balance + e.DepositedAmount },

         DebitedTransfer e
            => acc with { Balance = acc.Balance - e.DebitedAmount },

         DebitedAccount e
            => acc with { Balance = acc.Balance - e.DebitedAmount },

         LockedCard _
            => acc with { Status = AccountStatus.ActiveWithLockedCard },

         UnlockedCard _
            => acc with { Status = AccountStatus.Active },

         RegisteredInternalTransferRecipient e => acc with {
            TransferRecipients = acc.TransferRecipients.AddOrUpdate(e.AccountNumber, e)
         },

         RegisteredDomesticTransferRecipient e => acc with {
            TransferRecipients = acc
               .TransferRecipients
               .AddOrUpdate($"{e.RoutingNumber}_{e.AccountNumber}", e)
         },

         RegisteredInternationalTransferRecipient e => acc with {
            TransferRecipients = acc.TransferRecipients.AddOrUpdate(e.Identification, e)
         },

         _ => acc
      };

   public static StateTransitionResult StateTransition(
      this AccountState state,
      Command command
   ) =>
      command switch {
         TransferCmd cmd => state.Transfer(cmd),
         DebitCmd cmd => state.Debit(cmd),
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
      if (state.TransferRecipients.Find(cmd.Identification).IsSome)
         return new Err($"Transfer recipient {cmd.Identification} " +
            $"already added to account: {cmd.EntityId}");

      var evt = cmd.ToEvent().Unwrap();
      return (evt, state.Apply(evt));
   }

   public static AccountState Create(CreatedAccount evt) =>
      new AccountState(
         EntityId: evt.EntityId,
         Currency: evt.Currency,
         Balance: evt.Balance
      );
}

public sealed record AccountState(
   Guid EntityId,
   string Currency,
   AccountStatus Status = AccountStatus.Active,
   decimal Balance = 0,
   decimal AllowedOverdraft = 0,
   Map<string, TransferRecipient> TransferRecipients = default
);

public enum AccountStatus {
   Active,
   ActiveWithLockedCard,
   Closed
}