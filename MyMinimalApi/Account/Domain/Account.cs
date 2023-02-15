using LanguageExt;
using static LanguageExt.Prelude;
using static LanguageExt.List;
//using LaYumba.Functional;
//using static LaYumba.Functional.F;
using System.Collections.Immutable;
using EventStore.Client;

using Lib;
using Account.Domain.Commands;
using Account.Domain.Events;
using ES = Lib.Persistence.EventStoreManager;

namespace Account.Domain;

/*
var ty = AppDomain.CurrentDomain
   .GetAssemblies()
   .SelectMany(a =>
      a.GetTypes().Where(x =>
         x.FullName == "DebitedTransfer" || x.Name == "DebitedTransfer")
   )
   .FirstOrDefault();
*/

public static class API
{
   //public static Task<Validation<InvalidCurrencyError, Guid>> Create(
   public static Task<TryOption<Guid>> Create(
      EventStoreClient client,
      CurrencyValidator validate,
      CreateAccountCmd command
   )
   {
      /*
      var evt = validate(command);
      await ES.SaveAndPublish(
         client,
         Account.EventTypeMapping,
         Account.StreamName(evt.EntityId),
         evt,
         // Create event stream only if it doesn't already exist.
         StreamState.NoStream
      );
      */
      /* 
      var save = (CreatedAccount evt) => TryAsync(ES.SaveAndPublish(
         client,
         Account.EventTypeMapping,
         Account.StreamName(evt.EntityId),
         evt,
         // Create event stream only if it doesn't already exist.
         StreamState.NoStream
      ).Map(() => evt.EntityId));

      return validate(command).Match(
         Succ: evt => evt.EntityId,
         Fail: e => e
      );
      */
      
      return validate(command).Map(async evt => {
         //var evt = cmd.ToEvent();
         await ES.SaveAndPublish(
            client,
            Account.EventTypeMapping,
            Account.StreamName(evt.EntityId),
            evt,
            // Create event stream only if it doesn't already exist.
            StreamState.NoStream
         );
         return evt.EntityId;
      })
      .ToTryOption()
      .Sequence();
      
      //.Traverse
      //.MapFail(e => e)
      //.Traverse(evt => evt.EntityId);
   }

   public static Task<Option<Lst<object>>> GetAccountEvents(
   //public static TryOptionAsync<Lst<object>> GetAccountEvents(
      EventStoreClient client,
      Guid accountId,
      ImmutableDictionary<string, Type> mapping
   )
   => ES.ReadStream(client, Account.StreamName(accountId), mapping);

   public static async Task<LanguageExt.Option<AccountState>> GetAccount(
      EventStoreClient client,
      Guid accountId,
      ImmutableDictionary<string, Type> mapping
   )
   {
      /*
      var agg =
         from events in GetAccountEvents(client, accountId, mapping)
         select Account.Aggregate(events);
      return agg;
      */

      var res = await GetAccountEvents(client, accountId, mapping);

      return res.Map(events =>
         fold(
            tail(events),
            Account.Create((CreatedAccount) head(events)),
            (state, evt) => state.Apply(evt)
         )
      );
   }

/*
   public static Task<Validation<AccountState>> ProcessCommand<T>(
      T command,
      AccountRegistry accounts,
      Validator<T>? validate = null
   )
   where T : Command
   {
      if (validate is null) validate = Valid;

      var getAccountVal = (Guid id) =>
         accounts
            .Lookup(id)
            .Map(opt => opt.ToValidation(Errors.UnknownAccountId(id)));

      var outcome =
         from cmd in Async(validate(command))
         from acc in getAccountVal(cmd.EntityId)
         from result in acc.SyncStateChange(cmd)
         select result.NewState;

      return outcome;
   }
   */
}

public static class Account
{
   public static string StreamName(Guid id) => $"accounts_{id}";

   public static ImmutableDictionary<string, Type> EventTypeMapping
      => new Dictionary<string, Type>
      {
         { nameof(CreatedAccount), typeof(CreatedAccount) },
         { nameof(DebitedTransfer), typeof(DebitedTransfer) },
         { nameof(DepositedCash), typeof(DepositedCash)},
         { nameof(FrozeAccount), typeof(FrozeAccount)}
      }
      .ToImmutableDictionary();
/*
   public static Validation<(Event Event, AccountState NewState)>
      StateTransition(this AccountState state, Command cmd) =>
         cmd switch
         {
            TransferCmd transfer => state.Debit(transfer),
            DepositCashCmd deposit => state.Deposit(deposit),
            FreezeAccountCmd freeze => state.Freeze(freeze)
         };

   public static Validation<(Event Event, AccountState NewState)> Debit(
      this AccountState state,
      TransferCmd cmd
   )
   {
      if (state.Status != AccountStatus.Active)
         return Errors.AccountNotActive;

      if (state.Balance - cmd.Amount < state.AllowedOverdraft)
         return Errors.InsufficientBalance;

      var evt = cmd.ToEvent();
      var newState = state.Apply(evt);

      return (evt, newState);
   }

   public static Validation<(Event evt, AccountState newState)> Deposit(
      this AccountState state,
      DepositCashCmd cmd
   )
   {
      if (state.Status != AccountStatus.Active)
         return Errors.AccountNotActive;

      /*
      if (cmd.Amount <= 0)
         return Errors.InvalidDepositAmount;

      var evt = cmd.ToEvent();
      var newState = state.Apply(evt);


      return (evt, newState);
   }

   public static Validation<(Event evt, AccountState newState)> Freeze(
      this AccountState state,
      FreezeAccountCmd cmd
   )
   {
      if (state.Status == AccountStatus.Frozen)
         return Errors.AccountNotActive;

      var evt = cmd.ToEvent();
      var newState = state.Apply(evt);

      return (evt, newState);
   }
   */

   // apply events

   public static AccountState Create(CreatedAccount evt)
      => new AccountState(
            EntityId: evt.EntityId,
            Currency: evt.Currency,
            Status: AccountStatus.Active,
            Balance: 0
         );

   public static AccountState Apply(this AccountState acc, object evt)
   => evt switch
   {
      DepositedCash e
         => acc with { Balance = acc.Balance + e.DepositedAmount },

      DebitedTransfer e
         => acc with { Balance = acc.Balance - e.DebitedAmount },

      FrozeAccount _
         => acc with { Status = AccountStatus.Frozen },

      _ => throw new InvalidOperationException()
   };

/*
   public static LanguageExt.Option<AccountState> Aggregate(Lst<object> history)
      => match(history,
         Some: (otherEvents) =>
            fold(otherEvents,
               Account.Create((CreatedAccount) head(otherEvents)),
               (state, evt) => state.Apply(evt)),
         None: () => null);//new List<object>() {});
         //() => None,
         /*
         (createdEvent, otherEvents) => Some(
            otherEvents.Aggregate(
               Account.Create((CreatedAccount) createdEvent),
               func: (state, evt) => state.Apply(evt))));
*/
}


public enum AccountStatus
{ Requested, Active, Frozen, Dormant, Closed }

public abstract record State (Guid EntityId);
/*
public interface IState<T>
{
   Validation<(Event Event, T NewState)> StateTransition(Command cmd);
}
*/

public sealed record AccountState
(
   Guid EntityId,
   //CurrencyCode Currency,
   string Currency,
   AccountStatus Status = AccountStatus.Requested,
   decimal Balance = 0,
   decimal AllowedOverdraft = 0
) : State(EntityId);/*, IState<AccountState>
{
   public Validation<(Event Event, AccountState NewState)> StateTransition(Command cmd) =>
      cmd switch
      {
         TransferCmd transfer => this.Debit(transfer),
         DepositCashCmd deposit => this.Deposit(deposit),
         FreezeAccountCmd freeze => this.Freeze(freeze)
      };

}
*/

public struct CurrencyCode
{
   string Value { get; }
   public CurrencyCode(string value) => Value = value;

   public static implicit operator string(CurrencyCode c) => c.Value;
   public static implicit operator CurrencyCode(string s) => new(s);

   public override string ToString() => Value;
}