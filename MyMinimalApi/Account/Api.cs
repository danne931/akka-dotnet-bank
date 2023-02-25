using LanguageExt;
using static LanguageExt.Prelude;
using static LanguageExt.List;
using System.Collections.Immutable;
using EventStore.Client;

using Lib;
using Lib.Types;
using AD = Account.Domain.Account;
using ES = Lib.Persistence.EventStoreManager;
using Account.Domain;

namespace Account.API;

public static class AccountAPI {
   public static Task<Option<EchoCmd>> TestEchoProcess(EchoCmd cmd) {
      return TaskSucc(Some(cmd));
   }

   //public static TryOptionAsync<Validation<InvalidCurrencyError, Guid>> Create(
   public static Task<TryOption<Guid>> Create(
      EventStoreClient client,
      Validator<CreateAccountCmd> validate,
      CreateAccountCmd command
   ) {
      /*
      return TryOptionAsync(validate(command).Map(async evt => {
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
      .Sequence());
      */

      return validate(command).Map(async cmd => {
         var evt = cmd.ToEvent();
         await ES.SaveAndPublish(
            client,
            AD.EventTypeMapping,
            AD.StreamName(evt.EntityId),
            evt,
            // Create event stream only if it doesn't already exist.
            StreamState.NoStream
         );
         return evt.EntityId;
      })
      .ToTryOption()
      .Sequence();
   }

   public static Task<Option<Lst<object>>> GetAccountEvents(
   //public static TryOptionAsync<Lst<object>> GetAccountEvents(
      EventStoreClient client,
      Guid accountId,
      ImmutableDictionary<string, Type> mapping
   )
   => ES.ReadStream(client, AD.StreamName(accountId), mapping);

   public static async Task<Option<AccountState>> GetAccount(
      EventStoreClient client,
      Guid accountId,
      ImmutableDictionary<string, Type> mapping
   ) {
      var res = await GetAccountEvents(client, accountId, mapping);

      return res.Map(events =>
         fold(
            tail(events),
            AD.Create((CreatedAccount) head(events)),
            (state, evt) => state.Apply(evt)
         )
      );
   }

   public static TryAsync<Validation<Err, Unit>> ProcessCommand<T>(
      T command,
      AccountRegistry accounts,
      Validator<T>? validate = null
   )
   where T : Command
   {
      if (validate is null)
         validate = Validators.Pass<T>();

      var getAccountVal = (Command cmd) =>
         accounts
            .Lookup(cmd.EntityId)
            .Map(opt => opt.ToValidation(Errors.UnknownAccountId(cmd.EntityId)));

      var res =
         from cmd in TaskSucc(validate(command))
         from acc in getAccountVal(cmd)
         select acc.SyncStateChange(cmd);

      return TryAsync(res);
   }
}