using LanguageExt;
using static LanguageExt.Prelude;
using static LanguageExt.List;
using System.Collections.Immutable;
using EventStore.Client;

using Lib;
using Lib.Types;
using static Lib.Validators;
using ES = Lib.Persistence.EventStoreManager;
using AD = Bank.Account.Domain.Account;
using Bank.Account.Domain;

namespace Bank.Account.API;

public static class AccountAPI {
   public static Task<Option<EchoCmd>> TestEchoProcess(EchoCmd cmd) {
      return TaskSucc(Some(cmd));
   }

   public static TryAsync<Validation<Err, Guid>> Create(
      EventStoreClient client,
      Validator<CreateAccountCmd> validate,
      CreateAccountCmd command
   ) {
      return TryAsync(validate(command).Map(async cmd => {
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
      .Sequence());
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
      Validator<T>? validate = null,
      AsyncValidator<T>? asyncValidate = null
   )
   where T : Command
   {
      if (validate is null && asyncValidate is null)
         validate = Pass<T>();

      var validation = asyncValidate != null
         ? asyncValidate(command)
         : TaskSucc(validate(command));

      var getAccountVal = (Command cmd) =>
         accounts
            .Lookup(cmd.EntityId)
            .Map(opt => opt.ToValidation(Errors.UnknownAccountId(cmd.EntityId)));

      var res =
         from cmd in validation
         from acc in getAccountVal(cmd)
         select acc.SyncStateChange(cmd);

      return TryAsync(res);
   }

   public static Task<Unit> SoftDeleteEvents(
      AccountRegistry accounts,
      EventStoreClient client,
      Guid accountId
   ) {
      accounts.Delete(accountId);
      return ES.SoftDelete(client, AD.StreamName(accountId));
   }
}