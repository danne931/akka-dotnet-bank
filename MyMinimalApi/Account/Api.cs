using LanguageExt;
using static LanguageExt.Prelude;
using static LanguageExt.List;
using EventStore.Client;
using OneOf;

using Lib.Types;
using static Lib.Validators;
using ES = Lib.Persistence.EventStoreManager;
using AD = Bank.Account.Domain.Account;
using Bank.Account.Domain;
using static Bank.Account.Domain.Errors;
using Bank.Transfer.API;
using Bank.Transfer.Domain;
using Bank.Account.Actors;

namespace Bank.Account.API;

public static class AccountAPI {
   public static TryAsync<Validation<Err, Guid>> Create(
      EventStoreClient client,
      AccountRegistry accounts,
      Validator<CreateAccountCmd> validate,
      CreateAccountCmd command
   ) {
      var save = async (CreateAccountCmd cmd) => {
         var evt = cmd.ToEvent();
         await ES.SaveAndPublish(
            client,
            AD.EventTypeMapping,
            AD.StreamName(evt.EntityId),
            evt,
            // Create event stream only if it doesn't already exist.
            StreamState.NoStream
         );
         return Pass<CreatedAccount>()(evt);
      };
      var registerAccount = (CreatedAccount evt) => {
         var account = AD.Create(evt);
         return accounts
            .Register(account)
            .ToValidation(new Err("Account registration fail"))
            .AsTask();
      };

      var res =
         from cmd in TaskSucc(validate(command))
         from evt in save(cmd)
         from _ in registerAccount(evt)
         select evt.EntityId;
      return TryAsync(res);
   }

   public static Task<Option<Lst<object>>> GetAccountEvents(
   //public static TryOptionAsync<Lst<object>> GetAccountEvents(
      EventStoreClient client,
      Guid id
   )
   => ES.ReadStream(client, AD.StreamName(id), AD.EventTypeMapping);

   public static async Task<Option<AccountState>> GetAccount(
      Func<Guid, Task<Option<Lst<object>>>> getAccountEvents,
      Guid accountId
   ) {
      var res = await getAccountEvents(accountId);

      return res.Map(events =>
         fold(
            tail(events),
            AD.Create((CreatedAccount) head(events)),
            (state, evt) => state.Apply(evt)
         )
      );
   }

   public static Task<bool> Exists(EventStoreClient es, Guid id) =>
      ES.Exists(es, AD.StreamName(id));

   public static TryAsync<Validation<Err, Unit>> ProcessCommand<T>(
      T command,
      AccountRegistry accounts,
      OneOf<Validator<T>, AsyncValidator<T>> validate
   )
   where T : Command
   {
      var validation = validate.Match(
         validate => TaskSucc(validate(command)),
         asyncValidate => asyncValidate(command)
      );

      var getAccountVal = (Command cmd) =>
         accounts
            .Lookup(cmd.EntityId)
            .Map(opt => opt.ToValidation(UnknownAccountId(cmd.EntityId)));

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

   public static async Task<Unit> SaveAndPublish(
      EventStoreClient esClient,
      Event evt
   ) {
      await ES.SaveAndPublish(
         esClient,
         AD.EventTypeMapping,
         AD.StreamName(evt.EntityId),
         evt
      );

      if (evt is DebitedTransfer)
         BankTransferAPI.IssueTransferToRecipient((DebitedTransfer) evt);
/*
      if (evt is CreatedAccount)
         ScheduleMaintenanceFee()
*/
      return unit;
   }
}