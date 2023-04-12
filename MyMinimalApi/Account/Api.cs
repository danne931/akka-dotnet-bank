using LanguageExt;
using static LanguageExt.Prelude;
using static LanguageExt.List;
using EventStore.Client;
using OneOf;
using static Echo.Process;

using Lib.Types;
using static Lib.Validators;
using ES = Lib.Persistence.EventStoreManager;
using AD = Bank.Account.Domain.Account;
using Bank.Account.Domain;
using static Bank.Account.Domain.Errors;
using Bank.Transfer.Domain;
using Bank.Transfer.Actors;
using Bank.Account.Actors;

namespace Bank.Account.API;

public static class AccountAPI {
   public static Task<Validation<Err, Guid>> Create(
      EventStoreClient client,
      AccountRegistry registry,
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
      var registerAccount = (CreatedAccount evt) =>
         Optional(AccountActor.Start(AD.Create(evt), registry))
            .ToValidation(new Err("Account registration fail"))
            .AsTask();

      return
         from cmd in TaskSucc(validate(command))
         from evt in save(cmd)
         from _ in registerAccount(evt)
         select evt.EntityId;
   }

   public static Task<Option<Lst<object>>> GetAccountEvents(
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

   /// <summary>
   /// Get all CreatedAccount events for UI demonstration purposes.
   /// Allows demonstration consumer to choose what account to process
   /// transactions on.
   /// </summary>
   public static Task<Option<Lst<object>>>
      GetAccountCreationEvents(EventStoreClient client) =>
         ES.ReadStream(
            client,
            "$et-CreatedAccount",
            AD.EventTypeMapping,
            resolveLinkTos: true
         );

   public static Task<bool> Exists(EventStoreClient es, Guid id) =>
      ES.Exists(es, AD.StreamName(id));

   public static Task<Validation<Err, Unit>> ProcessCommand<T>(
      T command,
      AccountRegistry registry,
      OneOf<Validator<T>, AsyncValidator<T>> validate
   )
   where T : Command
   {
      var validation = validate.Match(
         validate => TaskSucc(validate(command)),
         asyncValidate => asyncValidate(command)
      );

      var getAccountVal = (Command cmd) =>
         registry
            .Lookup(cmd.EntityId)
            .Map(opt => opt.ToValidation(UnknownAccountId(cmd.EntityId)));

      return
         from cmd in validation
         from acc in getAccountVal(cmd)
         select AccountActor.SyncStateChange(cmd);
   }

   public static Task<Unit> SoftDeleteEvents(
      EventStoreClient client,
      Guid accountId
   ) {
      AccountActor.Delete(accountId);
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

      switch (evt) {
         case RegisteredInternalTransferRecipient:
         case RegisteredDomesticTransferRecipient:
         case RegisteredInternationalTransferRecipient:
            TransferRecipientActor.Start();
            break;
         case DebitedTransfer:
            tellChild(TransferRecipientActor.ActorName, (DebitedTransfer) evt);
            break;
      }

      return unit;
   }
}