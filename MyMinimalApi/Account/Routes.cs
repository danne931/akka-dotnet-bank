/* TODO:
 *   X 1. Add persistence (geteventstore.com)
 *   X 2. Implement loadAccount events with EventStore
 *   X 3. Implement saveAndPublish with EventStore
 *   X 4. Add Create, DepositedCash, & FreezeAccount
 *   5. Replace Agent.Start & Agent.Tell with library equivalent
 *      (Orleans, Akka.NET, or github.com/louthy/echo-process)
 *   6. Replace ImmutableDictionary<Guid, AccountProcess> cache
 *      (https://learn.microsoft.com/en-us/dotnet/core/extensions/caching
 *       https://docs.redis.com/latest/rs/references/client_references/client_csharp/) 
 *   X 7. Replace LaYumba.Functional with louthy/language-ext
 *   8. Add System.ComponentModel.DataAnnotations to TransferCmd & perform
 *      parameter validation?
 *   X 9. Add diagnostic route to get all events for account
 *   X 10. Figure out how to remove stream data so can clean up improperly inserted data
 *   X 11. Slice app vertically.  Store tests alongside project.
 *   X 12. Add account id to end of stream id
 *   X 13. Handle readstream not found exception with Option
 *   X 14. Change UnwrapValidation to act on this
 *   X 15. Implement account load aggregate
 *   16. Implement event deletion (diagnostic)
 *   - 17. Make sure mapping exists at SaveAndPublish?
 *   18. Fix "The JSON value could not be converted to CurrencyCode error"
 *   X 19. Replace AccountRegistry loadAccount stub
 *   20. Account EntityId defaults to 0000-... if none supplied.  How to require
 *       non-default value from definition?
 *   X 21. Consolidate Freeze, Debit, Transfer methods into a generic ProcessCommand<T>
 *   X 22. Validate create account (currency)
 *   - 23. Move Create into generic process command
 *   X 24. Prevent CreateAccount happening more than once
 *   25. Update valid currency lookup for better perf?
 *   26. See if can make AccountRegistry agnostic to domain
 *   27. Move Agent process to Lib
 *   X 28. Fix tests not running after moving directory
 */
using EventStore.Client;
using ES = Lib.Persistence.EventStoreManager;
using System.Collections.Immutable;
using LanguageExt;

using Lib;
using static Lib.Route.Response;
using Account.Domain;
using Account.Domain.Commands;

namespace Account.Routes;

public static class AccountRoutes
{
   public static class Path
   {
      public const string Base = "/accounts";
      public const string Deposit = $"{Base}/deposit";
      public const string Transfer = $"{Base}/transfer";
      public const string FreezeAccount = $"{Base}/freeze";
   }

   public static void Configure(WebApplicationBuilder builder)
   {
      builder.Services.AddSingleton(Account.Domain.Account.EventTypeMapping);

      builder.Services.AddSingleton<Validator<TransferCmd>>(
         Validators.TransferValidation(() => DateTime.UtcNow.Date));

      builder.Services.AddSingleton<Validator<CreateAccountCmd>>(
         Validators.AccountInitValidation());

      var esClient = ES.Connect();

      builder.Services.AddSingleton<AccountRegistry>(
         new AccountRegistry(
            //Cache: ImmutableDictionary<Guid, AccountProcess>.Empty,
            loadAccount: id => API.GetAccount(
               esClient,
               id,
               Account.Domain.Account.EventTypeMapping
            )
            .Map(opt => opt.Match(
               Some: LaYumba.Functional.F.Some,
               None: LaYumba.Functional.F.None
            )),
            //stateTransition: Account.Domain.Account.StateTransition,
            saveAndPublish: evt => {
               Console.WriteLine("SAVE AND PUBLISH: " + evt.GetType());
               
               return ES.SaveAndPublish(
                  esClient,
                  Account.Domain.Account.EventTypeMapping,
                  Account.Domain.Account.StreamName(evt.EntityId),
                  evt);
            }));

      builder.Services.AddSingleton<EventStoreClient>(esClient);
   }

   public static void Start(WebApplication app)
   {
      app.MapGet(Path.Base + "/{id}", GetAccount);
      app.MapGet(Path.Base + "/events/{id}", GetAccountEvents);

      app.MapPost(Path.Base, CreateAccount);
      app.MapPost(Path.Transfer, Transfer);
      app.MapPost(Path.Deposit, Deposit);
      app.MapPost(Path.FreezeAccount, FreezeAccount);
      //.WithParameterValidation();
   }

   // QUERY

   static Task<IResult> GetAccountEvents(
      Guid id,
      EventStoreClient es,
      ImmutableDictionary<string, Type> mapping
   )
   => API.GetAccountEvents(es, id, mapping).Unwrap<Lst<object>>();

   static Task<IResult> GetAccount(
      Guid id,
      EventStoreClient es,
      ImmutableDictionary<string, Type> mapping
   )
   => API.GetAccount(es, id, mapping).Unwrap<AccountState>();

   // COMMAND

   static Task<IResult> CreateAccount(
      CreateAccountCmd cmd,
      EventStoreClient es,
      Validator<CreateAccountCmd> validate
   )
   => API.Create(es, validate, cmd).Unwrap<Guid>();

   public static Task<IResult> Transfer(
      TransferCmd cmd,
      Validator<TransferCmd> validate,
      AccountRegistry accounts
   )
   => API
      .ProcessCommand<TransferCmd>(cmd, accounts, validate)
      .Unwrap<AccountState>(newState => new { Balance = newState.Balance });

   static Task<IResult> Deposit(
      DepositCashCmd cmd,
      AccountRegistry accounts
   )
   => API
      .ProcessCommand<DepositCashCmd>(cmd, accounts)
      .Unwrap<AccountState>(newState => new { Balance = newState.Balance });

   static Task<IResult> FreezeAccount(
      FreezeAccountCmd cmd,
      AccountRegistry accounts
   )
   => API
      .ProcessCommand<FreezeAccountCmd>(cmd, accounts)
      .Unwrap<AccountState>(newState => new { Status = newState.Status });
}