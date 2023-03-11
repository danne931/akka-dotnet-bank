using EventStore.Client;
using ES = Lib.Persistence.EventStoreManager;
using System.Collections.Immutable;
using LanguageExt;

using Lib;
using static Lib.Route.Response;
using Account.API;
using Account.Domain;

namespace Account.Routes;

public static class AccountRoutes {
   public static class Path {
      public const string Base = "/accounts";
      public const string Deposit = $"{Base}/deposit";
      public const string Debit = $"{Base}/debit";
      public const string Transfer = $"{Base}/transfer";
      public const string FreezeAccount = $"{Base}/freeze";
   }

   public static void Configure(WebApplicationBuilder builder) {
      builder.Services.AddSingleton(Account.Domain.Account.EventTypeMapping);

      builder.Services.AddSingleton<Validator<TransferCmd>>(
         Validators.TransferValidation(() => DateTime.UtcNow.Date));

      builder.Services.AddSingleton<Validator<CreateAccountCmd>>(
         Validators.AccountInitValidation());

      var esClient = ES.Connect();
      Echo.ProcessConfig.initialise();

      builder.Services.AddSingleton<AccountRegistry>(
         new AccountRegistry(
            loadAccount: id => AccountAPI.GetAccount(
               esClient,
               id,
               Account.Domain.Account.EventTypeMapping
            ),
            saveAndPublish: evt => ES.SaveAndPublish(
               esClient,
               Account.Domain.Account.EventTypeMapping,
               Account.Domain.Account.StreamName(evt.EntityId),
               evt
            )
         )
      );

      builder.Services.AddSingleton<EventStoreClient>(esClient);
   }

   public static void Start(WebApplication app) {
      app.MapGet(Path.Base + "/{id}", GetAccount);
      app.MapGet(Path.Base + "/events/{id}", GetAccountEvents);

      app.MapPost(Path.Base, CreateAccount);
      app.MapPost(Path.Transfer, Transfer);
      app.MapPost(Path.Deposit, Deposit);
      app.MapPost(Path.Debit, Debit);
      app.MapPost(Path.FreezeAccount, FreezeAccount);
      //.WithParameterValidation();

      app.MapPost("/echo", TestEchoProcess);
   }

   // QUERY

   static Task<IResult> TestEchoProcess(
      EchoCmd cmd
   )
   => AccountAPI.TestEchoProcess(cmd).Unwrap<EchoCmd>();

   static Task<IResult> GetAccountEvents(
      Guid id,
      EventStoreClient es,
      ImmutableDictionary<string, Type> mapping
   )
   => AccountAPI.GetAccountEvents(es, id, mapping).Unwrap<Lst<object>>();

   static Task<IResult> GetAccount(
      Guid id,
      EventStoreClient es,
      ImmutableDictionary<string, Type> mapping
   )
   => AccountAPI.GetAccount(es, id, mapping).Unwrap<AccountState>();

   // COMMAND

   static Task<IResult> CreateAccount(
      CreateAccountCmd cmd,
      EventStoreClient es,
      Validator<CreateAccountCmd> validate
   )
   => AccountAPI.Create(es, validate, cmd).Unwrap<Guid>();

   public static Task<IResult> Transfer(
      TransferCmd cmd,
      Validator<TransferCmd> validate,
      AccountRegistry accounts
   )
   => AccountAPI
      .ProcessCommand<TransferCmd>(cmd, accounts, validate)
      .Unwrap<Unit>();

   static Task<IResult> Deposit(
      DepositCashCmd cmd,
      AccountRegistry accounts
   )
   => AccountAPI
      .ProcessCommand<DepositCashCmd>(cmd, accounts)
      .Unwrap<Unit>();

   static Task<IResult> Debit(
      DebitCmd cmd,
      AccountRegistry accounts
   )
   => AccountAPI
      .ProcessCommand<DebitCmd>(cmd, accounts)
      .Unwrap<Unit>();

   static Task<IResult> FreezeAccount(
      FreezeAccountCmd cmd,
      AccountRegistry accounts
   )
   => AccountAPI
      .ProcessCommand<FreezeAccountCmd>(cmd, accounts)
      .Unwrap<Unit>();
}