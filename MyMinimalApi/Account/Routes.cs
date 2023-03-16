using EventStore.Client;
using ES = Lib.Persistence.EventStoreManager;
using System.Collections.Immutable;
using LanguageExt;
using Echo;

using Lib;
using static Lib.Route.Response;
using Account.API;
using Account.Domain;

namespace Account.Routes;

public static class AccountRoutes {
   public static class Path {
      public const string Base = "/accounts";
      public const string Diagnostic = "/diagnostic";
      public const string Deposit = $"{Base}/deposit";
      public const string Debit = $"{Base}/debit";
      public const string Transfer = $"{Base}/transfer";
      public const string LockCard = $"{Base}/lock";
      public const string UnlockCard = $"{Base}/unlock";
      public const string TransferRecipient = $"{Base}/transfer-recipient";
   }

   public static void Configure(WebApplicationBuilder builder) {
      builder.Services.AddSingleton(Account.Domain.Account.EventTypeMapping);

      builder.Services.AddSingleton<Validator<TransferCmd>>(
         Validators.TransferValidation(() => DateTime.UtcNow.Date));

      var esClient = ES.Connect();
      ProcessConfig.initialise();
      Process.DeadLetters()
         .Observe<DeadLetter>()
         .Subscribe(Console.WriteLine);

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
      app.MapGet(Path.Diagnostic + "/events/{id}", GetAccountEvents);
      app.MapDelete(Path.Diagnostic + "/events/{id}", SoftDeleteEvents);

      app.MapPost(Path.Base, CreateAccount);
      app.MapPost(Path.Deposit, Deposit);
      app.MapPost(Path.Debit, Debit);
      app.MapPost(Path.LockCard, LockCard);
      app.MapPost(Path.UnlockCard, UnlockCard);
      app.MapPost(Path.Transfer, Transfer);
      app.MapPost(Path.TransferRecipient, RegisterTransferRecipient);

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

   static Task<IResult> SoftDeleteEvents(
      Guid id,
      EventStoreClient es,
      AccountRegistry accounts
   )
   => AccountAPI.SoftDeleteEvents(accounts, es, id).Unwrap<Unit>();

   static Task<IResult> GetAccount(
      Guid id,
      EventStoreClient es,
      ImmutableDictionary<string, Type> mapping
   )
   => AccountAPI.GetAccount(es, id, mapping).Unwrap<AccountState>();

   // COMMAND

   static Task<IResult> CreateAccount(
      CreateAccountCmd cmd,
      EventStoreClient es
   )
   => AccountAPI
      .Create(es, Validators.AccountInitValidation(), cmd)
      .Unwrap<Guid>();

   static Task<IResult> RegisterTransferRecipient(
      RegisterTransferRecipientCmd cmd,
      EventStoreClient es,
      AccountRegistry accounts
   ) =>
      AccountAPI.ProcessCommand<RegisterTransferRecipientCmd>(
         cmd,
         accounts,
         asyncValidate: Validators.RegisterTransferRecipient(es)
      )
      .Unwrap<Unit>();

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

   static Task<IResult> LockCard(
      LockCardCmd cmd,
      AccountRegistry accounts
   )
   => AccountAPI
      .ProcessCommand<LockCardCmd>(cmd, accounts)
      .Unwrap<Unit>();

   static Task<IResult> UnlockCard(
      UnlockCardCmd cmd,
      AccountRegistry accounts
   )
   => AccountAPI
      .ProcessCommand<UnlockCardCmd>(cmd, accounts)
      .Unwrap<Unit>();
}