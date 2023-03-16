using Echo;
using EventStore.Client;

using ES = Lib.Persistence.EventStoreManager;
using Lib;
using Lib.Types;
using Bank.Transfer.Domain;
using static Bank.Transfer.Domain.Validators;
using Bank.Account.API;

namespace Bank;

public static class Config {
   public static void StartActorModel() {
      ProcessConfig.initialise();
      Process.DeadLetters()
         .Observe<DeadLetter>()
         .Subscribe(Console.WriteLine);
   }

   public static EventStoreClient StartEventStore() => ES.Connect();

   public static void InjectDependencies(
      WebApplicationBuilder builder,
      EventStoreClient esClient
   ) {
      builder.Services.AddSingleton(Account.Domain.Account.EventTypeMapping);

      builder.Services.AddSingleton<Validator<TransferCmd>>(
         TransferValidation(() => DateTime.UtcNow.Date));


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
}