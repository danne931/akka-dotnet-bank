using Echo;
using EventStore.Client;
using static LanguageExt.Prelude;

using ES = Lib.Persistence.EventStoreManager;
using Lib.Types;
using Bank.Transfer.Domain;
using static Bank.Transfer.Domain.Validators;
using Bank.Account.API;
using Bank.Account.Actors;

namespace Bank;

public static class Config {
   public static void StartActorModel() {
      ProcessConfig.initialise();
      Process.DeadLetters()
         .Observe<DeadLetter>()
         .Subscribe(Console.WriteLine);
   }

   public static EventStoreClient StartEventStore(WebApplicationBuilder builder)
      => ES.Connect(builder.Configuration.GetConnectionString("EventStore"));

   public static void InjectDependencies(
      WebApplicationBuilder builder,
      EventStoreClient esClient
   ) {
      builder.Services.AddSingleton<Validator<TransferCmd>>(
         TransferValidation(() => DateTime.UtcNow.Date));

      builder.Services.AddSingleton<AccountRegistry>(
         new AccountRegistry(
            loadAccount: id => AccountAPI.GetAccount(
               id => AccountAPI.GetAccountEvents(esClient, id),
               id
            ),
            saveAndPublish: evt => AccountAPI.SaveAndPublish(
               esClient,
               evt
            ),
            startChildActors: id => List(
               MaintenanceFeeActor.Start(
                  id => AccountAPI.GetAccountEvents(esClient, id),
                  //lookBackDate: () => DateTime.UtcNow.AddDays(-30),
                  //scheduledAt: () => TimeSpan.FromDays(30),
                  lookBackDate: () => DateTime.UtcNow.AddSeconds(-30),
                  scheduledAt: () => TimeSpan.FromSeconds(30),
                  id
               )
            )
         )
      );

      builder.Services.AddSingleton<EventStoreClient>(esClient);
   }
}