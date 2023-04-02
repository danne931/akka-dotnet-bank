using Echo;
using EventStore.Client;

using ES = Lib.Persistence.EventStoreManager;
using Lib;
using Lib.Types;
using Bank.Transfer.API;
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

      var processIds = BankTransferAPI.StartThirdPartyTransferSystem();
      Console.WriteLine($"Started 3rd party bank transfer system {processIds}");
   }

   public static EventStoreClient StartEventStore() => ES.Connect();

   public static void InjectDependencies(
      WebApplicationBuilder builder,
      EventStoreClient esClient
   ) {
      builder.Services.AddSingleton<Validator<TransferCmd>>(
         TransferValidation(() => DateTime.UtcNow.Date));

      builder.Services.AddSingleton<AccountRegistry>(
         new AccountRegistry(
            loadAccount: AccountAPI.GetAccount(
               AccountAPI.GetAccountEvents(esClient)
            ),
            saveAndPublish: evt => AccountAPI.SaveAndPublish(
               esClient,
               evt
            )
         )
      );

      builder.Services.AddSingleton<EventStoreClient>(esClient);
   }
}