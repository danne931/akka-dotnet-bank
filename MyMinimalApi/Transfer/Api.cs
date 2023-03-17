using EventStore.Client;
using LanguageExt;
using static LanguageExt.Prelude;
using ES = Lib.Persistence.EventStoreManager;
using Bank.Account.API;
using Bank.Transfer.Domain;

namespace Bank.Transfer.API;

public static class BankTransferAPI {
   public static Task<bool> RecipientExists(
      EventStoreClient es,
      TransferRecipient recipient
   ) =>
      recipient.AccountEnvironment switch {
         RecipientAccountEnvironment.Internal => RecipientExistsInternally(es, recipient),
         RecipientAccountEnvironment.Domestic => RecipientExistsDomestically(recipient),
         RecipientAccountEnvironment.International => RecipientExistsInternationally(recipient)
      };

   private static Task<bool> RecipientExistsInternally(
      EventStoreClient es,
      TransferRecipient recipient
   ) =>
      AccountAPI.Exists(es, new Guid(recipient.Identification));

   private static async Task<bool> RecipientExistsDomestically(
      TransferRecipient recipient
   ) {
      // Simulate network request to verify account in national bank
      await Task.Delay(1*sec);
      return true;
   }

   private static async Task<bool> RecipientExistsInternationally(
      TransferRecipient recipient
   ) {
      // Simulate network request to verify account in international bank
      await Task.Delay(1*sec);
      return true;
   }
}