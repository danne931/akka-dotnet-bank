using Echo;
using static Echo.Process;
using EventStore.Client;
using LanguageExt;
using static LanguageExt.Prelude;

using Lib.Types;
using Bank.Account.API;
using Bank.Account.Domain;
using Bank.Transfer.Domain;

namespace Bank.Transfer.API;

public static class BankTransferAPI {
   public const string ThirdPartyBankTransferProcessName = "transfers_thirdpartybanks";

   public static Task<bool> RecipientExists(
      EventStoreClient es,
      TransferRecipient recipient
   ) =>
      recipient.AccountEnvironment switch {
         RecipientAccountEnvironment.Internal => RecipientExistsInternally(es, recipient),
         RecipientAccountEnvironment.Domestic => RecipientExistsDomestically(recipient),
         RecipientAccountEnvironment.International => RecipientExistsInternationally(recipient)
      };

   public static Unit IssueTransferToRecipient(DebitedTransfer evt) {
      var recipient = evt.Recipient;
      if (recipient.AccountEnvironment is RecipientAccountEnvironment.Internal) {
         tell($"@accounts_{evt.EntityId}", new DepositCashCmd(
            new Guid(evt.Recipient.Identification),
            evt.Date,
            evt.DebitedAmount
         ));
      } else
         ProcessThirdPartyBankTransfer(evt);

      return unit;
   }

   public static ProcessId StartThirdPartyTransferSystem() {
      var pid = Router.leastBusy<DebitedTransfer>(
         ThirdPartyBankTransferProcessName,
         10,
         async evt => {
            Console.WriteLine("Issuing 3rd party bank transfer: " + evt.EntityId);
            await ThirdPartyBankTransfer(evt);
         }
      );
      register(pid.Name, pid);
      return pid;
   }

   public static Unit ProcessThirdPartyBankTransfer(DebitedTransfer evt) {
      tell("@" + ThirdPartyBankTransferProcessName, evt);
      return unit;
   }

   public static async Task<Unit> ThirdPartyBankTransfer(DebitedTransfer evt) =>
      evt.Recipient.AccountEnvironment switch {
         RecipientAccountEnvironment.Domestic =>
            await DomesticTransfer(evt),
         RecipientAccountEnvironment.International =>
            await InternationalTransfer(evt),
         _ =>
            throw new Err("Third party transfer requires a " +
                           "domestic or international account.")
      };

   private static async Task<Unit> DomesticTransfer(DebitedTransfer evt) {
      // Simulate network request to send money to domestic bank
      /* 
      await DomesticTransferService.Handle(new {
         AccountNumber = cmd.Recipient.Identification,
         RoutingNumber = cmd.Recipient.RoutingNumber,
         Amount = cmd.Amount,
         Timestamp = cmd.Timestamp
      });
      */
      await Task.Delay(1*sec);
      return unit;
   }

   private static async Task<Unit> InternationalTransfer(DebitedTransfer evt) {
      // await InternationalTransferService.Handle();
      // Simulate network request to verify account in international bank
      await Task.Delay(1*sec);
      return unit;
   }

   private static Task<bool> RecipientExistsInternally(
      EventStoreClient es,
      TransferRecipient recipient
   ) =>
      AccountAPI.Exists(es, new Guid(recipient.Identification));

   private static async Task<bool> RecipientExistsDomestically(
      TransferRecipient recipient
   ) {
      // Simulate network request to verify account in domestic bank
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