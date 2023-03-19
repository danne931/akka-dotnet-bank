using Echo;
using static Echo.Process;
using EventStore.Client;
using LanguageExt;
using static LanguageExt.Prelude;
using Lib;
using Lib.Types;
using static Lib.Validators;
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

   public static TryAsync<Validation<Err, Unit>> IssueTransferToRecipient(
      TransferCmd cmd,
      AccountRegistry accounts
   ) {
      var recipient = cmd.Recipient;
      if (recipient.AccountEnvironment is RecipientAccountEnvironment.Internal) {
         return AccountAPI.ProcessCommand<DepositCashCmd>(
            new DepositCashCmd(
               new Guid(cmd.Recipient.Identification),
               cmd.Date,
               cmd.Amount
            ),
            accounts,
            Pass<DepositCashCmd>()
         );
      }
      return TryAsync(ProcessThirdPartyBankTransfer(cmd));
   }

   public static IEnumerable<ProcessId>
      StartThirdPartyTransferSystem() =>
         Process.spawnMany<TransferCmd>(
            10,
            ThirdPartyBankTransferProcessName,
            async cmd => {
               Console.WriteLine("Issuing 3rd party bank transfer");
               await ThirdPartyBankTransfer(cmd);
            }
         );

   public static Validation<Err, Unit> ProcessThirdPartyBankTransfer(TransferCmd cmd) {
      Console.WriteLine("Process 3rd party bank transfer");

      var pid = find(ThirdPartyBankTransferProcessName);
      Console.WriteLine("Found PID " + pid);
      tell(pid, cmd);
      return Pass<Unit>()(unit);
   }

   public static async Task<Unit> ThirdPartyBankTransfer(TransferCmd cmd) =>
      cmd.Recipient.AccountEnvironment switch {
         RecipientAccountEnvironment.Domestic =>
            await DomesticTransfer(cmd),
         RecipientAccountEnvironment.International =>
            await InternationalTransfer(cmd),
         _ =>
            throw new Err("Third party transfer requires a " +
                           "domestic or international account.")
      };

   private static async Task<Unit> DomesticTransfer(TransferCmd cmd) {
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

   private static async Task<Unit> InternationalTransfer(TransferCmd cmd) {
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