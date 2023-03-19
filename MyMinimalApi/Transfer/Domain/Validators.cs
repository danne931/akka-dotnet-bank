using EventStore.Client;
using static LanguageExt.Prelude;

using Lib.Types;

namespace Bank.Transfer.Domain;

public static class Validators {
   public static Validator<TransferCmd>
      TransferValidation(Func<DateTime> clock) =>
      cmd => {
         if (cmd.Date.Date < clock().Date)
            return Fail<Err, TransferCmd>(TransferErr.DateIsPast);
         if (isEmpty(cmd.Recipient.Identification))
            return Fail<Err, TransferCmd>(TransferErr.InvalidDetails);

         return Success<Err, TransferCmd>(cmd);
      };

   public static AsyncValidator<RegisterTransferRecipientCmd>
      RegisterTransferRecipient(
         EventStoreClient es,
         Func<EventStoreClient, TransferRecipient, Task<bool>> RecipientExists
      ) =>
      async cmd => {
         var rec = cmd.Recipient;
         if (isEmpty(rec.LastName) || isEmpty(rec.Identification)) {
            return Fail<Err, RegisterTransferRecipientCmd>(
               TransferErr.InvalidRecipient
            );
         }
         if (rec.AccountEnvironment is RecipientAccountEnvironment.Domestic &&
            isEmpty(rec.RoutingNumber)
         ) {
            return Fail<Err, RegisterTransferRecipientCmd>(
               TransferErr.InvalidDomesticRecipient
            );
         }
         // TODO: XXX - use lenses
         if (rec.AccountEnvironment is RecipientAccountEnvironment.International &&
            isnull(rec.IdentificationStrategy)
         ) {
            return Fail<Err, RegisterTransferRecipientCmd>(
               TransferErr.InvalidInternationalRecipient
            );
         }
         // TODO: Remove Guid play
         var accountExists = await RecipientExists(es, rec);
         if (!accountExists)
            return TransferErr.RecipientNotFound(new Guid(rec.Identification));

         return Success<Err, RegisterTransferRecipientCmd>(cmd);
      };
}