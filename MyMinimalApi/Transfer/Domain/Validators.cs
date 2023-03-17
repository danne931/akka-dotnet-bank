using EventStore.Client;
using static LanguageExt.Prelude;

using Lib.Types;

namespace Bank.Transfer.Domain;

public static class Validators {
   public static Validator<TransferCmd>
      TransferValidation(Func<DateTime> clock) =>
      cmd => cmd.Date.Date < clock().Date
         ? Fail<Err, TransferCmd>(Errors.TransferDateIsPast)
         : Success<Err, TransferCmd>(cmd);

   public static AsyncValidator<RegisterTransferRecipientCmd>
      RegisterTransferRecipient(
         EventStoreClient es,
         Func<EventStoreClient, TransferRecipient, Task<bool>> RecipientExists
      ) =>
      async cmd => {
         var rec = cmd.Recipient;
         if (isEmpty(rec.LastName) || isEmpty(rec.Identification)) {
            return Fail<Err, RegisterTransferRecipientCmd>(
               new Err("LastName & Identification required.")
            );
         }
         if (rec.AccountEnvironment is RecipientAccountEnvironment.Internal &&
            isEmpty(rec.RoutingNumber)
         ) {
            return Fail<Err, RegisterTransferRecipientCmd>(
               new Err("RoutingNumber required for domestic transfers.")
            );
         }
         // TODO: XXX - use lenses
         if (rec.AccountEnvironment is RecipientAccountEnvironment.International &&
            isnull(rec.IdentificationStrategy)
         ) {
            return Fail<Err, RegisterTransferRecipientCmd>(
               new Err("IdentificationMethod required for international transfers.")
            );
         }
         // TODO: Remove Guid play
         var accountExists = await RecipientExists(es, rec);
         if (!accountExists)
            return Errors.TransferRecipientNotFound(new Guid(rec.Identification));

         return Success<Err, RegisterTransferRecipientCmd>(cmd);
      };
}