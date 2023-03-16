using EventStore.Client;
using static LanguageExt.Prelude;

using ES = Lib.Persistence.EventStoreManager;
using Lib.Types;
using static Bank.Account.Domain.Account;

namespace Bank.Transfer.Domain;

public static class Validators {
   public static Validator<TransferCmd>
      TransferValidation(Func<DateTime> clock) =>
      cmd => cmd.Date.Date < clock().Date
         ? Fail<Err, TransferCmd>(Errors.TransferDateIsPast)
         : Success<Err, TransferCmd>(cmd);

   public static AsyncValidator<RegisterTransferRecipientCmd>
      RegisterTransferRecipient(EventStoreClient es) =>
      async cmd => {
         var rec = cmd.Recipient;
         if (isEmpty(rec.LastName) || isEmpty(rec.Identification)) {
            return Fail<Err, RegisterTransferRecipientCmd>(
               new Err("LastName & Identification required.")
            );
         }

         switch (cmd.Recipient.AccountEnvironment) {
            case RecipientAccountEnvironment.Internal:
               // TODO: Remove Guid play
               var accountExists = await ES.Exists(es, StreamName(new Guid(rec.Identification)));
               if (!accountExists)
                  return Errors.TransferRecipientNotFound(new Guid(rec.Identification));
               break;

            case RecipientAccountEnvironment.Domestic:
               if (isEmpty(rec.RoutingNumber))
                  return Fail<Err, RegisterTransferRecipientCmd>(
                     new Err("RoutingNumber required for domestic transfers.")
                  );

               // Simulate network request to verify account in national bank
               await Task.Delay(1*sec);
               break;

            case RecipientAccountEnvironment.International:
               // TODO: XXX - use lenses
               if (isnull(rec.IdentificationStrategy))
                  return Fail<Err, RegisterTransferRecipientCmd>(
                     new Err("IdentificationMethod required for international transfers.")
                  );

               // Simulate network request to verify account in international bank
               await Task.Delay(1*sec);
               break;

            default:
               return Fail<Err, RegisterTransferRecipientCmd>(
                  new Err("Unknown AccountEnvironment.  Cannot verify recipient.")
               );
         }

         return Success<Err, RegisterTransferRecipientCmd>(cmd);
      };
}