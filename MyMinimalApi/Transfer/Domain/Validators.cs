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
         if (isEmpty(cmd.LastName) || isEmpty(cmd.Identification)) {
            return Fail<Err, RegisterTransferRecipientCmd>(
               new Err("LastName & Identification required.")
            );
         }

         switch (cmd.AccountEnvironment) {
            case RecipientAccountEnvironment.Internal:
               // TODO: Remove Guid play
               var accountExists = await ES.Exists(es, StreamName(new Guid(cmd.Identification)));
               if (!accountExists)
                  return Errors.TransferRecipientNotFound(new Guid(cmd.Identification));
               break;

            case RecipientAccountEnvironment.Domestic:
               if (isEmpty(cmd.RoutingNumber))
                  return Fail<Err, RegisterTransferRecipientCmd>(
                     new Err("RoutingNumber required for domestic transfers.")
                  );

               // Simulate network request to verify account in national bank
               await Task.Delay(1*sec);
               break;

            case RecipientAccountEnvironment.International:
               // TODO: XXX - use lenses
               if (isnull(cmd.IdentificationStrategy))
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