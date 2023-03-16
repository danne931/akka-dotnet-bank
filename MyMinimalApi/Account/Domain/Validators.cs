using LanguageExt;
using static LanguageExt.Prelude;
using EventStore.Client;
using OneOf;
using ES = Lib.Persistence.EventStoreManager;

namespace Account.Domain;

public delegate Validation<Err, T> Validator<T>(T t);
public delegate Task<Validation<Err, T>> AsyncValidator<T>(T t);

public static class Validators {
   // TODO: Change to a dictionary or something else for better perf?
   public static Lst<string> Currencies => List(
      "USD",
      "EUR",
      "THB",
      "VND"
   );

   public static Validator<TransferCmd>
      TransferValidation(Func<DateTime> clock) =>
      cmd => cmd.Date.Date < clock().Date
         ? Fail<Err, TransferCmd>(Errors.TransferDateIsPast)
         : Success<Err, TransferCmd>(cmd);


   public static Validator<CreateAccountCmd> AccountInitValidation() =>
      cmd => {
         // TODO: handle varying currency codes
         if (cmd.Balance < 100)
            return Fail<Err, CreateAccountCmd>(Errors.InvalidStartBalance(cmd.Balance));
         if (!Currencies.Contains(cmd.Currency))
            return Fail<Err, CreateAccountCmd>(Errors.InvalidCurrency(cmd.Currency));

         return Success<Err, CreateAccountCmd>(cmd);
      };

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
               var accountExists = await ES.Exists(es, Account.StreamName(new Guid(cmd.Identification)));
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

   public static Validator<T> Pass<T>() => Success<Err, T>;
}