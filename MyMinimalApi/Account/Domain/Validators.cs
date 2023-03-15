using LanguageExt;
using static LanguageExt.Prelude;
using EventStore.Client;
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

   public static AsyncValidator<RegisterInternalTransferRecipientCmd>
      RegisterInternalTransferRecipient(EventStoreClient es) =>
      async cmd => {
         if (isEmpty(cmd.LastName) || cmd.AccountNumber.IsDefault())
            return Fail<Err, RegisterInternalTransferRecipientCmd>(
               new Err("LastName & AccountNumber required.")
            );

         var accountExists = await ES.Exists(es, Account.StreamName(cmd.AccountNumber));
         if (!accountExists)
            return Errors.TransferRecipientNotFound(cmd.AccountNumber);

         return Success<Err, RegisterInternalTransferRecipientCmd>(cmd);
      };

   public static Validator<T> Pass<T>() => Success<Err, T>;
}