using LanguageExt;
using static LanguageExt.Prelude;
using Account.Domain.Commands;

namespace Account.Domain;

public delegate Validation<Err, T> Validator<T>(T t);

public static class Validators {
   // TODO: Change to a dictionary or something else for better perf?
   public static Lst<string> Currencies => List(
      "USD",
      "EUR"
   );

   public static Validator<TransferCmd>
      TransferValidation(Func<DateTime> clock) =>
      cmd => cmd.Date.Date < clock().Date
         ? Fail<Err, TransferCmd>(Errors.TransferDateIsPast)
         : Success<Err, TransferCmd>(cmd);


   public static Validator<CreateAccountCmd>
      AccountInitValidation() =>
      cmd => Currencies.Contains(cmd.Currency) 
         ? Success<Err, CreateAccountCmd>(cmd)
         : Fail<Err, CreateAccountCmd>(Errors.InvalidCurrency(cmd.Currency));

   public static Validator<T> Pass<T>() => Success<Err, T>;
}