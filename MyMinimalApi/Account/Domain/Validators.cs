using LanguageExt;
using static LanguageExt.Prelude;
using Account.Domain.Commands;

namespace Account.Domain;

public delegate Validation<Error, T> Validator<T>(T t);

public static class Validators
{
   // TODO: Change to a dictionary or something else for better perf?
   public static Lst<string> Currencies => List("USD", "EUR");

   public static Validator<TransferCmd>
      TransferValidation(Func<DateTime> clock) =>
      cmd => cmd.Date.Date < clock().Date
         ? Fail<Error, TransferCmd>(Errors.TransferDateIsPast(cmd.Date))
         : Success<Error, TransferCmd>(cmd);


   public static Validator<CreateAccountCmd>
      AccountInitValidation() =>
      cmd => Currencies.Contains(cmd.Currency) 
         ? Success<Error, CreateAccountCmd>(cmd)
         : Fail<Error, CreateAccountCmd>(Errors.InvalidCurrency(cmd.Currency));

   public static Validator<T> Pass<T>() => Success<Error, T>;
}