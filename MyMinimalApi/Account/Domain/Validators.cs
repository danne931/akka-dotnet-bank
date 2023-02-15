using LaYumba.Functional;
using static LaYumba.Functional.F;
using Account.Domain.Commands;

namespace Account.Domain;

public delegate Validation<T> Validator<T>(T t);

public static class Validators
{
   // TODO: Change to a dictionary or something else for better perf?
   public static List<string> Currencies => new List<string>
   {
      "USD",
      "EUR"
   };

   public static Validator<TransferCmd> TransferValidation(Func<DateTime> clock)
      => cmd
      => cmd.Date.Date < clock().Date
         ? Errors.TransferDateIsPast
         : Valid(cmd);

   public static Validator<CreateAccountCmd> AccountInitValidation()
      => cmd
      => Currencies.Contains(cmd.Currency) 
         ? Valid(cmd)
         : Errors.InvalidCurrency(cmd.Currency);
}