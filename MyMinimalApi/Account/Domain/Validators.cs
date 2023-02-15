//using LaYumba.Functional;
//using static LaYumba.Functional.F;
using LanguageExt;
using static LanguageExt.Prelude;
using Account.Domain.Commands;
using Account.Domain.Events;

namespace Account.Domain;

//public delegate Validation Validator<T>(T t);
//public delegate Validation<TError, T> Validator<TError, T>(T t);
public delegate Validation<InvalidCurrencyError, CreatedAccount> CurrencyValidator(CreateAccountCmd cmd);
public delegate Validation<TransferDateIsPastError, DebitedTransfer> TransferValidator(TransferCmd cmd);

public static class Validators
{
   // TODO: Change to a dictionary or something else for better perf?
   public static List<string> Currencies => new List<string>
   {
      "USD",
      "EUR"
   };

   public static TransferValidator TransferValidation(Func<DateTime> clock) =>
      cmd => cmd.Date.Date < clock().Date
         ? Fail<TransferDateIsPastError, DebitedTransfer>(Errors.TransferDateIsPast(cmd.Date))
         : Success<TransferDateIsPastError, DebitedTransfer>(cmd.ToEvent());

   public static CurrencyValidator AccountInitValidation() =>
      cmd => Currencies.Contains(cmd.Currency) 
         ? Success<InvalidCurrencyError, CreatedAccount>(cmd.ToEvent())
         : Fail<InvalidCurrencyError, CreatedAccount>(Errors.InvalidCurrency(cmd.Currency));
}