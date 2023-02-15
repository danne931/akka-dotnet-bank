//using LaYumba.Functional;
using LanguageExt;
using static LanguageExt.Prelude;

namespace Account.Domain;

public static class Errors
{
   /*
   public static Error InsufficientBalance
      => new InsufficientBalanceError();

   public static Error InvalidBic
      => new InvalidBicError();

   public static Error CannotActivateClosedAccount
      => new CannotActivateClosedAccountError();
   */
   public static TransferDateIsPastError TransferDateIsPast(DateTime date)
      => TransferDateIsPastError.New(date.ToString());

/*
   public static Error AccountNotActive
      => new AccountNotActiveError();

   public static Error UnexpectedError
      => new UnexpectedError();

   public static Error UnknownAccountId(Guid id)
      => new UnknownAccountIdError(id);

   public static Error InvalidDepositAmount
      => new InvalidDepositAmountError();
*/
   public static InvalidCurrencyError InvalidCurrency(string currency)
      => InvalidCurrencyError.New($"Cannot create account with unknown currency {currency}");
}
/*
public sealed record UnknownAccountIdError(Guid Id)
   : Error($"No account found for id {Id}");

public sealed record UnexpectedError()
   : Error("An unexpected error has occurred");

public sealed record AccountNotActiveError()
   : Error("The account is not active; the requested operation cannot be completed");

public sealed record InvalidBicError()
   : Error("The beneficiary's BIC/SWIFT code is invalid");

public sealed record InsufficientBalanceError()
   : Error("Insufficient funds to fulfil the requested operation");

public sealed record CannotActivateClosedAccountError()
   : Error("Cannot activate an account that has been closed");


public sealed record InvalidDepositAmountError()
   : Error("Deposit amount must be greater than 0");
*/
/*
public class Error : NewType<Error, string>
{
   public Error(string e) : base(e) {}
}
*/

public sealed class TransferDateIsPastError : NewType<TransferDateIsPastError, string>
{
   public TransferDateIsPastError(string date) : base($"Transfer date {date} cannot be in the past") {}
}

public sealed class InvalidCurrencyError : NewType<InvalidCurrencyError, string>
{
   public InvalidCurrencyError(string currency) : base(currency) {}
}