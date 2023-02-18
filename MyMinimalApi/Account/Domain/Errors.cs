using LanguageExt;

namespace Account.Domain;

public static class Errors
{
   public static InsufficientBalanceError InsufficientBalance
      => new InsufficientBalanceError();

   public static TransferDateIsPastError TransferDateIsPast(DateTime date) =>
      new TransferDateIsPastError(date);

   public static AccountNotActiveError AccountNotActive
      => new AccountNotActiveError();

   public static UnexpectedError UnexpectedError
      => new UnexpectedError();

   public static UnknownAccountIdError UnknownAccountId(Guid id)
      => new UnknownAccountIdError(id);

   public static InvalidDepositAmountError InvalidDepositAmount
      => new InvalidDepositAmountError();

   public static InvalidCurrencyError InvalidCurrency(string currency)
      => new InvalidCurrencyError(currency);
}

public class Error : NewType<Error, string>
{
   public Error(string e) : base(e) {}
}

public sealed class UnknownAccountIdError : Error
{
   public UnknownAccountIdError(Guid id) : base($"No account found for id {id}") {}

   // Required for tests (json deserialize)
   public Guid id { get; init; }
}

public sealed class AccountNotActiveError : Error
{
   public AccountNotActiveError() : base("The account is not active; the requested operation cannot be completed") {}
}

public sealed class TransferDateIsPastError : Error
{
   public TransferDateIsPastError(DateTime date) : base($"Transfer date {date.ToString()} cannot be in the past") {}

   // Required for tests (json deserialize)
   public DateTime date { get; init; }
}

public sealed class InvalidCurrencyError : Error
{
   public InvalidCurrencyError(string currency) : base($"Cannot create account with unknown currency {currency}") {}
}

public sealed class InsufficientBalanceError : Error
{
   public InsufficientBalanceError() : base("Insufficient funds to fulfil the requested operation") {}
}

public sealed class InvalidDepositAmountError : Error
{
   public InvalidDepositAmountError() : base("Deposit amount must be greater than 0") {}
}

public sealed class UnexpectedError : Error
{
   public UnexpectedError() : base("An unexpected error has occurred") {}
}