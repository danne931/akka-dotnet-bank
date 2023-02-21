using LanguageExt.Common;

namespace Account.Domain;

public static class Errors {
   public static Err InsufficientBalance
      => new Err("Insufficient funds to fulfil the requested operation");

   public static Err TransferDateIsPast
      => new Err(nameof(TransferDateIsPast));

   public static Err AccountNotActive
      => new Err(nameof(AccountNotActive));

   public static Err UnknownAccountId(Guid id)
      => new Err($"{nameof(UnknownAccountId)}: {id.ToString()}");

   public static Err InvalidDepositAmount
      => new Err("Deposit amount must be greater than 0");

   public static Err InvalidCurrency(string currency)
      => new Err($"{nameof(InvalidCurrency)}: {currency}");
}

public record Err(string Message, int Code = 100) : Expected(Message, Code);