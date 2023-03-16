using Lib.Types;

namespace Bank.Transfer.Domain;

public static class Errors {
   public static Err TransferDateIsPast
      => new Err(nameof(TransferDateIsPast));

   public static Err TransferRecipientNotFound(Guid id)
      => new Err($"{nameof(TransferRecipientNotFound)}: {id.ToString()}");
}