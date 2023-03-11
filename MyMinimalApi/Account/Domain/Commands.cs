using Lib.Types;

namespace Account.Domain;

/*
public static class CommandExt
{
   // When a command is received by the server, set a timestamp
   public static T SetTimestamp<T>(this T cmd) where T : Command
   => cmd with { Timestamp = DateTime.UtcNow };
}
*/

public record EchoCmd(
   Guid EntityId,
   string Message
)
: Command(EntityId);

public record CreateAccountCmd(
   Guid EntityId,
   string Currency
)
: Command(EntityId)
{
   //public AccountStatus Status { get; init; } = AccountStatus.Active;

   public CreatedAccount ToEvent() => new(
      EntityId: this.EntityId,
      Timestamp: this.Timestamp,
      Currency: this.Currency
   );
}

public record DepositCashCmd(
   Guid EntityId,
   DateTime Date,
   decimal Amount
)
: Command(EntityId)
{
   public DepositedCash ToEvent() => new(
      DepositedAmount: this.Amount,
      EntityId: this.EntityId,
      Timestamp: this.Timestamp
   );
}

public record DebitCmd(
   Guid EntityId,
   DateTime Date,
   decimal Amount,
   string Origin,
   string Reference
)
: Command(EntityId)
{
   public DebitedAccount ToEvent() => new(
      EntityId: this.EntityId,
      DebitedAmount: this.Amount,
      Date: this.Date,
      Timestamp: this.Timestamp,
      Origin: this.Origin,
      Reference: this.Reference
   );
}

public record TransferCmd(
   Guid EntityId,
   string Beneficiary,
   string Iban,
   string Bic,

   DateTime Date,
   decimal Amount,
   string Reference
)
: Command(EntityId)
{
   public DebitedTransfer ToEvent() => new(
      EntityId: this.EntityId,
      Date: this.Date,
      Beneficiary: this.Beneficiary,
      Bic: this.Bic,
      DebitedAmount: this.Amount,
      Iban: this.Iban,
      Reference: this.Reference,
      Timestamp: this.Timestamp
   );
}

public record FreezeAccountCmd(
   Guid EntityId,
   string Reference
)
: Command(EntityId)
{
   public FrozeAccount ToEvent() => new(
      EntityId: this.EntityId,
      Reference: this.Reference,
      Timestamp: this.Timestamp
   );
}