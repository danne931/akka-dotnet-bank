using Account.Domain.Events;

namespace Account.Domain.Commands;

public abstract record Command(Guid EntityId) {
   public DateTime Timestamp { get; init; } = DateTime.UtcNow;
};

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
   //CurrencyCode Currency
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
   decimal Amount,
   Guid BranchId
)
: Command(EntityId)
{
   public DepositedCash ToEvent() => new(
      DepositedAmount: this.Amount,
      EntityId: this.EntityId,
      Timestamp: this.Timestamp,
      BranchId: this.BranchId
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