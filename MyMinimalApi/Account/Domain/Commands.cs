using Lib.Types;

namespace Bank.Account.Domain;

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
   string Currency,
   decimal Balance,
   string FirstName,
   string LastName
)
: Command(EntityId)
{
   //public AccountStatus Status { get; init; } = AccountStatus.Active;

   public CreatedAccount ToEvent() =>
      new(EntityId, Timestamp, Currency, Balance);
}

public record DepositCashCmd(
   Guid EntityId,
   DateTime Date,
   decimal Amount
)
: Command(EntityId)
{
   public DepositedCash ToEvent() => new(
      DepositedAmount: Amount,
      EntityId: EntityId,
      Timestamp: Timestamp
   );
}

public record DebitCmd(
   Guid EntityId,
   DateTime Date,
   decimal Amount,
   string Origin,
   string? Reference = default
)
: Command(EntityId)
{
   public DebitedAccount ToEvent() => new(
      EntityId: EntityId,
      DebitedAmount: Amount,
      Date: Date,
      Timestamp: Timestamp,
      Origin: Origin,
      Reference: Reference
   );
}

public record LimitDailyDebitsCmd(
   Guid EntityId,
   decimal DebitLimit
)
: Command(EntityId)
{
   public DailyDebitLimitUpdated ToEvent() =>
      new(EntityId, DebitLimit, Timestamp);
}

public record LockCardCmd(
   Guid EntityId,
   string Reference
)
: Command(EntityId)
{
   public LockedCard ToEvent() => new(EntityId, Timestamp, Reference);
}

public record UnlockCardCmd(
   Guid EntityId,
   string Reference
)
: Command(EntityId)
{
   public UnlockedCard ToEvent() => new(EntityId, Timestamp, Reference);
}