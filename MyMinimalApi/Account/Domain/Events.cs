using Lib.Types;

namespace Bank.Account.Domain;

public record CreatedAccount(
   Guid EntityId,
   DateTime Timestamp,
   string Currency,
   decimal Balance
)
: Event(EntityId, Timestamp, nameof(CreatedAccount));

public record LockedCard(
   Guid EntityId,
   DateTime Timestamp,
   string Reference
)
: Event(EntityId, Timestamp, nameof(LockedCard));

public record UnlockedCard(
   Guid EntityId,
   DateTime Timestamp,
   string Reference
)
: Event(EntityId, Timestamp, nameof(UnlockedCard));

public record DepositedCash(
   Guid EntityId,
   DateTime Timestamp,
   decimal DepositedAmount
)
: Event(EntityId, Timestamp, nameof(DepositedCash));

public record DebitedAccount(
   Guid EntityId,
   DateTime Date,
   DateTime Timestamp,
   decimal DebitedAmount,
   string Origin,
   string Reference
) : Event(EntityId, Timestamp, nameof(DebitedAccount));