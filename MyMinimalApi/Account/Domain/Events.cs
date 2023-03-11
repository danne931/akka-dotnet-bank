using Lib.Types;

namespace Account.Domain;

public record CreatedAccount(
   Guid EntityId,
   DateTime Timestamp,
   string Currency,
   decimal Balance
)
: Event(EntityId, Timestamp, nameof(CreatedAccount));

public record DebitedTransfer(
   Guid EntityId,
   DateTime Date,
   DateTime Timestamp,
   string Beneficiary,
   string Iban,
   string Bic,
   decimal DebitedAmount,
   string Reference
)
: Event(EntityId, Timestamp, nameof(DebitedTransfer), 1.2F);

public record FrozeAccount(
   Guid EntityId,
   DateTime Timestamp,
   string Reference
)
: Event(EntityId, Timestamp, nameof(FrozeAccount));

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