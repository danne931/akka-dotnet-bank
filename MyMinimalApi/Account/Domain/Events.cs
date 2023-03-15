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

public record RegisteredInternalTransferRecipient(
   Guid EntityId,
   DateTime Timestamp,
   string LastName,
   string FirstName,
   string AccountNumber
)
: Event(EntityId, Timestamp, nameof(RegisteredInternalTransferRecipient));

public record RegisteredDomesticTransferRecipient(
   Guid EntityId,
   string LastName,
   string FirstName,
   string NickName,
   string RoutingNumber,
   string AccountNumber,
   string StreetAddress,
   string City,
   string State,
   DateTime Timestamp
)
: Event(EntityId, Timestamp, nameof(RegisteredDomesticTransferRecipient));

public record RegisteredInternationalTransferRecipient(
   Guid EntityId,
   string LastName,
   string FirstName,
   string NickName,
   string Identification,
   AccountIdentificationMethod IdentificationMethod,
   string Currency,
   string StreetAddress,
   string City,
   string Country,
   DateTime Timestamp
)
: Event(EntityId, Timestamp, nameof(RegisteredInternationalTransferRecipient));