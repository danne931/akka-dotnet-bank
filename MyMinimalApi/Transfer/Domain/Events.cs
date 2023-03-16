using Lib.Types;

namespace Bank.Transfer.Domain;

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
   DateTime Timestamp
)
: Event(EntityId, Timestamp, nameof(RegisteredDomesticTransferRecipient));

public record RegisteredInternationalTransferRecipient(
   Guid EntityId,
   string LastName,
   string FirstName,
   string NickName,
   string Identification,
   InternationalRecipientAccountIdentificationStrategy IdentificationStrategy,
   string Currency,
   DateTime Timestamp
)
: Event(EntityId, Timestamp, nameof(RegisteredInternationalTransferRecipient));