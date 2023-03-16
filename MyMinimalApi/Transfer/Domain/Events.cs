using OneOf;

using Lib.Types;

namespace Bank.Transfer.Domain;

using TransferRecipientEvent = OneOf<
   RegisteredInternalTransferRecipient,
   RegisteredDomesticTransferRecipient,
   RegisteredInternationalTransferRecipient
>;

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
: Event(EntityId, Timestamp, nameof(RegisteredInternalTransferRecipient))
{
   public TransferRecipient AsTransferRecipient() =>
      new TransferRecipient(
         LastName,
         FirstName,
         AccountNumber,
         RecipientAccountEnvironment.Internal,
         RecipientAccountIdentificationStrategy.AccountID
      );
}

public record RegisteredDomesticTransferRecipient(
   Guid EntityId,
   string LastName,
   string FirstName,
   string RoutingNumber,
   string AccountNumber,
   DateTime Timestamp
)
: Event(EntityId, Timestamp, nameof(RegisteredDomesticTransferRecipient))
{
   public TransferRecipient AsTransferRecipient() =>
      new TransferRecipient(
         LastName,
         FirstName,
         AccountNumber,
         RecipientAccountEnvironment.Domestic,
         RecipientAccountIdentificationStrategy.AccountID,
         RoutingNumber
      );
}

public record RegisteredInternationalTransferRecipient(
   Guid EntityId,
   string LastName,
   string FirstName,
   string Identification,
   RecipientAccountIdentificationStrategy IdentificationStrategy,
   string Currency,
   DateTime Timestamp
)
: Event(EntityId, Timestamp, nameof(RegisteredInternationalTransferRecipient))
{
   public TransferRecipient AsTransferRecipient() =>
      new TransferRecipient(
         LastName,
         FirstName,
         Identification,
         RecipientAccountEnvironment.International,
         IdentificationStrategy,
         Currency: Currency
      );
}

public static class TransferEventsExt {
   public static Event Unwrap(this TransferRecipientEvent evtWrapped) =>
      evtWrapped.Match<Event>(
         _ => evtWrapped.AsT0,
         _ => evtWrapped.AsT1,
         _ => evtWrapped.AsT2
      );
}