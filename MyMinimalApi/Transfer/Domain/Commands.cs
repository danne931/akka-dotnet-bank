using OneOf;
using Lib.Types;

namespace Bank.Transfer.Domain;

using TransferRecipientEvent = OneOf<
   RegisteredInternalTransferRecipient,
   RegisteredDomesticTransferRecipient,
   RegisteredInternationalTransferRecipient
>;

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
      EntityId: EntityId,
      Date: Date,
      Beneficiary: Beneficiary,
      Bic: Bic,
      DebitedAmount: Amount,
      Iban: Iban,
      Reference: Reference,
      Timestamp: Timestamp
   );
}

public record RegisterTransferRecipientCmd(
   Guid EntityId,
   string LastName,
   string FirstName,
   string NickName,
   string Identification,
   RecipientAccountEnvironment AccountEnvironment,
   InternationalRecipientAccountIdentificationStrategy IdentificationStrategy,
   string RoutingNumber,
   string Currency
)
: Command(EntityId)
{
   public TransferRecipientEvent ToEvent() =>
      AccountEnvironment switch {
         RecipientAccountEnvironment.Internal => new RegisteredInternalTransferRecipient(
            EntityId: EntityId,
            Timestamp: Timestamp,
            LastName: LastName,
            FirstName: FirstName,
            AccountNumber: Identification
         ),
         RecipientAccountEnvironment.Domestic => new RegisteredDomesticTransferRecipient(
            EntityId: EntityId,
            LastName: LastName,
            FirstName: FirstName,
            NickName: NickName,
            AccountNumber: Identification,
            RoutingNumber: RoutingNumber,
            Timestamp: Timestamp
         ),
         RecipientAccountEnvironment.International => new RegisteredInternationalTransferRecipient(
            EntityId: EntityId,
            LastName: LastName,
            FirstName: FirstName,
            NickName: NickName,
            Identification: Identification,
            IdentificationStrategy: IdentificationStrategy,
            Currency: Currency,
            Timestamp: Timestamp
         )
      };
}

public static class TransferRecipientExt {
   public static Event Unwrap(this TransferRecipientEvent evtWrapped) =>
      evtWrapped.Match<Event>(
         _ => evtWrapped.AsT0,
         _ => evtWrapped.AsT1,
         _ => evtWrapped.AsT2
      );
}