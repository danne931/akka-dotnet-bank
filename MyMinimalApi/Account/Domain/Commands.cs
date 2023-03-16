using OneOf;
using Lib.Types;

namespace Account.Domain;

using TransferRecipientEvent = OneOf<
   RegisteredInternalTransferRecipient,
   RegisteredDomesticTransferRecipient,
   RegisteredInternationalTransferRecipient
>;

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
   decimal Balance
)
: Command(EntityId)
{
   //public AccountStatus Status { get; init; } = AccountStatus.Active;

   public CreatedAccount ToEvent() => new(
      EntityId: EntityId,
      Timestamp: Timestamp,
      Currency: Currency,
      Balance: Balance
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
   string Reference
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

public record LockCardCmd(
   Guid EntityId,
   string Reference
)
: Command(EntityId)
{
   public LockedCard ToEvent() => new(
      EntityId: EntityId,
      Reference: Reference,
      Timestamp: Timestamp
   );
}

public record UnlockCardCmd(
   Guid EntityId,
   string Reference
)
: Command(EntityId)
{
   public UnlockedCard ToEvent() => new(
      EntityId: EntityId,
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