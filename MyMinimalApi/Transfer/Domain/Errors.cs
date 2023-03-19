using Lib.Types;

namespace Bank.Transfer.Domain;

public static class TransferErr {
   public static Err DateIsPast
      => new Err("Transfer date is past.");

   public static Err InvalidDetails
      => new Err("RecipientIdentification required.");

   public static Err InvalidRecipient
      => new Err("LastName & Identification required.");

   public static Err InvalidDomesticRecipient
      => new Err("Routing number required for domestic transfers.");

   public static Err InvalidInternationalRecipient
      => new Err("IdentificationMethod required for international transfers.");

   public static Err RecipientNotFound(Guid id)
      => new Err($"Recipient not found: {id.ToString()}");

   public static Err RecipientRegistrationRequired(TransferCmd cmd)
      => new Err("Recipient registration required before issuing transfers. " +
                 $"{cmd.Recipient.LastName}/{cmd.Recipient.Identification}");

   public static Err RecipientAlreadyRegistered(RegisterTransferRecipientCmd cmd)
      => new Err($"Transfer recipient {cmd.Recipient.LastName}" +
                 $"/{cmd.Recipient.Identification} " +
                 $"already added to account: {cmd.EntityId}");
}