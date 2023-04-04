using Echo;
using static Echo.Process;

using Bank.Transfer.API;
using Bank.Transfer.Domain;

namespace Bank.Transfer.Actors;

public static class TransferRecipientActor {
   public const string ActorName = "transfer_recipient";

   public static ProcessId Start() =>
      spawn<DebitedTransfer>(
         ActorName,
         evt => {
            BankTransferAPI.IssueTransferToRecipient(evt).Wait();
         }
      );
}