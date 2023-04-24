namespace Bank.Transfer.Domain

open System
open Microsoft.FSharp.Core.Option

type TransferCommand = {
   Recipient: TransferRecipient
   Date: DateTime
   Amount: decimal
   Reference: string
   EntityId: Guid
   Timestamp: DateTime
}

//module TransferCommand =
//   let create cmd = { cmd with Timestamp = DateTime.UtcNow }

type RegisterTransferRecipientCommand = {
   Recipient: TransferRecipient
   EntityId: Guid
   Timestamp: DateTime
}

module RegisterTransferRecipientCommand =
   let create (cmd: RegisterTransferRecipientCommand) = {
      cmd with
         Timestamp = DateTime.UtcNow
         Recipient = {
            cmd.Recipient with
               Currency = cmd.Recipient.Currency |> orElse (Some "USD")
         }
   }
