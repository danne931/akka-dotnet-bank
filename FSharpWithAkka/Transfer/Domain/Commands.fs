namespace Bank.Transfer.Domain

open System

open Lib.Types

type TransferCommand
   (
      entityId,
      recipient: TransferRecipient,
      date: DateTime,
      amount: decimal,
      reference: string
   ) =
   inherit Command(entityId)
   member x.Recipient = recipient
   member x.Date = date
   member x.Amount = amount
   member x.Reference = reference

type RegisterTransferRecipientCommand(entityId, recipient: TransferRecipient) =
   inherit Command(entityId)

   member x.Recipient = {
      recipient with
         Currency =
            if isNull recipient.Currency then
               "USD"
            else
               recipient.Currency
   }
