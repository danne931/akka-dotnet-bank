namespace Bank.Transfer.Domain

open System.Threading.Tasks
open EventStore.Client
open FSharp.Control
open Microsoft.FSharp.Core.Option

open Lib.Types

module Validators =
   let DailyDebitLimitValidation () : Validator<TransferCommand> =
      (fun (cmd: TransferCommand) -> Ok cmd)

   let RegisterTransferRecipient
      (es: EventStoreClient)
      (cmd: RegisterTransferRecipientCommand)
      (RecipientExists: EventStoreClient -> TransferRecipient -> bool Task)
      : AsyncValidator<RegisterTransferRecipientCommand>
      =
      (fun (cmd: RegisterTransferRecipientCommand) ->
         task {
            let recipient = cmd.Recipient

            if
               recipient.AccountEnvironment = RecipientAccountEnvironment.Domestic
               && isNone recipient.RoutingNumber
            then
               return Error "TransferErr.InvalidDomesticRecipient"
            elif
               recipient.AccountEnvironment = RecipientAccountEnvironment.International
               && isNone recipient.Currency
            then
               return Error "TransferErr.InvalidInternationalRecipient"
            else
               let! exists = RecipientExists es recipient

               // TODO: Remove Guid play
               if not exists then
                  return
                     Error
                        $"TransferErr.RecipientNotFound(Guid(recipient.Identification))"
               else
                  return Ok cmd
         })
