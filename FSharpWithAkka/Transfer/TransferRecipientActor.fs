[<RequireQualifiedAccess>]
module TransferRecipientActor

open type Echo.Process

open Lib.Types
open Bank.Transfer.Domain
open Bank.Transfer.Api

let ActorName = "transfer_recipient"

let start () =
   spawn (
      ActorName,
      (fun (evt: BankEvent<DebitedTransfer>) ->
         issueTransferToRecipient(evt).Wait())
   )
