[<RequireQualifiedAccess>]
module TransferRecipientActor

open Akkling

open Lib.Types
open Bank.Transfer.Domain
open Bank.Transfer.Api

let ActorName = "transfer_recipient"

let start (mailbox: Actor<_>) =
   spawn
      mailbox
      ActorName
      (props (
         actorOf2 (fun (ctx: Actor<BankEvent<DebitedTransfer>>) evt ->
            (issueTransferToRecipient evt ctx).Wait()
            ignored ())
      ))
