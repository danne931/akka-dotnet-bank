[<RequireQualifiedAccess>]
module BankTransferActor

open type Echo.Process

open Lib.Types
open Bank.Transfer.Api
open Bank.Transfer.Domain

let private ActorName = "transfer_recipient"

let start () =
   spawn (
      ActorName,
      (fun (evt: BankEvent<DebitedTransfer>) ->
         IssueTransferToRecipient(evt).Wait())
   )
