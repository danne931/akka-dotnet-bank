module Bank.Transfer.Api

open System
open type Echo.Process
open EventStore.Client
open FSharp.Control
open System.Threading.Tasks

open Lib.Types
open Bank.Account.Api
open Bank.Account.Domain
open Bank.Transfer.Domain

let DomesticTransfer (evt: BankEvent<DebitedTransfer>) =
   task {
      // Simulate network request to send money to domestic bank
      (*
      do! DomesticTransferService.Handle {
         AccountNumber = cmd.Recipient.Identification
         RoutingNumber = cmd.Recipient.RoutingNumber
         Amount = cmd.Amount
         Timestamp = cmd.Timestamp
      }
      *)
      do! Task.Delay 1000
   }

// Simulate network request to send money to international bank
let InternationalTransfer (evt: BankEvent<DebitedTransfer>) =
   Task.Delay(1000) |> Task.ofTask

let RecipientExistsInternally
   (es: EventStoreClient)
   (recipient: TransferRecipient)
   =
   accountExists es (Guid recipient.Identification)

// Simulate network request to verify account in domestic bank
let RecipientExistsDomestically (recipient: TransferRecipient) =
   Task.Delay(1000).ContinueWith(fun _ -> true)

// Simulate network request to verify account in international bank
let RecipientExistsInternationally (recipient: TransferRecipient) =
   Task.Delay(1000).ContinueWith(fun _ -> true)

let RecipientExists (es: EventStoreClient) (recipient: TransferRecipient) =
   match recipient.AccountEnvironment with
   | RecipientAccountEnvironment.Internal ->
      RecipientExistsInternally es recipient
   | RecipientAccountEnvironment.Domestic ->
      RecipientExistsDomestically recipient
   | RecipientAccountEnvironment.International ->
      RecipientExistsInternationally recipient


let ThirdPartyBankTransfer (evt: BankEvent<DebitedTransfer>) =
   match evt.Data.Recipient.AccountEnvironment with
   | RecipientAccountEnvironment.Domestic -> DomesticTransfer evt
   | RecipientAccountEnvironment.International -> InternationalTransfer evt
   | _ ->
      raise (
         Exception
            "Third party transfer requires a domestic or international account."
      )

let IssueTransferToRecipient (evt: BankEvent<DebitedTransfer>) =
   task {
      let recipient = evt.Data.Recipient

      if
         recipient.AccountEnvironment = RecipientAccountEnvironment.Internal
      then
         let origin = evt.EntityId.ToString()

         tell (
            $"@accounts_{recipient.Identification}",
            {
               EntityId = Guid recipient.Identification
               Timestamp = evt.Data.Date
               Amount = evt.Data.DebitedAmount
               Origin = Some $"Account ({origin.Substring(origin.Length - 4)})"
            }
         )
         |> ignore
      else
         do! ThirdPartyBankTransfer(evt)
   }
