[<RequireQualifiedAccess>]
module AccountService

open Fable.SimpleHttp
open FsToolkit.ErrorHandling

open UIDomain.Account
open Bank.Account.Domain
open Lib.SharedTypes
open RoutePaths

let private serviceName = "AccountService"

let private notImplemented (cmd: AccountCommand) =
   let msg = $"{serviceName}: Not implemented command: {cmd}"
   Log.error msg
   failwith msg

let postJson (command: AccountCommand) =
   let serialized, url =
      match command with
      | AccountCommand.CreateAccount cmd ->
         Serialization.serialize cmd, AccountPath.Base
      | AccountCommand.DepositCash cmd ->
         Serialization.serialize cmd, AccountPath.Deposit
      | AccountCommand.InternalTransfer cmd ->
         Serialization.serialize cmd, TransferPath.InternalWithinOrg
      | AccountCommand.InternalTransferBetweenOrgs cmd ->
         Serialization.serialize cmd, TransferPath.InternalBetweenOrgs
      | AccountCommand.ScheduleInternalTransferBetweenOrgs cmd ->
         Serialization.serialize cmd, TransferPath.ScheduleInternalBetweenOrgs
      | AccountCommand.DomesticTransfer cmd ->
         Serialization.serialize cmd, TransferPath.Domestic
      | AccountCommand.ScheduleDomesticTransfer cmd ->
         Serialization.serialize cmd, TransferPath.ScheduleDomestic
      | AccountCommand.RequestPlatformPayment cmd ->
         Serialization.serialize cmd, PaymentPath.RequestPayment
      | AccountCommand.CancelPlatformPayment cmd ->
         Serialization.serialize cmd, PaymentPath.CancelPayment
      | AccountCommand.DeclinePlatformPayment cmd ->
         Serialization.serialize cmd, PaymentPath.DeclinePayment
      | AccountCommand.FulfillPlatformPayment cmd ->
         Serialization.serialize cmd, PaymentPath.FulfillPayment
      | AccountCommand.ConfigureAutoTransferRule cmd ->
         Serialization.serialize cmd, TransferPath.ConfigureAutoTransferRule
      | AccountCommand.DeleteAutoTransferRule cmd ->
         Serialization.serialize cmd, TransferPath.DeleteAutoTransferRule
      | other -> notImplemented other

   Http.postJson url serialized

let getAccount (accountId: AccountId) : Async<AccountMaybe> = async {
   let path = AccountPath.account accountId

   let! (code, responseText) = Http.get path

   if code = 404 then
      return Ok None
   elif code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return
         responseText |> Serialization.deserialize<Account> |> Result.map Some
}

let submitCommand
   (account: Account)
   (command: AccountCommand)
   : Async<Result<AccountCommandReceipt, Err>>
   =
   asyncResult {
      // Pre-network request validation checks the command itself
      // (Err.ValidationError) and the Result of applying the command
      // to the current state (Err.StateTransitionError).
      // This same validation occurs on the server when an actor is
      // processing a command.
      let state = {
         AccountSnapshot.empty with
            Info = account
      }

      let! evt, updatedState = Account.stateTransition state command

      let! res = postJson command
      let code = res.statusCode

      if code <> 200 then
         return! Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         let! envelope = Serialization.deserialize<Envelope> res.responseText

         return {
            Envelope = envelope
            PendingState = updatedState.Info
            PendingEvent = evt
            PendingCommand = command
         }
   }

open Bank.Transfer.Domain

let getPayments (orgId: OrgId) : Async<Result<PaymentSummary option, Err>> = async {
   let path = PaymentPath.payments orgId

   let! (code, responseText) = Http.get path

   if code = 404 then
      return Ok None
   elif code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return
         responseText
         |> Serialization.deserialize<PaymentSummary>
         |> Result.map Some
}

let getDomesticTransfersRetryableUponRecipientEdit
   (recipientId: AccountId)
   : Async<Result<DomesticTransfer list option, Err>>
   =
   async {
      let path =
         TransferPath.retryableDomesticTransfersUponRecipientCorrection
            recipientId

      let! (code, responseText) = Http.get path

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<DomesticTransfer list>
            |> Result.map Some
   }
