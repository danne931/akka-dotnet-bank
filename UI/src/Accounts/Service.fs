[<RequireQualifiedAccess>]
module AccountService

open Fable.SimpleHttp
open FsToolkit.ErrorHandling

open UIDomain.Account
open Bank.Account.Domain
open Bank.Transfer.Domain
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
      | AccountCommand.CreateVirtualAccount cmd ->
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
      | AccountCommand.RequestPayment cmd ->
         Serialization.serialize cmd, PaymentPath.RequestPayment
      | AccountCommand.CancelPaymentRequest cmd ->
         Serialization.serialize cmd, PaymentPath.CancelPayment
      | AccountCommand.DeclinePaymentRequest cmd ->
         Serialization.serialize cmd, PaymentPath.DeclinePayment
      | AccountCommand.ConfigureAutoTransferRule cmd ->
         Serialization.serialize cmd, TransferPath.ConfigureAutoTransferRule
      | AccountCommand.DeleteAutoTransferRule cmd ->
         Serialization.serialize cmd, TransferPath.DeleteAutoTransferRule
      | AccountCommand.ParentAccount cmd ->
         match cmd with
         | ParentAccountCommand.RegisterCounterparty cmd ->
            Serialization.serialize cmd, TransferPath.RegisterCounterparty
         | ParentAccountCommand.EditCounterparty cmd ->
            Serialization.serialize cmd, TransferPath.EditCounterparty
         | ParentAccountCommand.NicknameCounterparty cmd ->
            Serialization.serialize cmd, TransferPath.NicknameCounterparty
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
      let! evt, updatedAccount = Account.stateTransition account command
      let! res = postJson command
      let code = res.statusCode

      if code <> 200 then
         return! Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         let! envelope = Serialization.deserialize<Envelope> res.responseText

         return {
            Envelope = envelope
            PendingState = updatedAccount
            PendingEvent = evt
            PendingCommand = command
         }
   }

let submitParentAccountCommand
   (command: ParentAccountCommand)
   : Async<Result<ParentAccountCommandReceipt, Err>>
   =
   asyncResult {
      // Pre-network request validation typically checks the command itself
      // and the Result of applying the command to the current state as
      // seen with AccountService.submitCommand, OrgService.submitcommand,
      // and EmployeeService.submitCommand
      // This same validation occurs on the server when an actor is
      // processing a command.
      //
      // However, for the small selection of commands applied to the parent
      // account, rather than one of its subaccounts, we will simply validate
      // the command itself since we do not have access to the
      // ParentAccountSnapshot type in the client at this moment.
      // The application of the command to the aggregate ParentAccountSnapshot
      // state will still happen on the server & we will get success/error
      // via SignalR so no need to bother with providing the UI with the
      // ParentAccountSnapshot at this time.  The application of these
      // ParentAccount-specific commands have minimal if any validation
      // during ParentAccount.stateTransition anyway.
      let evtResult =
         match command with
         | ParentAccountCommand.RegisterCounterparty cmd ->
            RegisterCounterpartyCommand.toEvent cmd
            |> Result.map ParentAccountEvent.RegisteredCounterparty
         | ParentAccountCommand.EditCounterparty cmd ->
            EditCounterpartyCommand.toEvent cmd
            |> Result.map ParentAccountEvent.EditedCounterparty
         | ParentAccountCommand.NicknameCounterparty cmd ->
            NicknameCounterpartyCommand.toEvent cmd
            |> Result.map ParentAccountEvent.NicknamedCounterparty

      let! evt = evtResult |> Result.mapError ValidationError

      let! res = postJson (AccountCommand.ParentAccount command)
      let code = res.statusCode

      if code <> 200 then
         return! Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         let! envelope = Serialization.deserialize<Envelope> res.responseText

         return {
            Envelope = envelope
            PendingEvent = evt
            PendingCommand = command
         }
   }
