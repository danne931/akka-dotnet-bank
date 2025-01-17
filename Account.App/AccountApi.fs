module Bank.Account.Api

open System
open System.Threading.Tasks
open FSharp.Control
open Akkling
open Akka.Actor
open FsToolkit.ErrorHandling
open Validus

open Lib.Postgres
open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Transfer.Domain

module Fields = AccountSqlMapper.AccountFields
module Reader = AccountSqlMapper.AccountSqlReader
module Writer = AccountSqlMapper.AccountSqlWriter
let table = AccountSqlMapper.table

let getAccount (id: AccountId) =
   pgQuerySingle<Account>
      $"SELECT * FROM {table}
        WHERE {Fields.accountId} = @accountId"
      (Some [ "accountId", Writer.accountId id ])
      Reader.account

let getAccountsByIds (accountIds: AccountId list) =
   pgQuery<Account>
      $"SELECT * FROM {table}
        WHERE {Fields.accountId} = ANY(@accountIds)"
      (Some [
         "accountIds",
         accountIds |> List.map AccountId.get |> List.toArray |> Sql.uuidArray
      ])
      Reader.account

let processCommand (system: ActorSystem) (command: AccountCommand) = taskResult {
   let validation =
      match command with
      | CreateAccount cmd ->
         CreateAccountCommand.toEvent cmd |> Result.map AccountEnvelope.get
      | DepositCash cmd ->
         DepositCashCommand.toEvent cmd |> Result.map AccountEnvelope.get
      | InternalTransfer cmd ->
         InternalTransferWithinOrgCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | ScheduleInternalTransferBetweenOrgs cmd ->
         ScheduleInternalTransferBetweenOrgsCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | ScheduleDomesticTransfer cmd ->
         ScheduleDomesticTransferCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | RegisterDomesticTransferRecipient cmd ->
         RegisterDomesticTransferRecipientCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | EditDomesticTransferRecipient cmd ->
         EditDomesticTransferRecipientCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | NicknameRecipient cmd ->
         NicknameRecipientCommand.toEvent cmd |> Result.map AccountEnvelope.get
      | CloseAccount cmd ->
         CloseAccountCommand.toEvent cmd |> Result.map AccountEnvelope.get
      | RequestPlatformPayment cmd ->
         RequestPlatformPaymentCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | CancelPlatformPayment cmd ->
         CancelPlatformPaymentCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | DeclinePlatformPayment cmd ->
         DeclinePlatformPaymentCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | ConfigureAutoTransferRule cmd ->
         ConfigureAutoTransferRuleCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | DeleteAutoTransferRule cmd ->
         DeleteAutoTransferRuleCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | cmd ->
         Error
         <| ValidationErrors.create "" [
            $"Command processing not implemented for {cmd}"
         ]

   let! res = validation |> Result.mapError Err.ValidationError

   let ref = AccountActor.get system (AccountId.fromEntityId res.EntityId)
   ref <! AccountMessage.StateChange command

   return res
}

// Diagnostic
let getAccountFromAkka
   (sys: ActorSystem)
   (accountId: AccountId)
   : Account option Task
   =
   let ref = AccountActor.get sys accountId

   ref.Ask(AccountMessage.GetAccount, Some(TimeSpan.FromSeconds 3))
   |> Async.toTask
