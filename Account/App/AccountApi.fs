module Bank.Account.Api

open Akkling
open FsToolkit.ErrorHandling
open Validus

open Lib.Postgres
open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Payment.Domain
open BankActorRegistry

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
         accountIds |> List.map _.Value |> List.toArray |> Sql.uuidArray
      ])
      Reader.account

let processCommand
   (registry: #IAccountGuaranteedDeliveryActor)
   (command: AccountCommand)
   =
   taskResult {
      let validation =
         match command with
         | AccountCommand.CreateVirtualAccount cmd ->
            CreateVirtualAccountCommand.toEvent cmd
            |> Result.map AccountEnvelope.get
         | AccountCommand.DepositCash cmd ->
            DepositCashCommand.toEvent cmd |> Result.map AccountEnvelope.get
         | AccountCommand.InternalTransfer cmd ->
            InternalTransferWithinOrgCommand.toEvent cmd
            |> Result.map AccountEnvelope.get
         | AccountCommand.ScheduleInternalTransferBetweenOrgs cmd ->
            ScheduleInternalTransferBetweenOrgsCommand.toEvent cmd
            |> Result.map AccountEnvelope.get
         | AccountCommand.ScheduleDomesticTransfer cmd ->
            ScheduleDomesticTransferCommand.toEvent cmd
            |> Result.map AccountEnvelope.get
         | AccountCommand.CloseAccount cmd ->
            CloseAccountCommand.toEvent cmd |> Result.map AccountEnvelope.get
         | AccountCommand.RequestPayment cmd ->
            RequestPaymentCommand.toEvent cmd |> Result.map AccountEnvelope.get
         | AccountCommand.CancelPaymentRequest cmd ->
            CancelPaymentRequestCommand.toEvent cmd
            |> Result.map AccountEnvelope.get
         | AccountCommand.DeclinePaymentRequest cmd ->
            DeclinePaymentRequestCommand.toEvent cmd
            |> Result.map AccountEnvelope.get
         | AccountCommand.ConfigureAutoTransferRule cmd ->
            ConfigureAutoTransferRuleCommand.toEvent cmd
            |> Result.map AccountEnvelope.get
         | AccountCommand.DeleteAutoTransferRule cmd ->
            DeleteAutoTransferRuleCommand.toEvent cmd
            |> Result.map AccountEnvelope.get
         | AccountCommand.ParentAccount(ParentAccountCommand.RegisterDomesticTransferRecipient cmd) ->
            RegisterDomesticTransferRecipientCommand.toEvent cmd
            |> Result.map AccountEnvelope.get
         | AccountCommand.ParentAccount(ParentAccountCommand.EditDomesticTransferRecipient cmd) ->
            EditDomesticTransferRecipientCommand.toEvent cmd
            |> Result.map AccountEnvelope.get
         | AccountCommand.ParentAccount(ParentAccountCommand.NicknameDomesticTransferRecipient cmd) ->
            NicknameDomesticTransferRecipientCommand.toEvent cmd
            |> Result.map AccountEnvelope.get
         | cmd ->
            Error
            <| ValidationErrors.create "" [
               $"Command processing not implemented for {cmd}"
            ]

      let! envelope = validation |> Result.mapError Err.ValidationError

      let msg =
         GuaranteedDelivery.message
            envelope.EntityId.Value
            (AccountMessage.StateChange command)

      registry.AccountGuaranteedDeliveryActor() <! msg

      return envelope
   }
