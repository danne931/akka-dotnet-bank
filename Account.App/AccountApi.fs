module Bank.Account.Api

open System
open System.Threading.Tasks
open FSharp.Control
open Akkling
open Akka.Actor
open FsToolkit.ErrorHandling
open Validus

open Lib.NetworkQuery
open Lib.Postgres
open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Transfer.Domain

module Fields = AccountSqlMapper.AccountFields
module Reader = AccountSqlMapper.AccountSqlReader
module Writer = AccountSqlMapper.AccountSqlWriter
module TypeCast = AccountSqlMapper.AccountTypeCast
let accountTable = AccountSqlMapper.table

let getAccount (id: AccountId) =
   pgQuerySingle<Account>
      $"SELECT * FROM {accountTable} 
        WHERE {Fields.accountId} = @accountId"
      (Some [ "accountId", Writer.accountId id ])
      Reader.account

let getAccountAndTransactions (txnQuery: TransactionQuery) = taskResultOption {
   let queryParams, txnQueryString =
      Bank.Transaction.Api.transactionQuery txnQuery

   let query =
      $"""
      SELECT
         {accountTable}.*,
         COALESCE(
            (
               SELECT jsonb_agg(event)
               FROM ({txnQueryString})
            ),
            '[]'::jsonb
         ) as txns
      FROM {accountTable}
      WHERE {Fields.accountId} = @accountId
      """

   return!
      pgQuerySingle<Account * AccountEvent list>
         query
         (Some queryParams)
         (fun read ->
            Reader.account read,
            read.text "txns"
            |> Serialization.deserializeUnsafe<AccountEvent list>)
}

open OrganizationSqlMapper

let getOrg (id: OrgId) =
   pgQuerySingle<Org>
      $"SELECT * FROM {OrganizationSqlMapper.table} 
        WHERE {Fields.orgId} = @orgId"
      (Some [ "orgId", OrgSqlWriter.orgId id ])
      OrgSqlReader.org

let getOrgAndAccountProfiles
   (orgId: OrgId)
   : Task<Result<Option<Org * AccountProfile list>, Err>>
   =
   taskResultOption {
      let orgTable = OrganizationSqlMapper.table

      let query =
         $"""
         SELECT
            {orgTable}.{OrgFields.orgId},
            {orgTable}.{OrgFields.name},
            {orgTable}.{OrgFields.requiresEmployeeInviteApproval},
            {accountTable}.{Fields.accountId},
            {accountTable}.{Fields.name},
            {accountTable}.{Fields.depository}
         FROM {orgTable}
         LEFT JOIN {accountTable} using({OrgFields.orgId})
         WHERE {Fields.orgId} = @orgId
         """

      let! res =
         pgQuery<Org * AccountProfile>
            query
            (Some [ "orgId", Writer.orgId orgId ])
            (fun read -> OrgSqlReader.org read, Reader.accountProfile read)

      return fst (List.head res), List.map snd res
   }

let getAccountsByIds (accountIds: AccountId list) =
   pgQuery<Account>
      $"SELECT * FROM {accountTable} 
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
         InternalTransferCommand.toEvent cmd |> Result.map AccountEnvelope.get
      | DomesticTransfer cmd ->
         DomesticTransferCommand.toEvent cmd |> Result.map AccountEnvelope.get
      | RegisterInternalTransferRecipient cmd ->
         RegisterInternalTransferRecipientCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | RegisterDomesticTransferRecipient cmd ->
         RegisterDomesticTransferRecipientCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | EditDomesticTransferRecipient cmd ->
         EditDomesticTransferRecipientCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | DeactivateInternalRecipient cmd ->
         DeactivateInternalRecipientCommand.toEvent cmd
         |> Result.map AccountEnvelope.get
      | NicknameRecipient cmd ->
         NicknameRecipientCommand.toEvent cmd |> Result.map AccountEnvelope.get
      | CloseAccount cmd ->
         CloseAccountCommand.toEvent cmd |> Result.map AccountEnvelope.get
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
let getAccountEventsFromAkka
   (sys: ActorSystem)
   (accountId: AccountId)
   : AccountEvent list Task
   =
   let ref = AccountActor.get sys accountId

   ref.Ask(AccountMessage.GetEvents, Some(TimeSpan.FromSeconds 3))
   |> Async.toTask

// Diagnostic
let getAccountFromAkka
   (sys: ActorSystem)
   (accountId: AccountId)
   : Account option Task
   =
   let ref = AccountActor.get sys accountId

   ref.Ask(AccountMessage.GetAccount, Some(TimeSpan.FromSeconds 3))
   |> Async.toTask
