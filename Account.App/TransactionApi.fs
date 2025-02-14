module Bank.Transaction.Api

open System
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.NetworkQuery
open Lib.Postgres
open Bank.Account.Domain
open Bank.Transfer.Domain
open CategorySqlMapper
open AncillaryTransactionInfoSqlMapper
open TransactionSqlMapper

module Fields = TransactionFields
module Writer = TransactionSqlWriter
module Reader = TransactionSqlReader

let filtersToEventNames (filters: TransactionGroupFilter list) : string array =
   filters
   |> List.fold
      (fun acc e ->
         acc
         @ match e with
           | TransactionGroupFilter.Purchase -> [ typeof<DebitedAccount>.Name ]
           | TransactionGroupFilter.Deposit -> [ typeof<DepositedCash>.Name ]
           | TransactionGroupFilter.InternalTransferWithinOrg -> [
              typeof<InternalTransferWithinOrgPending>.Name
              typeof<InternalTransferWithinOrgCompleted>.Name
              typeof<InternalTransferWithinOrgFailed>.Name
              typeof<InternalTransferWithinOrgDeposited>.Name
             ]
           | TransactionGroupFilter.InternalTransferBetweenOrgs -> [
              typeof<InternalTransferBetweenOrgsPending>.Name
              typeof<InternalTransferBetweenOrgsCompleted>.Name
              typeof<InternalTransferBetweenOrgsFailed>.Name
              typeof<InternalTransferBetweenOrgsDeposited>.Name
             ]
           | TransactionGroupFilter.InternalAutomatedTransfer -> [
              typeof<InternalAutomatedTransferPending>.Name
              typeof<InternalAutomatedTransferCompleted>.Name
              typeof<InternalAutomatedTransferFailed>.Name
              typeof<InternalAutomatedTransferDeposited>.Name
             ]
           | TransactionGroupFilter.DomesticTransfer -> [
              typeof<DomesticTransferPending>.Name
              typeof<DomesticTransferCompleted>.Name
              typeof<DomesticTransferFailed>.Name
              typeof<DomesticTransferProgressUpdate>.Name
             ]
           | TransactionGroupFilter.PlatformPayment -> [
              typeof<PlatformPaymentRequested>.Name
              typeof<PlatformPaymentPaid>.Name
              typeof<PlatformPaymentDeposited>.Name
              typeof<PlatformPaymentDeclined>.Name
              typeof<PlatformPaymentCancelled>.Name
             ])
      []
   |> List.toArray

let filtersToOriginatingEventNames
   (filters: TransactionGroupFilter list)
   : string array
   =
   filters
   |> List.fold
      (fun acc e ->
         acc
         @ match e with
           | TransactionGroupFilter.Purchase -> [ typeof<DebitedAccount>.Name ]
           | TransactionGroupFilter.Deposit -> [ typeof<DepositedCash>.Name ]
           | TransactionGroupFilter.InternalTransferWithinOrg -> [
              typeof<InternalTransferWithinOrgPending>.Name
             ]
           | TransactionGroupFilter.InternalTransferBetweenOrgs -> [
              typeof<InternalTransferBetweenOrgsPending>.Name
             ]
           | TransactionGroupFilter.InternalAutomatedTransfer -> [
              typeof<InternalAutomatedTransferPending>.Name
             ]
           | TransactionGroupFilter.DomesticTransfer -> [
              typeof<DomesticTransferPending>.Name
             ]
           | TransactionGroupFilter.PlatformPayment -> [
              typeof<PlatformPaymentPaid>.Name // Outgoing payments
              typeof<PlatformPaymentDeposited>.Name // Incoming payments
             ])
      []
   |> List.toArray

let transactionQuery (query: TransactionQuery) =
   let table = TransactionSqlMapper.table
   let txnLimit = 30

   let agg =
      [
         "orgId", Writer.orgId query.OrgId
         "offset", Sql.int <| Math.Max(query.Page - 1, 0) * txnLimit
      ],
      $"{Fields.orgId} = @orgId",
      false

   let agg =
      Option.fold
         (fun (queryParams, where, joinAncillary) accountIds ->
            [ "accountIds", Writer.accountIds accountIds ] @ queryParams,
            $"{where} AND {Fields.accountId} = ANY(@accountIds)",
            joinAncillary)
         agg
         query.AccountIds

   let agg =
      Option.fold
         (fun (queryParams, where, joinAncillary) cardIds ->
            [ "cardIds", Writer.cardIds cardIds ] @ queryParams,
            $"{where} AND {Fields.cardId} = ANY(@cardIds)",
            joinAncillary)
         agg
         query.CardIds

   let agg =
      Option.fold
         (fun (queryParams, where, joinAncillary) initiatedByIds ->
            [ "initiatedByIds", Writer.initiatedByIds initiatedByIds ]
            @ queryParams,
            $"{where} AND {Fields.initiatedById} = ANY(@initiatedByIds)",
            joinAncillary)
         agg
         query.InitiatedByIds

   let agg =
      Option.fold
         (fun (queryParams, where, joinAncillary) (startDate, endDate) ->
            [
               "start", Writer.timestamp startDate
               "end", Writer.timestamp endDate
            ]
            @ queryParams,
            $"{where} AND {Fields.timestamp} >= @start 
                      AND {Fields.timestamp} <= @end",
            joinAncillary)
         agg
         query.DateRange

   let agg =
      match query.MoneyFlow with
      | None ->
         let queryParams, where, joinAncillary = agg

         queryParams,
         $"{where} AND {Fields.moneyFlow} IS NOT NULL",
         joinAncillary
      | Some direction ->
         let queryParams, where, joinAncillary = agg

         ("direction", Writer.moneyFlow (Some direction)) :: queryParams,
         $"{where} AND {Fields.moneyFlow} = @direction::{TransactionTypeCast.moneyFlow}",
         joinAncillary

   let agg =
      Option.fold
         (fun (queryParams, where, joinAncillary) amountFilter ->
            let where, amountParams =
               match amountFilter with
               | AmountFilter.LessThanOrEqualTo max ->
                  $"{where} AND {Fields.amount} <= @max",
                  [ "max", Writer.amount (Some max) ]
               | AmountFilter.GreaterThanOrEqualTo min ->
                  $"{where} AND {Fields.amount} >= @min",
                  [ "min", Writer.amount (Some min) ]
               | AmountFilter.Between(min, max) ->
                  $"{where} AND {Fields.amount} >= @min AND {Fields.amount} <= @max",
                  [
                     "min", Writer.amount (Some min)
                     "max", Writer.amount (Some max)
                  ]

            amountParams @ queryParams, where, joinAncillary)
         agg
         query.Amount

   let atiTable = AncillaryTransactionInfoSqlMapper.table

   let agg =
      Option.fold
         (fun (queryParams, where, _) categoryFilter ->
            match categoryFilter with
            | CategoryFilter.CategoryIds catIds ->
               let catParam =
                  "categories",
                  AncillaryTransactionSqlWriter.categoryIds (Some catIds)

               catParam :: queryParams,
               $"{where} AND {atiTable}.{AncillaryTransactionFields.categoryId} = ANY(@categories)",
               true
            | CategoryFilter.IsCategorized isCat ->
               let isCatClause = if isCat then "IS NOT NULL" else "IS NULL"

               queryParams,
               $"{where} AND {atiTable}.{AncillaryTransactionFields.categoryId} {isCatClause}",
               true)
         agg
         query.Category

   let agg =
      let queryParams, where, joinAncillary = agg

      let filters =
         query.EventType |> Option.defaultValue TransactionGroupFilter.All

      let queryParams =
         [
            "eventTypes",
            filters |> filtersToOriginatingEventNames |> Sql.stringArray
         ]
         @ queryParams

      queryParams,
      $"{where} AND {Fields.name} = ANY(@eventTypes)",
      joinAncillary

   let queryParams, where, joinAncillaryTransactionTable = agg

   let joinAncillaryTxnInfo =
      if joinAncillaryTransactionTable then
         Some $"LEFT JOIN {atiTable} using ({Fields.eventId})"
      else
         None

   queryParams,
   $"""
   WITH matching_transactions AS (
      SELECT
         {Fields.event},
         {Fields.timestamp},
         {Fields.correlationId},
         {Fields.eventId},
         {Fields.orgId}
      FROM {table}
      {joinAncillaryTxnInfo |> Option.defaultValue ""}
      WHERE {where}
      ORDER BY {Fields.timestamp} desc
      LIMIT {txnLimit}
      OFFSET @offset
   ),
   correlated_transactions AS (
      SELECT
         t.{Fields.event},
         t.{Fields.timestamp},
         t.{Fields.correlationId},
         t.{Fields.eventId}
      FROM {table} t
      JOIN matching_transactions mt ON
         t.{Fields.correlationId} = mt.{Fields.correlationId}
         AND t.{Fields.eventId} != mt.{Fields.eventId}
   )
   SELECT {Fields.event}, {Fields.correlationId}, {Fields.timestamp} FROM matching_transactions
   UNION
   SELECT {Fields.event}, {Fields.correlationId}, {Fields.timestamp} FROM correlated_transactions
   ORDER BY {Fields.timestamp}, {Fields.correlationId}
   """

let getTransactions
   (query: TransactionQuery)
   : TaskResultOption<Map<TransactionId, Transaction.T>, Err>
   =
   taskResultOption {
      let queryParams, queryString = transactionQuery query

      let! events =
         pgQuery<AccountEvent> queryString (Some queryParams) (fun read ->
            read.text Fields.event
            |> Serialization.deserializeUnsafe<AccountEvent>)

      return Transaction.fromAccountEvents events
   }

module Fields = AncillaryTransactionFields
module Writer = AncillaryTransactionSqlWriter
module Reader = AncillaryTransactionSqlReader
let private atiTable = AncillaryTransactionInfoSqlMapper.table

let upsertTransactionCategory (transactionId: TransactionId) (categoryId: int) = taskResult {
   let query =
      $"""
      INSERT INTO {atiTable}
         ({Fields.transactionId}, {Fields.categoryId})
      VALUES
         (@transactionId, @categoryId)
      ON CONFLICT ({Fields.transactionId})
      DO UPDATE SET {Fields.categoryId} = @categoryId
      """

   let! res =
      pgPersist query [
         "transactionId", Writer.transactionId transactionId
         "categoryId", Writer.categoryId (Some categoryId)
      ]

   return res
}

let deleteTransactionCategory (transactionId: TransactionId) =
   let query =
      $"""
      UPDATE {atiTable}
      SET {Fields.categoryId} = null
      WHERE {Fields.transactionId} = @txnId
      """

   pgPersist query [ "txnId", Writer.transactionId transactionId ]

let upsertTransactionNote (transactionId: TransactionId) (note: string) =
   let query =
      $"""
      INSERT INTO {atiTable}
         ({Fields.transactionId}, {Fields.note}, {Fields.categoryId})
      VALUES
         (@transactionId, @note, @categoryId)
      ON CONFLICT ({Fields.transactionId})
      DO UPDATE SET {Fields.note} = @note
      """

   pgPersist query [
      "transactionId", Writer.transactionId transactionId
      "note", Writer.note note
      "categoryId", Writer.categoryId None
   ]

let getCategories () =
   pgQuery<TransactionCategory>
      "SELECT category_id, name FROM category"
      None
      (fun read -> {
         Id = read.int "category_id"
         Name = read.string "name"
      })

let getCorrelatedTransactionConfirmations (correlationId: CorrelationId) =
   let query =
      $"""
      SELECT
         {TransactionSqlMapper.table}.{Fields.timestamp} as txn_timestamp,
         {TransactionSqlMapper.table}.{Fields.event},
         {AccountSqlMapper.table}.*
      FROM {TransactionSqlMapper.table}
         JOIN {AccountSqlMapper.table} using({Fields.accountId})
      WHERE {Fields.correlationId} = @correlationId
      ORDER BY txn_timestamp DESC
      """

   let rowReader (read: RowReader) = {
      EventPersisted = Reader.event read
      Account = AccountSqlMapper.AccountSqlReader.account read
      Date = read.dateTime "txn_timestamp"
   }

   pgQuery<AccountEventPersistedConfirmation>
      query
      (Some [ "correlationId", Writer.correlationId correlationId ])
      rowReader

let getTransactionInfo
   (txnId: TransactionId)
   : TaskResultOption<Transaction.TransactionWithAncillaryInfo, Err>
   =
   taskResultOption {
      let fieldCorrelationId =
         $"{TransactionSqlMapper.table}.{Fields.correlationId}"

      let fieldTxnId = $"{atiTable}.{Fields.transactionId}"

      let query =
         $"""
         SELECT
            {fieldCorrelationId},
            {TransactionSqlMapper.table}.{Fields.event},
            {atiTable}.{Fields.categoryId},
            {atiTable}.{Fields.note},
            {CategorySqlMapper.table}.{CategoryFields.name} as category_name
         FROM {TransactionSqlMapper.table}
            LEFT JOIN {atiTable} ON {fieldTxnId} = {fieldCorrelationId}
            LEFT JOIN {CategorySqlMapper.table} using({Fields.categoryId})
         WHERE {fieldCorrelationId} = @transactionId
         """

      let! res =
         pgQuery<AccountEvent * TransactionCategory option * string option>
            query
            (Some [ "transactionId", Writer.transactionId txnId ])
            (fun read ->
               Reader.event read,
               Reader.categoryId read
               |> Option.map (fun catId -> {
                  Id = catId
                  Name = read.string "category_name"
               }),
               Reader.note read)

      let events = res |> List.map (fun (e, _, _) -> e)
      let txn = (Transaction.fromAccountEvents events)[txnId]
      let _, category, note = List.head res

      return {
         Id = txnId
         Transaction = txn
         Category = category
         Note = note
      }
   }
