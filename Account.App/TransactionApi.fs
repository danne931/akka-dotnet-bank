module Bank.Transaction.Api

open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.NetworkQuery
open Lib.Postgres
open Bank.Account.Domain
open Bank.Transfer.Domain
open CategorySqlMapper
open AncillaryTransactionInfoSqlMapper

let table = AccountEventSqlMapper.table
module Fields = AccountEventSqlMapper.Fields
module Writer = AccountEventSqlMapper.SqlWriter
module Reader = AccountEventSqlMapper.SqlReader
module TypeCast = AccountEventSqlMapper.TypeCast

module atiFields = AncillaryTransactionFields
module atiWriter = AncillaryTransactionSqlWriter
module atiReader = AncillaryTransactionSqlReader
let private atiTable = AncillaryTransactionInfoSqlMapper.table

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
   let agg =
      [ "orgId", Writer.orgId query.OrgId; "limit", Sql.int query.PageLimit ],
      $"{Fields.orgId} = @orgId",
      false

   let agg =
      Option.fold
         (fun (queryParams, where, joinAncillary) cursor ->
            let queryParams =
               [
                  "timestamp", Writer.timestamp cursor.Timestamp
                  "txnId",
                  cursor.TransactionId
                  |> TransactionId.toCorrelationId
                  |> Writer.correlationId
               ]
               @ queryParams

            queryParams,
            $"{where} AND
              ({Fields.timestamp} < @timestamp OR
              ({Fields.timestamp} = @timestamp AND {Fields.correlationId} < @txnId))",
            joinAncillary)
         agg
         query.Cursor

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
         $"{where} AND {Fields.moneyFlow} = @direction::{TypeCast.moneyFlow}",
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

   let atiTxnId =
      AncillaryTransactionInfoSqlMapper.AncillaryTransactionFields.transactionId

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
         Some
            $"LEFT JOIN {atiTable} ON {atiTable}.{atiTxnId} = {table}.{Fields.correlationId}"
      else
         None

   queryParams,
   $"""
   SELECT
      events.{Fields.event},
      events.{Fields.correlationId},
      events.{Fields.timestamp}
   FROM (
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
      LIMIT @limit
   ) AS matching
   CROSS JOIN LATERAL (
      SELECT
         matching.{Fields.event},
         matching.{Fields.timestamp},
         matching.{Fields.correlationId}

      UNION ALL

      SELECT
         correlated.{Fields.event},
         correlated.{Fields.timestamp},
         correlated.{Fields.correlationId}
      FROM {table} correlated
      WHERE
         correlated.{Fields.correlationId} = matching.{Fields.correlationId}
         AND correlated.{Fields.eventId} != matching.{Fields.eventId}
   ) AS events
   ORDER BY events.{Fields.timestamp}
   """

let getTransactions
   (query: TransactionQuery)
   : TaskResultOption<Transaction.T list, Err>
   =
   taskResultOption {
      let queryParams, queryString = transactionQuery query

      let! events =
         pgQuery<AccountEvent> queryString (Some queryParams) (fun read ->
            read.text Fields.event
            |> Serialization.deserializeUnsafe<AccountEvent>)

      return Transaction.fromAccountEvents events
   }

let getTransactionInfo
   (txnId: TransactionId)
   : TaskResultOption<Transaction.TransactionWithAncillaryInfo, Err>
   =
   taskResultOption {
      let fieldCorrelationId = $"{table}.{Fields.correlationId}"

      let query =
         $"""
         SELECT
            {fieldCorrelationId},
            {table}.{Fields.event},
            {atiTable}.{atiFields.categoryId},
            {atiTable}.{atiFields.note},
            {CategorySqlMapper.table}.{CategoryFields.name} as category_name
         FROM {table}
            LEFT JOIN {atiTable}
               ON {atiTable}.{atiFields.transactionId} = {fieldCorrelationId}
            LEFT JOIN {CategorySqlMapper.table} using({atiFields.categoryId})
         WHERE {fieldCorrelationId} = @transactionId
         ORDER BY timestamp
         """

      let! res =
         pgQuery<AccountEvent * TransactionCategory option * string option>
            query
            (Some [ "transactionId", atiWriter.transactionId txnId ])
            (fun read ->
               Reader.event read,
               atiReader.categoryId read
               |> Option.map (fun catId -> {
                  Id = catId
                  Name = read.string "category_name"
               }),
               atiReader.note read)

      let events = res |> List.map (fun (e, _, _) -> e)
      let txn = (Transaction.fromAccountEvents events).Head
      let _, category, note = List.head res

      return {
         Id = txnId
         Transaction = txn
         Category = category
         Note = note
      }
   }

let isEventPersistenceConfirmed
   (correlationId: CorrelationId)
   : TaskResult<bool, Err>
   =
   taskResult {
      let query =
         $"""
         SELECT EXISTS (
            SELECT 1 FROM {OrganizationEventSqlMapper.table}
            WHERE {Fields.correlationId} = @corrId

            UNION ALL

            SELECT 1 FROM {EmployeeEventSqlMapper.table}
            WHERE {Fields.correlationId} = @corrId

            UNION ALL

            SELECT 1 FROM {table}
            WHERE {Fields.correlationId} = @corrId
         ) AS correlation_id_exists;
         """

      let! isConfirmed =
         pgQuerySingle<bool>
            query
            (Some [ "corrId", Writer.correlationId correlationId ])
            (fun read -> read.bool "correlation_id_exists")

      return isConfirmed |> Option.defaultValue false
   }

let upsertTransactionCategory (transactionId: TransactionId) (categoryId: int) = taskResult {
   let query =
      $"""
      INSERT INTO {atiTable}
         ({atiFields.transactionId}, {atiFields.categoryId})
      VALUES
         (@transactionId, @categoryId)
      ON CONFLICT ({atiFields.transactionId})
      DO UPDATE SET {atiFields.categoryId} = @categoryId
      """

   let! res =
      pgPersist query [
         "transactionId", atiWriter.transactionId transactionId
         "categoryId", atiWriter.categoryId (Some categoryId)
      ]

   return res
}

let deleteTransactionCategory (transactionId: TransactionId) =
   let query =
      $"""
      UPDATE {atiTable}
      SET {atiFields.categoryId} = null
      WHERE {atiFields.transactionId} = @txnId
      """

   pgPersist query [ "txnId", atiWriter.transactionId transactionId ]

let upsertTransactionNote (transactionId: TransactionId) (note: string) =
   let query =
      $"""
      INSERT INTO {atiTable}
         ({atiFields.transactionId}, {atiFields.note}, {atiFields.categoryId})
      VALUES
         (@transactionId, @note, @categoryId)
      ON CONFLICT ({atiFields.transactionId})
      DO UPDATE SET {atiFields.note} = @note
      """

   pgPersist query [
      "transactionId", atiWriter.transactionId transactionId
      "note", atiWriter.note note
      "categoryId", atiWriter.categoryId None
   ]

let getCategories () =
   pgQuery<TransactionCategory>
      "SELECT category_id, name FROM category"
      None
      (fun read -> {
         Id = read.int "category_id"
         Name = read.string "name"
      })
