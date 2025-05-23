module AncillaryTransactionInfoSqlMapper

open Lib.SharedTypes

let table = "ancillary_transaction_info"

module AncillaryTransactionFields =
   let transactionId = "transaction_id"
   let categoryId = PurchaseCategorySqlMapper.Fields.catId
   let note = "note"

module AncillaryTransactionSqlReader =
   let transactionId (read: RowReader) : TransactionId =
      read.uuid AncillaryTransactionFields.transactionId
      |> CorrelationId
      |> TransactionId

   let categoryId (read: RowReader) =
      read.intOrNone AncillaryTransactionFields.categoryId

   let note (read: RowReader) =
      read.textOrNone AncillaryTransactionFields.note

module AncillaryTransactionSqlWriter =
   let transactionId (id: TransactionId) = TransactionId.get id |> Sql.uuid

   let categoryId = Sql.intOrNone

   let categoryIds (ids: int list option) =
      ids |> Option.map Array.ofList |> Sql.intArrayOrNone

   let note = Sql.text
