module PurchaseCategorySqlMapper

let table = "purchase_category"

module Fields =
   let catId = "category_id"
   let name = "name"

module Reader =
   let catId (read: RowReader) = read.int Fields.catId
   let name (read: RowReader) = read.string Fields.name

   let category (read: RowReader) : Transaction.TransactionCategory = {
      Id = catId read
      Name = name read
   }

module Writer =
   let catId = Sql.int
   let name = Sql.string
