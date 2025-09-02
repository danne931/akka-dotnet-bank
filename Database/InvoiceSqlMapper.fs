module InvoiceSqlMapper

open Bank.Payment.Domain

let table = "invoice"

module Fields =
   let invoiceId = "invoice_id"
   let lineItems = "line_items"
   let taxPercent = "tax_percent"
   let subtotal = "subtotal"
   let total = "total"

module Reader =
   let invoiceId (read: RowReader) =
      Fields.invoiceId |> read.uuid |> InvoiceId

   let lineItems (read: RowReader) =
      Fields.lineItems
      |> read.text
      |> Serialization.deserializeUnsafe<InvoiceLineItem list>

   let taxPercent (read: RowReader) = Fields.taxPercent |> read.decimal

   let invoice (read: RowReader) : Invoice = {
      Id = invoiceId read
      LineItems = lineItems read
      TaxPercent = taxPercent read
   }

module Writer =
   let invoiceId (InvoiceId id) = Sql.uuid id

   let lineItems (items: InvoiceLineItem list) =
      items |> Serialization.serialize |> Sql.jsonb

   let taxPercent = Sql.decimal
   let subtotal = Sql.decimal
   let total = Sql.decimal
