module InvoiceDraftSqlMapper

open Lib.SharedTypes
open Bank.Payment.Domain

let table = "invoice_draft"

module TypeCast =
   let status = "invoice_draft_status"

module Fields =
   let id = "id"
   let invoiceUploadId = "invoice_upload_id"
   let orgId = "org_id"
   let parsedData = "parsed_data"
   let status = "status"
   let statusDetail = "status_detail"

module Reader =
   let invoiceUploadId (read: RowReader) =
      read.uuid Fields.invoiceUploadId |> InvoiceUploadId

   let orgId (read: RowReader) = read.uuid Fields.orgId |> OrgId

   let statusDetail (read: RowReader) =
      read.text Fields.statusDetail
      |> Serialization.deserializeUnsafe<InvoiceDraftStatus>

   let parsedData (read: RowReader) : ParsedInvoice option =
      read.textOrNone Fields.parsedData
      |> Option.map Serialization.deserializeUnsafe<ParsedInvoice>

   let invoiceDraft (read: RowReader) : InvoiceDraft = {
      InvoiceUploadId = invoiceUploadId read
      OrgId = orgId read
      ParsedData = parsedData read
      Status = statusDetail read
   }

module Writer =
   let invoiceUploadId (InvoiceUploadId id) = Sql.uuid id
   let orgId (OrgId id) = Sql.uuid id
   let status (status: InvoiceDraftStatus) = Sql.string (string status)

   let statusDetail (status: InvoiceDraftStatus) =
      status |> Serialization.serialize |> Sql.jsonb

   let parsedData (data: ParsedInvoice option) =
      data |> Option.map Serialization.serialize |> Sql.jsonbOrNone
