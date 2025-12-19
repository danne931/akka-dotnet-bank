module InvoiceDraftSqlMapper

open System

open Lib.SharedTypes
open Bank.Payment.Domain

let table = "invoice_draft"

module TypeCast =
   let status = "invoice_draft_status"

module Fields =
   let invoiceDraftId = "invoice_draft_id"
   let orgId = "org_id"
   let blobUrl = "blob_url"
   let parsedData = "parsed_data"
   let status = "status"
   let statusDetail = "status_detail"

module Reader =
   let invoiceDraftId (read: RowReader) =
      read.uuid Fields.invoiceDraftId |> InvoiceDraftId

   let orgId (read: RowReader) = read.uuid Fields.orgId |> OrgId

   let blobUrl (read: RowReader) = read.string Fields.blobUrl

   let statusDetail (read: RowReader) =
      read.text Fields.statusDetail
      |> Serialization.deserializeUnsafe<InvoiceDraftStatus>

   let parsedData (read: RowReader) : ParsedInvoice option =
      read.textOrNone Fields.parsedData
      |> Option.map Serialization.deserializeUnsafe<ParsedInvoice>

   let invoiceDraft (read: RowReader) : InvoiceDraft = {
      Id = invoiceDraftId read
      OrgId = orgId read
      BlobUrl = Uri(blobUrl read)
      ParsedData = parsedData read
      Status = statusDetail read
   }

module Writer =
   let invoiceDraftId (InvoiceDraftId id) = Sql.uuid id
   let orgId (OrgId id) = Sql.uuid id
   let blobUrl (url: Uri) = Sql.string (string url)

   let status (status: InvoiceDraftStatus) = Sql.string (string status)

   let statusDetail (status: InvoiceDraftStatus) =
      status |> Serialization.serialize |> Sql.jsonb

   let parsedData (data: ParsedInvoice option) =
      data |> Option.map Serialization.serialize |> Sql.jsonbOrNone
