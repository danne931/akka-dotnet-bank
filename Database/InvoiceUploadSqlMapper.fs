module InvoiceUploadSqlMapper

open System

open Lib.SharedTypes
open Bank.Payment.Domain

let table = "invoice_upload"

module TypeCast =
   let fileType = "invoice_upload_file_type"

module Fields =
   let id = "id"
   let orgId = "org_id"
   let fileName = "file_name"
   let fileType = "file_type"
   let blobUrl = "blob_url"

module Reader =
   let id (read: RowReader) = read.uuid Fields.id |> InvoiceUploadId

   let orgId (read: RowReader) = read.uuid Fields.orgId |> OrgId

   let fileName (read: RowReader) = read.string Fields.fileName

   let fileType (read: RowReader) =
      let fileType = read.string Fields.fileType

      match InvoiceFileType.ofString fileType with
      | Some ft -> ft
      | None -> failwith $"Unknown invoice file type in DB: {fileType}"

   let blobUrl (read: RowReader) = read.string Fields.blobUrl |> Uri

   let invoiceUpload (read: RowReader) : InvoiceUpload = {
      UploadId = id read
      OrgId = orgId read
      FileName = fileName read
      FileType = fileType read
      BlobUrl = blobUrl read
   }

module Writer =
   let id (InvoiceUploadId id) = Sql.uuid id
   let orgId (OrgId id) = Sql.uuid id
   let fileName (fileName: string) = Sql.string fileName
   let fileType (fileType: InvoiceFileType) = Sql.string (string fileType)
   let blobUrl (url: Uri) = Sql.string (string url)
