namespace Bank.Payment.Domain

open System
open System.Threading.Tasks

open Lib.SharedTypes

type InvoiceUploadId =
   | InvoiceUploadId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (InvoiceUploadId id) = x in id

[<RequireQualifiedAccess>]
type InvoiceFileType =
   | PDF
   | PNG
   | JPEG

   override x.ToString() =
      match x with
      | PDF -> "PDF"
      | PNG -> "PNG"
      | JPEG -> "JPEG"

   static member ofString(value: string) =
      match value.ToUpper() with
      | "PDF" -> Some InvoiceFileType.PDF
      | "PNG" -> Some InvoiceFileType.PNG
      | "JPEG" -> Some InvoiceFileType.JPEG
      | _ -> None

   member x.AsContentType =
      match x with
      | InvoiceFileType.PDF -> "application/pdf"
      | InvoiceFileType.PNG -> "image/png"
      | InvoiceFileType.JPEG -> "image/jpeg"

type InvoiceUpload = {
   UploadId: InvoiceUploadId
   OrgId: OrgId
   FileName: string
   FileType: InvoiceFileType
   BlobUrl: Uri
}

type ParsedInvoiceLineItem = {
   Description: string
   Quantity: int option
   UnitPrice: decimal option
   Amount: decimal option
}

type ParsedInvoice = {
   VendorName: string option
   InvoiceNumber: string option
   InvoiceDate: DateTime option
   DueDate: DateTime option
   SubTotal: decimal option
   Tax: decimal option
   TaxPercent: decimal option
   Total: decimal option
   LineItems: ParsedInvoiceLineItem list
} with

   static member computeTaxPercent
      (props:
         {|
            Tax: decimal option
            SubTotal: decimal option
         |})
      =
      Option.map2
         (fun tax total -> if total > 0m then tax / total * 100m else 0m)
         props.Tax
         props.SubTotal

[<RequireQualifiedAccess>]
type InvoiceDraftStatus =
   | Parsing
   | Parsed
   | ParseFailed of string

   override x.ToString() =
      match x with
      | Parsing -> "Parsing"
      | Parsed -> "Parsed"
      | ParseFailed _ -> "ParseFailed"

type InvoiceDraft = {
   InvoiceUploadId: InvoiceUploadId
   OrgId: OrgId
   ParsedData: ParsedInvoice option
   Status: InvoiceDraftStatus
} with

   static member create invoiceUploadId orgId : InvoiceDraft = {
      InvoiceUploadId = invoiceUploadId
      OrgId = orgId
      ParsedData = None
      Status = InvoiceDraftStatus.Parsing
   }

type InvoiceDraftPersistence = {
   create: InvoiceDraft -> Task<Result<unit, Err>>
   parseCompleted: InvoiceUploadId -> ParsedInvoice -> Task<Result<unit, Err>>
   parseFailed: InvoiceUploadId -> string -> Task<Result<unit, Err>>
}
