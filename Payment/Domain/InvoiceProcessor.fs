namespace Bank.Payment.Domain

open System
open System.Threading.Tasks

open Lib.SharedTypes

type InvoiceDraftId =
   | InvoiceDraftId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (InvoiceDraftId id) = x in id

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
            Total: decimal option
         |})
      =
      Option.map2
         (fun tax total -> if total > 0m then tax / total * 100m else 0m)
         props.Tax
         props.Total

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
   Id: InvoiceDraftId
   OrgId: OrgId
   BlobUrl: Uri
   ParsedData: ParsedInvoice option
   Status: InvoiceDraftStatus
} with

   static member create id orgId blobUrl : InvoiceDraft = {
      Id = id
      OrgId = orgId
      BlobUrl = blobUrl
      ParsedData = None
      Status = InvoiceDraftStatus.Parsing
   }

type InvoiceDraftPersistence = {
   create: InvoiceDraft -> Task<Result<unit, Err>>
   parseCompleted: InvoiceDraftId -> ParsedInvoice -> Task<Result<unit, Err>>
   parseFailed: InvoiceDraftId -> string -> Task<Result<unit, Err>>
}
