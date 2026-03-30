module Bank.Payment.InvoiceApi

open System
open System.Collections.Generic
open System.Threading.Tasks
open Azure.AI.DocumentIntelligence
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.Postgres
open Bank.Payment.Domain
open InvoiceDraftSqlMapper

/// Save pointer to invoice uploaded to blob storage
let createAttachment (upload: InvoiceUpload) : Task<Result<unit, Err>> = task {
   let query =
      $"""
      INSERT INTO {InvoiceUploadSqlMapper.table}
         ({InvoiceUploadSqlMapper.Fields.id},
          {InvoiceUploadSqlMapper.Fields.orgId},
          {InvoiceUploadSqlMapper.Fields.fileName},
          {InvoiceUploadSqlMapper.Fields.fileType},
          {InvoiceUploadSqlMapper.Fields.blobUrl})
      VALUES (@id, @orgId, @fileName, @fileType::{InvoiceUploadSqlMapper.TypeCast.fileType}, @blobUrl)
      """

   let! result =
      pgPersist query [
         "id", InvoiceUploadSqlMapper.Writer.id upload.UploadId
         "orgId", InvoiceUploadSqlMapper.Writer.orgId upload.OrgId
         "fileName", InvoiceUploadSqlMapper.Writer.fileName upload.FileName
         "fileType", InvoiceUploadSqlMapper.Writer.fileType upload.FileType
         "blobUrl", InvoiceUploadSqlMapper.Writer.blobUrl upload.BlobUrl
      ]

   return result |> Result.map ignore
}

let getAttachment
   (uploadId: InvoiceUploadId)
   : Task<Result<InvoiceUpload option, Err>>
   =
   pgQuerySingle
      $"""
      SELECT * FROM {InvoiceUploadSqlMapper.table}
      WHERE {InvoiceUploadSqlMapper.Fields.id} = @id
      """
      (Some [ "id", InvoiceUploadSqlMapper.Writer.id uploadId ])
      InvoiceUploadSqlMapper.Reader.invoiceUpload

/// Save an invoice draft corresponding to a document uploaded to
/// Azure blob storage for interpretation by Azure Document Intelligence.
let createDraft (draft: InvoiceDraft) : Task<Result<unit, Err>> = task {
   let query =
      $"""
      INSERT INTO {InvoiceDraftSqlMapper.table}
         ({Fields.invoiceUploadId},
          {Fields.orgId},
          {Fields.status},
          {Fields.statusDetail},
          {Fields.parsedData})
      VALUES
         (@invoiceUploadId,
          @orgId,
          @status::{TypeCast.status},
          @statusDetail,
          @parsedData)
      """

   let! result =
      pgPersist query [
         "invoiceUploadId", Writer.invoiceUploadId draft.InvoiceUploadId
         "orgId", Writer.orgId draft.OrgId
         "status", Writer.status draft.Status
         "statusDetail", Writer.statusDetail draft.Status
         "parsedData", Writer.parsedData draft.ParsedData
      ]

   return result |> Result.map ignore
}

let markDraftAsParsed
   (uploadId: InvoiceUploadId)
   (parsedData: ParsedInvoice)
   : Task<Result<unit, Err>>
   =
   task {
      let query =
         $"""
         UPDATE {table}
         SET
            {Fields.parsedData} = @parsedData,
            {Fields.status} = @status::{TypeCast.status},
            {Fields.statusDetail} = @statusDetail
         WHERE {Fields.invoiceUploadId} = @uploadId
         """

      let status = InvoiceDraftStatus.Parsed

      let! result =
         pgPersist query [
            "uploadId", Writer.invoiceUploadId uploadId
            "parsedData", Writer.parsedData (Some parsedData)
            "status", Writer.status status
            "statusDetail", Writer.statusDetail status
         ]

      return result |> Result.map ignore
   }

let markDraftAsFailed
   (draftId: InvoiceUploadId)
   (errorMsg: string)
   : Task<Result<unit, Err>>
   =
   task {
      let query =
         $"""
         UPDATE {table}
         SET
            {Fields.status} = @status::{TypeCast.status},
            {Fields.statusDetail} = @statusDetail
         WHERE {Fields.invoiceUploadId} = @uploadId
         """

      let status = InvoiceDraftStatus.ParseFailed errorMsg

      let! result =
         pgPersist query [
            "uploadId", Writer.invoiceUploadId draftId
            "status", Writer.status status
            "statusDetail", Writer.statusDetail status
         ]

      return result |> Result.map ignore
   }

module FileStorage =
   open Azure.Storage.Blobs
   open Microsoft.AspNetCore.Http
   open System.Threading
   open System.IO

   let verifyParserConfigured () : Result<unit, string> =
      if Env.config.AzureDocumentIntelligence.Endpoint.IsNone then
         Error "AzureDocumentIntelligence.Endpoint not configured."
      elif Env.config.AzureDocumentIntelligence.ApiKey.IsNone then
         Error "AzureDocumentIntelligence.ApiKey not configured."
      else
         Ok()

   let validateFile (file: IFormFile) : TaskResult<InvoiceFileType, string> = task {
      if file.Length = 0L then
         return Error "File is empty"
      elif file.Length > 5L * 1024L * 1024L then
         return Error "File size exceeds 5MB limit"
      else
         let validator =
            MagicBytesValidator.Services.Http.FormFileTypeProvider()

         let! fileType =
            validator.FindValidatedTypeAsync(file, null, CancellationToken.None)

         let mime = fileType.MimeTypes

         if Seq.contains "application/pdf" mime then
            return Ok InvoiceFileType.PDF
         elif Seq.contains "image/png" mime then
            return Ok InvoiceFileType.PNG
         elif Seq.contains "image/jpeg" mime then
            return Ok InvoiceFileType.JPEG
         else
            return
               Error
                  $"{mime} not allowed. Only JPEG, PNG, and PDF files are accepted"
   }

   let getBlobName
      (orgId: OrgId)
      (uploadId: InvoiceUploadId)
      (fileName: string)
      =
      $"invoices/{orgId}/{uploadId}/{fileName}"

   let uploadInvoice
      (containerClient: BlobContainerClient)
      (orgId: OrgId)
      (file: IFormFile)
      : TaskResult<InvoiceUpload, string>
      =
      taskResult {
         let! fileType = validateFile file

         let uploadId = InvoiceUploadId(Guid.NewGuid())

         let blobName = getBlobName orgId uploadId file.FileName
         let blobClient = containerClient.GetBlobClient blobName

         use stream = file.OpenReadStream()
         let! _ = blobClient.UploadAsync(stream, overwrite = true)

         return {
            UploadId = uploadId
            OrgId = orgId
            FileName = file.FileName
            FileType = fileType
            BlobUrl = blobClient.Uri
         }
      }
      |> TaskResult.catch _.Message

   let getInvoiceAttachment
      (containerClient: BlobContainerClient)
      (uploadId: InvoiceUploadId)
      : TaskResultOption<InvoiceUpload * Stream, Err>
      =
      taskResultOption {
         let! attachment = getAttachment uploadId

         let blobName =
            getBlobName attachment.OrgId attachment.UploadId attachment.FileName

         let blobClient = containerClient.GetBlobClient blobName

         let! response =
            blobClient.OpenReadAsync() |> Task.map (Some >> Result.Ok)

         return attachment, response
      }

module Parser =
   let decipherAnalyzedDocuments (docs: IReadOnlyList<AnalyzedDocument>) =
      let tryGetStringField
         (fields: IReadOnlyDictionary<string, DocumentField>)
         (fieldName: string)
         =
         match fields.TryGetValue(fieldName) with
         | true, field when field.FieldType = DocumentFieldType.String ->
            Some field.ValueString
         | _ -> None

      let tryGetMoneyField
         (fields: IReadOnlyDictionary<string, DocumentField>)
         (fieldName: string)
         =
         match fields.TryGetValue(fieldName) with
         | true, field when field.FieldType = DocumentFieldType.Currency ->
            Some field.ValueCurrency.Amount
         | _ -> None

      let tryGetDateField
         (fields: IReadOnlyDictionary<string, DocumentField>)
         (fieldName: string)
         =
         match fields.TryGetValue(fieldName) with
         | true, field when field.FieldType = DocumentFieldType.Date ->
            Some field.ValueDate.Value.DateTime
         | _ -> None

      let parseLineItems (fields: IReadOnlyDictionary<string, DocumentField>) =
         match fields.TryGetValue("Items") with
         | true, itemsField when itemsField.FieldType = DocumentFieldType.List ->
            itemsField.ValueList
            |> Seq.choose (fun itemField ->
               if itemField.FieldType = DocumentFieldType.Dictionary then
                  let itemFields = itemField.ValueDictionary

                  let description =
                     tryGetStringField itemFields "Description"
                     |> Option.defaultValue ""

                  let amount =
                     tryGetMoneyField itemFields "Amount" |> Option.map decimal

                  let quantity =
                     match itemFields.TryGetValue("Quantity") with
                     | true, field when
                        field.FieldType = DocumentFieldType.Double
                        ->
                        if field.ValueDouble.HasValue then
                           Some(int field.ValueDouble.Value)
                        else
                           None
                     | true, field when
                        field.FieldType = DocumentFieldType.Int64
                        ->
                        if field.ValueInt64.HasValue then
                           Some(int field.ValueInt64.Value)
                        else
                           None
                     | _ -> None

                  let unitPrice =
                     match itemFields.TryGetValue("UnitPrice") with
                     | true, field when
                        field.FieldType = DocumentFieldType.Currency
                        ->
                        Some(decimal field.ValueCurrency.Amount)
                     | _ -> None

                  Some {
                     Description = description
                     Quantity = quantity
                     UnitPrice = unitPrice
                     Amount = amount
                  }
               else
                  None)
            |> List.ofSeq
         | _ -> []

      if docs.Count < 1 then
         {
            VendorName = None
            InvoiceNumber = None
            InvoiceDate = None
            DueDate = None
            SubTotal = None
            Tax = None
            TaxPercent = None
            Total = None
            LineItems = []
         }
      else
         let parsedInvoice =
            docs
            |> Seq.fold
               (fun acc document ->
                  let fields = document.Fields

                  {
                     VendorName =
                        acc.VendorName
                        |> Option.orElse (
                           tryGetStringField fields "VendorName"
                        )
                     InvoiceNumber =
                        acc.InvoiceNumber
                        |> Option.orElse (tryGetStringField fields "InvoiceId")
                     InvoiceDate =
                        acc.InvoiceDate
                        |> Option.orElse (tryGetDateField fields "InvoiceDate")
                     DueDate =
                        acc.DueDate
                        |> Option.orElse (tryGetDateField fields "DueDate")
                     SubTotal =
                        acc.SubTotal
                        |> Option.orElse (
                           tryGetMoneyField fields "SubTotal"
                           |> Option.map decimal
                        )
                     Tax =
                        acc.Tax
                        |> Option.orElse (
                           tryGetMoneyField fields "TotalTax"
                           |> Option.map decimal
                        )
                     TaxPercent = acc.TaxPercent
                     Total =
                        acc.Total
                        |> Option.orElse (
                           tryGetMoneyField fields "InvoiceTotal"
                           |> Option.map decimal
                        )
                     LineItems = acc.LineItems @ parseLineItems fields
                  })
               {
                  VendorName = None
                  InvoiceNumber = None
                  InvoiceDate = None
                  DueDate = None
                  SubTotal = None
                  Tax = None
                  TaxPercent = None
                  Total = None
                  LineItems = []
               }

         let taxPercent =
            ParsedInvoice.computeTaxPercent {|
               Tax = parsedInvoice.Tax
               SubTotal = parsedInvoice.SubTotal
            |}

         {
            parsedInvoice with
               TaxPercent = taxPercent
         }

   let parseWithAzureAI
      (client: DocumentIntelligenceClient)
      (blobUrl: Uri)
      : TaskResult<ParsedInvoice, string>
      =
      task {
         try
            let! res =
               client.AnalyzeDocumentAsync(
                  Azure.WaitUntil.Completed,
                  "prebuilt-invoice",
                  blobUrl
               )

            let parsedInvoice = decipherAnalyzedDocuments res.Value.Documents
            return Ok parsedInvoice
         with ex ->
            return Error ex.Message
      }

   let private mockParse (_: Uri) : TaskResult<ParsedInvoice, string> = task {
      do! Async.Sleep(TimeSpan.FromSeconds 3.)

      return
         Ok {
            VendorName = Some "Office ++"
            InvoiceNumber = Some "INV-12345"
            InvoiceDate = Some(DateTime.UtcNow.AddDays(-7.))
            DueDate = Some(DateTime.UtcNow.AddDays(30.))
            SubTotal = Some 12000m
            Tax = Some 1200m
            TaxPercent = Some 10m
            Total = Some 13200m
            LineItems = [
               {
                  Description = "Chair"
                  Quantity = Some 10
                  UnitPrice = Some 500m
                  Amount = Some 5000m
               }
               {
                  Description = "Desk"
                  Quantity = Some 10
                  UnitPrice = Some 700m
                  Amount = Some 7000m
               }
            ]
         }
   }

   [<RequireQualifiedAccess>]
   type InvoiceParsingClient =
      | Mock
      | Azure of DocumentIntelligenceClient

   let getClient () =
      match
         Env.config.AzureDocumentIntelligence.Endpoint,
         Env.config.AzureDocumentIntelligence.ApiKey,
         EnvPayment.config.MockInvoiceProcessing
      with
      | _, _, true -> Some InvoiceParsingClient.Mock
      | Some endpoint, Some apiKey, false ->
         DocumentIntelligenceClient(
            Uri endpoint.Value,
            Azure.AzureKeyCredential apiKey.Value
         )
         |> InvoiceParsingClient.Azure
         |> Some
      | _ -> None

   let parseInvoice (client: InvoiceParsingClient) =
      match client with
      | InvoiceParsingClient.Mock -> mockParse
      | InvoiceParsingClient.Azure client -> parseWithAzureAI client
