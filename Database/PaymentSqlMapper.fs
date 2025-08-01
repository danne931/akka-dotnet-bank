module PaymentSqlMapper

open System

open Lib.SharedTypes
open OrganizationSqlMapper
open AccountSqlMapper
open Bank.Payment.Domain

module Table =
   let payment = "payment_request"
   let platformPayment = "payment_request_platform"
   let thirdPartyPayment = "payment_request_third_party"

module PaymentTypeCast =
   let status = "payment_request_status"
   let requestType = "payment_request_type"

module PaymentFields =
   let paymentId = "payment_id"
   let initiatedById = "initiated_by_id"
   let amount = "amount"
   let status = "status"
   let statusDetail = "status_detail"
   let memo = "memo"
   let requestType = "request_type"
   let dueAt = "due_at"
   let payeeOrgId = "payee_org_id"
   let payeeParentAccountId = "payee_parent_account_id"
   let payeeAccountId = "payee_account_id"
   let fulfilledByTransferId = "fulfilled_by_transfer_id"
   let fulfilledAt = "fulfilled_at"
   let recurringPaymentScheduleId = "recurring_payment_schedule_id"
   let invoiceId = "invoice_id"
   let createdAt = "created_at"

   // Specific to payment_request_platform table
   module Platform =
      let payerOrgId = "payer_org_id"
      let payerParentAccountId = "payer_parent_account_id"

   // Specific to payment_request_third_party table
   module ThirdParty =
      let payerEmail = "payer_email"
      let payerName = "payer_name"
      let shortId = "payment_short_id"

module PaymentSqlReader =
   let paymentId (read: RowReader) =
      PaymentFields.paymentId |> read.uuid |> PaymentRequestId

   let initiatedById (read: RowReader) =
      PaymentFields.initiatedById |> read.uuid |> EmployeeId |> InitiatedById

   let amount (read: RowReader) = PaymentFields.amount |> read.decimal

   let status (read: RowReader) =
      PaymentFields.statusDetail
      |> read.text
      |> Serialization.deserializeUnsafe<PaymentRequestStatus>

   let memo (read: RowReader) = PaymentFields.memo |> read.text

   let requestType (read: RowReader) : PaymentRequestType =
      PaymentFields.requestType
      |> read.string
      |> PaymentRequestType.fromStringUnsafe

   let dueAt (read: RowReader) = PaymentFields.dueAt |> read.dateTime

   let payeeOrgId (read: RowReader) =
      PaymentFields.payeeOrgId |> read.uuid |> OrgId

   let payeeParentAccountId (read: RowReader) =
      PaymentFields.payeeParentAccountId |> read.uuid |> ParentAccountId

   let payeeAccountId (read: RowReader) =
      PaymentFields.payeeAccountId |> read.uuid |> AccountId

   let fulfilledByTransferId (read: RowReader) =
      PaymentFields.fulfilledByTransferId
      |> read.uuidOrNone
      |> Option.map TransferId

   let fulfilledAt (read: RowReader) =
      PaymentFields.fulfilledAt |> read.dateTimeOrNone

   let recurringPaymentScheduleId (read: RowReader) =
      PaymentFields.recurringPaymentScheduleId
      |> read.uuidOrNone
      |> Option.map RecurringPaymentSchedule.RecurrenceScheduleId

   let invoiceId (read: RowReader) =
      PaymentFields.invoiceId |> read.uuidOrNone |> Option.map InvoiceId

   let createdAt (read: RowReader) = read.dateTime PaymentFields.createdAt

   module Platform =
      let payerOrgId (read: RowReader) =
         PaymentFields.Platform.payerOrgId |> read.uuid |> OrgId

      let payerParentAccountId (read: RowReader) =
         PaymentFields.Platform.payerParentAccountId
         |> read.uuid
         |> ParentAccountId

   module ThirdParty =
      let payerEmail (read: RowReader) =
         PaymentFields.ThirdParty.payerEmail |> read.string |> Email.deserialize

      let payerName (read: RowReader) =
         PaymentFields.ThirdParty.payerName |> read.string

      let shortId (read: RowReader) =
         PaymentFields.ThirdParty.shortId
         |> read.string
         |> PaymentPortalShortId.ShortId

module PaymentSqlWriter =
   let paymentId (paymentId: PaymentRequestId) =
      let (PaymentRequestId id) = paymentId
      Sql.uuid id

   let accountId = AccountSqlWriter.accountId

   let initiatedById = InitiatedById.get >> Sql.uuid

   let amount = Sql.decimal

   let status =
      function
      | PaymentRequestStatus.Requested -> "Requested"
      | PaymentRequestStatus.Fulfilled _ -> "Fulfilled"
      | PaymentRequestStatus.Cancelled -> "Cancelled"
      | PaymentRequestStatus.Declined -> "Declined"
      | PaymentRequestStatus.Failed _ -> "Failed"
      >> Sql.string

   let statusDetail (status: PaymentRequestStatus) =
      status |> Serialization.serialize |> Sql.jsonb

   let memo = Sql.text

   let requestType (requestType: PaymentRequestType) =
      Sql.string (string requestType)

   let dueAt (date: DateTime) = Sql.timestamptz date

   let payeeOrgId = OrgSqlWriter.orgId
   let payeeParentAccountId (ParentAccountId id) = Sql.uuid id
   let payeeAccountId = AccountSqlWriter.accountId

   let fulfilledByTransferId =
      function
      | PaymentRequestStatus.Fulfilled p -> Some(TransferId.get p.TransferId)
      | _ -> None
      >> Sql.uuidOrNone

   let fulfilledAt =
      function
      | PaymentRequestStatus.Fulfilled p -> Some p.FulfilledAt
      | _ -> None
      >> Sql.timestamptzOrNone

   let recurringPaymentScheduleId
      (idOpt: RecurringPaymentSchedule.RecurrenceScheduleId option)
      =
      idOpt |> Option.map _.Value |> Sql.uuidOrNone

   let invoiceId (idOpt: InvoiceId option) =
      idOpt |> Option.map _.Value |> Sql.uuidOrNone

   let createdAt (date: DateTime) = Sql.timestamptz date

   module Platform =
      let payerOrgId = OrgSqlWriter.orgId
      let payerParentAccountId (ParentAccountId id) = Sql.uuid id

   module ThirdParty =
      let payerEmail (email: Email) = email |> string |> Sql.string
      let payerName = Sql.string

      let shortId (PaymentPortalShortId.ShortId shortId) = Sql.string shortId
