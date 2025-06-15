module PaymentSqlMapper

open System

open Lib.SharedTypes
open OrganizationSqlMapper
open AccountSqlMapper
open Bank.Transfer.Domain

module Table =
   let payment = "payment"
   let platformPayment = "payment_platform"
   let thirdPartyPayment = "payment_third_party"

module PaymentTypeCast =
   let platformPaymentStatus = "platform_payment_status"
   let thirdPartyPaymentStatus = "third_party_payment_status"
   let paymentType = "payment_type"

module PaymentFields =
   let paymentId = "payment_id"
   let initiatedById = "initiated_by_id"
   let amount = "amount"
   let memo = "memo"
   let paymentType = "payment_type"
   let expiration = "expiration"
   let payeeOrgId = "payee_org_id"
   let payeeParentAccountId = "payee_parent_account_id"
   let payeeAccountId = "payee_account_id"
   let createdAt = "created_at"

   // Specific to platform_payment table
   module Platform =
      let payerOrgId = "payer_org_id"
      let payerParentAccountId = "payer_parent_account_id"
      let status = "status"
      let statusDetail = "status_detail"
      let payByAccount = "pay_by_account"

   // Specific to third_party_payment table
   module ThirdParty =
      let payerEmail = "payer_email"
      let payerName = "payer_name"
      let status = "status_tp"

module PaymentSqlReader =
   let paymentId (read: RowReader) =
      PaymentFields.paymentId |> read.uuid |> PaymentId

   let initiatedById (read: RowReader) =
      PaymentFields.initiatedById |> read.uuid |> EmployeeId |> InitiatedById

   let amount (read: RowReader) = PaymentFields.amount |> read.decimal

   let memo (read: RowReader) = PaymentFields.memo |> read.text

   let expiration (read: RowReader) =
      PaymentFields.expiration |> read.dateTime

   let payeeOrgId (read: RowReader) =
      PaymentFields.payeeOrgId |> read.uuid |> OrgId

   let payeeParentAccountId (read: RowReader) =
      PaymentFields.payeeParentAccountId |> read.uuid |> ParentAccountId

   let payeeAccountId (read: RowReader) =
      PaymentFields.payeeAccountId |> read.uuid |> AccountId

   let createdAt (read: RowReader) = read.dateTime PaymentFields.createdAt

   let paymentType (read: RowReader) : PaymentType =
      PaymentFields.paymentType |> read.string |> PaymentType.fromStringUnsafe

   // Specific to platform_payment table
   module Platform =
      let payerOrgId (read: RowReader) =
         PaymentFields.Platform.payerOrgId |> read.uuid |> OrgId

      let payerParentAccountId (read: RowReader) =
         PaymentFields.Platform.payerParentAccountId
         |> read.uuid
         |> ParentAccountId

      let status (read: RowReader) =
         PaymentFields.Platform.statusDetail
         |> read.text
         |> Serialization.deserializeUnsafe<PlatformPaymentStatus>

      let payByAccount (read: RowReader) : AccountId option =
         PaymentFields.Platform.payByAccount
         |> read.uuidOrNone
         |> Option.map AccountId

   // Specific to third_party_payment table
   module ThirdParty =
      let payerEmail (read: RowReader) =
         PaymentFields.ThirdParty.payerEmail |> read.string |> Email.deserialize

      let payerName (read: RowReader) =
         PaymentFields.ThirdParty.payerName |> read.string

      let status (read: RowReader) =
         PaymentFields.ThirdParty.status
         |> read.string
         |> ThirdPartyPaymentStatus.fromStringUnsafe

module PaymentSqlWriter =
   let paymentId (paymentId: PaymentId) =
      let (PaymentId id) = paymentId
      Sql.uuid id

   let accountId = AccountSqlWriter.accountId

   let initiatedById = InitiatedById.get >> Sql.uuid

   let amount = Sql.decimal

   let memo = Sql.text

   let paymentType (pType: PaymentType) = Sql.string (string pType)

   let expiration (date: DateTime) = Sql.timestamptz date

   let payeeOrgId = OrgSqlWriter.orgId
   let payeeParentAccountId (ParentAccountId id) = Sql.uuid id
   let payeeAccountId = AccountSqlWriter.accountId

   let createdAt (date: DateTime) = Sql.timestamptz date

   // Specific to platform_payment table
   module Platform =
      let payerOrgId = OrgSqlWriter.orgId

      let payerParentAccountId (ParentAccountId id) = Sql.uuid id

      let status =
         function
         | PlatformPaymentStatus.Unpaid -> "Unpaid"
         | PlatformPaymentStatus.Paid -> "Paid"
         | PlatformPaymentStatus.Deposited -> "Deposited"
         | PlatformPaymentStatus.Settled -> "Settled"
         | PlatformPaymentStatus.Cancelled -> "Cancelled"
         | PlatformPaymentStatus.Declined -> "Declined"
         | PlatformPaymentStatus.Failed _ -> "Failed"
         >> Sql.string

      let statusDetail (status: PlatformPaymentStatus) =
         status |> Serialization.serialize |> Sql.jsonb

      let payByAccount (opt: AccountId option) =
         opt |> Option.map AccountId.get |> Sql.uuidOrNone

   // Specific to third_party_payment table
   module ThirdParty =
      let payerEmail (email: Email) = email |> string |> Sql.string
      let payerName = Sql.string
      let status (status: ThirdPartyPaymentStatus) = Sql.string (string status)
