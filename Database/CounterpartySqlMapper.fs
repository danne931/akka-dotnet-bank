module CounterpartySqlMapper

open Lib.SharedTypes
open OrganizationSqlMapper
open AccountSqlMapper
open Bank.Transfer.Domain

let table = "counterparty"

module CounterpartyTypeCast =
   let paymentNetwork = "payment_network"
   let accountDepository = "counterparty_account_depository"

module CounterpartyFields =
   let counterpartyId = "counterparty_id"
   let partnerBankCounterpartyId = "partner_bank_counterparty_id"
   let orgId = "org_id"
   let firstName = "first_name"
   let lastName = "last_name"
   let nickname = "nickname"
   let routingNumber = "routing_number"
   let accountNumber = "account_number"
   let depository = "depository"
   let paymentNetwork = "payment_network"
   let address = "address"

module CounterpartyReader =
   let id (read: RowReader) =
      CounterpartyFields.counterpartyId |> read.uuid |> CounterpartyId

   let partnerBankCounterpartyId (read: RowReader) =
      CounterpartyFields.partnerBankCounterpartyId
      |> read.string
      |> PartnerBankCounterpartyId

   let orgId (read: RowReader) =
      CounterpartyFields.orgId |> read.uuid |> OrgId

   let firstName (read: RowReader) =
      CounterpartyFields.firstName |> read.string

   let lastName (read: RowReader) =
      CounterpartyFields.lastName |> read.string

   let nickname (read: RowReader) =
      CounterpartyFields.nickname |> read.stringOrNone

   let routingNumber = AccountSqlReader.routingNumber
   let accountNumber = AccountSqlReader.accountNumber

   let depository (read: RowReader) =
      CounterpartyFields.depository
      |> read.string
      |> CounterpartyAccountDepository.fromStringUnsafe

   let paymentNetwork (read: RowReader) =
      CounterpartyFields.paymentNetwork
      |> read.string
      |> PaymentNetwork.fromStringUnsafe

   let address (read: RowReader) =
      read.text CounterpartyFields.address
      |> Serialization.deserializeUnsafe<Address>

   let counterparty (read: RowReader) : Counterparty = {
      FirstName = firstName read
      LastName = lastName read
      Nickname = nickname read
      AccountNumber = accountNumber read
      RoutingNumber = routingNumber read
      CounterpartyId = id read
      PartnerBankCounterpartyId = partnerBankCounterpartyId read
      OrgId = orgId read
      Depository = depository read
      PaymentNetwork = paymentNetwork read
      Address = address read
      CreatedAt = read.dateTime "created_at"
   }

module CounterpartyWriter =
   let counterpartyId (CounterpartyId id) = Sql.uuid id
   let partnerBankCounterpartyId (PartnerBankCounterpartyId id) = Sql.string id
   let orgId = OrgSqlWriter.orgId
   let firstName = Sql.string
   let lastName = Sql.string
   let nickname = Sql.stringOrNone
   let routingNumber = AccountSqlWriter.routingNumber
   let accountNumber = AccountSqlWriter.accountNumber

   let address (address: Address) =
      Sql.jsonb (Serialization.serialize address)

   let depository (status: CounterpartyAccountDepository) =
      Sql.string (string status)

   let paymentNetwork (network: PaymentNetwork) = Sql.string (string network)
