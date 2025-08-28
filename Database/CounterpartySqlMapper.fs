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
   let orgId = "org_id"
   let firstName = "first_name"
   let lastName = "last_name"
   let nickname = "nickname"
   let routingNumber = "routing_number"
   let accountNumber = "account_number"
   let depository = "depository"
   let paymentNetwork = "payment_network"

module CounterpartyReader =
   let id (read: RowReader) =
      CounterpartyFields.counterpartyId |> read.uuid |> AccountId

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

   let counterparty (read: RowReader) : Counterparty = {
      FirstName = firstName read
      LastName = lastName read
      Nickname = nickname read
      AccountNumber = accountNumber read
      RoutingNumber = routingNumber read
      CounterpartyId = id read
      OrgId = orgId read
      Depository = depository read
      PaymentNetwork = paymentNetwork read
      CreatedAt = read.dateTime "created_at"
   }

module CounterpartyWriter =
   let counterpartyId (AccountId id) = Sql.uuid id
   let orgId = OrgSqlWriter.orgId
   let firstName = Sql.string
   let lastName = Sql.string
   let nickname = Sql.stringOrNone
   let routingNumber = AccountSqlWriter.routingNumber
   let accountNumber = AccountSqlWriter.accountNumber

   let depository (status: CounterpartyAccountDepository) =
      Sql.string (string status)

   let paymentNetwork (network: PaymentNetwork) = Sql.string (string network)
