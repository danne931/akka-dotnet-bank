module TransactionMerchantSqlMapper

open OrganizationSqlMapper
open Bank.Org.Domain
open Lib.SharedTypes

let table = "merchant"

module MerchantFields =
   let orgId = OrgFields.orgId
   let name = "name"
   let alias = "alias"

module MerchantSqlReader =
   let orgId = OrgSqlReader.orgId

   let name (read: RowReader) =
      read.string MerchantFields.name |> NonEmptyString.deserializeUnsafe

   let alias (read: RowReader) =
      read.stringOrNone MerchantFields.alias
      |> Option.map NonEmptyString.deserializeUnsafe

   let merchant (read: RowReader) : Merchant = {
      OrgId = orgId read
      Name = name read
      Alias = alias read
   }

module MerchantSqlWriter =
   let orgId = OrgSqlWriter.orgId
   let name (str: NonEmptyString) = Sql.string str.Value

   let alias (strOpt: NonEmptyString option) =
      strOpt |> Option.map _.Value |> Sql.stringOrNone
