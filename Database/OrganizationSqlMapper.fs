module OrganizationSqlMapper

open Lib.SharedTypes
open Bank.Account.Domain

let table = "organization"
let featureFlagsTable = "org_feature_flag"

module OrgFields =
   let orgId = "org_id"
   let name = "org_name"
   let socialTransferDiscoveryAccountId = "social_transfer_discovery_account_id"

module OrgSqlReader =
   let orgId (read: RowReader) = OrgFields.orgId |> read.uuid |> OrgId
   let name (read: RowReader) = read.string OrgFields.name

   let socialTransferDiscoveryAccountId (read: RowReader) =
      OrgFields.socialTransferDiscoveryAccountId
      |> read.uuidOrNone
      |> Option.map AccountId

   let org (read: RowReader) : Org = {
      OrgId = orgId read
      Name = name read
      FeatureFlags = {
         SocialTransferDiscoveryPrimaryAccountId =
            socialTransferDiscoveryAccountId read
      }
   }

module OrgSqlWriter =
   let orgId (orgId: OrgId) =
      let (OrgId id) = orgId
      Sql.uuid id

   let name = Sql.string

   let socialTransferDiscoveryAccountId (accountId: AccountId option) =
      accountId |> Option.map AccountId.get |> Sql.uuidOrNone
