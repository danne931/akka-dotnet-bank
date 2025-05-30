module OrganizationSqlMapper

open Lib.SharedTypes
open Bank.Org.Domain

let table = "organization"
let featureFlagsTable = "org_feature_flag"

module OrgTypeCast =
   let status = "organization_status"

module OrgFields =
   let orgId = "org_id"
   let name = "org_name"
   let status = "status"
   let statusDetail = "status_detail"
   let socialTransferDiscoveryAccountId = "social_transfer_discovery_account_id"
   let adminTeamEmail = "admin_team_email"
   let parentAccountId = "parent_account_id"
   let employerIdentificationNumber = "ein"

module OrgSqlReader =
   let orgId (read: RowReader) = OrgFields.orgId |> read.uuid |> OrgId
   let name (read: RowReader) = read.string OrgFields.name

   let statusDetail (read: RowReader) =
      read.text OrgFields.statusDetail
      |> Serialization.deserializeUnsafe<OrgStatus>

   let socialTransferDiscoveryAccountId (read: RowReader) =
      OrgFields.socialTransferDiscoveryAccountId
      |> read.uuidOrNone
      |> Option.map AccountId

   let adminTeamEmail (read: RowReader) =
      OrgFields.adminTeamEmail |> read.string |> Email.deserialize

   let parentAccountId (read: RowReader) =
      OrgFields.parentAccountId |> read.uuid |> ParentAccountId

   let employerIdentificationNumber (read: RowReader) =
      OrgFields.employerIdentificationNumber |> read.string

   let org (read: RowReader) : Org = {
      OrgId = orgId read
      ParentAccountId = parentAccountId read
      Name = name read
      Status = statusDetail read
      AdminTeamEmail = adminTeamEmail read
      EmployerIdentificationNumber = employerIdentificationNumber read
      FeatureFlags = {
         SocialTransferDiscoveryPrimaryAccountId =
            socialTransferDiscoveryAccountId read
      }
      CommandApprovalRules = Map.empty
      CommandApprovalProgress = Map.empty
   }

module OrgSqlWriter =
   let orgId (orgId: OrgId) =
      let (OrgId id) = orgId
      Sql.uuid id

   let name = Sql.string

   let status (status: OrgStatus) = status |> string |> Sql.string

   let statusDetail (status: OrgStatus) =
      status |> Serialization.serialize |> Sql.jsonb

   let socialTransferDiscoveryAccountId (accountId: AccountId option) =
      accountId |> Option.map AccountId.get |> Sql.uuidOrNone

   let adminTeamEmail (email: Email) = email |> string |> Sql.string

   let parentAccountId (ParentAccountId id) = Sql.uuid id

   let employerIdentificationNumber = Sql.string
