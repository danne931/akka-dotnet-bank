module Bank.Org.Api

open System.Threading.Tasks
open Akkling
open Akka.Actor
open FsToolkit.ErrorHandling
open Validus

open Lib.Postgres
open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Org.Domain
open TransactionMerchantSqlMapper

module Fields = OrganizationSqlMapper.OrgFields
module Reader = OrganizationSqlMapper.OrgSqlReader
module Writer = OrganizationSqlMapper.OrgSqlWriter
module TypeCast = OrganizationSqlMapper.OrgTypeCast
let table = OrganizationSqlMapper.table

let processCommand (system: ActorSystem) (command: OrgCommand) = taskResult {
   let validation =
      match command with
      | CreateOrg cmd ->
         CreateOrgCommand.toEvent cmd |> Result.map OrgEnvelope.get
      | ConfigureApprovalRule cmd ->
         CommandApprovalRule.ConfigureApprovalRuleCommand.toEvent cmd
         |> Result.map OrgEnvelope.get
      | AcquireCommandApproval cmd ->
         CommandApprovalProgress.AcquireCommandApproval.toEvent cmd
         |> Result.map OrgEnvelope.get
      | DeclineCommandApproval cmd ->
         CommandApprovalProgress.DeclineCommandApproval.toEvent cmd
         |> Result.map OrgEnvelope.get
      | cmd ->
         ValidationErrors.create "" [
            $"Command processing not implemented for {cmd}"
         ]
         |> Error

   let! res = validation |> Result.mapError Err.ValidationError
   let ref = OrgActor.get system res.OrgId
   ref <! OrgMessage.StateChange command
   return res
}

let getOrg (id: OrgId) : Result<Org option, Err> Task =
   let query =
      $"""
      SELECT
         o.{Fields.orgId},
         o.{Fields.name},
         o.{Fields.statusDetail},
         features.{Fields.socialTransferDiscoveryAccountId}
      FROM {table} o
      LEFT JOIN {OrganizationSqlMapper.featureFlagsTable} features using({Fields.orgId})
      WHERE {Fields.orgId} = @orgId
      """

   pgQuerySingle<Org> query (Some [ "orgId", Writer.orgId id ]) Reader.org

let getOrgAndAccountProfiles
   (orgId: OrgId)
   : Task<Result<Option<OrgWithAccountProfiles>, Err>>
   =
   taskResultOption {
      let dpaView = TransactionSqlMapper.TransactionViews.dailyPurchaseAccrued
      let mpaView = TransactionSqlMapper.TransactionViews.monthlyPurchaseAccrued

      let transferAccrued =
         TransactionSqlMapper.TransactionFunctions.transferAccrued

      let accountIdField = AccountSqlMapper.AccountFields.accountId

      let query =
         $"""
         SELECT
            o.{Fields.name},
            o.{Fields.statusDetail},
            features.{Fields.socialTransferDiscoveryAccountId},
            a.*,
            dta.internal_transfer_accrued as dita,
            dta.domestic_transfer_accrued as dida,
            mta.internal_transfer_accrued as mita,
            mta.domestic_transfer_accrued as mida,
            {mpaView}.amount_accrued as mpa,
            {dpaView}.amount_accrued as dpa
         FROM {table} o
         LEFT JOIN {OrganizationSqlMapper.featureFlagsTable} features using({Fields.orgId})
         JOIN {AccountSqlMapper.table} a using({Fields.orgId})
         LEFT JOIN (SELECT * FROM {transferAccrued}(@orgId, 'Day')) dta using({accountIdField})
         LEFT JOIN (SELECT * FROM {transferAccrued}(@orgId, 'Month')) mta using({accountIdField})
         LEFT JOIN {dpaView} using({accountIdField})
         LEFT JOIN {mpaView} using({accountIdField})
         WHERE {Fields.orgId} = @orgId
         """

      let! res =
         pgQuery<Org * AccountProfile>
            query
            (Some [ "orgId", Writer.orgId orgId ])
            (fun read ->
               Reader.org read,
               {
                  Account = AccountSqlMapper.AccountSqlReader.account read
                  Metrics = {
                     DailyInternalTransferAccrued =
                        read.decimalOrNone "dita" |> Option.defaultValue 0m
                     DailyDomesticTransferAccrued =
                        read.decimalOrNone "dida" |> Option.defaultValue 0m
                     MonthlyInternalTransferAccrued =
                        read.decimalOrNone "mita" |> Option.defaultValue 0m
                     MonthlyDomesticTransferAccrued =
                        read.decimalOrNone "mida" |> Option.defaultValue 0m
                     DailyPurchaseAccrued =
                        read.decimalOrNone "dpa" |> Option.defaultValue 0m
                     MonthlyPurchaseAccrued =
                        read.decimalOrNone "mpa" |> Option.defaultValue 0m
                  }
               })

      return {
         Org = fst (List.head res)
         AccountProfiles =
            [ for _, profile in res -> profile.Account.AccountId, profile ]
            |> Map.ofList
         Balance = res |> List.sumBy (snd >> _.Account.Balance)
      }
   }

let searchOrgTransferSocialDiscovery (fromOrgId: OrgId) (nameQuery: string) =
   let query =
      $$"""
      SELECT
         o.{{Fields.orgId}},
         o.{{Fields.name}},
         o.{{Fields.statusDetail}},
         features.{{Fields.socialTransferDiscoveryAccountId}}
      FROM {{table}} o
      LEFT JOIN {{OrganizationSqlMapper.featureFlagsTable}} features using({{Fields.orgId}})
      WHERE
         o.{{Fields.orgId}} <> @orgIdToExclude
         AND o.{{Fields.name}} %> @nameQuery
         AND features.{{Fields.socialTransferDiscoveryAccountId}} IS NOT NULL
      ORDER BY o.{{Fields.name}} <-> @nameQuery DESC
      """

   pgQuery<Org>
      query
      (Some [
         "orgIdToExclude", Writer.orgId fromOrgId
         "nameQuery", Writer.name nameQuery
      ])
      Reader.org

module Fields = MerchantFields
module Writer = MerchantSqlWriter
module Reader = MerchantSqlReader

let getMerchants (orgId: OrgId) =
   let query =
      $"""
      SELECT {Fields.orgId}, {Fields.name}, {Fields.alias}
      FROM {TransactionMerchantSqlMapper.table}
      WHERE {Fields.orgId} = @orgId
      """

   pgQuery<Merchant>
      query
      (Some [ "@orgId", Writer.orgId orgId ])
      Reader.merchant

let upsertMerchant (merchant: Merchant) =
   let query =
      $"""
      INSERT INTO {TransactionMerchantSqlMapper.table}
         ({Fields.orgId}, {Fields.name}, {Fields.alias})
      VALUES
         (@orgId, @name, @alias)
      ON CONFLICT ({Fields.orgId}, {Fields.name})
      DO UPDATE SET {Fields.alias} = @alias
      """

   pgPersist query [
      "orgId", Writer.orgId merchant.OrgId
      "name", Writer.name <| merchant.Name.ToLower()
      "alias", Writer.alias merchant.Alias
   ]
