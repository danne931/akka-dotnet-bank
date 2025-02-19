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
open Bank.Transfer.Domain
open TransactionMerchantSqlMapper

module Fields = OrganizationSqlMapper.OrgFields
module Reader = OrganizationSqlMapper.OrgSqlReader
module Writer = OrganizationSqlMapper.OrgSqlWriter
module TypeCast = OrganizationSqlMapper.OrgTypeCast
let table = OrganizationSqlMapper.table

let processCommand (system: ActorSystem) (command: OrgCommand) = taskResult {
   let validation =
      match command with
      | OrgCommand.CreateOrg cmd ->
         CreateOrgCommand.toEvent cmd |> Result.map OrgEnvelope.get
      | OrgCommand.ConfigureApprovalRule cmd ->
         CommandApprovalRule.ConfigureApprovalRuleCommand.toEvent cmd
         |> Result.map OrgEnvelope.get
      | OrgCommand.DeleteApprovalRule cmd ->
         CommandApprovalRule.DeleteApprovalRuleCommand.toEvent cmd
         |> Result.map OrgEnvelope.get
      | OrgCommand.AcquireCommandApproval cmd ->
         CommandApprovalProgress.AcquireCommandApproval.toEvent cmd
         |> Result.map OrgEnvelope.get
      | OrgCommand.DeclineCommandApproval cmd ->
         CommandApprovalProgress.DeclineCommandApproval.toEvent cmd
         |> Result.map OrgEnvelope.get
      | OrgCommand.RegisterDomesticTransferRecipient cmd ->
         RegisterDomesticTransferRecipientCommand.toEvent cmd
         |> Result.map OrgEnvelope.get
      | OrgCommand.EditDomesticTransferRecipient cmd ->
         EditDomesticTransferRecipientCommand.toEvent cmd
         |> Result.map OrgEnvelope.get
      | OrgCommand.NicknameDomesticTransferRecipient cmd ->
         NicknameDomesticTransferRecipientCommand.toEvent cmd
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

let getDomesticTransferRecipients
   (orgId: OrgId)
   : Result<DomesticTransferRecipient list option, Err> Task
   =
   pgQuery<DomesticTransferRecipient>
      $"""
      {TransferSqlMapper.Query.domesticTransferRecipient}
      WHERE dr.{TransferSqlMapper.TransferFields.DomesticRecipient.senderOrgId} = @orgId
      """
      (Some [
         "orgId",
         TransferSqlMapper.TransferSqlWriter.DomesticRecipient.senderOrgId orgId
      ])
      TransferSqlMapper.TransferSqlReader.DomesticRecipient.recipient

type private OrgDBResult =
   | AccountProfilesWithOrg of (Org * AccountProfile) list option
   | CommandApprovalRules of CommandApprovalRule.T list option
   | CommandApprovalProgress of CommandApprovalProgress.T list option
   | DomesticTransferRecipients of DomesticTransferRecipient list option

let getOrgAndAccountProfiles
   (orgId: OrgId)
   : Task<Result<Option<OrgWithAccountProfiles>, Err>>
   =
   taskResult {
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
            dta.internal_transfer_within_org_accrued as daily_internal_within,
            dta.internal_transfer_between_orgs_accrued as daily_internal_between,
            dta.domestic_transfer_accrued as daily_domestic,
            dta.payment_paid_accrued as daily_payment_paid,
            mta.internal_transfer_within_org_accrued as monthly_internal_within,
            mta.internal_transfer_between_orgs_accrued as monthly_internal_between,
            mta.domestic_transfer_accrued as monthly_domestic,
            mta.payment_paid_accrued as monthly_payment_paid,
            {mpaView}.amount_accrued as monthly_purchase,
            {dpaView}.amount_accrued as daily_purchase
         FROM {table} o
         LEFT JOIN {OrganizationSqlMapper.featureFlagsTable} features using({Fields.orgId})
         JOIN {AccountSqlMapper.table} a using({Fields.orgId})
         LEFT JOIN (SELECT * FROM {transferAccrued}(@orgId, 'Day')) dta using({accountIdField})
         LEFT JOIN (SELECT * FROM {transferAccrued}(@orgId, 'Month')) mta using({accountIdField})
         LEFT JOIN {dpaView} using({accountIdField})
         LEFT JOIN {mpaView} using({accountIdField})
         WHERE {Fields.orgId} = @orgId
         """

      let orgTask =
         pgQuery<Org * AccountProfile>
            query
            (Some [ "orgId", Writer.orgId orgId ])
            (fun read ->
               Reader.org read,
               {
                  Account = AccountSqlMapper.AccountSqlReader.account read
                  Metrics = {
                     DailyInternalTransferWithinOrg =
                        read.decimalOrNone "daily_internal_within"
                        |> Option.defaultValue 0m
                     DailyInternalTransferBetweenOrgs =
                        read.decimalOrNone "daily_internal_between"
                        |> Option.defaultValue 0m
                     DailyDomesticTransfer =
                        read.decimalOrNone "daily_domestic"
                        |> Option.defaultValue 0m
                     DailyPaymentPaid =
                        read.decimalOrNone "daily_payment_paid"
                        |> Option.defaultValue 0m
                     DailyPurchase =
                        read.decimalOrNone "daily_purchase"
                        |> Option.defaultValue 0m
                     MonthlyInternalTransferWithinOrg =
                        read.decimalOrNone "monthly_internal_within"
                        |> Option.defaultValue 0m
                     MonthlyInternalTransferBetweenOrgs =
                        read.decimalOrNone "monthly_internal_between"
                        |> Option.defaultValue 0m
                     MonthlyDomesticTransfer =
                        read.decimalOrNone "monthly_domestic"
                        |> Option.defaultValue 0m
                     MonthlyPaymentPaid =
                        read.decimalOrNone "monthly_payment_paid"
                        |> Option.defaultValue 0m
                     MonthlyPurchase =
                        read.decimalOrNone "monthly_purchase"
                        |> Option.defaultValue 0m
                  }
               })
         |> TaskResult.map OrgDBResult.AccountProfilesWithOrg

      let approvalRuleTask =
         Bank.CommandApproval.Api.getApprovalRules orgId
         |> TaskResult.map OrgDBResult.CommandApprovalRules

      let approvalProgressTask =
         Bank.CommandApproval.Api.getCommandApprovals orgId
         |> TaskResult.map OrgDBResult.CommandApprovalProgress

      let domesticTransferRecipientTask =
         getDomesticTransferRecipients orgId
         |> TaskResult.map OrgDBResult.DomesticTransferRecipients

      let! res =
         Task.WhenAll [|
            orgTask
            approvalRuleTask
            approvalProgressTask
            domesticTransferRecipientTask
         |]

      let! res = res |> List.ofArray |> List.traverseResultM id

      return
         match res with
         | [ OrgDBResult.AccountProfilesWithOrg(Some accountProfilesWithOrg)
             OrgDBResult.CommandApprovalRules rulesOpt
             OrgDBResult.CommandApprovalProgress progressOpt
             OrgDBResult.DomesticTransferRecipients recipientsOpt ] ->
            let org = fst (List.head accountProfilesWithOrg)

            let org =
               match rulesOpt with
               | None -> org
               | Some rules -> {
                  org with
                     CommandApprovalRules =
                        [ for rule in rules -> rule.RuleId, rule ] |> Map.ofList
                 }

            let org =
               match progressOpt with
               | None -> org
               | Some progress -> {
                  org with
                     CommandApprovalProgress =
                        [ for p in progress -> p.ProgressId, p ] |> Map.ofList
                 }

            let org =
               match recipientsOpt with
               | None -> org
               | Some recipients -> {
                  org with
                     DomesticTransferRecipients =
                        [ for r in recipients -> r.RecipientAccountId, r ]
                        |> Map.ofList
                 }

            Some {
               Org = org
               AccountProfiles =
                  [
                     for _, profile in accountProfilesWithOrg ->
                        profile.Account.AccountId, profile
                  ]
                  |> Map.ofList
               Balance =
                  accountProfilesWithOrg
                  |> List.sumBy (snd >> _.Account.Balance)
            }
         | _ -> None
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
