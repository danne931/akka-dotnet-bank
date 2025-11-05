module Bank.Org.Api

open System.Threading.Tasks
open Akkling
open FsToolkit.ErrorHandling
open Validus

open Lib.Postgres
open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Org.Domain
open Bank.Transfer.Domain
open TransactionMerchantSqlMapper
open CommandApproval
open BankActorRegistry

module Fields = OrganizationSqlMapper.OrgFields
module Reader = OrganizationSqlMapper.OrgSqlReader
module Writer = OrganizationSqlMapper.OrgSqlWriter
module TypeCast = OrganizationSqlMapper.OrgTypeCast
let table = OrganizationSqlMapper.table

let processCommand
   (registry: #IOrgGuaranteedDeliveryActor)
   (command: OrgCommand)
   =
   taskResult {
      let validation =
         match command with
         | OrgCommand.ConfigureApprovalRule cmd ->
            CommandApprovalRule.ConfigureApprovalRuleCommand.toEvent cmd
            |> Result.map OrgEnvelope.get
         | OrgCommand.DeleteApprovalRule cmd ->
            CommandApprovalRule.DeleteApprovalRuleCommand.toEvent cmd
            |> Result.map OrgEnvelope.get
         | OrgCommand.RequestCommandApproval cmd ->
            CommandApprovalProgress.RequestCommandApproval.toEvent cmd
            |> Result.map OrgEnvelope.get
         | OrgCommand.AcquireCommandApproval cmd ->
            CommandApprovalProgress.AcquireCommandApproval.toEvent cmd
            |> Result.map OrgEnvelope.get
         | OrgCommand.DeclineCommandApproval cmd ->
            CommandApprovalProgress.DeclineCommandApproval.toEvent cmd
            |> Result.map OrgEnvelope.get
         | cmd ->
            ValidationErrors.create "" [
               $"Command processing not implemented for {cmd}"
            ]
            |> Error

      let! envelope = validation |> Result.mapError Err.ValidationError

      let msg =
         GuaranteedDelivery.message
            envelope.EntityId.Value
            (OrgMessage.StateChange command)

      registry.OrgGuaranteedDeliveryActor() <! msg

      return envelope
   }

let submitOnboardingApplication
   (registry: #IOrgGuaranteedDeliveryActor)
   (cmd: SubmitOrgOnboardingApplicationCommand)
   =
   taskResult {
      let! e =
         SubmitOrgOnboardingApplicationCommand.toEvent cmd
         |> Result.mapError Err.ValidationError

      let query =
         $"""
         INSERT into {OrganizationSqlMapper.table}
            ({Fields.orgId},
             {Fields.parentAccountId},
             {Fields.name},
             {Fields.status},
             {Fields.statusDetail},
             {Fields.adminTeamEmail},
             {Fields.employerIdentificationNumber},
             {Fields.address},
             {Fields.businessType},
             {Fields.description},
             {Fields.website})
         VALUES
            (@orgId,
             @parentAccountId,
             @name,
             @status::{TypeCast.status},
             @statusDetail,
             @adminTeamEmail,
             @employerIdentificationNumber,
             @address,
             @businessType::{TypeCast.businessType},
             @description,
             @website)
         """

      let sqlParams = [
         "orgId", Writer.orgId e.OrgId
         "parentAccountId", Writer.parentAccountId e.Data.ParentAccountId
         "name", Writer.name e.Data.BusinessDetails.BusinessName
         "status", Writer.status OrgStatus.PendingOnboardingTasksFulfilled
         "statusDetail",
         Writer.statusDetail OrgStatus.PendingOnboardingTasksFulfilled
         "adminTeamEmail", Writer.adminTeamEmail e.Data.AdminTeamEmail
         "employerIdentificationNumber",
         Writer.employerIdentificationNumber
            e.Data.BusinessDetails.EmployerIdentificationNumber
         "address", Writer.address e.Data.BusinessDetails.Address
         "businessType", Writer.businessType e.Data.BusinessDetails.LegalType
         "description", Writer.description e.Data.BusinessDetails.Description
         "website", Writer.website e.Data.BusinessDetails.Website
      ]

      let! _ = pgPersist query sqlParams

      let msg =
         GuaranteedDelivery.message
            e.EntityId.Value
            (OrgMessage.StateChange(OrgCommand.SubmitOnboardingApplication cmd))

      registry.OrgGuaranteedDeliveryActor() <! msg

      return ()
   }

type private OrgDBResult =
   | AccountProfilesWithOrg of (Org * AccountProfile) list option
   | CommandApprovalRules of CommandApprovalRule list option
   | CommandApprovalProgress of CommandApprovalProgress.T list option
   | Counterparties of Counterparty list option

let getOrgAndAccountProfiles
   (orgId: OrgId)
   (getCounterparties: OrgId -> Task<Result<Counterparty list option, Err>>)
   : Task<Result<Option<OrgWithAccountProfiles>, Err>>
   =
   taskResult {
      let dpaView = AccountEventSqlMapper.Views.dailyPurchaseAccrued
      let mpaView = AccountEventSqlMapper.Views.monthlyPurchaseAccrued

      let transferAccrued = AccountEventSqlMapper.Functions.transferAccrued

      let accountIdField = AccountSqlMapper.AccountFields.accountId

      let query =
         $"""
         SELECT
            o.{Fields.name},
            o.{Fields.statusDetail},
            o.{Fields.adminTeamEmail},
            o.{Fields.employerIdentificationNumber},
            o.{Fields.address},
            o.{Fields.businessType},
            o.{Fields.description},
            o.{Fields.website},
            features.{Fields.socialTransferDiscoveryAccountId},
            a.*,
            dta.internal_transfer_within_org_accrued as daily_internal_within,
            dta.internal_transfer_between_orgs_accrued as daily_internal_between,
            dta.domestic_transfer_accrued as daily_domestic,
            mta.internal_transfer_within_org_accrued as monthly_internal_within,
            mta.internal_transfer_between_orgs_accrued as monthly_internal_between,
            mta.domestic_transfer_accrued as monthly_domestic,
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

      let counterpartiesTask =
         getCounterparties orgId |> TaskResult.map OrgDBResult.Counterparties

      let! res =
         Task.WhenAll [|
            orgTask
            approvalRuleTask
            approvalProgressTask
            counterpartiesTask
         |]

      let! res = res |> List.ofArray |> List.traverseResultM id

      return
         match res with
         | [ OrgDBResult.AccountProfilesWithOrg(Some accountProfilesWithOrg)
             OrgDBResult.CommandApprovalRules rulesOpt
             OrgDBResult.CommandApprovalProgress progressOpt
             OrgDBResult.Counterparties counterpartiesOpt ] ->
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
               Counterparties =
                  match counterpartiesOpt with
                  | Some counterparties ->
                     [ for c in counterparties -> c.CounterpartyId, c ]
                     |> Map.ofList
                  | None -> Map.empty
            }
         | _ -> None
   }

let searchOrgTransferSocialDiscovery (fromOrgId: OrgId) (nameQuery: string) =
   let query =
      $$"""
      SELECT
         {{Fields.orgId}},
         {{Fields.name}},
         {{Fields.parentAccountId}},
         features.{{Fields.socialTransferDiscoveryAccountId}} AS account_id
      FROM {{table}}
      LEFT JOIN {{OrganizationSqlMapper.featureFlagsTable}} features using({{Fields.orgId}})
      WHERE
         {{Fields.orgId}} <> @orgIdToExclude
         AND {{Fields.name}} %> @nameQuery
         AND features.{{Fields.socialTransferDiscoveryAccountId}} IS NOT NULL
      ORDER BY {{Fields.name}} <-> @nameQuery DESC
      """

   pgQuery<SocialTransferDiscoveryCandidate>
      query
      (Some [
         "orgIdToExclude", Writer.orgId fromOrgId
         "nameQuery", Writer.name nameQuery
      ])
      (fun read -> {
         OrgName = Reader.name read
         OrgId = Reader.orgId read
         ParentAccountId = Reader.parentAccountId read
         PrimaryReceivingAccountId = read.uuid "account_id" |> AccountId
      })

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
      "name", Writer.name (NonEmptyString.map _.ToLower() merchant.Name)
      "alias", Writer.alias merchant.Alias
   ]
