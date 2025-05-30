[<RequireQualifiedAccess>]
module AccountSeederActor

// NOTE:
// Actor runs in local development or staging environments to ensure
// some data is available to interact with.

open System
open System.Threading.Tasks
open Akka.Hosting
open Akka.Cluster
open Akkling
open Akkling.Cluster.Sharding
open FSharp.Control
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.Postgres
open EmployeeEventSqlMapper
open OrganizationEventSqlMapper
open Bank.Account.Api
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Employee.Domain
open ActorUtil
open AutomaticTransfer

module aeFields = AccountEventSqlMapper.Fields

let randomAmount min max =
   let rnd = new Random()
   decimal (rnd.Next(min, max)) + decimal (rnd.NextDouble())

type Status =
   | AwaitingClusterUp
   | AwaitingVerification
   | FinishedSeeding

type CreateAccountsMap = Map<AccountId, CreateAccountCommand>

type State = {
   Status: Status
   AccountsToCreate: CreateAccountsMap
   AccountsToSeedWithTransactions: Map<AccountId, bool>
}

type OrgSetup = {
   OrgId: OrgId
   ParentAccountId: ParentAccountId
   PrimaryAccountId: AccountId
   OpsAccountId: AccountId
   SavingsAccountId: AccountId
   AccountOwnerId: EmployeeId
   AccountOwnerName: {| First: string; Last: string |}
   AccountOwnerEmail: string
   BusinessName: string
   EIN: string
}

let myOrg = {
   OrgId = Constants.ORG_ID_REMOVE_SOON
   BusinessName = "Sapphire Health"
   ParentAccountId =
      ParentAccountId(Guid.Parse("06751328-320e-477a-9b89-2c193d8cf3a0"))
   PrimaryAccountId = AccountId(Guid.NewGuid())
   OpsAccountId =
      "ec3e94cc-eba1-4ff4-b3dc-55010ecf67a6" |> Guid.Parse |> AccountId
   SavingsAccountId =
      "ec3e94cc-eba1-4ff4-b3dc-55010ecf67a7" |> Guid.Parse |> AccountId
   AccountOwnerId = Constants.LOGGED_IN_EMPLOYEE_ID_REMOVE_SOON
   AccountOwnerName = {|
      First = "Daniel"
      Last = "Eisenbarger"
   |}
   AccountOwnerEmail = "jellyfish@gmail.com"
   EIN = "124536789"
}

let arCheckingAccountId =
   "ec3e94cc-eba1-4ff4-b3dc-55010ecf67a4" |> Guid.Parse |> AccountId

let apCheckingAccountId =
   "ec3e94cc-eba1-4ff4-b3dc-55010ecf67a5" |> Guid.Parse |> AccountId

let socialTransferCandidates = [
   {
      OrgId = "6b20162e-61f3-4434-82e0-e7d27337316b" |> Guid.Parse |> OrgId
      ParentAccountId =
         "1e6961fe-081d-4f6f-bfb0-19a2ebc38109" |> Guid.Parse |> ParentAccountId
      PrimaryAccountId =
         "2eb09ebc-bb20-4ef8-88c0-15d5e23125f5" |> Guid.Parse |> AccountId
      OpsAccountId =
         "1b3bb3b3-0cbe-4a7c-8716-736a864076b4" |> Guid.Parse |> AccountId
      SavingsAccountId =
         "8b13cf01-c950-415b-b906-4873e24eaecb" |> Guid.Parse |> AccountId
      AccountOwnerId =
         "54804db3-1a1c-42dd-b8bb-94cc71519557" |> Guid.Parse |> EmployeeId
      AccountOwnerName = {|
         First = "Elsieanne"
         Last = "Caplette"
      |}
      AccountOwnerEmail = "elsiane@gmail.com"
      BusinessName = "Figma"
      EIN = "124537689"
   }
   {
      OrgId = "55e75321-b9ad-48cc-b5ad-74af0a7e31b2" |> Guid.Parse |> OrgId
      ParentAccountId =
         "8524548e-6112-4249-b59b-a7e6c1d0a72e" |> Guid.Parse |> ParentAccountId
      PrimaryAccountId =
         "7918f574-9600-481f-bbc8-0a0430f5a416" |> Guid.Parse |> AccountId
      OpsAccountId =
         "7918f574-9600-481f-bbc8-0a0430f5a417" |> Guid.Parse |> AccountId
      SavingsAccountId =
         "7918f574-9600-481f-bbc8-0a0430f5a418" |> Guid.Parse |> AccountId
      AccountOwnerId =
         "54804db3-1a1c-42dd-b8bb-94cc71519558" |> Guid.Parse |> EmployeeId
      AccountOwnerName = {| First = "Paul"; Last = "Haslinger" |}
      AccountOwnerEmail = "haslinger@gmail.com"
      BusinessName = "Lendtable"
      EIN = "124536798"
   }
   {
      OrgId = "4c6d31a0-51de-4ab8-b805-f61eadb78f81" |> Guid.Parse |> OrgId
      ParentAccountId =
         "a5b36dee-e8a6-462a-ba9f-c0e411c80b2a" |> Guid.Parse |> ParentAccountId
      PrimaryAccountId =
         "c923710d-610d-4bea-a324-de8a66d072cc" |> Guid.Parse |> AccountId
      OpsAccountId =
         "7918f574-9600-481f-bbc8-0a0430f5a419" |> Guid.Parse |> AccountId
      SavingsAccountId =
         "3452f54a-02d6-4c22-82f0-71ad7cd8a613" |> Guid.Parse |> AccountId
      AccountOwnerId =
         "c931c059-8bff-4a30-ad5f-fc85b3092c11" |> Guid.Parse |> EmployeeId
      AccountOwnerName = {|
         First = "Finnegan"
         Last = "Swiftshadow"
      |}
      AccountOwnerEmail = "finneganswift@gmail.com"
      BusinessName = "Shopify"
      EIN = "124536788"
   }
]

let socialTransferSenders = [
   {
      OrgId = "5f808408-04a1-45f3-9e45-eba1ecd7a2da" |> Guid.Parse |> OrgId
      ParentAccountId =
         "e1323f2f-a560-4534-9687-486c503663c9" |> Guid.Parse |> ParentAccountId
      PrimaryAccountId =
         "24dc1103-0682-4141-a97e-b3870b8fadbf" |> Guid.Parse |> AccountId
      OpsAccountId =
         "edcdc95a-a372-4a4c-ba3e-b07a148724e3" |> Guid.Parse |> AccountId
      SavingsAccountId =
         "547f5d9d-6bf3-4458-90b9-ec39be5d9aa5" |> Guid.Parse |> AccountId
      AccountOwnerId =
         "3c2d88fd-d3aa-4999-8e73-d8129404f00b" |> Guid.Parse |> EmployeeId
      AccountOwnerName = {| First = "Meorise"; Last = "Sarlee" |}
      AccountOwnerEmail = "msarlee@yahoo.com"
      BusinessName = "Huntress"
      EIN = "124566789"
   }
   {
      OrgId = "b854d513-c40e-4582-bf2f-688d6a73a21a" |> Guid.Parse |> OrgId
      ParentAccountId =
         "2a4c6d7a-40d3-4967-97cf-01ce2ffd807a" |> Guid.Parse |> ParentAccountId
      PrimaryAccountId =
         "29963c00-c366-431e-84c2-cee5a3258581" |> Guid.Parse |> AccountId
      OpsAccountId =
         "9f592501-64a4-41aa-9321-d78c29c4629f" |> Guid.Parse |> AccountId
      SavingsAccountId =
         "52a9b399-7094-409e-87de-f9c141f4e066" |> Guid.Parse |> AccountId
      AccountOwnerId =
         "e8e8de39-dce0-4c70-89e4-5ab345949f99" |> Guid.Parse |> EmployeeId
      AccountOwnerName = {| First = "Pruncha"; Last = "Yukio" |}
      AccountOwnerEmail = "yukio@yahoo.com"
      BusinessName = "Github"
      EIN = "124436789"
   }
   {
      OrgId = "1e01868e-adcb-4c5d-8a8b-f75bc36db0f5" |> Guid.Parse |> OrgId
      ParentAccountId =
         "5209ad5c-8c38-444b-b564-6fd951ff906f" |> Guid.Parse |> ParentAccountId
      PrimaryAccountId =
         "b260b00f-8f09-47c2-9fe5-c6d19742b125" |> Guid.Parse |> AccountId
      OpsAccountId =
         "9c5fe0bd-fb24-46a3-a419-e5685ea5f56a" |> Guid.Parse |> AccountId
      SavingsAccountId =
         "82a62039-4d61-4c98-a063-697385631c54" |> Guid.Parse |> AccountId
      AccountOwnerId =
         "20038dd5-e7f2-478d-a57b-6165969471a4" |> Guid.Parse |> EmployeeId
      AccountOwnerName = {| First = "Aio"; Last = "Usagi" |}
      AccountOwnerEmail = "usagi@yahoo.com"
      BusinessName = "Segment"
      EIN = "524536789"
   }
]

let socialTransferOrgs = socialTransferCandidates @ socialTransferSenders

let paymentRequesters = [
   {
      OrgId = "e6641c29-b980-4a7a-9b5a-24e01ebd9af0" |> Guid.Parse |> OrgId
      ParentAccountId =
         "900b2a64-c9d3-4327-a6a1-b6c3230f4aa3" |> Guid.Parse |> ParentAccountId
      PrimaryAccountId =
         "ade87fa5-df43-43da-9903-90d66e986bf8" |> Guid.Parse |> AccountId
      OpsAccountId =
         "5c6cff2c-34ae-48bd-983e-6604a7e0e9ec" |> Guid.Parse |> AccountId
      SavingsAccountId =
         "26deaa3f-8453-4eaa-96eb-a25c156f4fef" |> Guid.Parse |> AccountId
      AccountOwnerId =
         "ff4b01e1-5d63-4eb1-a03a-11a976cb2f68" |> Guid.Parse |> EmployeeId
      AccountOwnerName = {|
         First = "Nakiasha"
         Last = "Daleth"
      |}
      AccountOwnerEmail = "nakiasha@yahoo.com"
      BusinessName = "Dusty Robotics"
      EIN = "534536789"
   }
   {
      OrgId = "7ef9d8f8-741f-4138-8aa8-37ab6e2e576d" |> Guid.Parse |> OrgId
      ParentAccountId =
         "96718323-01fd-49ba-9d52-06b91b7d8d70" |> Guid.Parse |> ParentAccountId
      PrimaryAccountId =
         "0b6c7486-4228-411d-8f39-6db9a98faf27" |> Guid.Parse |> AccountId
      OpsAccountId =
         "96364a32-4974-4ce7-afc8-4ef4eba57e41" |> Guid.Parse |> AccountId
      SavingsAccountId =
         "5f5fa2bd-5374-4115-87b2-fd799b200d6a" |> Guid.Parse |> AccountId
      AccountOwnerId =
         "d7cbb60f-c7e7-4cf9-b28c-b5da6c57f493" |> Guid.Parse |> EmployeeId
      AccountOwnerName = {| First = "Bích"; Last = "Phương" |}
      AccountOwnerEmail = "bichphuong@gmail.com"
      BusinessName = "Linear"
      EIN = "524578789"
   }
]

let paymentPayers = [
   {
      OrgId = "4ccbb100-c023-4957-82fd-36c35004a9fd" |> Guid.Parse |> OrgId
      ParentAccountId =
         "85110da9-68d2-41a6-b5bf-2ab30fdec5db" |> Guid.Parse |> ParentAccountId
      PrimaryAccountId =
         "a8ffdd09-4502-4054-bb20-1c12e3f26821" |> Guid.Parse |> AccountId
      OpsAccountId =
         "accda5d0-7b33-4f84-8f43-a6cbac2af008" |> Guid.Parse |> AccountId
      SavingsAccountId =
         "32d73bb5-5cc4-4f96-856f-ff7dc32734ee" |> Guid.Parse |> AccountId
      AccountOwnerId =
         "3565a715-a7e9-4ef2-b81a-628605aabaa0" |> Guid.Parse |> EmployeeId
      AccountOwnerName = {| First = "Mahatma"; Last = "Gandhi" |}
      AccountOwnerEmail = "gandhi@yahoo.com"
      BusinessName = "Dropbox"
      EIN = "524536711"
   }
   {
      OrgId = "9f7a2ae5-9cb5-46ac-908e-4427230bf0fe" |> Guid.Parse |> OrgId
      ParentAccountId =
         "aa2c5176-1b59-436d-97f2-774810bc2dac" |> Guid.Parse |> ParentAccountId
      PrimaryAccountId =
         "60b76142-bded-4eb3-8c87-89ab9949a65e" |> Guid.Parse |> AccountId
      OpsAccountId =
         "e44504ef-1dc1-47a4-a65f-0187dce7e100" |> Guid.Parse |> AccountId
      SavingsAccountId =
         "3561a703-5c37-456c-942c-ecf38090d919" |> Guid.Parse |> AccountId
      AccountOwnerId =
         "d260f814-186f-451c-8a9c-8ce2e565fbb4" |> Guid.Parse |> EmployeeId
      AccountOwnerName = {| First = "Zhi"; Last = "Ng" |}
      AccountOwnerEmail = "zhi@yahoo.com"
      BusinessName = "Xero"
      EIN = "524496789"
   }
]

let otherOrgs = socialTransferOrgs @ paymentRequesters @ paymentPayers

// Disable account_event & employee_event update prevention triggers
// during seeding so can backdate the timestamps during seeding.
let disableUpdatePreventionTriggers () =
   pgTransaction [
      "ALTER TABLE employee_event DISABLE TRIGGER prevent_update;", []
      "ALTER TABLE account_event DISABLE TRIGGER prevent_update;", []
      "ALTER TABLE organization_event DISABLE TRIGGER prevent_update;", []
   ]

// Ensure updates will not occur on employee_event & account_event
// tables once seeding finished.
let enableUpdatePreventionTriggers () =
   pgTransaction [
      "ALTER TABLE employee_event ENABLE TRIGGER prevent_update;", []
      "ALTER TABLE account_event ENABLE TRIGGER prevent_update;", []
      "ALTER TABLE organization_event ENABLE TRIGGER prevent_update;", []
   ]

let createOrgs (getOrgRef: OrgId -> IEntityRef<OrgMessage>) =
   let orgs = myOrg :: otherOrgs

   for org in orgs do
      let cmd =
         {
            SubmitOrgOnboardingApplicationCommand.create {
               LegalBusinessName = org.BusinessName
               // TODO: Allow the org to set admin team email
               AdminTeamEmail =
                  $"adminteam@{org.BusinessName}.com"
                  |> _.ToLower().Replace(" ", "")
                  |> Email.deserialize
               OrgId = org.OrgId
               ParentAccountId = org.ParentAccountId
               InitiatedBy = Initiator.System
               EmployerIdentificationNumber = org.EIN
            } with
               Timestamp = DateTime.UtcNow.AddMonths -4
         }
         |> OrgCommand.SubmitOnboardingApplication

      (getOrgRef org.OrgId) <! OrgMessage.StateChange cmd

// Enable other orgs using the platform to be discoverable for
// InternalTransferBetweenOrgs.
let enableOrgSocialTransferDiscovery
   (getOrgRef: OrgId -> IEntityRef<OrgMessage>)
   =
   for org in otherOrgs do
      let cmd =
         ConfigureFeatureFlagCommand.create org.OrgId Initiator.System {
            Config = {
               SocialTransferDiscoveryPrimaryAccountId =
                  Some org.PrimaryAccountId
            }
         }
         |> OrgCommand.ConfigureFeatureFlag

      (getOrgRef org.OrgId) <! OrgMessage.StateChange cmd

// NOTE:
// Initial employee purchase requests are configured with timestamps
// in the past.  However, subsequent employee purchase approval/decline
// in the employee_event table as well as recording of debits in the
// account_event table have current timestamps since we
// don't have control over the creation of system-produced events.
// The same goes for InternalTransferBetweenOrgsDeposited where we have control
// over the timestamp of the initial request but need to modify timestamps
// of the subsequent Deposited events here.
// Modifying system-produced event timestamps via update statements on
// the read models as demonstrated below should be a sufficient strategy
// for observing time based analytics on seed data.
let seedBalanceHistory () = taskResultOption {
   let query =
      $"""
      SELECT {EmployeeEventFields.timestamp}, {EmployeeEventFields.correlationId}
      FROM {EmployeeEventSqlMapper.table}
      WHERE {EmployeeEventFields.name} = 'PurchaseApplied'

      UNION ALL

      SELECT {aeFields.timestamp}, {aeFields.correlationId}
      FROM {AccountEventSqlMapper.table}
      WHERE {aeFields.name} = 'InternalTransferBetweenOrgsPending'

      UNION ALL

      SELECT {OrgEventFields.timestamp}, {OrgEventFields.correlationId}
      FROM {OrganizationEventSqlMapper.table}
      WHERE {OrgEventFields.name} = 'OrgCreated'
      """

   let! initialRequestsToModify =
      pgQuery query None (fun read -> {|
         CorrelationId = EmployeeEventSqlReader.correlationId read
         Timestamp = EmployeeEventSqlReader.timestamp read
      |})

   let sqlParams =
      initialRequestsToModify
      |> List.map (fun o -> [
         "correlationId", EmployeeEventSqlWriter.correlationId o.CorrelationId
         "timestamp",
         EmployeeEventSqlWriter.timestamp (o.Timestamp.AddSeconds 3)
      ])

   let query = [
      $"""
      UPDATE {EmployeeEventSqlMapper.table}
      SET
         {EmployeeEventFields.timestamp} = @timestamp,
         {EmployeeEventFields.event} = jsonb_set(
            {EmployeeEventFields.event},
            '{{1,Timestamp}}',
            to_jsonb(@timestamp),
            false
         )
      WHERE
         {EmployeeEventFields.correlationId} = @correlationId
         AND {EmployeeEventFields.name} <> 'PurchaseApplied';
      """,
      sqlParams

      $"""
      UPDATE {AccountEventSqlMapper.table}
      SET
         {aeFields.timestamp} = @timestamp,
         {aeFields.event} = jsonb_set(
            {aeFields.event},
            '{{1,Timestamp}}',
            to_jsonb(@timestamp),
            false
         )
      WHERE
         {aeFields.correlationId} = @correlationId
         AND {aeFields.name} <> 'InternalTransferBetweenOrgsPending';
      """,
      sqlParams

      $"""
      UPDATE {OrganizationEventSqlMapper.table}
      SET
         {OrgEventFields.timestamp} = @timestamp,
         {OrgEventFields.event} = jsonb_set(
            {OrgEventFields.event},
            '{{1,Timestamp}}',
            to_jsonb(@timestamp),
            false
         )
      WHERE
         {OrgEventFields.correlationId} = @correlationId
         AND {OrgEventFields.name} <> 'OrgCreated';
      """,
      sqlParams
   ]

   let! _ = pgTransaction query |> TaskResult.map Some
   let! res = pgProcedure "seed_balance_history" None |> TaskResult.map Some
   return res
}

let mockAccountOwnerCmd =
   let date = DateTime.Today
   let startOfMonth = DateTime(date.Year, date.Month, 1).ToUniversalTime()

   {
      CreateAccountOwnerCommand.create {
         Email = myOrg.AccountOwnerEmail
         FirstName = myOrg.AccountOwnerName.First
         LastName = myOrg.AccountOwnerName.Last
         OrgId = myOrg.OrgId
         EmployeeId = myOrg.AccountOwnerId
      } with
         Timestamp = startOfMonth.AddMonths -3
   }

let mockAccountOwner: Initiator = {
   Id = InitiatedById myOrg.AccountOwnerId
   Name = "Daniel Eisenbarger"
}

let mockAccounts =
   let orgId = myOrg.OrgId
   let parentAccountId = myOrg.ParentAccountId

   let withModifiedTimestamp (command: CreateAccountCommand) = {
      command with
         Timestamp = mockAccountOwnerCmd.Timestamp
   }

   let cmd1 =
      CreateAccountCommand.create {
         Name = "AP"
         Currency = Currency.USD
         Depository = AccountDepository.Checking
         ParentAccountId = parentAccountId
         AccountId = apCheckingAccountId
         AccountNumber = AccountNumber.generate ()
         OrgId = orgId
         InitiatedBy = mockAccountOwner
      }
      |> withModifiedTimestamp

   let cmd2 =
      CreateAccountCommand.create {
         Name = "AR"
         Currency = Currency.USD
         Depository = AccountDepository.Checking
         ParentAccountId = parentAccountId
         AccountId = arCheckingAccountId
         AccountNumber = AccountNumber.generate ()
         OrgId = orgId
         InitiatedBy = mockAccountOwner
      }
      |> withModifiedTimestamp

   let cmd3 =
      CreateAccountCommand.create {
         Name = "Operations"
         Currency = Currency.USD
         Depository = AccountDepository.Checking
         ParentAccountId = parentAccountId
         AccountId = myOrg.OpsAccountId
         AccountNumber = AccountNumber.generate ()
         OrgId = orgId
         InitiatedBy = mockAccountOwner
      }
      |> withModifiedTimestamp

   let cmd4 =
      CreateAccountCommand.create {
         Name = "Savings"
         Currency = Currency.USD
         Depository = AccountDepository.Savings
         ParentAccountId = parentAccountId
         AccountId = myOrg.SavingsAccountId
         AccountNumber = AccountNumber.generate ()
         OrgId = orgId
         InitiatedBy = mockAccountOwner
      }
      |> withModifiedTimestamp

   let otherOrgs =
      otherOrgs
      |> List.fold
         (fun acc o ->
            acc
            |> List.append [
               o.PrimaryAccountId,
               CreateAccountCommand.create {
                  Name = "AR"
                  Currency = Currency.USD
                  Depository = AccountDepository.Checking
                  ParentAccountId = o.ParentAccountId
                  AccountId = o.PrimaryAccountId
                  AccountNumber = AccountNumber.generate ()
                  OrgId = o.OrgId
                  InitiatedBy = mockAccountOwner
               }
               |> withModifiedTimestamp

               o.OpsAccountId,
               CreateAccountCommand.create {
                  Name = "Operations"
                  Currency = Currency.USD
                  Depository = AccountDepository.Checking
                  ParentAccountId = o.ParentAccountId
                  AccountId = o.OpsAccountId
                  AccountNumber = AccountNumber.generate ()
                  OrgId = o.OrgId
                  InitiatedBy = {
                     Id = InitiatedById o.AccountOwnerId
                     Name =
                        $"{o.AccountOwnerName.First} {o.AccountOwnerName.Last}"
                  }
               }
               |> withModifiedTimestamp

               o.SavingsAccountId,
               CreateAccountCommand.create {
                  Name = "Savings"
                  Currency = Currency.USD
                  Depository = AccountDepository.Savings
                  ParentAccountId = o.ParentAccountId
                  AccountId = o.SavingsAccountId
                  AccountNumber = AccountNumber.generate ()
                  OrgId = o.OrgId
                  InitiatedBy = {
                     Id = InitiatedById o.AccountOwnerId
                     Name =
                        $"{o.AccountOwnerName.First} {o.AccountOwnerName.Last}"
                  }
               }
               |> withModifiedTimestamp
            ])
         []

   Map [
      cmd1.Data.AccountId, cmd1

      cmd2.Data.AccountId, cmd2

      cmd3.Data.AccountId, cmd3

      cmd4.Data.AccountId, cmd4

      yield! otherOrgs
   ]

let accountInitialDeposits =
   Map [
      apCheckingAccountId, 2_500_931m
      myOrg.OpsAccountId, 1_391_100m
      myOrg.SavingsAccountId, 70_000m

      for org in otherOrgs do
         org.PrimaryAccountId, randomAmount 700_000 13_550_003
   ]

let mockAccountOwnerCards =
   let cmd = mockAccountOwnerCmd
   let emId = EmployeeId.fromEntityId mockAccountOwnerCmd.EntityId

   let cardCmd1 = {
      CreateCardCommand.create {
         CardId = Guid.NewGuid() |> CardId
         EmployeeId = emId
         OrgId = myOrg.OrgId
         AccountId = myOrg.OpsAccountId
         PersonName = $"{cmd.Data.FirstName} {cmd.Data.LastName}"
         CardNickname = Some "Travel"
         CardType = CardType.Debit
         Virtual = true
         DailyPurchaseLimit = Some 9_310m
         MonthlyPurchaseLimit = None
         InitiatedBy = mockAccountOwner
      } with
         Timestamp = cmd.Timestamp.AddHours 1
   }

   let cardCmd2 = {
      cardCmd1 with
         Data.CardId = Guid.NewGuid() |> CardId
         Data.CardNickname = Some "Office Supplies"
         Data.DailyPurchaseLimit = None
         Timestamp = cmd.Timestamp.AddHours 1.1
   }

   cardCmd1, cardCmd2

let mockEmployees =
   let cmds = [
      CreateEmployeeCommand.create mockAccountOwner {
         Email = "pongkool@gmail.com"
         FirstName = "Pop"
         LastName = "Pongkool"
         OrgId = myOrg.OrgId
         Role = Role.Admin
         OrgRequiresEmployeeInviteApproval = None
         CardInfo = None
      }
      CreateEmployeeCommand.create mockAccountOwner {
         Email = "fishinthesea@gmail.com"
         FirstName = "Devon"
         LastName = "Eisenbarger"
         OrgId = myOrg.OrgId
         Role = Role.CardOnly
         OrgRequiresEmployeeInviteApproval = None
         CardInfo = None
      }
      CreateEmployeeCommand.create mockAccountOwner {
         Email = "inkwaruntorn@gmail.com"
         FirstName = "Ink"
         LastName = "Waruntorn"
         OrgId = myOrg.OrgId
         Role = Role.CardOnly
         OrgRequiresEmployeeInviteApproval = None
         CardInfo = None
      }
      CreateEmployeeCommand.create mockAccountOwner {
         Email = "hanzzimmer@gmail.com"
         FirstName = "Hanz"
         LastName = "Zimmer"
         OrgId = myOrg.OrgId
         Role = Role.CardOnly
         OrgRequiresEmployeeInviteApproval = None
         CardInfo = None
      }
      CreateEmployeeCommand.create mockAccountOwner {
         Email = "denvau@gmail.com"
         FirstName = "Den"
         LastName = "Vau"
         OrgId = myOrg.OrgId
         Role = Role.Admin
         OrgRequiresEmployeeInviteApproval = None
         CardInfo = None
      }
      CreateEmployeeCommand.create mockAccountOwner {
         Email = "sadeadu@gmail.com"
         FirstName = "Sade"
         LastName = "Adu"
         OrgId = myOrg.OrgId
         Role = Role.CardOnly
         OrgRequiresEmployeeInviteApproval = None
         CardInfo = None
      }
   ]

   let createCard (cmd: CreateEmployeeCommand) = {
      CreateCardCommand.create {
         CardId = Guid.NewGuid() |> CardId
         EmployeeId = EmployeeId.fromEntityId cmd.EntityId
         OrgId = myOrg.OrgId
         AccountId = myOrg.OpsAccountId
         PersonName = $"{cmd.Data.FirstName} {cmd.Data.LastName}"
         CardNickname = Some "Travel"
         CardType = CardType.Debit
         Virtual = true
         DailyPurchaseLimit = Some 10_000m
         MonthlyPurchaseLimit = None
         InitiatedBy = mockAccountOwner
      } with
         Timestamp = cmd.Timestamp.AddHours 1
   }

   Map [
      for cmd in cmds ->
         let cmd = {
            cmd with
               Timestamp = mockAccountOwnerCmd.Timestamp
         }

         EmployeeId.fromEntityId cmd.EntityId, (cmd, createCard cmd)
   ]

let mockEmployeesPendingInviteConfirmation =
   [
      CreateEmployeeCommand.create mockAccountOwner {
         Email = "zikomo@gmail.com"
         FirstName = "Zikomo"
         LastName = "Fwasa"
         OrgId = myOrg.OrgId
         Role = Role.Admin
         OrgRequiresEmployeeInviteApproval = None
         CardInfo = None
      }
      CreateEmployeeCommand.create mockAccountOwner {
         Email = "megmeyers@gmail.com"
         FirstName = "Meg"
         LastName = "Meyers"
         OrgId = myOrg.OrgId
         Role = Role.Scholar
         OrgRequiresEmployeeInviteApproval = None
         CardInfo = None
      }
   ]
   |> List.map (fun cmd -> {
      cmd with
         Timestamp = mockAccountOwnerCmd.Timestamp
   })

let seedPayments
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   =
   task {
      let buffer = DateTime.UtcNow.AddDays(-2)

      // Payment requests from main demo org to other orgs
      let requestsFromDemoAccount = [
         for payer in paymentPayers ->
            let cmd =
               RequestPlatformPaymentCommand.create mockAccountOwner {
                  Memo = "Services rendered..."
                  Expiration = None
                  BaseInfo = {
                     InitiatedById = mockAccountOwner.Id
                     Id = Guid.NewGuid() |> PaymentId
                     Amount = randomAmount 3000 5000
                     Payee = {
                        OrgId = myOrg.OrgId
                        OrgName = myOrg.BusinessName
                        AccountId = arCheckingAccountId
                        ParentAccountId = myOrg.ParentAccountId
                     }
                     Payer = {
                        OrgId = payer.OrgId
                        OrgName = payer.BusinessName
                        ParentAccountId = payer.ParentAccountId
                     }
                  }
               }

            payer, { cmd with Timestamp = buffer }
      ]

      for _, request in requestsFromDemoAccount do
         let msg =
            request
            |> AccountCommand.RequestPlatformPayment
            |> AccountMessage.StateChange

         let payee = request.Data.BaseInfo.Payee

         (getAccountRef payee.ParentAccountId) <! msg

      do! Async.Sleep 2000

      // Some payment requests fulfilled
      let payer, request = List.head requestsFromDemoAccount
      let info = request.Data.BaseInfo

      let msg =
         {
            FulfillPlatformPaymentCommand.create
               {
                  Id = InitiatedById payer.AccountOwnerId
                  Name = payer.BusinessName
               }
               {
                  RequestedPayment = {
                     Memo = request.Data.Memo
                     Expiration =
                        request
                        |> RequestPlatformPaymentCommand.toEvent
                        |> Result.toValueOption
                        |> fun p -> p.Value.Data.Expiration
                     BaseInfo = info
                  }
                  PaymentMethod = PaymentMethod.Platform payer.PrimaryAccountId
               } with
               Timestamp = buffer.AddHours 2
         }
         |> AccountCommand.FulfillPlatformPayment
         |> AccountMessage.StateChange

      (getAccountRef payer.ParentAccountId) <! msg

      // Payment requests from other orgs to main demo org
      for payee in paymentRequesters do
         let cmd =
            RequestPlatformPaymentCommand.create
               {
                  Id = InitiatedById payee.AccountOwnerId
                  Name = payee.BusinessName
               }
               {
                  Expiration = Some <| DateTime.UtcNow.AddDays 13
                  Memo = "Services rendered..."
                  BaseInfo = {
                     InitiatedById = InitiatedById payee.AccountOwnerId
                     Id = Guid.NewGuid() |> PaymentId
                     Amount = 5000m + randomAmount 1000 3000
                     Payee = {
                        OrgId = payee.OrgId
                        OrgName = payee.BusinessName
                        AccountId = payee.PrimaryAccountId
                        ParentAccountId = payee.ParentAccountId
                     }
                     Payer = {
                        OrgId = myOrg.OrgId
                        OrgName = myOrg.BusinessName
                        ParentAccountId = myOrg.ParentAccountId
                     }
                  }
               }

         let msg =
            cmd
            |> AccountCommand.RequestPlatformPayment
            |> AccountMessage.StateChange

         (getAccountRef payee.ParentAccountId) <! msg
   }

let configureAutoTransferRules
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (timestamp: DateTime)
   =
   // Set up percent distribution rule for demo account
   // ----------------------------------------
   let sender: InternalTransferSender = {
      Name = mockAccounts[arCheckingAccountId].Data.Name
      AccountId = arCheckingAccountId
      ParentAccountId = myOrg.ParentAccountId
      OrgId = myOrg.OrgId
   }

   let percentDistributionRule =
      PercentDistributionRule.create
         AutomaticTransfer.Frequency.PerTransaction
         sender
         [
            let recipient = mockAccounts[myOrg.OpsAccountId]

            {
               ProposedPercentAllocated = 80m
               Recipient = {
                  OrgId = recipient.OrgId
                  AccountId = recipient.Data.AccountId
                  ParentAccountId = recipient.Data.ParentAccountId
                  Name = recipient.Data.Name
               }
            }

            let recipient = mockAccounts[myOrg.SavingsAccountId]

            {
               ProposedPercentAllocated = 20m
               Recipient = {
                  OrgId = recipient.OrgId
                  AccountId = recipient.Data.AccountId
                  ParentAccountId = recipient.Data.ParentAccountId
                  Name = recipient.Data.Name
               }
            }
         ]
      |> Result.toValueOption
      |> _.Value

   let msg =
      {
         ConfigureAutoTransferRuleCommand.create
            (sender.ParentAccountId, sender.OrgId)
            mockAccountOwner
            {
               AccountId = sender.AccountId
               RuleIdToUpdate = None
               Rule =
                  percentDistributionRule
                  |> AutomaticTransferRule.PercentDistribution
            } with
            Timestamp = timestamp
      }
      |> AccountCommand.ConfigureAutoTransferRule
      |> AccountMessage.StateChange

   (getAccountRef sender.ParentAccountId) <! msg

   // Set up zero balance rule for demo account
   // ----------------------------------------
   let sender: InternalTransferSender = {
      Name = mockAccounts[apCheckingAccountId].Data.Name
      AccountId = apCheckingAccountId
      ParentAccountId = myOrg.ParentAccountId
      OrgId = myOrg.OrgId
   }

   let recipient = mockAccounts[myOrg.SavingsAccountId]

   let msg =
      {
         ConfigureAutoTransferRuleCommand.create
            (sender.ParentAccountId, sender.OrgId)
            mockAccountOwner
            {
               AccountId = sender.AccountId
               RuleIdToUpdate = None
               Rule =
                  AutomaticTransferRule.ZeroBalance {
                     Sender = sender
                     Recipient = {
                        OrgId = recipient.OrgId
                        AccountId = recipient.Data.AccountId
                        ParentAccountId = recipient.Data.ParentAccountId
                        Name = recipient.Data.Name
                     }
                  }
            } with
            Timestamp = timestamp
      }
      |> AccountCommand.ConfigureAutoTransferRule
      |> AccountMessage.StateChange

   (getAccountRef sender.ParentAccountId) <! msg

   // Set up target balance rule for demo savings account
   // ----------------------------------------
   let target: BiDirectionalTransferContact = {
      Name = mockAccounts[myOrg.SavingsAccountId].Data.Name
      AccountId = myOrg.SavingsAccountId
      ParentAccountId = myOrg.ParentAccountId
      OrgId = myOrg.OrgId
   }

   let partner = mockAccounts[myOrg.OpsAccountId]

   let msg =
      {
         ConfigureAutoTransferRuleCommand.create
            (target.ParentAccountId, target.OrgId)
            mockAccountOwner
            {
               AccountId = target.AccountId
               RuleIdToUpdate = None
               Rule =
                  AutomaticTransferRule.TargetBalance {
                     TargetAccount = target
                     TargetAccountBalance =
                        PositiveAmount.create 40_000m
                        |> Result.toOption
                        |> _.Value
                     TargetBalanceRange =
                        Some {
                           LowerBound =
                              PositiveAmount.create 30_000m
                              |> Result.toOption
                              |> _.Value
                           UpperBound =
                              PositiveAmount.create 50_000m
                              |> Result.toOption
                              |> _.Value
                        }
                     ManagingPartnerAccount = {
                        OrgId = partner.OrgId
                        AccountId = partner.Data.AccountId
                        ParentAccountId = partner.Data.ParentAccountId
                        Name = partner.Data.Name
                     }
                  }
            } with
            Timestamp = timestamp
      }
      |> AccountCommand.ConfigureAutoTransferRule
      |> AccountMessage.StateChange

   (getAccountRef target.ParentAccountId) <! msg

   // Create auto transfer rules for other orgs
   // ---------------------------------------
   let initZeroBalanceRule (candidate: OrgSetup) =
      let sender: InternalTransferSender = {
         OrgId = candidate.OrgId
         AccountId = candidate.PrimaryAccountId
         ParentAccountId = candidate.ParentAccountId
         Name = candidate.BusinessName
      }

      let recipient = mockAccounts[candidate.OpsAccountId]

      let msg =
         {
            ConfigureAutoTransferRuleCommand.create
               (sender.ParentAccountId, sender.OrgId)
               {
                  Id = InitiatedById candidate.AccountOwnerId
                  Name = candidate.BusinessName
               }
               {
                  AccountId = sender.AccountId
                  RuleIdToUpdate = None
                  Rule =
                     AutomaticTransferRule.ZeroBalance {
                        Sender = sender
                        Recipient = {
                           OrgId = recipient.OrgId
                           AccountId = recipient.Data.AccountId
                           ParentAccountId = recipient.Data.ParentAccountId
                           Name = recipient.Data.Name
                        }
                     }
               } with
               Timestamp = timestamp
         }
         |> AccountCommand.ConfigureAutoTransferRule
         |> AccountMessage.StateChange

      (getAccountRef sender.ParentAccountId) <! msg

   socialTransferCandidates |> List.iter initZeroBalanceRule

let seedAccountOwnerActions
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (timestamp: DateTime)
   (account: Account)
   =
   let domesticRecipientCmd =
      RegisterDomesticTransferRecipientCommand.create mockAccountOwner {
         AccountId = Guid.NewGuid() |> AccountId
         Sender = {|
            OrgId = myOrg.OrgId
            ParentAccountId = myOrg.ParentAccountId
         |}
         FirstName = "Microsoft"
         LastName = "Azure"
         AccountNumber = AccountNumber.generate ()
         RoutingNumber = "123456789"
         Depository = DomesticRecipientAccountDepository.Checking
         PaymentNetwork = PaymentNetwork.ACH
      }

   let domesticRecipient =
      domesticRecipientCmd
      |> RegisterDomesticTransferRecipientCommand.toEvent
      |> Result.map (fun evt -> evt.Data.Recipient)
      |> Result.toValueOption
      |> _.Value

   let msg =
      domesticRecipientCmd
      |> ParentAccountCommand.RegisterDomesticTransferRecipient
      |> AccountCommand.ParentAccount
      |> AccountMessage.StateChange

   getAccountRef myOrg.ParentAccountId <! msg

   let accountRef = getAccountRef myOrg.ParentAccountId

   for month in [ 1..3 ] do
      let timestamp =
         if month = 3 then
            timestamp.AddMonths(month).AddDays(-2)
         else
            timestamp.AddMonths month

      let transferCmd = {
         DomesticTransferCommand.create
            (Guid.NewGuid() |> CorrelationId)
            mockAccountOwner
            {
               OriginatedFromSchedule = false
               ScheduledDateSeedOverride = Some timestamp
               Amount = 30_000m + (randomAmount 1000 7000)
               Recipient = domesticRecipient
               Sender = {
                  Name = account.Name
                  AccountNumber = account.AccountNumber
                  RoutingNumber = account.RoutingNumber
                  AccountId = account.AccountId
                  OrgId = myOrg.OrgId
                  ParentAccountId = myOrg.ParentAccountId
               }
               Memo = Some "Azure Bill"
            } with
            Timestamp = timestamp
      }

      let msg =
         transferCmd
         |> AccountCommand.DomesticTransfer
         |> AccountMessage.StateChange

      accountRef <! msg

      for num in [ 1..3 ] do
         let maxDays =
            let daysToAdd = num * (5 + num)

            if month = 3 then
               let buffer = DateTime.UtcNow.AddDays(-2).Day
               if buffer / daysToAdd >= 1 then daysToAdd else 0
            else
               daysToAdd

         if maxDays = 0 && num > 1 then
            ()
         else
            let msg =
               {
                  DepositCashCommand.create
                     (myOrg.ParentAccountId, myOrg.OrgId)
                     mockAccountOwner
                     {
                        AccountId = apCheckingAccountId
                        Amount = randomAmount 5000 20_000
                        Origin = Some "ATM"
                     } with
                     Timestamp = timestamp.AddDays(float maxDays)
               }
               |> AccountCommand.DepositCash
               |> AccountMessage.StateChange

            accountRef <! msg

            let ind = int (randomAmount 0 (socialTransferSenders.Length - 1))
            let sender = socialTransferSenders |> List.item ind

            // Transfers from other orgs on the platform to the primary demo org
            let msg =
               let ts = timestamp.AddDays(float (maxDays - 1))

               {
                  InternalTransferBetweenOrgsCommand.create
                     {
                        Name = sender.BusinessName
                        Id = InitiatedById sender.AccountOwnerId
                     }
                     {
                        Memo = None
                        OriginatedFromSchedule = false
                        Amount = 10_000m + randomAmount 1000 10_000
                        Recipient = {
                           OrgId = myOrg.OrgId
                           AccountId = apCheckingAccountId
                           ParentAccountId = myOrg.ParentAccountId
                           Name = myOrg.BusinessName
                        }
                        ScheduledDateSeedOverride = Some ts
                        Sender = {
                           Name = sender.BusinessName
                           AccountId = sender.PrimaryAccountId
                           ParentAccountId = sender.ParentAccountId
                           OrgId = sender.OrgId
                        }
                     } with
                     Timestamp = ts
               }
               |> AccountCommand.InternalTransferBetweenOrgs
               |> AccountMessage.StateChange

            (getAccountRef sender.ParentAccountId) <! msg

            let timestamp = timestamp.AddDays(float (maxDays - 1))

            // Transfers from primary demo org to other orgs on the platform
            let msg =
               {
                  InternalTransferBetweenOrgsCommand.create mockAccountOwner {
                     Memo = None
                     OriginatedFromSchedule = false
                     Recipient =
                        let ind =
                           randomAmount 0 (socialTransferCandidates.Length - 1)
                           |> int

                        let recipient =
                           socialTransferCandidates |> List.item ind

                        {
                           OrgId = recipient.OrgId
                           ParentAccountId = recipient.ParentAccountId
                           AccountId = recipient.PrimaryAccountId
                           Name = recipient.BusinessName
                        }
                     Amount = 3000m + randomAmount 1000 8000
                     ScheduledDateSeedOverride = Some timestamp
                     Sender = {
                        Name = myOrg.BusinessName
                        ParentAccountId = myOrg.ParentAccountId
                        AccountId = myOrg.OpsAccountId
                        OrgId = myOrg.OrgId
                     }
                  } with
                     Timestamp = timestamp
               }
               |> AccountCommand.InternalTransferBetweenOrgs
               |> AccountMessage.StateChange

            (getAccountRef myOrg.ParentAccountId) <! msg

            let msg =
               {
                  InternalTransferWithinOrgCommand.create mockAccountOwner {
                     Memo = None
                     OriginatedFromSchedule = false
                     Recipient =
                        let recipient = mockAccounts[apCheckingAccountId]

                        {
                           OrgId = recipient.OrgId
                           AccountId = recipient.Data.AccountId
                           ParentAccountId = recipient.Data.ParentAccountId
                           Name = recipient.Data.Name
                        }
                     Amount = 2000m + randomAmount 1000 2000
                     ScheduledDateSeedOverride = Some timestamp
                     Sender = {
                        Name = mockAccounts[myOrg.OpsAccountId].Data.Name
                        AccountId = myOrg.OpsAccountId
                        ParentAccountId = myOrg.ParentAccountId
                        OrgId = myOrg.OrgId
                     }
                  } with
                     Timestamp = timestamp
               }
               |> AccountCommand.InternalTransfer
               |> AccountMessage.StateChange

            (getAccountRef myOrg.ParentAccountId) <! msg

let seedEmployeeActions
   (card: Card)
   (employee: Employee)
   (employeeRef: IEntityRef<EmployeeMessage>)
   (timestamp: DateTime)
   =
   let purchaseMerchants = [
      [ "Cozy Hotel"; "Trader Joe's"; "In N Out"; "Lyft" ]
      [
         "Barn House BBQ and Beer"
         "Nem nướng Happy Belly"
         "Nhà Lồng Coffee"
         "Cơm Chay All Day"
         "Chickpea Eatery"
         "Pho Number 1"
         "Big C"
         "Grab"
         "Thai Spice ++"
         "ร้าน บ้านไร่ยามเย็น"
         "GoGym"
         "Coffee Window @ 14 Soi 7"
         "ร้านอาหารทับริมธาร"
      ]
      [
         "Lidl"
         "Coop"
         "Carrefour"
         "Baita Resch"
         "Mercato di Mezzo"
         "La Locanda Dei Grulli"
         "Rosso Vivo Pizzeria Verace"
         "CAFFE' GM S.R.L."
         "Pizzeria Via Cassia"
         "Cantina Tramin"
         "La Mora Viola (gelateria artigianale)"
      ]
   ]

   let rnd = new Random()

   for month in [ 1..3 ] do
      let timestamp = timestamp.AddMonths month
      let purchaseMerchants = purchaseMerchants[month - 1]

      let maxPurchases =
         if month = 3 then
            DateTime.UtcNow.Day
         else
            DateTime.DaysInMonth(timestamp.Year, timestamp.Month)

      for purchaseNum in [ 1..maxPurchases ] do
         let maxDays =
            if month = 3 then
               let today = DateTime.UtcNow
               if today.Day = 1 then None else Some(today.AddDays(-2).Day)
            else
               Some(DateTime.DaysInMonth(timestamp.Year, timestamp.Month) - 1)

         let purchaseDate =
            match (month = 3 && purchaseNum > maxPurchases - 1), maxDays with
            | false, Some days -> timestamp.AddDays(float (randomAmount 0 days))
            | false, None -> timestamp
            | true, _ -> DateTime.UtcNow

         let initiator: Initiator = {
            Id = InitiatedById employee.EmployeeId
            Name = employee.Name
         }

         let correlationId = CorrelationId(Guid.NewGuid())

         let purchaseCmd = {
            PurchaseCommand.create {
               CorrelationId = correlationId
               OrgId = employee.OrgId
               EmployeeId = employee.EmployeeId
               InitiatedBy = initiator
               ParentAccountId = myOrg.ParentAccountId
               AccountId = card.AccountId
               CardId = card.CardId
               CardNumberLast4 = card.CardNumberLast4
               EmployeeName = employee.Name
               EmployeeEmail = employee.Email
               Date = purchaseDate
               Amount = randomAmount 50 333
               Merchant =
                  purchaseMerchants[rnd.Next(0, purchaseMerchants.Length)]
               Reference = None
               CardNetworkTransactionId = Guid.NewGuid()
            } with
               Timestamp = purchaseDate
         }

         let msg =
            purchaseCmd
            |> EmployeeCommand.Purchase
            |> EmployeeMessage.StateChange

         employeeRef <! msg

let createAccountOwners getEmployeeRef =
   let createAccountOwnerCmd (business: OrgSetup) =
      let date = DateTime.Today
      let startOfMonth = DateTime(date.Year, date.Month, 1).ToUniversalTime()
      let ts = startOfMonth.AddMonths -3

      {
         CreateAccountOwnerCommand.create {
            Email = business.AccountOwnerEmail
            FirstName = business.AccountOwnerName.First
            LastName = business.AccountOwnerName.Last
            OrgId = business.OrgId
            EmployeeId = business.AccountOwnerId
         } with
            Timestamp = ts
      }

   let otherAccountOwners = otherOrgs |> List.map createAccountOwnerCmd

   for cmd in mockAccountOwnerCmd :: otherAccountOwners do
      let employeeId = cmd.Data.EmployeeId

      let createMsg =
         cmd
         |> EmployeeCommand.CreateAccountOwner
         |> EmployeeMessage.StateChange

      let confirmInviteCmd =
         ConfirmInvitationCommand.create cmd.InitiatedBy cmd.OrgId {
            Email = Email.deserialize cmd.Data.Email
            Reference = None
            AuthProviderUserId = Guid.NewGuid()
         }
         |> EmployeeCommand.ConfirmInvitation
         |> EmployeeMessage.StateChange

      let employeeRef = getEmployeeRef employeeId
      employeeRef <! createMsg
      employeeRef <! confirmInviteCmd

let createEmployees
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   =
   for employeeCreateCmd in mockEmployeesPendingInviteConfirmation do
      let employeeRef =
         getEmployeeRef (EmployeeId.fromEntityId employeeCreateCmd.EntityId)

      let msg =
         employeeCreateCmd
         |> EmployeeCommand.CreateEmployee
         |> EmployeeMessage.StateChange

      employeeRef <! msg

   for employeeId, (employeeCreateCmd, _) in Map.toSeq mockEmployees do
      let employeeRef = getEmployeeRef employeeId

      let msg =
         employeeCreateCmd
         |> EmployeeCommand.CreateEmployee
         |> EmployeeMessage.StateChange

      employeeRef <! msg

      let confirmInviteCmd = {
         ConfirmInvitationCommand.create
            employeeCreateCmd.InitiatedBy
            myOrg.OrgId
            {
               Email = Email.deserialize employeeCreateCmd.Data.Email
               AuthProviderUserId = Guid.NewGuid()
               Reference = None
            } with
            Timestamp = employeeCreateCmd.Timestamp.AddHours 1
      }

      let msg =
         confirmInviteCmd
         |> EmployeeCommand.ConfirmInvitation
         |> EmployeeMessage.StateChange

      employeeRef <! msg

let createEmployeeCards
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   =
   let accountOwnerTravelCardCreateCmd, accountOwnerBusinessCardCreateCmd =
      mockAccountOwnerCards

   let employeeCardCreateCmds =
      mockEmployees.Values |> Seq.toList |> List.map snd

   let cardCreateCmds =
      [ accountOwnerBusinessCardCreateCmd; accountOwnerTravelCardCreateCmd ]
      @ employeeCardCreateCmds

   for cmd in cardCreateCmds do
      let employeeRef = getEmployeeRef (EmployeeId.fromEntityId cmd.EntityId)

      let msg = cmd |> EmployeeCommand.CreateCard |> EmployeeMessage.StateChange

      employeeRef <! msg

   {|
      AccountOwnerTravelCard = accountOwnerTravelCardCreateCmd
      AccountOwnerBusinessCard = accountOwnerBusinessCardCreateCmd
      Employee = employeeCardCreateCmds
   |}

let getEmployeeCardPair
   (employeeRef: IEntityRef<EmployeeMessage>)
   (cardId: CardId)
   =
   async {
      let! (employeeOpt: Employee option) =
         employeeRef <? EmployeeMessage.GetEmployee

      return option {
         let! employee = employeeOpt
         let! card = employee.Cards.TryFind cardId
         return employee, card
      }
   }

let seedAccountTransactions
   (mailbox: Actor<AccountSeederMessage>)
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   (command: CreateAccountCommand)
   =
   task {
      let accountId = command.Data.AccountId
      let compositeId = command.Data.ParentAccountId, command.OrgId
      let accountRef = getAccountRef command.Data.ParentAccountId

      let timestamp = command.Timestamp.AddHours 2

      match Map.tryFind accountId accountInitialDeposits with
      | Some depositAmount ->
         let msg =
            {
               DepositCashCommand.create compositeId mockAccountOwner {
                  AccountId = command.Data.AccountId
                  Amount = depositAmount
                  Origin = Some "ATM"
               } with
                  Timestamp = timestamp
            }
            |> AccountCommand.DepositCash
            |> AccountMessage.StateChange

         accountRef <! msg
      | None -> ()

      if accountId = myOrg.OpsAccountId then
         createEmployees getEmployeeRef
         do! Task.Delay 10_000
         let cardCreateCmds = createEmployeeCards getEmployeeRef
         do! Task.Delay 10_000

         let businessCardCreateCmd = cardCreateCmds.AccountOwnerBusinessCard
         let timestamp = businessCardCreateCmd.Timestamp

         configureAutoTransferRules getAccountRef timestamp

         let! account = accountRef <? AccountMessage.GetVirtualAccount accountId

         match account with
         | None ->
            let accountOwnerId = businessCardCreateCmd.EntityId

            logError
               mailbox
               $"Can not proceed with account owner actions - eId: {accountOwnerId}"
         | Some account ->
            seedAccountOwnerActions getAccountRef timestamp account

         for cmd in
            cardCreateCmds.AccountOwnerTravelCard :: cardCreateCmds.Employee do
            let employeeId = EmployeeId.fromEntityId cmd.EntityId
            let cardId = cmd.Data.CardId
            let employeeRef = getEmployeeRef employeeId
            let! employeeCardPairOpt = getEmployeeCardPair employeeRef cardId

            match employeeCardPairOpt with
            | None ->
               logError
                  mailbox
                  $"Can not proceed with purchases - eId: {employeeId} cId: {cardId}"
            | Some(employee, card) ->
               seedEmployeeActions card employee employeeRef timestamp

         do! seedPayments getAccountRef
   }

// Creates a new Map consisting of initial state of accounts to create
// minus accounts created thus far.
[<TailCall>]
let rec getRemainingAccountsToCreate
   (createdAccounts: Account list)
   (accountsToCreate: CreateAccountsMap)
   : CreateAccountsMap
   =
   match createdAccounts with
   | [] -> accountsToCreate
   | [ account ] -> Map.remove account.AccountId accountsToCreate
   | account :: rest ->
      getRemainingAccountsToCreate rest
      <| Map.remove account.AccountId accountsToCreate

let private scheduleVerification
   (ctx: Actor<AccountSeederMessage>)
   (seconds: float)
   =
   ctx.Schedule
      (TimeSpan.FromSeconds seconds)
      ctx.Self
      AccountSeederMessage.VerifyAccountsCreated
   |> ignore

let private scheduleInitialization (ctx: Actor<AccountSeederMessage>) =
   ctx.Schedule
      (TimeSpan.FromSeconds 2.)
      ctx.Self
      AccountSeederMessage.SeedAccounts
   |> ignore

let private getVerifiedAccounts (accountsToCreate: CreateAccountsMap) = async {
   let! lookupResults =
      accountsToCreate.Keys |> List.ofSeq |> getAccountsByIds |> Async.ofTask

   return
      lookupResults
      |> Result.toOption
      |> Option.flatten
      |> Option.defaultValue []
}

let private initState = {
   Status = AwaitingClusterUp
   AccountsToCreate = mockAccounts
   AccountsToSeedWithTransactions =
      Map [ for acctId in mockAccounts.Keys -> acctId, true ]
}

let actorProps
   (getOrgRef: OrgId -> IEntityRef<OrgMessage>)
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (getEmployeeRef: EmployeeId -> IEntityRef<EmployeeMessage>)
   =
   let handler (ctx: Actor<AccountSeederMessage>) =
      let logInfo = logInfo ctx

      let rec loop (state: State) = actor {
         let! msg = ctx.Receive()

         match msg with
         | AccountSeederMessage.SeedAccounts ->
            if state.Status <> AwaitingClusterUp then
               logInfo "Account seed already initialized"
            else if not (Cluster.Get(ctx.System).IsUp) then
               logInfo "Cluster not up yet.  Retry account seeding momentarily."
               scheduleInitialization ctx
            else
               let! verified = getVerifiedAccounts state.AccountsToCreate

               if verified.Length = state.AccountsToCreate.Count then
                  return!
                     loop {
                        state with
                           Status = FinishedSeeding
                           AccountsToCreate = Map.empty
                     }
               else
                  logInfo "Seed postgres with accounts"
                  do! disableUpdatePreventionTriggers ()

                  createOrgs getOrgRef

                  do! Task.Delay 10_000

                  createAccountOwners getEmployeeRef

                  do! Task.Delay 10_000

                  let remaining =
                     getRemainingAccountsToCreate
                        verified
                        state.AccountsToCreate

                  for command in remaining.Values do
                     let accountRef = getAccountRef command.Data.ParentAccountId

                     let msg =
                        command
                        |> AccountCommand.CreateAccount
                        |> AccountMessage.StateChange

                     accountRef <! msg

                  scheduleVerification ctx 5.

                  return!
                     loop {
                        state with
                           Status = AwaitingVerification
                     }
         | AccountSeederMessage.VerifyAccountsCreated ->
            let! verified = getVerifiedAccounts state.AccountsToCreate

            let accountsToSeedWithTxns, seeded =
               verified
               |> List.partition (fun account ->
                  state.AccountsToSeedWithTransactions[account.AccountId])

            for acct in accountsToSeedWithTxns do
               do!
                  seedAccountTransactions
                     ctx
                     getAccountRef
                     getEmployeeRef
                     state.AccountsToCreate[acct.AccountId]

               ()

            let txnsSeedMap =
               accountsToSeedWithTxns @ seeded
               |> List.fold
                     (fun acc account -> acc |> Map.add account.AccountId false)
                     state.AccountsToSeedWithTransactions

            if verified.Length = state.AccountsToCreate.Count then
               logInfo "Finished seeding accounts"

               enableOrgSocialTransferDiscovery getOrgRef

               logInfo "Enabled social transfer discovery"

               do! Task.Delay 50_000

               let! res = seedBalanceHistory ()
               do! enableUpdatePreventionTriggers ()

               match res with
               | Ok(Some _) ->
                  logInfo "Seeded balance history"

                  return!
                     loop {
                        Status = FinishedSeeding
                        AccountsToCreate = Map.empty
                        AccountsToSeedWithTransactions = txnsSeedMap
                     }
               | Ok None ->
                  logError ctx "Error seeding balance history.  No purchases."
                  return unhandled ()
               | Error err ->
                  logError ctx $"Error seeding balance history {err}"
                  return unhandled ()
            else
               let remaining =
                  getRemainingAccountsToCreate verified state.AccountsToCreate

               logInfo $"{remaining.Count} accounts need to finish seeding"

               scheduleVerification ctx 3.

               return!
                  loop {
                     Status = AwaitingVerification
                     AccountsToCreate = remaining
                     AccountsToSeedWithTransactions = txnsSeedMap
                  }
      }

      loop initState

   props handler

let get (registry: IActorRegistry) : IActorRef<AccountSeederMessage> =
   typed <| registry.Get<ActorMetadata.AccountSeederMarker>()
