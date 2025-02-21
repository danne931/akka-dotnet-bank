module Bank.History.Api

open System

open Lib.Postgres
open Lib.SharedTypes
open Bank.Org.Domain
open Bank.Employee.Domain
open Bank.Account.Domain
open Bank.Transfer.Domain
open CommandApproval

module Fields = EmployeeEventSqlMapper.EmployeeEventFields
module Reader = EmployeeEventSqlMapper.EmployeeEventSqlReader
module Writer = EmployeeEventSqlMapper.EmployeeEventSqlWriter

let orgEventFilterNames (filters: OrgEventGroupFilter list) : string array =
   filters
   |> List.fold
      (fun acc e ->
         acc
         @ match e with
           | OrgEventGroupFilter.Onboarding -> [
              typeof<OrgCreated>.Name
              typeof<OrgOnboardingFinished>.Name
             ]
           | OrgEventGroupFilter.FeatureFlagConfigured -> [
              typeof<FeatureFlagConfigured>.Name
             ]
           | OrgEventGroupFilter.CommandApprovalRule -> [
              typeof<CommandApprovalRule.ConfigureApprovalRule>.Name
              typeof<CommandApprovalRule.ApprovalRuleDeleted>.Name
             ]
           | OrgEventGroupFilter.CommandApprovalProgress -> [
              typeof<CommandApprovalProgress.CommandApprovalRequested>.Name
              typeof<CommandApprovalProgress.CommandApprovalAcquired>.Name
              typeof<CommandApprovalProgress.CommandApprovalDeclined>.Name
              typeof<CommandApprovalProgress.CommandApprovalTerminated>.Name
              typeof<CommandApprovalProgress.CommandApprovalProcessCompleted>
                 .Name
             ]
           | OrgEventGroupFilter.DomesticTransferRecipient -> [
              typeof<RegisteredDomesticTransferRecipient>.Name
              typeof<EditedDomesticTransferRecipient>.Name
              typeof<NicknamedDomesticTransferRecipient>.Name
              typeof<DomesticTransferRetryConfirmsRecipientCommand>.Name
              typeof<DomesticTransferRecipientFailed>.Name
             ])
      []
   |> List.toArray

let accountEventFilterNames
   (filters: TransactionGroupFilter list)
   : string array
   =
   filters
   |> List.fold
      (fun acc e ->
         acc
         @ match e with
           | TransactionGroupFilter.Purchase -> [ typeof<DebitedAccount>.Name ]
           | TransactionGroupFilter.Deposit -> [ typeof<DepositedCash>.Name ]
           | TransactionGroupFilter.InternalTransferWithinOrg -> [
              typeof<InternalTransferWithinOrgPending>.Name
              typeof<InternalTransferWithinOrgCompleted>.Name
              typeof<InternalTransferWithinOrgFailed>.Name
              typeof<InternalTransferWithinOrgDeposited>.Name
             ]
           | TransactionGroupFilter.InternalTransferBetweenOrgs -> [
              typeof<InternalTransferBetweenOrgsPending>.Name
              typeof<InternalTransferBetweenOrgsCompleted>.Name
              typeof<InternalTransferBetweenOrgsFailed>.Name
              typeof<InternalTransferBetweenOrgsDeposited>.Name
             ]
           | TransactionGroupFilter.InternalAutomatedTransfer -> [
              typeof<InternalAutomatedTransferPending>.Name
              typeof<InternalAutomatedTransferCompleted>.Name
              typeof<InternalAutomatedTransferFailed>.Name
              typeof<InternalAutomatedTransferDeposited>.Name
             ]
           | TransactionGroupFilter.DomesticTransfer -> [
              typeof<DomesticTransferPending>.Name
              typeof<DomesticTransferCompleted>.Name
              typeof<DomesticTransferFailed>.Name
              typeof<DomesticTransferProgressUpdate>.Name
             ]
           | TransactionGroupFilter.PlatformPayment -> [
              typeof<PlatformPaymentRequested>.Name
              typeof<PlatformPaymentPaid>.Name
              typeof<PlatformPaymentDeposited>.Name
              typeof<PlatformPaymentDeclined>.Name
              typeof<PlatformPaymentCancelled>.Name
             ])
      []
   |> List.toArray

let employeeEventFilterNames
   (filters: EmployeeEventGroupFilter list)
   : string array
   =
   filters
   |> List.fold
      (fun acc e ->
         acc
         @ match e with
           | EmployeeEventGroupFilter.Invitation -> [
              typeof<CreatedEmployee>.Name
              typeof<InvitationConfirmed>.Name
              typeof<InvitationCancelled>.Name
             ]
           | EmployeeEventGroupFilter.CardFrozenUnfrozen -> [
              typeof<LockedCard>.Name
              typeof<UnlockedCard>.Name
             ]
           | EmployeeEventGroupFilter.AccessRestored -> [
              typeof<AccessRestored>.Name
             ]
           | EmployeeEventGroupFilter.Purchase -> [
              typeof<PurchasePending>.Name
              typeof<PurchaseConfirmedByAccount>.Name
              typeof<PurchaseRejectedByAccount>.Name
             ]
           | EmployeeEventGroupFilter.CreatedCard -> [
              typeof<CreatedCard>.Name
             ]
           | EmployeeEventGroupFilter.UpdatedRole -> [
              typeof<RoleUpdated>.Name
             ]
           | EmployeeEventGroupFilter.PurchaseLimitUpdated -> [
              typeof<DailyDebitLimitUpdated>.Name
              typeof<MonthlyDebitLimitUpdated>.Name
             ])
      []
   |> List.toArray

type private HistoryWhere = {
   Employee: string
   Account: string
   Org: string
}

let getHistory (orgId: OrgId) (query: HistoryQuery) =
   let employeeTable = EmployeeSqlMapper.table
   let employeeEventTable = EmployeeEventSqlMapper.table
   let accountEventTable = TransactionSqlMapper.table
   let orgEventTable = OrganizationEventSqlMapper.table
   let limit = 50

   let query =
      match
         query.OrgEventType, query.AccountEventType, query.EmployeeEventType
      with
      | None, None, None -> {
         query with
            OrgEventType = Some OrgEventGroupFilter.All
            AccountEventType = Some TransactionGroupFilter.All
            EmployeeEventType = Some EmployeeEventGroupFilter.All
        }
      | _ -> query

   let where = {
      Employee = $"{employeeEventTable}.{Fields.orgId} = @orgId"
      Account = $"{accountEventTable}.{Fields.orgId} = @orgId"
      Org = $"{orgEventTable}.{Fields.orgId} = @orgId"
   }

   let queryParams = [
      "orgId", Writer.orgId orgId
      "offset", Sql.int <| Math.Max(query.Page - 1, 0) * limit
   ]

   let agg =
      Option.fold
         (fun (queryParams, where) initiatedByIds ->
            [ "iIds", Writer.initiatedByIds initiatedByIds ] @ queryParams,
            {
               Employee =
                  $"{where.Employee} AND {employeeEventTable}.{Fields.initiatedById} = ANY(@iIds)"
               Account =
                  $"{where.Account} AND {accountEventTable}.{Fields.initiatedById} = ANY(@iIds)"
               Org =
                  $"{where.Org} AND {orgEventTable}.{Fields.initiatedById} = ANY(@iIds)"
            })
         (queryParams, where)
         query.InitiatedByIds

   let agg =
      Option.fold
         (fun (queryParams, where) (startDate, endDate) ->
            let queryParams =
               [
                  "start", Writer.timestamp startDate
                  "end", Writer.timestamp endDate
               ]
               @ queryParams

            let timestampQuery = "timestamp >= @start AND timestamp <= @end"

            queryParams,
            {
               Employee = $"{where.Employee} AND {timestampQuery}"
               Account = $"{where.Account} AND {timestampQuery}"
               Org = $"{where.Org} AND {timestampQuery}"
            })
         agg
         query.DateRange

   let agg =
      Option.fold
         (fun (queryParams, (where: HistoryWhere)) filters ->
            [
               "employeeEventTypes",
               filters |> employeeEventFilterNames |> Sql.stringArray
            ]
            @ queryParams,
            {
               where with
                  Employee =
                     $"{where.Employee} AND {Fields.name} = ANY(@employeeEventTypes)"
            })
         agg
         query.EmployeeEventType

   let agg =
      Option.fold
         (fun (queryParams, (where: HistoryWhere)) filters ->
            [
               "accountEventTypes",
               filters |> accountEventFilterNames |> Sql.stringArray
            ]
            @ queryParams,
            {
               where with
                  Account =
                     $"{where.Account} AND {Fields.name} = ANY(@accountEventTypes)"
            })
         agg
         query.AccountEventType

   let agg =
      Option.fold
         (fun (queryParams, (where: HistoryWhere)) filters ->
            [
               "orgEventTypes",
               filters |> orgEventFilterNames |> Sql.stringArray
            ]
            @ queryParams,
            {
               where with
                  Org = $"{where.Org} AND {Fields.name} = ANY(@orgEventTypes)"
            })
         agg
         query.OrgEventType

   let queryParams, where = agg

   let employeeEventQuery =
      query.EmployeeEventType
      |> Option.map (fun _ ->
         $"""
         SELECT
            'employee' as event_type,
            e.first_name || ' ' || e.last_name AS employee_name,
            timestamp,
            initiated_by_id,
            employee_event.event
         FROM {employeeEventTable}
         JOIN {employeeTable} e using({Fields.employeeId})
         WHERE {where.Employee}
         """)

   let accountEventQuery =
      query.AccountEventType
      |> Option.map (fun _ ->
         $"""
         SELECT
            'account' as event_type,
            '' as employee_name,
            timestamp,
            initiated_by_id,
            transaction.event
         FROM {accountEventTable}
         WHERE {where.Account}
         """)

   let orgEventQuery =
      query.OrgEventType
      |> Option.map (fun _ ->
         $"""
         SELECT 
            'org' as event_type,
            '' as employee_name,
            timestamp,
            initiated_by_id,
            organization_event.event
         FROM {orgEventTable}
         WHERE {where.Org}
         """)

   let historySubQueries =
      [ employeeEventQuery; accountEventQuery; orgEventQuery ]
      |> List.choose id
      |> List.map (fun query -> "(" + query + ")")
      |> String.concat (" UNION ALL ")

   let query =
      $"""
      SELECT 
         event_type,
         timestamp,
         event,
         employee_name,
         CASE
            WHEN {Fields.initiatedById} = '{string Constants.SYSTEM_USER_ID}' THEN 'System'
            ELSE initiators.first_name || ' ' || initiators.last_name
         END as initiator_name
      FROM (
         {historySubQueries}

         ORDER BY timestamp desc
         LIMIT {limit}
         OFFSET @offset
      )
      JOIN {employeeTable} initiators ON {Fields.initiatedById} = initiators.{Fields.employeeId}
      ORDER BY timestamp desc
      """

   pgQuery<History> query (Some queryParams) (fun read ->
      let eventType = read.string "event_type"
      let initiator = read.string "initiator_name"

      match eventType with
      | "employee" ->
         History.Employee {
            EmployeeName = read.string "employee_name"
            InitiatedByName = initiator
            Event = Reader.event read
         }
      | "account" ->
         History.Account {
            InitiatedByName = initiator
            Event = TransactionSqlMapper.TransactionSqlReader.event read
         }
      | "org" ->
         History.Org {
            InitiatedByName = initiator
            Event = OrganizationEventSqlMapper.OrgEventSqlReader.event read
         }
      | _ -> failwith "Unknown event type")
