module Bank.History.Api

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
              typeof<OrgOnboardingApplicationSubmitted>.Name
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
             ])
      []
   |> List.toArray

let parentAccountEventFilterNames
   (filters: ParentAccountEventGroupFilter list)
   : string array
   =
   filters
   |> List.fold
      (fun acc e ->
         acc
         @ match e with
           | ParentAccountEventGroupFilter.DomesticTransferRecipient -> [
              typeof<RegisteredDomesticTransferRecipient>.Name
              typeof<EditedDomesticTransferRecipient>.Name
              typeof<NicknamedDomesticTransferRecipient>.Name
             ])
      []
   |> List.toArray

let accountEventFilterNames
   (filters: AccountEventGroupFilter list)
   : string array
   =
   filters
   |> List.fold
      (fun acc e ->
         acc
         @ match e with
           | AccountEventGroupFilter.Purchase -> [
              typeof<DebitedAccount>.Name
              typeof<PurchaseSettled>.Name
             ]
           | AccountEventGroupFilter.Deposit -> [ typeof<DepositedCash>.Name ]
           | AccountEventGroupFilter.InternalTransferWithinOrg -> [
              typeof<InternalTransferWithinOrgPending>.Name
              typeof<InternalTransferWithinOrgFailed>.Name
              typeof<InternalTransferWithinOrgDeposited>.Name
             ]
           | AccountEventGroupFilter.InternalTransferBetweenOrgs -> [
              typeof<InternalTransferBetweenOrgsPending>.Name
              typeof<InternalTransferBetweenOrgsFailed>.Name
              typeof<InternalTransferBetweenOrgsDeposited>.Name
              typeof<InternalTransferBetweenOrgsSettled>.Name
             ]
           | AccountEventGroupFilter.InternalAutomatedTransfer -> [
              typeof<InternalAutomatedTransferPending>.Name
              typeof<InternalAutomatedTransferFailed>.Name
              typeof<InternalAutomatedTransferDeposited>.Name
             ]
           | AccountEventGroupFilter.DomesticTransfer -> [
              typeof<DomesticTransferPending>.Name
              typeof<DomesticTransferCompleted>.Name
              typeof<DomesticTransferFailed>.Name
              typeof<DomesticTransferProgressUpdated>.Name
             ]
           | AccountEventGroupFilter.PlatformPayment -> [
              typeof<PlatformPaymentRequested>.Name
              typeof<PlatformPaymentPaid>.Name
              typeof<PlatformPaymentDeposited>.Name
              typeof<PlatformPaymentRefunded>.Name
              typeof<PlatformPaymentSettled>.Name
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
              typeof<PurchaseApplied>.Name
              typeof<PurchaseRefunded>.Name
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
   ParentAccount: string
   Org: string
}

let getHistory (orgId: OrgId) (query: HistoryQuery) =
   let employeeTable = EmployeeSqlMapper.table
   let employeeEventTable = EmployeeEventSqlMapper.table
   let accountEventTable = AccountEventSqlMapper.table
   let parentAccountEventTable = ParentAccountEventSqlMapper.table
   let orgEventTable = OrganizationEventSqlMapper.table

   let query =
      match
         query.OrgEventType,
         query.AccountEventType,
         query.ParentAccountEventType,
         query.EmployeeEventType
      with
      | None, None, None, None -> {
         query with
            OrgEventType = Some OrgEventGroupFilter.All
            AccountEventType = Some AccountEventGroupFilter.All
            ParentAccountEventType = Some ParentAccountEventGroupFilter.All
            EmployeeEventType = Some EmployeeEventGroupFilter.All
        }
      | _ -> query

   let where = {
      Employee = $"{employeeEventTable}.{Fields.orgId} = @orgId"
      ParentAccount = $"{parentAccountEventTable}.{Fields.orgId} = @orgId"
      Account = $"{accountEventTable}.{Fields.orgId} = @orgId"
      Org = $"{orgEventTable}.{Fields.orgId} = @orgId"
   }

   let queryParams = [
      "orgId", Writer.orgId orgId
      "limit", Sql.int query.PageLimit
   ]

   let agg =
      Option.fold
         (fun (queryParams, where) initiatedByIds ->
            [ "iIds", Writer.initiatedByIds initiatedByIds ] @ queryParams,
            {
               Employee =
                  $"{where.Employee} AND {employeeEventTable}.{Fields.initiatedById} = ANY(@iIds)"
               ParentAccount =
                  $"{where.ParentAccount} AND {parentAccountEventTable}.{Fields.initiatedById} = ANY(@iIds)"
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
               ParentAccount = $"{where.ParentAccount} AND {timestampQuery}"
               Org = $"{where.Org} AND {timestampQuery}"
            })
         agg
         query.DateRange

   let agg =
      Option.fold
         (fun (queryParams, where) (cursor: HistoryCursor) ->
            let queryParams =
               [
                  "timestamp", Sql.timestamptz cursor.Timestamp
                  "eventId", Writer.eventId cursor.EventId
               ]
               @ queryParams

            let cursorWhere =
               // NOTE:
               // Use of date.toISOString() browser API causes the timestamp
               // to lose a wee bit of specificity.
               // Ex: 2025-02-27T13:17:57.06234Z -> 2025-02-27T13:17:57.062Z
               // Postgres has .06234 but our equivalence query has .062 so it
               // will not match unless we truncate the Postgres value via the
               // date_trunc function below.  I reckon this will be plenty
               // sufficient for now.
               let ts = $"date_trunc('milliseconds', {Fields.timestamp})"
               $"{ts} < @timestamp OR ({ts} = @timestamp AND {Fields.eventId} < @eventId)"

            queryParams,
            {
               Employee = $"{where.Employee} AND {cursorWhere}"
               Account = $"{where.Account} AND {cursorWhere}"
               ParentAccount = $"{where.ParentAccount} AND {cursorWhere}"
               Org = $"{where.Org} AND {cursorWhere}"
            })
         agg
         query.Cursor

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
               "parentAccountEventTypes",
               filters |> parentAccountEventFilterNames |> Sql.stringArray
            ]
            @ queryParams,
            {
               where with
                  ParentAccount =
                     $"{where.ParentAccount} AND {Fields.name} = ANY(@parentAccountEventTypes)"
            })
         agg
         query.ParentAccountEventType

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
            {Fields.initiatedById},
            {Fields.eventId},
            {employeeEventTable}.event
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
            {Fields.initiatedById},
            {Fields.eventId},
            {accountEventTable}.event
         FROM {accountEventTable}
         WHERE {where.Account}
         """)

   let parentAccountEventQuery =
      query.ParentAccountEventType
      |> Option.map (fun _ ->
         $"""
         SELECT 
            'parent_account' as event_type,
            '' as employee_name,
            timestamp,
            {Fields.initiatedById},
            {Fields.eventId},
            {parentAccountEventTable}.event
         FROM {parentAccountEventTable}
         WHERE {where.ParentAccount}
         """)

   let orgEventQuery =
      query.OrgEventType
      |> Option.map (fun _ ->
         $"""
         SELECT 
            'org' as event_type,
            '' as employee_name,
            timestamp,
            {Fields.initiatedById},
            {Fields.eventId},
            {orgEventTable}.event
         FROM {orgEventTable}
         WHERE {where.Org}
         """)

   let historySubQueries =
      [
         employeeEventQuery
         accountEventQuery
         orgEventQuery
         parentAccountEventQuery
      ]
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
            WHEN {Fields.initiatedById} = '{string Initiator.System.Id}' THEN '{Initiator.System.Name}'
            ELSE initiators.first_name || ' ' || initiators.last_name
         END as initiator_name
      FROM (
         {historySubQueries}
         ORDER BY timestamp desc, {Fields.eventId} desc
         LIMIT @limit
      )
      JOIN {employeeTable} initiators ON {Fields.initiatedById} = initiators.{Fields.employeeId}
      ORDER BY timestamp desc
      """

   pgQuery<Transaction.History> query (Some queryParams) (fun read ->
      let eventType = read.string "event_type"
      let initiator = read.string "initiator_name"

      match eventType with
      | "employee" ->
         Transaction.History.Employee {
            EmployeeName = read.string "employee_name"
            InitiatedByName = initiator
            Event = Reader.event read
         }
      | "account" ->
         Transaction.History.Account {
            InitiatedByName = initiator
            Event = AccountEventSqlMapper.SqlReader.event read
         }
      | "parent_account" ->
         Transaction.History.ParentAccount {
            InitiatedByName = initiator
            Event = ParentAccountEventSqlMapper.SqlReader.event read
         }
      | "org" ->
         Transaction.History.Org {
            InitiatedByName = initiator
            Event = OrganizationEventSqlMapper.OrgEventSqlReader.event read
         }
      | _ -> failwith "Unknown event type")
