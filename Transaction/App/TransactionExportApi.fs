module Bank.Transaction.ExportApi

open System.Text
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.Postgres
open Bank.Account.Domain
open Bank.Org.Domain
open Transaction

let table = AccountEventSqlMapper.table
module Fields = AccountEventSqlMapper.Fields
module Writer = AccountEventSqlMapper.SqlWriter

let private historyReader (read: RowReader) =
   match read.string "event_aggregate" with
   | "account" ->
      let evt = AccountEventSqlMapper.SqlReader.event read
      let _, envelope = AccountEnvelope.unwrap evt

      History.Account {
         InitiatedByName = envelope.InitiatedBy.Name
         Event = evt
      }
   | "employee" ->
      let evt = EmployeeEventSqlMapper.EmployeeEventSqlReader.event read
      let _, envelope = Bank.Employee.Domain.EmployeeEnvelope.unwrap evt

      History.Employee {
         InitiatedByName = envelope.InitiatedBy.Name
         EmployeeName = read.string "employee_name"
         Event = evt
      }
   | _ ->
      let evt = OrganizationEventSqlMapper.OrgEventSqlReader.event read
      let _, envelope = OrgEnvelope.unwrap evt

      History.Org {
         InitiatedByName = envelope.InitiatedBy.Name
         Event = evt
      }

let getTransactionsByIds
   (orgId: OrgId)
   (transactionIds: TransactionId list)
   : TaskResultOption<Transaction list, Err>
   =
   taskResultOption {
      if transactionIds.IsEmpty then
         return []
      else
         let employeeEventTable = EmployeeEventSqlMapper.table

         let employeeIdField =
            EmployeeEventSqlMapper.EmployeeEventFields.employeeId

         let query =
            $"""
            SELECT
               events.event_aggregate,
               correlation_id,
               employee_name,
               events.event,
               events.timestamp
            FROM (
               SELECT
                  'account' as event_aggregate,
                  '' as employee_name,
                  correlation_id,
                  event,
                  timestamp
               FROM {table}
               WHERE {table}.correlation_id = ANY(@transactionIds)
                  AND {table}.{Fields.orgId} = @orgId

               UNION ALL

               SELECT
                  'employee' as event_aggregate,
                  e.first_name || ' ' || e.last_name AS employee_name,
                  correlation_id,
                  event,
                  timestamp
               FROM {employeeEventTable}
               JOIN {EmployeeSqlMapper.table} e using({employeeIdField})
               WHERE {employeeEventTable}.correlation_id = ANY(@transactionIds)
                  AND {employeeEventTable}.{EmployeeEventSqlMapper.EmployeeEventFields.orgId} = @orgId

               UNION ALL

               SELECT
                  'org' as event_aggregate,
                  '' AS employee_name,
                  correlation_id,
                  event,
                  timestamp
               FROM {OrganizationEventSqlMapper.table}
               WHERE {OrganizationEventSqlMapper.table}.correlation_id = ANY(@transactionIds)
                  AND {OrganizationEventSqlMapper.table}.{OrganizationEventSqlMapper.OrgEventFields.orgId} = @orgId
            ) AS events
            ORDER BY events.timestamp
            """

         let txnIdParams =
            transactionIds |> List.map _.Value |> Array.ofList |> Sql.uuidArray

         let! events =
            pgQuery<History>
               query
               (Some [
                  "transactionIds", txnIdParams
                  "orgId", Writer.orgId orgId
               ])
               historyReader

         return Transaction.fromHistory events
   }

let generateCsv (transactions: Transaction list) =
   let header = "Date,Type,Status,Amount,Initiated By,Transaction ID,Org ID"

   let rows = [
      for txn in transactions do
         let txnType =
            match txn.Type with
            | TransactionType.Purchase -> "Purchase"
            | TransactionType.Deposit -> "Deposit"
            | TransactionType.DomesticTransfer -> "Domestic Transfer"
            | TransactionType.InternalTransferBetweenOrgs ->
               "Internal Transfer Between Orgs"
            | TransactionType.InternalTransferWithinOrg ->
               "Internal Transfer Within Org"
            | TransactionType.InternalAutomatedTransfer ->
               "Internal Automated Transfer"
            | TransactionType.Payment -> "Payment"

         let dateStr = txn.Timestamp.ToString("yyyy-MM-ddTHH:mm:ss")

         $"{dateStr},{txnType},{txn.Status.Display},{txn.Amount},{txn.InitiatedBy.Name},{txn.Id.Value},{txn.OrgId}"
   ]

   (header :: rows) |> String.concat "\n"
