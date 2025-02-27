module AccountEventSqlMapper

open System

open Bank.Account.Domain
open Lib.SharedTypes
open AccountSqlMapper
open OrganizationSqlMapper
open CardSqlMapper

let table = "account_event"

module TypeCast =
   let timeFrame = "time_frame"
   let moneyFlow = "money_flow"
   let timeSeriesMonthlyFilterBy = "monthly_time_series_filter_by"

module Functions =
   let employeePurchaserTopNMonthly = "purchase_top_n_employees_by_month"
   let moneyFlowTopNMonthly = "money_flow_top_n_source_by_month"

   let moneyFlowTimeSeriesDaily = "money_flow_time_series_daily"

   let moneyFlowTimeSeriesMonthly = "money_flow_time_series_monthly"

   let transferAccrued = "transfer_accrued"

module Views =
   let dailyPurchaseAccrued = "daily_purchase_accrued"
   let monthlyPurchaseAccrued = "monthly_purchase_accrued"
   let dailyPurchaseAccruedByCard = "daily_purchase_accrued_by_card"
   let monthlyPurchaseAccruedByCard = "monthly_purchase_accrued_by_card"

module Fields =
   let eventId = "event_id"
   let accountId = AccountFields.accountId
   let orgId = OrgFields.orgId
   let correlationId = "correlation_id"
   let initiatedById = EmployeeEventSqlMapper.EmployeeEventFields.initiatedById
   let cardId = CardFields.cardId
   let name = "name"
   let amount = "amount"
   let moneyFlow = "money_flow"
   let source = "source"
   let timestamp = "timestamp"
   let event = "event"

module SqlReader =
   let eventId (read: RowReader) = Fields.eventId |> read.uuid |> EventId

   let accountId = AccountSqlReader.accountId

   let orgId = OrgSqlReader.orgId

   let correlationId (read: RowReader) =
      Fields.correlationId |> read.uuid |> CorrelationId

   let initiatedById =
      EmployeeEventSqlMapper.EmployeeEventSqlReader.initiatedById

   let cardId (read: RowReader) : CardId option =
      read.uuidOrNone Fields.cardId |> Option.map CardId

   let name (read: RowReader) = read.text Fields.name

   let amount (read: RowReader) = read.decimalOrNone Fields.amount

   let moneyFlow (read: RowReader) =
      read.stringOrNone Fields.moneyFlow |> Option.bind MoneyFlow.fromString

   let source (read: RowReader) = read.stringOrNone Fields.source

   let timestamp (read: RowReader) = read.dateTime Fields.timestamp

   let event (read: RowReader) =
      read.text Fields.event |> Serialization.deserializeUnsafe<AccountEvent>

module SqlWriter =
   let eventId (evtId: EventId) =
      let (EventId id) = evtId
      Sql.uuid id

   let correlationId (corrId: CorrelationId) =
      let (CorrelationId id) = corrId
      Sql.uuid id

   let initiatedById =
      EmployeeEventSqlMapper.EmployeeEventSqlWriter.initiatedById

   let initiatedByIds =
      EmployeeEventSqlMapper.EmployeeEventSqlWriter.initiatedByIds

   let cardId (cardId: CardId option) =
      let uuidOpt =
         cardId
         |> Option.map (fun cardId ->
            let (CardId id) = cardId
            id)

      Sql.uuidOrNone uuidOpt

   let cardIds (ids: CardId list) =
      ids |> List.map CardId.get |> Array.ofList |> Sql.uuidArray

   let accountId = AccountSqlWriter.accountId

   let accountIds (ids: AccountId list) =
      ids |> List.map AccountId.get |> Array.ofList |> Sql.uuidArray

   let orgId = OrgSqlWriter.orgId
   let name = Sql.text
   let amount = Sql.moneyOrNone

   let moneyFlow (direction: MoneyFlow option) =
      direction |> Option.map string |> Sql.stringOrNone

   let source = Sql.stringOrNone

   let timestamp (date: DateTime) = Sql.timestamptz date

   let event (event: AccountEvent) =
      Sql.jsonb <| Serialization.serialize event
