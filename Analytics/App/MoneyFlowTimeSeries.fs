module Bank.Analytics.MoneyFlowTimeSeriesApi

open System
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.Postgres
open Lib.Time
open AccountEventSqlMapper
open Bank.Analytics.Domain

type MoneyFlowDailyTimeSeriesQuery = {
   OrgId: OrgId
   Start: DateTime
   End: DateTime
}

[<RequireQualifiedAccess>]
type MoneyFlowMonthlyTimeSeriesFilterBy =
   | Org of OrgId
   | Account of AccountId

type MoneyFlowMonthlyTimeSeriesQuery = {
   FilterBy: MoneyFlowMonthlyTimeSeriesFilterBy
   LookbackMonths: int
}

let moneyFlowDailyTimeSeriesAnalytics
   (txnQuery: MoneyFlowDailyTimeSeriesQuery)
   : TaskResultOption<MoneyFlowDailyTimeSeriesAnalytics, Err>
   =
   taskResultOption {
      let endDateAdjustedToPreviousDay =
         if DateTime.isToday txnQuery.End then
            Some <| txnQuery.End.AddDays -1
         else
            None

      let qParams = [
         "orgId", Sql.uuid txnQuery.OrgId.Value
         "startDate", Sql.timestamptz txnQuery.Start
         "endDate",
         endDateAdjustedToPreviousDay
         |> Option.defaultValue txnQuery.End
         |> Sql.timestamptz
      ]

      let queryBalanceHistory =
         $"""
         SELECT
            bh.account_id,
            bh.date,
            tsd.amount_in,
            tsd.amount_out,

            COALESCE(
               LAG(bh.balance) OVER (PARTITION BY bh.account_id ORDER BY bh.date),
               0
            ) AS prev_balance,

            bh.balance
         FROM {Functions.moneyFlowTimeSeriesDaily}(
            @orgId,
            @startDate,
            @endDate
         ) tsd
         JOIN balance_history bh
            ON tsd.day = bh.date AND tsd.account_id = bh.account_id
         """

      let queryToday =
         $"""
         SELECT
            t.account_id,
            t.date,
            t.amount_in,
            t.amount_out,
            COALESCE(bh.balance, 0) AS prev_balance,
            COALESCE(bh.balance, 0) + t.daily_diff AS balance
         FROM (
            SELECT
               t.account_id,
               t.day AS date,
               t.amount_in,
               t.amount_out,
               t.amount_in - t.amount_out AS daily_diff
            FROM {Functions.moneyFlowTimeSeriesDaily}(
               @orgId,
               CURRENT_DATE,
               CURRENT_DATE
            ) t
         ) t
         JOIN balance_history bh
            ON bh.account_id = t.account_id
            AND bh.date = t.date - interval '1 day'
         """

      // NOTE: A balance history record is created daily for the previous day.
      // If the txnQuery.End falls on the current day then we need to compute
      // today's balance from the previous day balance history record with
      // the sum of todays transaction amounts.
      let query =
         if endDateAdjustedToPreviousDay.IsSome then
            $"SELECT *
              FROM ({queryBalanceHistory} UNION ALL {queryToday})
              ORDER BY date ASC"
         else
            queryBalanceHistory

      let! series =
         pgQuery<MoneyFlowDailyTimeSeriesByAccount>
            query
            (Some qParams)
            (fun read ->
               let prevBalance = read.decimal "prev_balance"
               let balance = read.decimal "balance"

               {
                  Day = read.dateTime "date"
                  AccountId = SqlReader.accountId read
                  AmountIn = read.decimal "amount_in"
                  AmountOut = read.decimal "amount_out"
                  BalanceHistory = {
                     Balance = balance
                     PreviousBalance = prevBalance
                     PercentChange =
                        if prevBalance = 0m then
                           0m
                        else
                           (balance - prevBalance) / prevBalance * 100m
                  }
               })

      return {
         ByAccount = series
         ByOrg =
            series
            |> List.groupBy _.Day
            |> List.map (fun (day, series) ->
               series
               |> List.fold
                  (fun acc item -> {
                     Day = day
                     BalanceHistory = {
                        Balance =
                           acc.BalanceHistory.Balance
                           + item.BalanceHistory.Balance
                        PreviousBalance =
                           acc.BalanceHistory.PreviousBalance
                           + item.BalanceHistory.PreviousBalance
                        PercentChange =
                           acc.BalanceHistory.PercentChange
                           + item.BalanceHistory.PercentChange
                     }
                     AmountIn = acc.AmountIn + item.AmountIn
                     AmountOut = acc.AmountOut + item.AmountOut
                  })
                  {
                     Day = day
                     BalanceHistory = {
                        Balance = 0m
                        PreviousBalance = 0m
                        PercentChange = 0m
                     }
                     AmountIn = 0m
                     AmountOut = 0m
                  })
      }
   }

let moneyFlowMonthlyTimeSeriesAnalytics
   (txnQuery: MoneyFlowMonthlyTimeSeriesQuery)
   : TaskResultOption<MoneyFlowMonthlyTimeSeriesAnalytics, Err>
   =
   taskResultOption {
      let qParams = [
         "filterBy",
         match txnQuery.FilterBy with
         | MoneyFlowMonthlyTimeSeriesFilterBy.Account _ -> Sql.string "Account"
         | MoneyFlowMonthlyTimeSeriesFilterBy.Org _ -> Sql.string "Org"

         "filterId",
         match txnQuery.FilterBy with
         | MoneyFlowMonthlyTimeSeriesFilterBy.Account accountId ->
            Sql.uuid accountId.Value
         | MoneyFlowMonthlyTimeSeriesFilterBy.Org orgId -> Sql.uuid orgId.Value

         "lookbackMonths", Sql.int txnQuery.LookbackMonths
      ]

      let query =
         $"SELECT * 
           FROM {Functions.moneyFlowTimeSeriesMonthly}(
              @filterBy::{TypeCast.timeSeriesMonthlyFilterBy},
              @filterId,
              @lookbackMonths
           )"

      let! series =
         pgQuery<MoneyFlowMonthlyTimeSeries> query (Some qParams) (fun read -> {
            Month = read.dateTime "month"
            AmountIn = read.decimal "amount_in"
            AmountOut = read.decimal "amount_out"
         })

      let avg sum =
         Math.Round(
            sum / decimal series.Length,
            2,
            MidpointRounding.AwayFromZero
         )

      return {
         TimeSeries = series
         AverageIn = series |> List.sumBy _.AmountIn |> avg
         AverageOut = series |> List.sumBy _.AmountOut |> avg
      }
   }
