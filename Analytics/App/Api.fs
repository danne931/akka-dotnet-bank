module Bank.Analytics.Api

open System.Threading.Tasks
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Bank.Analytics.TopNApi
open Bank.Analytics.MoneyFlowTimeSeriesApi
open Bank.Analytics.Domain

type private MoneyFlowAnalyticsDBResult =
   | TimeSeriesDaily of MoneyFlowDailyTimeSeriesAnalytics option
   | TimeSeriesMonthly of MoneyFlowMonthlyTimeSeriesAnalytics option
   | TopN of MoneyFlowTopNAnalytics option
   | TopNPurchasers of EmployeePurchaserTopN list option

type MoneyFlowAnalyticsQuery = {
   OrgId: OrgId
   TopNMoneyFlow: TopNQuery
   TopNPurchasers: TopNQuery
   MoneyFlowDailyTimeSeries: MoneyFlowDailyTimeSeriesQuery
   MoneyFlowMonthlyTimeSeries: MoneyFlowMonthlyTimeSeriesQuery
}

/// TopN and TimeSeries data for analytics dashboard initial load
let moneyFlowAnalytics
   (txnQuery: MoneyFlowAnalyticsQuery)
   : Task<Result<MoneyFlowAnalytics, Err>>
   =
   taskResult {
      let mfTopNTask =
         moneyFlowTopNAnalytics txnQuery.TopNMoneyFlow
         |> TaskResult.map MoneyFlowAnalyticsDBResult.TopN

      let mfTopNPurchasersTask =
         employeePurchaseTopNAnalytics txnQuery.TopNPurchasers
         |> TaskResult.map MoneyFlowAnalyticsDBResult.TopNPurchasers

      let mfDailyTimeSeriesTask =
         moneyFlowDailyTimeSeriesAnalytics txnQuery.MoneyFlowDailyTimeSeries
         |> TaskResult.map MoneyFlowAnalyticsDBResult.TimeSeriesDaily

      let mfMonthlyTimeSeriesTask =
         moneyFlowMonthlyTimeSeriesAnalytics txnQuery.MoneyFlowMonthlyTimeSeries
         |> TaskResult.map MoneyFlowAnalyticsDBResult.TimeSeriesMonthly

      let! res =
         Task.WhenAll [|
            mfTopNTask
            mfTopNPurchasersTask
            mfDailyTimeSeriesTask
            mfMonthlyTimeSeriesTask
         |]

      let! res = res |> List.ofArray |> List.traverseResultM id

      let res =
         match res with
         | [ MoneyFlowAnalyticsDBResult.TopN topNOpt
             MoneyFlowAnalyticsDBResult.TopNPurchasers topNPurchasersOpt
             MoneyFlowAnalyticsDBResult.TimeSeriesDaily seriesDailyOpt
             MoneyFlowAnalyticsDBResult.TimeSeriesMonthly seriesMonthlyOpt ] -> {
            TimeSeriesMonthly = seriesMonthlyOpt
            TimeSeriesDaily = seriesDailyOpt
            TopN = topNOpt
            TopNPurchasers = topNPurchasersOpt
           }
         | _ -> {
            TimeSeriesMonthly = None
            TimeSeriesDaily = None
            TopN = None
            TopNPurchasers = None
           }

      return res
   }
