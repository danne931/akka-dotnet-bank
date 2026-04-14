module Bank.Analytics.Api

open System.Threading.Tasks
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Bank.Analytics.TopNApi
open Bank.Analytics.MoneyFlowTimeSeriesApi
open Bank.Analytics.Domain

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
      let! topNOpt = moneyFlowTopNAnalytics txnQuery.TopNMoneyFlow

      and! topNPurchasersOpt =
         employeePurchaseTopNAnalytics txnQuery.TopNPurchasers

      and! seriesDailyOpt =
         moneyFlowDailyTimeSeriesAnalytics txnQuery.MoneyFlowDailyTimeSeries

      and! seriesMonthlyOpt =
         moneyFlowMonthlyTimeSeriesAnalytics txnQuery.MoneyFlowMonthlyTimeSeries

      return {
         TimeSeriesMonthly = seriesMonthlyOpt
         TimeSeriesDaily = seriesDailyOpt
         TopN = topNOpt
         TopNPurchasers = topNPurchasersOpt
      }
   }
