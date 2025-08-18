namespace Bank.Analytics.Domain

open System

open Lib.SharedTypes

type BalanceHistory = {
   Balance: decimal
   PreviousBalance: decimal
   PercentChange: decimal
}

type MoneyFlowDailyTimeSeriesByOrg = {
   Day: DateTime
   AmountIn: decimal
   AmountOut: decimal
   BalanceHistory: BalanceHistory
}

type MoneyFlowDailyTimeSeriesByAccount = {
   Day: DateTime
   AccountId: AccountId
   AmountIn: decimal
   AmountOut: decimal
   BalanceHistory: BalanceHistory
}

type MoneyFlowDailyTimeSeriesAnalytics = {
   ByOrg: MoneyFlowDailyTimeSeriesByOrg list
   ByAccount: MoneyFlowDailyTimeSeriesByAccount list
}

type MoneyFlowMonthlyTimeSeries = {
   Month: DateTime
   AmountIn: decimal
   AmountOut: decimal
}

type MoneyFlowMonthlyTimeSeriesAnalytics = {
   TimeSeries: MoneyFlowMonthlyTimeSeries list
   AverageIn: decimal
   AverageOut: decimal
}
