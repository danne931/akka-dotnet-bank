namespace Bank.Analytics.Domain

type MoneyFlowAnalytics = {
   TimeSeriesMonthly: MoneyFlowMonthlyTimeSeriesAnalytics option
   TimeSeriesDaily: MoneyFlowDailyTimeSeriesAnalytics option
   TopN: MoneyFlowTopNAnalytics option
   TopNPurchasers: EmployeePurchaserTopN list option
}
