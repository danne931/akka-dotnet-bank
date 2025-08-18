namespace Bank.Analytics.Domain

open Lib.SharedTypes

type EmployeePurchaserTopN = {
   Amount: decimal
   EmployeeId: EmployeeId
   EmployeeName: string
}

type MoneyFlowTopN = {
   MoneyFlow: MoneyFlow
   Amount: decimal
   Source: string
}

type MoneyFlowTopNAnalytics = {
   In: MoneyFlowTopN list
   Out: MoneyFlowTopN list
}
