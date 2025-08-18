module Bank.Analytics.TopNApi

open System
open System.Threading.Tasks
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.Postgres
open AccountEventSqlMapper
open Bank.Analytics.Domain

type TopNQuery = {
   OrgId: OrgId
   Limit: int
   Date: DateTime
}

/// Top-N sources of funds coming in or going out
let moneyFlowTopNAnalytics
   (txnQuery: TopNQuery)
   : Task<Result<MoneyFlowTopNAnalytics option, Err>>
   =
   taskResultOption {
      let query =
         $"""
         SELECT * FROM {Functions.moneyFlowTopNMonthly}(
            @orgId,
            @flowIn::{TypeCast.moneyFlow},
            @topN,
            @date
         )

         UNION ALL

         SELECT * FROM {Functions.moneyFlowTopNMonthly}(
            @orgId,
            @flowOut::{TypeCast.moneyFlow},
            @topN,
            @date
         )
         """

      let qParams = [
         "orgId", Sql.uuid txnQuery.OrgId.Value
         "flowIn", MoneyFlow.In |> string |> Sql.string
         "flowOut", MoneyFlow.Out |> string |> Sql.string
         "topN", Sql.int txnQuery.Limit
         "date", Sql.timestamptz txnQuery.Date
      ]

      let! topN =
         pgQuery<MoneyFlowTopN> query (Some qParams) (fun read -> {
            MoneyFlow =
               match SqlReader.moneyFlow read with
               | Some flow -> flow
               | None ->
                  failwith "Error attempting to cast string to MoneyFlow"
            Amount = read.decimal Fields.amount
            Source = read.string Fields.source
         })

      return
         topN
         |> List.partition (fun a -> a.MoneyFlow = MoneyFlow.In)
         |> fun (mfIn, mfOut) -> { In = mfIn; Out = mfOut }
   }

/// Top-N spenders
let employeePurchaseTopNAnalytics
   (txnQuery: TopNQuery)
   : Task<Result<EmployeePurchaserTopN list option, Err>>
   =
   let query =
      $"""
      SELECT * FROM {Functions.employeePurchaserTopNMonthly}(
         @orgId,
         @topN,
         @date
      )
      """

   let qParams = [
      "orgId", Sql.uuid txnQuery.OrgId.Value
      "topN", Sql.int txnQuery.Limit
      "date", Sql.timestamptz txnQuery.Date
   ]

   pgQuery<EmployeePurchaserTopN> query (Some qParams) (fun read -> {
      EmployeeId = read.uuid "employee_id" |> EmployeeId
      EmployeeName = read.string "employee_name"
      Amount = read.decimal "amount"
   })
