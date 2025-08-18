[<RequireQualifiedAccess>]
module TimeSeries

open Feliz
open Feliz.Recharts

open Bank.Org.Domain
open Bank.Account.Domain
open Lib.SharedTypes
open Lib.Time
open UIDomain

[<RequireQualifiedAccess>]
type Amount =
   | All
   | Balance
   | MoneyIn
   | MoneyOut

[<RequireQualifiedAccess>]
type Chart =
   | Org
   | Account

let private renderChart
   (dateFilter: DateFilter)
   (yAxisFilter: Amount)
   (children: ReactElement list)
   =
   let yAxisPaddingBottom = 20
   let chartMarginHorizontal = 30
   let amountInterval = 5

   Recharts.lineChart [
      lineChart.margin (
         left = chartMarginHorizontal,
         right = chartMarginHorizontal
      )

      lineChart.children [
         Recharts.xAxis [
            xAxis.allowDuplicatedCategory false
            xAxis.interval (
               match dateFilter with
               | DateFilter.LastYear
               | DateFilter.CurrentYear -> 30
               | _ -> 5
            )
            xAxis.dataKey (_.Day >> DateTime.formatShort)
         ]

         if yAxisFilter <> Amount.Balance then
            Recharts.yAxis [
               yAxis.yAxisId "moneyFlow"
               yAxis.padding (bottom = yAxisPaddingBottom)
               yAxis.tickCount amountInterval
               yAxis.tickFormatter (decimal >> Money.formatShort)
            ]

         Recharts.yAxis [
            yAxis.yAxisId "balance"
            if yAxisFilter <> Amount.Balance then
               yAxis.orientation.right
            yAxis.padding (bottom = yAxisPaddingBottom)
            yAxis.tickCount amountInterval
            yAxis.tickFormatter (decimal >> Money.formatShort)
         ]

         Recharts.tooltip [
            tooltip.formatter (fun value _ _ ->
               value
               |> decimal
               |> Money.format
               |> fun (m: string) -> m.Split "."
               |> Array.head)
         ]

         yield! children
      ]
   ]

let renderChartByOrg
   (series: MoneyFlowDailyTimeSeriesByOrg list)
   (dateFilter: DateFilter)
   (yAxisFilter: Amount)
   =
   renderChart dateFilter yAxisFilter [
      match yAxisFilter with
      | Amount.All
      | Amount.MoneyIn ->
         Recharts.line [
            line.yAxisId "moneyFlow"
            line.monotone
            line.data series
            line.dataKey (_.AmountIn >> int)
            line.stroke Style.color.moneyIn
            line.dot false
            line.name "Money In"
         ]
      | _ -> ()

      match yAxisFilter with
      | Amount.All
      | Amount.MoneyOut ->
         Recharts.line [
            line.yAxisId "moneyFlow"
            line.monotone
            line.data series
            line.dataKey (_.AmountOut >> int)
            line.stroke Style.color.moneyOut
            line.dot false
            line.name "Money Out"
         ]
      | _ -> ()

      match yAxisFilter with
      | Amount.All
      | Amount.Balance ->
         Recharts.line [
            line.yAxisId "balance"
            line.monotone
            line.data series
            line.dataKey (fun a -> int a.BalanceHistory.Balance)
            line.stroke Style.color.balance
            line.dot false
            line.name "Balance"
         ]
      | _ -> ()
   ]

let renderChartByAccount
   (series: MoneyFlowDailyTimeSeriesByAccount list)
   (accounts: Map<AccountId, Account>)
   (dateFilter: DateFilter)
   (yAxisFilter: Amount)
   =
   let series = series |> List.groupBy _.AccountId

   renderChart dateFilter yAxisFilter [
      for accountId, data in series do
         let name =
            accounts
            |> Map.tryFind accountId
            |> Option.map _.FullName
            |> Option.defaultValue (string accountId)

         React.keyedFragment (
            accountId.Value,
            [
               match yAxisFilter with
               | Amount.All
               | Amount.MoneyIn ->
                  Recharts.line [
                     line.yAxisId "moneyFlow"
                     line.monotone
                     line.data data
                     line.dataKey (_.AmountIn >> int)
                     line.stroke Style.color.moneyIn
                     line.dot false
                     line.name name
                  ]
               | _ -> ()

               match yAxisFilter with
               | Amount.All
               | Amount.MoneyOut ->
                  Recharts.line [
                     line.yAxisId "moneyFlow"
                     line.monotone
                     line.data data
                     line.dataKey (_.AmountOut >> int)
                     line.stroke Style.color.moneyOut
                     line.dot false
                     line.name name
                  ]
               | _ -> ()

               match yAxisFilter with
               | Amount.All
               | Amount.Balance ->
                  Recharts.line [
                     line.yAxisId "balance"
                     line.monotone
                     line.data data
                     line.dataKey (fun a -> int a.BalanceHistory.Balance)
                     line.stroke Style.color.balance
                     line.dot false
                     line.name name
                  ]
               | _ -> ()
            ]
         )
   ]

let renderChartSelect (selected: Chart) (onSelect: Chart -> unit) =
   Html.select [
      attr.onChange (
         function
         | "Org" -> Chart.Org
         | _ -> Chart.Account
         >> onSelect
      )
      attr.value (string selected)

      attr.children [
         Html.option [
            attr.value (string Chart.Org)
            attr.text "Time Series by Org"
         ]

         Html.option [
            attr.value (string Chart.Account)
            attr.text "Time Series by Account"
         ]
      ]
   ]

let renderAmountSelect (selected: Amount) (onSelect: Amount -> unit) =
   Html.select [
      attr.onChange (
         function
         | "Balance" -> Amount.Balance
         | "MoneyIn" -> Amount.MoneyIn
         | "MoneyOut" -> Amount.MoneyOut
         | _ -> Amount.All
         >> onSelect
      )
      attr.value (string selected)

      attr.children [
         Html.option [ attr.value (string Amount.All); attr.text "All" ]

         Html.option [ attr.value (string Amount.Balance); attr.text "Balance" ]

         Html.option [
            attr.value (string Amount.MoneyOut)
            attr.text "Money Out"
         ]

         Html.option [
            attr.value (string Amount.MoneyIn)
            attr.text "Money In"
         ]
      ]
   ]
