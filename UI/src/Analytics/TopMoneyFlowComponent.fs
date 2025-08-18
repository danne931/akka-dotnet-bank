[<RequireQualifiedAccess>]
module TopMoneyFlow

open Feliz
open Feliz.Router
open Fable.FontAwesome
open Feliz.Recharts
open System

open Bank.Analytics.Domain
open Lib.Time
open UIDomain
open UIDomain.Account

let private createGradient (id: string) color =
   Svg.linearGradient [
      svg.id id
      svg.x1 0
      svg.x2 0
      svg.y1 0
      svg.y2 1
      svg.children [
         Svg.stop [
            svg.offset (length.percent 5)
            svg.stopColor color
            svg.stopOpacity 0.8
         ]
         Svg.stop [
            svg.offset (length.percent 95)
            svg.stopColor color
            svg.stopOpacity 0.2
         ]
      ]
   ]

let render3MonthChart
   (flow: MoneyFlow)
   (data: MoneyFlowMonthlyTimeSeries list)
   (dimensions: {| Height: int; Width: int |})
   =
   let dataKey, barFillColor =
      match flow with
      | MoneyFlow.In -> _.AmountIn >> string, "url(#moneyIn)"
      | MoneyFlow.Out -> _.AmountOut >> string, "url(#moneyOut)"

   Recharts.barChart [
      barChart.width dimensions.Width
      barChart.height dimensions.Height
      barChart.data data

      barChart.children [
         Svg.defs [
            createGradient "moneyIn" Style.color.moneyIn
            createGradient "moneyOut" Style.color.moneyOut
         ]

         Recharts.tooltip [
            tooltip.position (-85, -13)

            tooltip.labelStyle [ style.color Style.color.primary ]
            tooltip.itemStyle [
               style.color (
                  match flow with
                  | MoneyFlow.In -> Style.color.moneyIn
                  | MoneyFlow.Out -> Style.color.moneyOut
               )
            ]

            tooltip.formatter (fun value _ _ ->
               Money.formatShort (decimal value))
         ]

         Recharts.xAxis [
            xAxis.hide true
            xAxis.dataKey (fun (d: MoneyFlowMonthlyTimeSeries) ->
               DateTime.numberToDisplayMonth[d.Month.Month])
         ]

         Recharts.bar [ bar.dataKey dataKey; bar.fill barFillColor ]
      ]
   ]

let renderTopNMonthSelect
   (selectedMonth: DateTime)
   (onSelect: DateTime -> unit)
   =
   let currDate = DateTime.Now
   let prevDate = currDate.AddMonths -1
   let prevPrevDate = currDate.AddMonths -2

   let dateMap =
      Map [
         currDate.Month, currDate
         prevDate.Month, prevDate
         prevPrevDate.Month, prevPrevDate
      ]

   let datesSorted =
      dateMap
      |> Map.toSeq
      |> Seq.sortByDescending (fun (month, date) -> date.Year, month)

   Html.select [
      attr.value selectedMonth.Month

      attr.onChange (fun (month: string) -> onSelect dateMap[int month])

      attr.children [
         for month, opt in datesSorted do
            Html.option [
               attr.value month
               attr.text $"{DateTime.numberToDisplayMonth[month]} {opt.Year}"
            ]
      ]
   ]

let txnsButton (flow: MoneyFlow) (selectedTopNMonth: DateTime) =
   let start = DateTime(selectedTopNMonth.Year, selectedTopNMonth.Month, 1)
   let filter = DateFilter.Custom(start, start.AddMonths(1).AddDays(-1))

   Html.button [
      attr.classes [ "outline" ]
      attr.children [ Fa.i [ Fa.Solid.History ] []; Html.span "View all" ]

      attr.onClick (fun _ ->
         {
            TransactionBrowserQuery.empty with
               MoneyFlow = Some flow
               Date = Some filter
         }
         |> Routes.TransactionsUrl.queryPath
         |> Router.navigate)
   ]

let renderTopMoneyListItems
   (flow: MoneyFlow)
   (topN: MoneyFlowTopNAnalytics)
   (last3MonthTimeSeries: MoneyFlowMonthlyTimeSeriesAnalytics)
   (selectedMonth: DateTime)
   =
   let selected =
      last3MonthTimeSeries.TimeSeries
      |> List.find (fun a -> a.Month.Month = selectedMonth.Month)

   let str, color, items, totalAmount =
      match flow with
      | MoneyFlow.In -> "in", "credit", topN.In, selected.AmountIn
      | MoneyFlow.Out -> "out", "debit", topN.Out, selected.AmountOut

   let renderAmount (amount: decimal) =
      Html.p [ attr.classes [ color ]; attr.text (Money.format amount) ]

   Html.div [
      classyNode Html.div [ "top-n-list-item" ] [
         Html.b $"Money {str}"
         renderAmount totalAmount
      ]

      Html.hr []
      Html.br []

      if items.Length = 0 then
         Html.p $"No money {str} for this period."
      else
         classyNode Html.div [ "top-n-list-container" ] [
            for item in items do
               Html.div [ Html.b item.Source; renderAmount item.Amount ]
         ]

         txnsButton flow selectedMonth

         Html.hr []
         Html.br []

         classyNode Html.div [ "analytics-monthly-time-series"; "grid" ] [
            Html.div [
               Html.p [
                  attr.style [ style.marginBottom 0 ]
                  attr.text "3 month average"
               ]

               match flow with
               | MoneyFlow.In -> renderAmount last3MonthTimeSeries.AverageIn
               | MoneyFlow.Out -> renderAmount last3MonthTimeSeries.AverageOut
            ]

            render3MonthChart flow last3MonthTimeSeries.TimeSeries {|
               Height = 90
               Width = 150
            |}
         ]
   ]
