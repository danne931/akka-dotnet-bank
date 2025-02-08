module AccountSummary

open Feliz
open Fable.Core.JS
open Feliz.Router

open Bank.Org.Domain
open Bank.Account.Domain

type private RotatingMetric =
   | DailyInternalTransferWithinOrg
   | DailyInternalTransferBetweenOrgs
   | DailyDomesticTransfer
   | DailyPaymentPaid
   | DailyPurchaseAccrued
   | MonthlyInternalTransferWithinOrg
   | MonthlyInternalTransferBetweenOrgs
   | MonthlyDomesticTransfer
   | MonthlyPaymentPaid
   | MonthlyPurchaseAccrued


let private rotatingMetrics = [
   RotatingMetric.DailyPurchaseAccrued
   RotatingMetric.DailyInternalTransferWithinOrg
   RotatingMetric.DailyInternalTransferBetweenOrgs
   RotatingMetric.DailyDomesticTransfer
   RotatingMetric.DailyPaymentPaid
   RotatingMetric.MonthlyPurchaseAccrued
   RotatingMetric.MonthlyInternalTransferWithinOrg
   RotatingMetric.MonthlyInternalTransferBetweenOrgs
   RotatingMetric.MonthlyDomesticTransfer
   RotatingMetric.MonthlyPaymentPaid
]

[<ReactComponent>]
let AccountSummaryComponent (org: OrgWithAccountProfiles) =
   let isFading, setFading = React.useState false
   let flow, setFlow = React.useState MoneyFlow.Out

   let rotatingMetric, setMetric =
      React.useState RotatingMetric.DailyPurchaseAccrued

   let monthlyMoneyFlow, setMonthlyMoneyFlow =
      React.useState Deferred.InProgress

   // Sometimes we have monthly analytics for just money flow in
   // or just money flow out.  No need to alternate the monthly
   // money flow chart between in/out in this scenario.
   let flowIsDynamic, setFlowIsDynamic = React.useState false

   let metrics = org.Metrics
   let changeDetection = box (string org.Org.OrgId)

   React.useEffect (
      fun () ->
         async {
            let! res =
               AnalyticsService.loadMoneyFlowMonthlyTimeSeriesForOrg
                  org.Org.OrgId

            setMonthlyMoneyFlow (Deferred.Resolved res)

            match res with
            | Ok(Some analytics) ->
               let moneyIn =
                  analytics.TimeSeries
                  |> List.tryFind (fun t -> t.AmountIn > 0m)

               let moneyOut =
                  analytics.TimeSeries
                  |> List.tryFind (fun t -> t.AmountOut > 0m)

               match moneyIn, moneyOut with
               | Some _, None -> setFlow MoneyFlow.In
               | None, Some _ -> setFlow MoneyFlow.Out
               | _ -> ()

               setFlowIsDynamic (moneyIn.IsSome && moneyOut.IsSome)
            | _ -> ()
         }
         |> Async.StartImmediate
      , [| changeDetection |]
   )

   React.useEffect (
      fun () ->
         let mutable flow = flow

         let interval =
            setInterval
               (fun () ->
                  if flowIsDynamic then
                     let updatedFlow =
                        if flow = MoneyFlow.In then
                           MoneyFlow.Out
                        else
                           MoneyFlow.In

                     flow <- updatedFlow
                     setFlow flow)
               10000

         React.createDisposable (fun _ -> clearInterval interval)
      , [| box flowIsDynamic |]
   )

   React.useEffect (
      fun () ->
         let mutable metricInd = 0
         let mutable rotatingMetric = rotatingMetric
         let mutable isFading = isFading
         let mutable timer = 0

         let setFading fading =
            isFading <- fading
            setFading fading

         let setMetric metric =
            rotatingMetric <- metric
            setMetric metric

         let rotate () =
            let metric = rotatingMetrics |> List.item metricInd

            metricInd <-
               if metricInd = (rotatingMetrics.Length - 1) then
                  0
               else
                  metricInd + 1

            setMetric metric
            setFading false

         let interval =
            setInterval
               (fun () ->
                  setFading true
                  timer <- setTimeout rotate 1000)
               5000

         React.createDisposable (fun _ ->
            clearTimeout timer
            clearInterval interval)
      , [||]
   )

   Html.footer [
      attr.classes [ "container-fluid grid" ]

      attr.children [
         Html.div [
            Html.b [ attr.classes [ "account-name" ]; attr.text org.Org.Name ]

            Html.div [
               Html.p "Balance: "
               Html.ins [ attr.text (Money.format org.Balance) ]
            ]
         ]

         Html.div [
            classyNode Html.div [
               "rotating-footer-metric"
               if isFading then "fade-out" else "fade-in"
            ] [
               match rotatingMetric with
               | RotatingMetric.DailyInternalTransferWithinOrg ->
                  Html.p "Daily Internal Transfer Within Org: "

                  Html.ins [
                     attr.text (
                        Money.formatShort metrics.DailyInternalTransferWithinOrg
                     )
                  ]
               | RotatingMetric.DailyInternalTransferBetweenOrgs ->
                  Html.p "Daily Internal Transfer Between Orgs: "

                  Html.ins [
                     attr.text (
                        Money.formatShort
                           metrics.DailyInternalTransferBetweenOrgs
                     )
                  ]
               | RotatingMetric.DailyDomesticTransfer ->
                  Html.p "Daily Domestic Transfer: "

                  Html.ins [
                     attr.text (Money.formatShort metrics.DailyDomesticTransfer)
                  ]
               | RotatingMetric.DailyPaymentPaid ->
                  Html.p "Daily Payments Paid: "

                  Html.ins [
                     attr.text (Money.formatShort metrics.DailyPaymentPaid)
                  ]
               | RotatingMetric.DailyPurchaseAccrued ->
                  Html.p "Daily Purchase: "

                  Html.ins [
                     attr.text (Money.formatShort metrics.DailyPurchase)
                  ]
               | RotatingMetric.MonthlyInternalTransferWithinOrg ->
                  Html.p "Monthly Internal Transfer Within Org: "

                  Html.ins [
                     attr.text (
                        Money.formatShort
                           metrics.MonthlyInternalTransferWithinOrg
                     )
                  ]
               | RotatingMetric.MonthlyInternalTransferBetweenOrgs ->
                  Html.p "Monthly Internal Transfer Between Orgs: "

                  Html.ins [
                     attr.text (
                        Money.formatShort
                           metrics.MonthlyInternalTransferWithinOrg
                     )
                  ]
               | RotatingMetric.MonthlyDomesticTransfer ->
                  Html.p "Monthly Domestic Transfer: "

                  Html.ins [
                     attr.text (
                        Money.formatShort metrics.MonthlyDomesticTransfer
                     )
                  ]
               | RotatingMetric.MonthlyPurchaseAccrued ->
                  Html.p "Monthly Purchase: "

                  Html.ins [
                     attr.text (Money.formatShort metrics.MonthlyPurchase)
                  ]
               | RotatingMetric.MonthlyPaymentPaid ->
                  Html.p "Monthly Payments Paid: "

                  Html.ins [
                     attr.text (Money.formatShort metrics.DailyPaymentPaid)
                  ]
            ]
         ]

         match monthlyMoneyFlow with
         | Deferred.Resolved(Ok(Some analytics)) ->
            Html.div [
               AnalyticsDashboard.render3MonthTimeSeriesChart
                  flow
                  analytics.TimeSeries
                  {| Height = 70; Width = 120 |}

               Html.a [
                  attr.href ""
                  attr.text "View Analytics"
                  attr.onClick (fun e ->
                     e.preventDefault ()
                     Router.navigate Routes.AnalyticsUrl.BasePath)
               ]
            ]
         | _ -> ()
      ]
   ]

// Necessary for dynamic import resolution. (Component lazily loaded so
// charting library fetched only when necessary.)
Fable.Core.JsInterop.exportDefault AccountSummaryComponent
