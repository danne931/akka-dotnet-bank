module AnalyticsDashboard

open Feliz
open Feliz.UseElmish
open Elmish
open Feliz.Recharts
open System

open Bank.Org.Domain
open Bank.Employee.Domain
open Lib.SharedTypes
open UIDomain

type State = {
   SelectedTimeSeriesChart: TimeSeries.Chart
   SelectedTimeSeriesDateFilter: DateFilter
   SelectedTimeSeriesAmountFilter: TimeSeries.Amount
   SelectedTopNMonth: DateTime
   SelectedTopNPurchasersMonth: DateTime
   Analytics: Deferred<Result<MoneyFlowAnalytics, Err>>
   TimeSeriesUpdateInProgress: bool
   TopNUpdateInProgress: bool
   TopNPurchasersUpdateInProgress: bool
}

type Msg =
   | LoadInitialAnalytics of
      AsyncOperationStatus<Result<MoneyFlowAnalytics, Err>>
   | ResolvedTimeSeriesFilterUpdate of
      Result<MoneyFlowDailyTimeSeriesAnalytics option, Err>
   | ResolvedTopNFilterUpdate of Result<MoneyFlowTopNAnalytics option, Err>
   | ResolvedTopNPurchasersFilterUpdate of
      Result<EmployeePurchaserTopN list option, Err>
   | SelectTimeSeriesChart of TimeSeries.Chart
   | SelectTimeSeriesAmountFilter of TimeSeries.Amount
   | SelectDateFilter of DateFilter
   | SelectTopNMonth of DateTime
   | SelectTopNPurchasersMonth of DateTime

let init () =
   {
      SelectedTopNMonth = DateTime.Now
      SelectedTopNPurchasersMonth = DateTime.Now
      SelectedTimeSeriesChart = TimeSeries.Chart.Org
      SelectedTimeSeriesDateFilter = DateFilter.Last30Days
      SelectedTimeSeriesAmountFilter = TimeSeries.Amount.All
      Analytics = Deferred.Idle
      TimeSeriesUpdateInProgress = false
      TopNUpdateInProgress = false
      TopNPurchasersUpdateInProgress = false
   },
   Cmd.ofMsg (LoadInitialAnalytics Started)

let update orgId msg state =
   match msg with
   | SelectTimeSeriesChart c ->
      {
         state with
            SelectedTimeSeriesChart = c
      },
      Cmd.none
   | SelectTimeSeriesAmountFilter filter ->
      {
         state with
            SelectedTimeSeriesAmountFilter = filter
      },
      Cmd.none
   | SelectDateFilter f ->
      let load = async {
         let! res = AnalyticsService.loadTimeSeriesAnalytics orgId f
         return Msg.ResolvedTimeSeriesFilterUpdate res
      }

      {
         state with
            SelectedTimeSeriesDateFilter = f
            TimeSeriesUpdateInProgress = true
      },
      Cmd.fromAsync load
   | SelectTopNMonth date ->
      let load = async {
         let! res = AnalyticsService.loadTopNAnalytics orgId date
         return Msg.ResolvedTopNFilterUpdate res
      }

      {
         state with
            SelectedTopNMonth = date
            TopNUpdateInProgress = true
      },
      Cmd.fromAsync load
   | SelectTopNPurchasersMonth date ->
      let load = async {
         let! res = AnalyticsService.loadTopNPurchasersAnalytics orgId date

         return Msg.ResolvedTopNPurchasersFilterUpdate res
      }

      {
         state with
            SelectedTopNPurchasersMonth = date
            TopNPurchasersUpdateInProgress = true
      },
      Cmd.fromAsync load
   | ResolvedTimeSeriesFilterUpdate(Ok series) ->
      {
         state with
            Analytics =
               state.Analytics
               |> (Deferred.map << Result.map) (fun analytics -> {
                  analytics with
                     TimeSeriesDaily = series
               })
            TimeSeriesUpdateInProgress = false
      },
      Cmd.none
   | ResolvedTimeSeriesFilterUpdate(Error err) ->
      Log.error $"Error updating time series filter {err}"
      state, Cmd.none
   | ResolvedTopNFilterUpdate(Ok topN) ->
      {
         state with
            Analytics =
               state.Analytics
               |> (Deferred.map << Result.map) (fun analytics -> {
                  analytics with
                     TopN = topN
               })
            TopNUpdateInProgress = false
      },
      Cmd.none
   | ResolvedTopNFilterUpdate(Error err) ->
      Log.error $"Error updating topN money flow filter {err}"
      state, Cmd.none
   | ResolvedTopNPurchasersFilterUpdate(Ok topN) ->
      {
         state with
            Analytics =
               state.Analytics
               |> (Deferred.map << Result.map) (fun analytics -> {
                  analytics with
                     TopNPurchasers = topN
               })
            TopNPurchasersUpdateInProgress = false
      },
      Cmd.none
   | ResolvedTopNPurchasersFilterUpdate(Error err) ->
      Log.error $"Error updating topN purchasers filter {err}"
      state, Cmd.none
   | LoadInitialAnalytics Started ->
      let load = async {
         let! res =
            AnalyticsService.loadInitialAnalytics
               orgId
               state.SelectedTimeSeriesDateFilter

         return LoadInitialAnalytics(Finished res)
      }

      {
         state with
            Analytics = Deferred.InProgress
      },
      Cmd.fromAsync load
   | LoadInitialAnalytics(Finished(Ok analytics)) ->
      {
         state with
            Analytics = Deferred.Resolved(Ok analytics)
      },
      Cmd.none
   | LoadInitialAnalytics(Finished(Error err)) ->
      {
         state with
            Analytics = Deferred.Resolved(Error err)
      },
      Cmd.none

[<ReactComponent>]
let AnalyticsDashboardComponent
   (props:
      {|
         Url: Routes.AnalyticsUrl
         Session: UserSession
      |})
   =
   let session = props.Session

   let state, dispatch =
      React.useElmish (init, update session.OrgId, [| box session.OrgId |])

   let orgCtx = React.useContext OrgProvider.context

   classyNode Html.div [ "analytics-dashboard" ] [
      classyNode Html.main [ "container-fluid" ] [
         match state.Analytics, orgCtx with
         | Deferred.Resolved(Ok analytics), Deferred.Resolved(Ok(Some org)) ->
            classyNode Html.div [ "analytics-chart-filter"; "grid" ] [
               TimeSeries.renderChartSelect
                  state.SelectedTimeSeriesChart
                  (Msg.SelectTimeSeriesChart >> dispatch)


               DateFilter.renderDateFilterSelect
                  (Some state.SelectedTimeSeriesDateFilter)
                  (Option.defaultValue DateFilter.Last30Days
                   >> Msg.SelectDateFilter
                   >> dispatch)
                  false

               TimeSeries.renderAmountSelect
                  state.SelectedTimeSeriesAmountFilter
                  (Msg.SelectTimeSeriesAmountFilter >> dispatch)

            ]

            Html.progress [
               if not state.TimeSeriesUpdateInProgress then
                  attr.value 100

               attr.style [ style.marginBottom 25 ]
            ]

            match analytics.TimeSeriesDaily with
            | None -> Html.p "No data for this period."
            | Some timeSeries ->
               Recharts.responsiveContainer [
                  responsiveContainer.width (length.percent 100)
                  responsiveContainer.height 400

                  responsiveContainer.chart (
                     match state.SelectedTimeSeriesChart with
                     | TimeSeries.Chart.Org ->
                        TimeSeries.renderChartByOrg
                           timeSeries.ByOrg
                           state.SelectedTimeSeriesDateFilter
                           state.SelectedTimeSeriesAmountFilter
                     | TimeSeries.Chart.Account ->
                        TimeSeries.renderChartByAccount
                           timeSeries.ByAccount
                           org.Accounts
                           state.SelectedTimeSeriesDateFilter
                           state.SelectedTimeSeriesAmountFilter
                  )
               ]

            classyNode Html.div [ "analytics-top-money-flow" ] [
               classyNode Html.div [ "analytics-topN-filter"; "grid" ] [
                  Html.h4 "Top Money Flow"
                  TopMoneyFlow.renderTopNMonthSelect
                     state.SelectedTopNMonth
                     (Msg.SelectTopNMonth >> dispatch)
               ]

               Html.progress [
                  if not state.TopNUpdateInProgress then
                     attr.value 100
               ]

               match analytics.TopN, analytics.TimeSeriesMonthly with
               | _, None
               | None, _ -> Html.p "No money movement for this period."
               | Some topN, Some timeSeries ->
                  classyNode Html.div [ "grid" ] [
                     Html.article [
                        TopMoneyFlow.renderTopMoneyListItems
                           MoneyFlow.In
                           topN
                           timeSeries
                           state.SelectedTopNMonth
                     ]

                     Html.article [
                        TopMoneyFlow.renderTopMoneyListItems
                           MoneyFlow.Out
                           topN
                           timeSeries
                           state.SelectedTopNMonth
                     ]
                  ]
            ]

            Html.br []

            classyNode Html.div [ "analytics-top-purchasers" ] [
               classyNode Html.div [ "analytics-topN-filter"; "grid" ] [
                  Html.h4 "Top Purchasers"
                  TopMoneyFlow.renderTopNMonthSelect
                     state.SelectedTopNPurchasersMonth
                     (Msg.SelectTopNPurchasersMonth >> dispatch)
               ]

               Html.progress [
                  if not state.TopNPurchasersUpdateInProgress then
                     attr.value 100
               ]

               match analytics.TopNPurchasers with
               | None -> Html.p "No employee purchases for this period."
               | Some topN -> Html.article [ TopPurchasers.render topN ]
            ]
         | _, Deferred.Resolved(Ok None) ->
            Html.p "Can not display analytics. Organization not found."
         | _ -> Html.progress []
      ]
   ]

// Necessary for dynamic import resolution. (Component lazily loaded so
// charting library fetched only when necessary.)
Fable.Core.JsInterop.exportDefault AnalyticsDashboardComponent
