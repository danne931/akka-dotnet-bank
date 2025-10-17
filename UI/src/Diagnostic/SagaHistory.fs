module SagaHistory

open Feliz
open Feliz.UseElmish
open Elmish

open Lib.SharedTypes
open DiagnosticsService
open SagaDTO
open Bank.Employee.Domain

type Msg = Load of AsyncOperationStatus<Result<SagaDTO list, Err>>

type State = {
   Sagas: Deferred<Result<SagaDTO list, Err>>
}

let init () =
   { Sagas = Deferred.Idle }, Cmd.ofMsg <| Load Started

let update msg (state: State) =
   match msg with
   | Load Started ->
      let loadInitStatus = async {
         let! res = getSagaHistory ()
         return Load(Finished res)
      }

      {
         state with
            Sagas = Deferred.InProgress
      },
      Cmd.fromAsync loadInitStatus
   | Load(Finished(Ok sagas)) ->
      {
         state with
            Sagas = Deferred.Resolved(Ok sagas)
      },
      Cmd.none
   | Load(Finished(Error err)) ->
      Log.error $"Error loading saga history {err}"

      {
         state with
            Sagas = Deferred.Resolved(Error err)
      },
      Alerts.toastCommand err

let renderSaga (saga: SagaDTO) =
   classyNode Html.div [ "saga-history-item" ] [
      Html.small saga.Name

      classyNode Html.div [ "saga-activities"; "grid" ] [
         for activity in saga.LifeCycle do
            let date =
               match activity.End with
               | Some finishedAt -> finishedAt
               | None -> activity.Start
               |> DateTime.dateUIFriendlyWithSecondsShort

            Html.div [
               attr.style [
                  style.backgroundColor (
                     match activity.Status with
                     | SagaActivityDTOStatus.InProgress -> Style.color.primary
                     | SagaActivityDTOStatus.Completed -> Style.color.moneyIn
                     | SagaActivityDTOStatus.Failed -> Style.color.alert
                     | SagaActivityDTOStatus.Aborted -> Style.color.secondary
                  )
               ]
               attr.custom ("data-tooltip", $"{activity.Name} {date}")
               attr.custom ("data-placement", "right")
            ]
      ]
   ]

[<ReactComponent>]
let SagaHistoryComponent (url: Routes.DiagnosticUrl) (session: UserSession) =
   let state, dispatch = React.useElmish (init, update, [||])

   let signalRConnection = React.useContext SignalRConnectionProvider.context

   (*
   React.useEffect (
      fun () ->
         match signalRConnection with
         | Some conn ->
            DiagnosticsService.listenForSagaHistory
               (SagaHistoryReceived >> dispatch)
               conn
         | _ -> ()
         ()
      , [| box signalRConnection |]
   )
   *)

   Html.div [
      Html.h6 [
         attr.text "Saga History"
         attr.style [ style.marginBottom (10) ]
      ]

      classyNode Html.div [ "saga-history-container" ] [
         match state.Sagas with
         | Deferred.Resolved(Ok sagas) ->
            Html.progress [ attr.value 100 ]

            classyNode Html.div [ "grid"; "saga-history" ] [
               for saga in sagas do
                  renderSaga saga
            ]
         | _ -> Html.progress []
      ]
   ]
