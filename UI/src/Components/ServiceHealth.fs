module ServiceHealth

open Feliz
open Feliz.UseElmish
open Elmish

open Lib.SharedTypes
open DiagnosticsService
open Lib.CircuitBreaker

type Msg =
   | Load of AsyncOperationStatus<Result<ServiceDiagnostics, Err>>
   | ServiceHealthEventReceived of CircuitBreakerEvent

type State = {
   DomesticTransfer: ServiceHealth
   Email: ServiceHealth
   KnowYourCustomer: ServiceHealth
   PartnerBank: ServiceHealth
}

let init () =
   {
      DomesticTransfer = ServiceHealth.Good
      Email = ServiceHealth.Good
      KnowYourCustomer = ServiceHealth.Good
      PartnerBank = ServiceHealth.Good
   },
   Cmd.ofMsg <| Load Started

let update msg (state: State) =
   match msg with
   | Load Started ->
      let loadInitStatus = async {
         let! res = getServiceHealth ()
         return Load(Finished res)
      }

      state, Cmd.fromAsync loadInitStatus
   | Load(Finished(Ok status)) ->
      {
         state with
            Email = status.Email
            DomesticTransfer = status.DomesticTransfer
      },
      Cmd.none
   | Load(Finished(Error err)) ->
      Log.error $"Error loading service health {err}"
      state, Cmd.none
   | ServiceHealthEventReceived evt ->
      let health = DiagnosticsService.health evt.Status

      let state =
         match evt.Service with
         | CircuitBreakerService.DomesticTransfer -> {
            state with
               DomesticTransfer = health
           }
         | CircuitBreakerService.Email -> { state with Email = health }
         | CircuitBreakerService.KnowYourCustomer -> {
            state with
               KnowYourCustomer = health
           }
         | CircuitBreakerService.PartnerBank -> {
            state with
               PartnerBank = health
           }

      state, Cmd.none

let renderServiceHealth (name: string) (health: ServiceHealth) =
   Html.span [
      attr.classes [ "system-op" ]
      attr.children [
         Html.text $"{name}: "

         Html.span [
            attr.classes [
               if health = ServiceHealth.Good then "success" else "alert"
            ]
            attr.text (string health)
         ]
      ]
   ]

[<ReactComponent>]
let ServiceHealthComponent () =
   let state, dispatch = React.useElmish (init, update, [||])

   let signalRConnection = React.useContext SignalRConnectionProvider.context

   React.useEffect (
      fun () ->
         match signalRConnection with
         | Some conn ->
            DiagnosticsService.listenForCircuitBreakerEvent
               (ServiceHealthEventReceived >> dispatch)
               conn
         | _ -> ()
      , [| box signalRConnection |]
   )

   Html.nav [
      attr.classes [ "container-fluid" ]
      attr.children [
         Html.small [
            Html.span [
               attr.classes [ "system-ops-title" ]
               attr.text "Service Health"
            ]

            renderServiceHealth "Domestic Transfer" state.DomesticTransfer
            renderServiceHealth "Email" state.Email
            renderServiceHealth "Know Your Customer" state.KnowYourCustomer
            renderServiceHealth "Partner Bank" state.PartnerBank
         ]
      ]
   ]
