module SignalREventProvider

open Feliz
open Feliz.UseElmish
open Elmish
open FsToolkit.ErrorHandling

open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Org.Domain
open SignalRBroadcast
open Lib.SharedTypes

[<RequireQualifiedAccess>]
type EventType =
   | ParentAccount
   | Account
   | Employee
   | Org

type SignalREventContext = {
   ParentAccountSubscribers:
      Map<
         string,
         (ParentAccountEventPersistedConfirmation -> unit) *
         (EventProcessingError -> unit)
       >
   AccountSubscribers:
      Map<
         string,
         (AccountEventPersistedConfirmation -> unit) *
         (EventProcessingError -> unit)
       >
   EmployeeSubscribers:
      Map<
         string,
         (EmployeeEventPersistedConfirmation -> unit) *
         (EventProcessingError -> unit)
       >
   OrgSubscribers:
      Map<
         string,
         (OrgEventPersistedConfirmation -> unit) *
         (EventProcessingError -> unit)
       >
   RealtimeParentAccountEvents: ParentAccountEvent list
   RealtimeAccountEvents: AccountEvent list
   RealtimeEmployeeEvents: EmployeeEvent list
   RealtimeOrgEvents: OrgEvent list
   Errors: EventProcessingError list
   CurrentOrgId: OrgId option
   QueuedOrgId: OrgId option
}

let private initState = {
   ParentAccountSubscribers = Map.empty
   AccountSubscribers = Map.empty
   EmployeeSubscribers = Map.empty
   OrgSubscribers = Map.empty
   RealtimeParentAccountEvents = []
   RealtimeAccountEvents = []
   RealtimeEmployeeEvents = []
   RealtimeOrgEvents = []
   Errors = []
   CurrentOrgId = None
   QueuedOrgId = None
}

// NOTE:
// Why is there a notion of removing an existing org from the connection
// group and adding a new one?  Once I sign in, wouldn't the org remain static?
//
// No. There is an org select in the navigation which, for demonstration/development
// purposes, allows the user to toggle between orgs.
// Besides this demonstration use case, in the future, it may be interesting
// to support business owners who own multiple businesses and thus need to
// manage all their businesses from the platform.
let private addOrgToConnection
   (connection: SignalR.Connection)
   (existingOrgId: OrgId option)
   (orgIdToAdd: OrgId)
   : Async<Result<OrgId, Err>>
   =
   asyncResult {
      if existingOrgId.IsSome && existingOrgId <> Some orgIdToAdd then
         let! _ = connection.removeOrgFromConnectionGroup existingOrgId.Value
         ()

      let! _ = connection.addOrgToConnectionGroup orgIdToAdd

      return orgIdToAdd
   }

let context =
   React.createContext<SignalREventContext> (
      name = "SignalREventContext",
      defaultValue = initState
   )

type Msg =
   | ParentAccountEventPersisted of ParentAccountEventPersistedConfirmation
   | AccountEventPersisted of AccountEventPersistedConfirmation
   | EmployeeEventPersisted of EmployeeEventPersistedConfirmation
   | OrgEventPersisted of OrgEventPersistedConfirmation
   | AddEventSubscriber of
      componentName: string *
      OrgId *
      EventType list *
      onPersist: (EventPersistedConfirmation -> unit) *
      onError: (EventProcessingError -> unit)
   | RemoveEventSubscriber of componentName: string * EventType list
   | QueueOrgConnectionStart of OrgId
   | AddOrgToConnection of
      SignalR.Connection *
      OrgId *
      AsyncOperationStatus<Result<OrgId, Err>>
   | ErrorReceived of EventProcessingError

let dispatchContext =
   React.createContext<Msg -> unit> (
      name = "SignalREventDispatchContext",
      defaultValue = ignore
   )

let init () = initState, Cmd.none

let update msg state =
   match msg with
   | QueueOrgConnectionStart orgId ->
      { state with QueuedOrgId = Some orgId }, Cmd.none
   | AddOrgToConnection(conn, orgId, Started) ->
      let addToConn = async {
         let! res = addOrgToConnection conn state.CurrentOrgId orgId
         return Msg.AddOrgToConnection(conn, orgId, Finished res)
      }

      {
         state with
            CurrentOrgId = Some orgId
            QueuedOrgId = None
            RealtimeAccountEvents = []
            RealtimeEmployeeEvents = []
            RealtimeOrgEvents = []
      },
      Cmd.fromAsync addToConn
   | AddOrgToConnection(_, _, Finished(Error _)) ->
      { state with CurrentOrgId = None }, Cmd.none
   | AddOrgToConnection(_, _, Finished(Ok _)) -> state, Cmd.none
   | AddEventSubscriber(componentName, orgId, eventTypes, onPersist, onError) ->
      let updateSubscribers subscribers onPersist =
         let handlers = onPersist, onError

         match state.CurrentOrgId, state.QueuedOrgId with
         | Some existingId, _ when existingId = orgId ->
            Map.add componentName handlers subscribers
         | None, Some queuedId when queuedId = orgId ->
            Map.add componentName handlers subscribers
         | _ ->
            // Reset subscribers when OrgId changes
            Map[componentName, handlers]

      let state = {
         state with
            ParentAccountSubscribers =
               if List.contains EventType.ParentAccount eventTypes then
                  updateSubscribers
                     state.ParentAccountSubscribers
                     (EventPersistedConfirmation.ParentAccount >> onPersist)
               else
                  state.ParentAccountSubscribers
            AccountSubscribers =
               if List.contains EventType.Account eventTypes then
                  updateSubscribers
                     state.AccountSubscribers
                     (EventPersistedConfirmation.Account >> onPersist)
               else
                  state.AccountSubscribers
            EmployeeSubscribers =
               if List.contains EventType.Employee eventTypes then
                  updateSubscribers
                     state.EmployeeSubscribers
                     (EventPersistedConfirmation.Employee >> onPersist)
               else
                  state.EmployeeSubscribers
            OrgSubscribers =
               if List.contains EventType.Org eventTypes then
                  updateSubscribers
                     state.OrgSubscribers
                     (EventPersistedConfirmation.Org >> onPersist)
               else
                  state.OrgSubscribers
      }

      state, Cmd.none
   | RemoveEventSubscriber(componentName, eventTypes) ->
      let state = {
         state with
            ParentAccountSubscribers =
               if List.contains EventType.ParentAccount eventTypes then
                  state.ParentAccountSubscribers.Remove componentName
               else
                  state.ParentAccountSubscribers
            AccountSubscribers =
               if List.contains EventType.Account eventTypes then
                  state.AccountSubscribers.Remove componentName
               else
                  state.AccountSubscribers
            EmployeeSubscribers =
               if List.contains EventType.Employee eventTypes then
                  state.EmployeeSubscribers.Remove componentName
               else
                  state.EmployeeSubscribers
            OrgSubscribers =
               if List.contains EventType.Org eventTypes then
                  state.OrgSubscribers.Remove componentName
               else
                  state.OrgSubscribers
      }

      state, Cmd.none
   | ParentAccountEventPersisted conf ->
      for onPersist, _ in state.ParentAccountSubscribers.Values do
         onPersist conf

      {
         state with
            RealtimeParentAccountEvents =
               conf.EventPersisted :: state.RealtimeParentAccountEvents
      },
      Cmd.none
   | AccountEventPersisted conf ->
      for onPersist, _ in state.AccountSubscribers.Values do
         onPersist conf

      {
         state with
            RealtimeAccountEvents =
               conf.EventPersisted :: state.RealtimeAccountEvents
      },
      Cmd.none
   | EmployeeEventPersisted conf ->
      for onPersist, _ in state.EmployeeSubscribers.Values do
         onPersist conf

      {
         state with
            RealtimeEmployeeEvents =
               conf.EventPersisted :: state.RealtimeEmployeeEvents
      },
      Cmd.none
   | OrgEventPersisted conf ->
      for onPersist, _ in state.OrgSubscribers.Values do
         onPersist conf

      {
         state with
            RealtimeOrgEvents = conf.EventPersisted :: state.RealtimeOrgEvents
      },
      Cmd.none
   | ErrorReceived error ->
      let errorHandlers =
         match error with
         | EventProcessingError.ParentAccount _ ->
            state.ParentAccountSubscribers.Values |> Seq.map snd
         | EventProcessingError.Account _ ->
            state.AccountSubscribers.Values |> Seq.map snd
         | EventProcessingError.Org _ ->
            state.OrgSubscribers.Values |> Seq.map snd
         | EventProcessingError.Employee _ ->
            state.EmployeeSubscribers.Values |> Seq.map snd

      for onError in errorHandlers do
         onError error

      {
         state with
            Errors = error :: state.Errors
      },
      Cmd.none

[<ReactComponent>]
let SignalREventProvider (child: Fable.React.ReactElement) =
   let state, dispatch = React.useElmish (init, update, [||])
   let connection = React.useContext SignalRConnectionProvider.context

   React.useEffect (
      fun () ->
         match connection with
         | Some conn ->
            conn.on (
               "EventProcessingError",
               fun errMsg ->
                  match
                     Serialization.deserialize<EventProcessingError> errMsg
                  with
                  | Error seriErr -> Log.error (string seriErr)
                  | Ok err -> dispatch (Msg.ErrorReceived err)
            )

            let deseriParentAccount =
               Serialization.deserialize<ParentAccountEventPersistedConfirmation>

            let deseriAccount =
               Serialization.deserialize<AccountEventPersistedConfirmation>

            let deseriEmployee =
               Serialization.deserialize<EmployeeEventPersistedConfirmation>

            let deseriOrg =
               Serialization.deserialize<OrgEventPersistedConfirmation>

            conn.on (
               "ParentAccountEventPersistenceConfirmation",
               fun (msg: string) ->
                  match deseriParentAccount msg with
                  | Error err -> Log.error (string err)
                  | Ok msg -> dispatch (Msg.ParentAccountEventPersisted msg)
            )

            conn.on (
               "AccountEventPersistenceConfirmation",
               fun (msg: string) ->
                  match deseriAccount msg with
                  | Error err -> Log.error (string err)
                  | Ok msg -> dispatch (Msg.AccountEventPersisted msg)
            )

            conn.on (
               "EmployeeEventPersistenceConfirmation",
               fun (msg: string) ->
                  match deseriEmployee msg with
                  | Error err -> Log.error (string err)
                  | Ok msg -> dispatch (Msg.EmployeeEventPersisted msg)
            )

            conn.on (
               "OrgEventPersistenceConfirmation",
               fun (msg: string) ->
                  match deseriOrg msg with
                  | Error err -> Log.error (string err)
                  | Ok msg -> dispatch (Msg.OrgEventPersisted msg)
            )
         | _ -> ()
      , [| box connection |]
   )

   React.useEffect (
      fun () ->
         React.createDisposable (fun () ->
            match connection, state.CurrentOrgId with
            | Some conn, Some orgId ->
               conn.removeOrgFromConnectionGroup orgId
               |> Async.Ignore
               |> Async.StartImmediate
            | _ -> ())
      , [| box connection; box (string state.CurrentOrgId) |]
   )

   React.contextProvider (
      context,
      state,
      React.contextProvider (dispatchContext, dispatch, child)
   )

type EventSubscription = {
   ComponentName: string
   OrgId: OrgId option
   EventTypes: EventType list
   OnPersist: EventPersistedConfirmation -> unit
   OnError: EventProcessingError -> unit
}

// Custom hook to subscribe to persisted events received via SignalR.
// The subscription is removed when the consuming component unmounts.
let useEventSubscription (sub: EventSubscription) =
   let state = React.useContext context
   let dispatch = React.useContext dispatchContext
   let connection = React.useContext SignalRConnectionProvider.context

   let orgIdOpt = sub.OrgId

   React.useEffect (
      fun () ->
         match orgIdOpt with
         | Some orgId ->
            // Add subscriber for all requested event types
            Msg.AddEventSubscriber(
               sub.ComponentName,
               orgId,
               sub.EventTypes,
               sub.OnPersist,
               sub.OnError
            )
            |> dispatch
         | _ -> ()

         React.createDisposable (fun _ ->
            // Remove all subscriptions on unmount
            Msg.RemoveEventSubscriber(sub.ComponentName, sub.EventTypes)
            |> dispatch)
      , [| box (string orgIdOpt) |]
   )

   React.useEffect (
      fun () ->
         match connection, orgIdOpt, state.CurrentOrgId with
         | Some conn, Some id, None ->
            dispatch <| Msg.AddOrgToConnection(conn, id, Started)
         | Some conn, Some id, Some existingId when id <> existingId ->
            dispatch <| Msg.AddOrgToConnection(conn, id, Started)
         | None, Some id, _ when (Some id) <> state.QueuedOrgId ->
            dispatch <| Msg.QueueOrgConnectionStart id
         | _ -> ()
      , [| box (string orgIdOpt); box connection |]
   )
