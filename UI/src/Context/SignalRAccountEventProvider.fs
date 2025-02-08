module SignalRAccountEventProvider

open Feliz
open Feliz.UseElmish
open Elmish
open FsToolkit.ErrorHandling

open Bank.Account.Domain
open Lib.SharedTypes

type SignalRAccountEventContext = {
   Subscribers: Map<string, (AccountEventPersistedConfirmation -> unit)>
   RealtimeEvents: AccountEvent list
   Errors: AccountEventRejected list
   CurrentOrgId: OrgId option
   QueuedOrgId: OrgId option
}

let private initState = {
   Subscribers = Map.empty
   RealtimeEvents = []
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
   React.createContext<SignalRAccountEventContext> (
      name = "SignalRAccountEventContext",
      defaultValue = initState
   )

type Msg =
   | AccountEventPersisted of AccountEventPersistedConfirmation
   | AddAccountEventSubscriber of
      componentName: string *
      OrgId *
      (AccountEventPersistedConfirmation -> unit)
   | RemoveAccountEventSubscriber of componentName: string
   | QueueOrgConnectionStart of OrgId
   | AddOrgToConnection of
      SignalR.Connection *
      OrgId *
      AsyncOperationStatus<Result<OrgId, Err>>
   | AccountErrorReceived of AccountEventRejected

let dispatchContext =
   React.createContext<Msg -> unit> (
      name = "SignalRAccountEventDispatchContext",
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
            RealtimeEvents = []
      },
      Cmd.fromAsync addToConn
   | AddOrgToConnection(_, _, Finished(Error _)) ->
      { state with CurrentOrgId = None }, Cmd.none
   | AddOrgToConnection(_, _, Finished(Ok _)) -> state, Cmd.none
   | AddAccountEventSubscriber(componentName, orgId, callback) ->
      {
         state with
            Subscribers =
               match state.CurrentOrgId, state.QueuedOrgId with
               | Some existingId, _ when existingId = orgId ->
                  Map.add componentName callback state.Subscribers
               | None, Some queuedId when queuedId = orgId ->
                  Map.add componentName callback state.Subscribers
               | _ ->
                  // Reset subscribers when OrgId changes
                  Map[componentName, callback]
      },
      Cmd.none
   | RemoveAccountEventSubscriber componentName ->
      {
         state with
            Subscribers = state.Subscribers.Remove componentName
      },
      Cmd.none
   | AccountEventPersisted conf ->
      for subscriberCallback in state.Subscribers.Values do
         subscriberCallback conf

      {
         state with
            RealtimeEvents = conf.EventPersisted :: state.RealtimeEvents
      },
      Cmd.none
   | AccountErrorReceived(msg) ->
      {
         state with
            Errors = msg :: state.Errors
      },
      Cmd.none

[<ReactComponent>]
let SignalRAccountEventProvider (child: Fable.React.ReactElement) =
   let state, dispatch = React.useElmish (init, update, [||])
   let connection = React.useContext SignalRConnectionProvider.context

   React.useEffect (
      fun () ->
         match connection with
         | Some conn ->
            let onSerializedAccountEventError (errMsg: string) =
               let deseri =
                  Serialization.deserialize<AccountEventRejected> errMsg

               match deseri with
               | Error seriErr -> Log.error (string seriErr)
               | Ok msg -> msg |> Msg.AccountErrorReceived |> dispatch

            conn.on (
               "AccountEventPersistenceFail",
               onSerializedAccountEventError
            )

            conn.on (
               "AccountEventValidationFail",
               onSerializedAccountEventError
            )

            let deseri =
               Serialization.deserialize<AccountEventPersistedConfirmation>

            conn.on (
               "AccountEventPersistenceConfirmation",
               fun (msg: string) ->
                  match deseri msg with
                  | Error err -> Log.error (string err)
                  | Ok msg -> dispatch <| Msg.AccountEventPersisted msg
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

type AccountEventSubscription = {
   ComponentName: string
   OrgId: OrgId option
   OnReceive: AccountEventPersistedConfirmation -> unit
}

// Custom hook to subscribe to persisted account events received
// via SignalR.
// The subscription is removed when the consuming component unmounts.
let useAccountEventSubscription (sub: AccountEventSubscription) =
   let state = React.useContext context
   let dispatch = React.useContext dispatchContext
   let connection = React.useContext SignalRConnectionProvider.context

   let orgIdOpt = sub.OrgId

   React.useEffect (
      fun () ->
         match orgIdOpt with
         | Some orgId ->
            (sub.ComponentName, orgId, sub.OnReceive)
            |> Msg.AddAccountEventSubscriber
            |> dispatch
         | _ -> ()

         React.createDisposable (fun _ ->
            sub.ComponentName |> Msg.RemoveAccountEventSubscriber |> dispatch)
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
