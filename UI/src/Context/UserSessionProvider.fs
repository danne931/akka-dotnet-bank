module UserSessionProvider

open Feliz
open Feliz.UseElmish
open Elmish

open Lib.SharedTypes
open Bank.Employee.Domain

type State = {
   CurrentSession: Deferred<Result<UserSession, Err>>
   SelectableSessions: Deferred<Result<UserSession list, Err>>
}

type Msg =
   | LoadDemoSessions of
      OrgId *
      AsyncOperationStatus<Result<UserSession list, Err>>
   | LoadInitialSession of AsyncOperationStatus<Result<UserSession, Err>>
   | SetUserSession of UserSession * AsyncOperationStatus<Result<unit, Err>>

let init () =
   {
      CurrentSession = Deferred.Idle
      SelectableSessions = Deferred.Idle
   },
   Cmd.ofMsg (LoadInitialSession Started)

let update (msg: Msg) (state: State) =
   match msg with
   | LoadInitialSession Started ->
      let load = async {
         let! res = UserSessionService.loginAndGetCurrentUserSession ()
         return Msg.LoadInitialSession(Finished res)
      }

      {
         state with
            CurrentSession = Deferred.InProgress
      },
      Cmd.fromAsync load
   | LoadInitialSession(Finished(Ok session)) ->
      {
         state with
            CurrentSession = Deferred.Resolved(Ok session)
      },
      Cmd.ofMsg (LoadDemoSessions(session.OrgId, Started))
   | LoadInitialSession(Finished(Error err)) ->
      Log.error (string err)

      {
         state with
            CurrentSession = Deferred.Resolved(Error err)
      },
      Cmd.none
   | LoadDemoSessions(orgId, Started) ->
      let load = async {
         let! res = UserSessionService.getDemoUserSessions orgId
         return Msg.LoadDemoSessions(orgId, Finished res)
      }

      {
         state with
            SelectableSessions = Deferred.InProgress
      },
      Cmd.fromAsync load
   | LoadDemoSessions(_, Finished(Ok sessions)) ->
      {
         state with
            SelectableSessions = Deferred.Resolved(Ok sessions)
      },
      Cmd.none
   | LoadDemoSessions(_, Finished(Error err)) ->
      Log.error (string err)

      {
         state with
            SelectableSessions = Deferred.Resolved(Error err)
      },
      Cmd.none
   | SetUserSession(session, Started) ->
      let overrideSession = async {
         let! res =
            UserSessionService.overrideDemoUserSession session.EmployeeId

         return Msg.SetUserSession(session, Finished res)
      }

      state, Cmd.fromAsync overrideSession
   | SetUserSession(session, Finished(Ok _)) ->
      {
         state with
            CurrentSession = Deferred.Resolved(Ok session)
      },
      Cmd.none
   | SetUserSession(_, Finished(Error err)) ->
      Log.error (string err)
      state, Alerts.toastCommand err

let context =
   React.createContext<Deferred<Result<UserSession, Err>>> (
      name = "UserSessionContext",
      defaultValue = Deferred.Idle
   )

let demoSessionsContext =
   React.createContext<Deferred<Result<UserSession list, Err>>> (
      name = "SelectableUserSessionsContext",
      defaultValue = Deferred.Idle
   )

// Dispatch context will allow overriding the user session with
// Msg.SetUserSession via NavigationComponent user selection (demo purposes).
let dispatchContext =
   React.createContext<Msg -> unit> (
      name = "UserSessionDispatchContext",
      defaultValue = ignore
   )

[<ReactComponent>]
let UserSessionProvider (child: Fable.React.ReactElement) =
   let state, dispatch = React.useElmish (init, update, [||])

   React.contextProvider (
      context,
      state.CurrentSession,
      React.contextProvider (
         demoSessionsContext,
         state.SelectableSessions,
         React.contextProvider (dispatchContext, dispatch, child)
      )
   )

[<ReactComponent>]
let UserSessionSuspense (childInit: UserSession -> Fable.React.ReactElement) =
   let session = React.useContext context

   match session with
   | Deferred.Resolved(Ok session) -> childInit session
   | _ -> Html.progress []
