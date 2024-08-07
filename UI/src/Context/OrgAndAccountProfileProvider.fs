module OrgProvider

open Feliz
open Feliz.UseElmish
open Elmish

open Lib.SharedTypes
open Bank.Account.Domain
open Bank.Employee.Domain

type State = Deferred<Result<OrgWithAccountProfiles option, Err>>

let updateProfiles
   (state: State)
   (transform: OrgWithAccountProfiles -> OrgWithAccountProfiles)
   : State
   =
   (Deferred.map << Result.map << Option.map) transform state

type Msg =
   | Load of
      OrgId *
      AsyncOperationStatus<Result<OrgWithAccountProfiles option, Err>>
   | BalanceUpdated of
      {|
         AccountId: AccountId
         Balance: decimal
         PersistedEvent: AccountEvent
      |}
   | AccountCreated of AccountProfile

let private initState = Deferred.Idle

let init () = initState, Cmd.none

let update msg state =
   match msg with
   | Load(orgId, Started) ->
      let load = async {
         let! res = AccountService.getOrgAndAccountProfiles orgId
         return Msg.Load(orgId, Finished res)
      }

      Deferred.InProgress, Cmd.fromAsync load
   | Load(_, Finished(Ok(Some(orgWithProfiles)))) ->
      orgWithProfiles |> Some |> Ok |> Deferred.Resolved, Cmd.none
   | Load _ ->
      Log.error "Issue loading org + account profiles."
      state, Cmd.none
   | BalanceUpdated o ->
      let persistedEvt = o.PersistedEvent
      let balance = o.Balance
      let accountId = o.AccountId

      let transform state =
         state.AccountProfiles
         |> Map.tryFind accountId
         |> Option.map (fun profile ->
            let profile = { profile with Balance = balance }

            let profile =
               match persistedEvt with
               | AccountEvent.InternalTransferPending e -> {
                  profile with
                     DailyInternalTransferAccrued =
                        profile.DailyInternalTransferAccrued
                        + e.Data.BaseInfo.Amount
                 }
               | AccountEvent.DomesticTransferPending e -> {
                  profile with
                     DailyDomesticTransferAccrued =
                        profile.DailyDomesticTransferAccrued
                        + e.Data.BaseInfo.Amount
                 }
               | AccountEvent.InternalTransferRejected e -> {
                  profile with
                     DailyInternalTransferAccrued =
                        profile.DailyInternalTransferAccrued
                        - e.Data.BaseInfo.Amount
                 }
               | AccountEvent.DomesticTransferRejected e -> {
                  profile with
                     DailyDomesticTransferAccrued =
                        profile.DailyDomesticTransferAccrued
                        - e.Data.BaseInfo.Amount
                 }
               | _ -> profile

            {
               Org = state.Org
               AccountProfiles =
                  Map.add accountId profile state.AccountProfiles
               Balance = state.Balance
            })
         |> Option.defaultValue state

      let state = updateProfiles state transform

      let internalTransferTransform state =
         let internalRecipient =
            match persistedEvt with
            | AccountEvent.InternalTransferPending e ->
               let info = e.Data.BaseInfo

               state.AccountProfiles
               |> Map.tryFind info.RecipientId
               |> Option.map (fun a -> {
                  a with
                     Balance = a.Balance + info.Amount
               })
            | AccountEvent.InternalTransferRejected e ->
               let info = e.Data.BaseInfo

               state.AccountProfiles
               |> Map.tryFind info.RecipientId
               |> Option.map (fun a -> {
                  a with
                     Balance = a.Balance - info.Amount
               })
            | _ -> None

         let profiles =
            match internalRecipient with
            | Some profile ->
               Map.add profile.AccountId profile state.AccountProfiles
            | None -> state.AccountProfiles

         {
            Org = state.Org
            AccountProfiles = profiles
            Balance = state.Balance
         }

      let state = updateProfiles state internalTransferTransform

      state, Cmd.none
   | AccountCreated profile ->
      updateProfiles state (fun state -> {
         state with
            AccountProfiles =
               state.AccountProfiles |> Map.add profile.AccountId profile
      }),
      Cmd.none

let context =
   React.createContext<State> (name = "OrgContext", defaultValue = initState)

let dispatchContext =
   React.createContext<Msg -> unit> (
      name = "OrgDispatchContext",
      defaultValue = ignore
   )

[<ReactComponent>]
let OrgProvider (child: Fable.React.ReactElement) =
   let state, dispatch = React.useElmish (init, update, [||])
   let session = React.useContext UserSessionProvider.context

   React.useEffect (
      fun () ->
         match session with
         | Deferred.Resolved session when state = Deferred.Idle ->
            dispatch <| Msg.Load(session.OrgId, Started)
         | _ -> ()
      , [| box session |]
   )

   React.contextProvider (
      context,
      state,
      React.contextProvider (dispatchContext, dispatch, child)
   )
