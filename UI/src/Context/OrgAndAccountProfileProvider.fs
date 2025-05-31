module OrgProvider

open Feliz
open Feliz.UseElmish
open Elmish

open Lib.SharedTypes
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open CommandApproval
open SignalRBroadcast

type State = Deferred<Result<OrgWithAccountProfiles option, Err>>

let updateState
   (state: State)
   (transform: OrgWithAccountProfiles -> OrgWithAccountProfiles)
   : State
   =
   (Deferred.map << Result.map << Option.map) transform state

type Msg =
   | Load of
      OrgId *
      AsyncOperationStatus<Result<OrgWithAccountProfiles option, Err>>
   | AccountCreated of Account
   | AccountUpdated of AccountEventPersistedConfirmation
   | ParentAccountUpdated of ParentAccountEventPersistedConfirmation
   | OrgUpdated of Org
   | OrgCommand of OrgCommand
   | CommandApprovalRulesLoaded of CommandApprovalRule list

let private initState = Deferred.Idle

let init () = initState, Cmd.none

let update msg state =
   match msg with
   | Load(orgId, Started) ->
      let load = async {
         let! res = OrgService.getOrgAndAccountProfiles orgId
         return Msg.Load(orgId, Finished res)
      }

      Deferred.InProgress, Cmd.fromAsync load
   | Load(_, Finished(Ok res)) -> res |> Ok |> Deferred.Resolved, Cmd.none
   | Load(_, Finished(Error err)) ->
      Log.error $"Issue loading org + account profiles. {err}"
      state, Cmd.none
   | OrgCommand cmd ->
      let state =
         updateState state (fun o ->
            let validation =
               Org.stateTransition
                  {
                     Info = o.Org
                     Events = []
                     AccrualMetrics = Map.empty
                  }
                  cmd

            match validation with
            | Error _ -> o
            | Ok(_, newState) -> { o with Org = newState.Info })

      state, Cmd.none
   | OrgUpdated org ->
      let state = updateState state (fun o -> { o with Org = org })
      state, Cmd.none
   | ParentAccountUpdated conf ->
      let state =
         match conf.EventPersisted with
         | ParentAccountEvent.RegisteredDomesticTransferRecipient e ->
            updateState state (fun o -> {
               o with
                  DomesticTransferRecipients =
                     o.DomesticTransferRecipients
                     |> Map.add
                           e.Data.Recipient.RecipientAccountId
                           e.Data.Recipient
            })
         | ParentAccountEvent.EditedDomesticTransferRecipient e ->
            updateState state (fun o -> {
               o with
                  DomesticTransferRecipients =
                     o.DomesticTransferRecipients
                     |> Map.add
                           e.Data.Recipient.RecipientAccountId
                           e.Data.Recipient
            })
         | ParentAccountEvent.NicknamedDomesticTransferRecipient e ->
            updateState state (fun o -> {
               o with
                  DomesticTransferRecipients =
                     o.DomesticTransferRecipients
                     |> Map.change
                           e.Data.RecipientId
                           (Option.map (fun recipient -> {
                              recipient with
                                 Nickname = e.Data.Nickname
                           }))
            })
         | _ -> state

      state, Cmd.none
   | AccountUpdated conf ->
      let accountId = conf.Account.AccountId
      let evt = conf.EventPersisted

      let transform state =
         state.AccountProfiles
         |> Map.tryFind accountId
         |> Option.map (fun profile ->
            let metrics = profile.Metrics

            let internalTransferWithinOrg (amount: decimal) = {
               metrics with
                  DailyInternalTransferWithinOrg =
                     metrics.DailyInternalTransferWithinOrg + amount
                  MonthlyInternalTransferWithinOrg =
                     metrics.MonthlyInternalTransferWithinOrg + amount
            }

            let internalTransferBetweenOrgs (amount: decimal) = {
               metrics with
                  DailyInternalTransferBetweenOrgs =
                     metrics.DailyInternalTransferBetweenOrgs + amount
                  MonthlyInternalTransferBetweenOrgs =
                     metrics.MonthlyInternalTransferBetweenOrgs + amount
            }

            let domesticTransfer (amount: decimal) = {
               metrics with
                  DailyDomesticTransfer =
                     metrics.DailyDomesticTransfer + amount
                  MonthlyDomesticTransfer =
                     metrics.MonthlyDomesticTransfer + amount
            }

            let metrics =
               match evt with
               | AccountEvent.InternalTransferWithinOrgPending e ->
                  internalTransferWithinOrg e.Data.BaseInfo.Amount
               | AccountEvent.InternalTransferWithinOrgFailed e ->
                  internalTransferWithinOrg -e.Data.BaseInfo.Amount
               | AccountEvent.InternalAutomatedTransferPending e ->
                  internalTransferWithinOrg e.Data.BaseInfo.Amount
               | AccountEvent.InternalAutomatedTransferFailed e ->
                  internalTransferWithinOrg -e.Data.BaseInfo.Amount
               | AccountEvent.DomesticTransferPending e ->
                  domesticTransfer e.Data.BaseInfo.Amount
               | AccountEvent.DomesticTransferFailed e ->
                  domesticTransfer -e.Data.BaseInfo.Amount
               | AccountEvent.InternalTransferBetweenOrgsPending e ->
                  internalTransferBetweenOrgs e.Data.BaseInfo.Amount
               | AccountEvent.InternalTransferBetweenOrgsFailed e ->
                  internalTransferBetweenOrgs -e.Data.BaseInfo.Amount
               | AccountEvent.PlatformPaymentPaid e -> {
                  metrics with
                     DailyPaymentPaid =
                        metrics.DailyPaymentPaid + e.Data.BaseInfo.Amount
                     MonthlyPaymentPaid =
                        metrics.MonthlyPaymentPaid + e.Data.BaseInfo.Amount
                 }
               | AccountEvent.DebitedAccount e -> {
                  metrics with
                     DailyPurchase = metrics.DailyPurchase + e.Data.Amount
                     MonthlyPurchase = metrics.MonthlyPurchase + e.Data.Amount
                 }
               | _ -> metrics

            let profile = {
               Metrics = metrics
               Account = conf.Account
            }

            let profiles = Map.add accountId profile state.AccountProfiles

            {
               Org = state.Org
               AccountProfiles = profiles
               Balance = profiles |> Map.values |> Seq.sumBy _.Account.Balance
               DomesticTransferRecipients = state.DomesticTransferRecipients
            })
         |> Option.defaultValue state

      let state = updateState state transform

      let internalTransferTransform state =
         let recipientBalanceUpdate recipientId transferAmount =
            state.AccountProfiles
            |> Map.tryFind recipientId
            |> Option.map (fun a ->
               let account = {
                  a.Account with
                     Balance = a.Account.Balance + transferAmount
               }

               { a with Account = account })

         let recipient =
            match evt with
            | AccountEvent.InternalTransferWithinOrgPending e ->
               recipientBalanceUpdate
                  e.Data.BaseInfo.Recipient.AccountId
                  e.Data.BaseInfo.Amount
            | AccountEvent.InternalAutomatedTransferPending e ->
               recipientBalanceUpdate
                  e.Data.BaseInfo.Recipient.AccountId
                  e.Data.BaseInfo.Amount
            | AccountEvent.InternalTransferWithinOrgFailed e ->
               recipientBalanceUpdate
                  e.Data.BaseInfo.Recipient.AccountId
                  -e.Data.BaseInfo.Amount
            | AccountEvent.InternalAutomatedTransferFailed e ->
               recipientBalanceUpdate
                  e.Data.BaseInfo.Recipient.AccountId
                  -e.Data.BaseInfo.Amount
            | _ -> None

         let profiles =
            match recipient with
            | Some profile ->
               Map.add profile.Account.AccountId profile state.AccountProfiles
            | None -> state.AccountProfiles

         {
            Org = state.Org
            AccountProfiles = profiles
            Balance = state.Balance
            DomesticTransferRecipients = state.DomesticTransferRecipients
         }

      let state = updateState state internalTransferTransform

      state, Cmd.none
   | AccountCreated account ->
      let profile = {
         Account = account
         Metrics = AccountMetrics.empty
      }

      updateState state (fun state -> {
         state with
            AccountProfiles =
               state.AccountProfiles |> Map.add account.AccountId profile
      }),
      Cmd.none
   | CommandApprovalRulesLoaded rules ->
      let state =
         updateState state (fun o ->
            let org = {
               o.Org with
                  CommandApprovalRules =
                     rules
                     |> List.map (fun rule -> rule.RuleId, rule)
                     |> Map.ofList
            }

            { o with Org = org })

      state, Cmd.none

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
         | Deferred.Resolved(Ok session) when state = Deferred.Idle ->
            dispatch <| Msg.Load(session.OrgId, Started)
         | _ -> ()
      , [| box session |]
   )

   React.contextProvider (
      context,
      state,
      React.contextProvider (dispatchContext, dispatch, child)
   )
