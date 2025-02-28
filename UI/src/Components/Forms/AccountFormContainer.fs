module Bank.Account.Forms.FormContainer

open Feliz
open Feliz.UseElmish
open Elmish
open Fable.Form.Simple

open Fable.Form.Simple.Pico
open Lib.SharedTypes
open Bank.Account.Domain
open UIDomain.Account

type State<'Values> = {
   FormModel: Form.View.Model<'Values>
   CommandInProgress: CorrelationId option
}

type Msg<'Values> =
   | FormChanged of Form.View.Model<'Values>
   | GetAccountAndSubmit of AccountId * AccountCommand
   | Submit of
      Account *
      AccountCommand *
      AsyncOperationStatus<Result<AccountCommandReceipt, Err>>
   | ErrorReceivedViaSignalR of Err

let init (initValues: 'Values) () =
   {
      FormModel = Form.View.idle initValues
      CommandInProgress = None
   },
   Cmd.none

let update
   (onSubmit: AccountCommandReceipt -> unit)
   (msg: Msg<'Values>)
   (state: State<'Values>)
   =
   match msg with
   | FormChanged formModel -> { state with FormModel = formModel }, Cmd.none
   | GetAccountAndSubmit(accountId, command) ->
      let getAccount = async {
         let! account = AccountService.getAccount accountId

         match account with
         | Ok(Some account) -> return Msg.Submit(account, command, Started)
         | Ok None ->
            let err =
               AccountStateTransitionError.AccountNotActive
               |> Err.AccountStateTransitionError
               |> Error

            return Msg.Submit(Account.empty, command, Finished err)
         | Error err ->
            return Msg.Submit(Account.empty, command, Finished(Error err))
      }

      {
         FormModel = Form.View.setLoading state.FormModel
         CommandInProgress = Some command.Envelope.CorrelationId
      },
      Cmd.fromAsync getAccount
   | Submit(account, command, Started) ->
      let submit = async {
         let! res = AccountService.submitCommand account command
         return Submit(account, command, Finished res)
      }

      {
         FormModel = Form.View.setLoading state.FormModel
         CommandInProgress = Some command.Envelope.CorrelationId
      },
      Cmd.fromAsync submit
   | Submit(_, _, Finished(Ok receipt)) ->
      onSubmit receipt
      state, Cmd.none
   | Submit(_, command, Finished(Error err)) ->
      Log.error $"Error submitting command {command} {err}"

      {
         state with
            CommandInProgress = None
            FormModel.State = Form.View.State.Error(err.HumanFriendly)
      },
      Alerts.toastCommand err
   | ErrorReceivedViaSignalR err ->
      Log.error $"Error received via SignalR {err}"

      {
         state with
            CommandInProgress = None
            FormModel.State = Form.View.State.Error(err.HumanFriendly)
      },
      Alerts.toastCommand err


[<ReactComponent>]
let AccountFormContainer
   (props:
      {|
         InitialValues: 'Values
         Form: Form.Form<'Values, Msg<'Values>, IReactProperty>
         Action: Form.View.Action<Msg<'Values>> option
         OnSubmit: AccountCommandReceipt -> unit
         Session: Bank.Employee.Domain.UserSession
      |})
   =
   let state, dispatch =
      React.useElmish (init props.InitialValues, update props.OnSubmit, [||])

   SignalREventProvider.useEventSubscription {
      ComponentName = "AccountFormContainer"
      OrgId = Some props.Session.OrgId
      EventTypes = [ SignalREventProvider.EventType.Account ]
      OnPersist = ignore
      OnError =
         React.useCallbackRef (fun err ->
            match state.FormModel.State, state.CommandInProgress with
            | Form.View.State.Loading, (Some inProgressId) when
               inProgressId = err.CorrelationId
               ->
               dispatch (Msg.ErrorReceivedViaSignalR err.Error)
            | _ -> ())
   }

   Form.View.asHtml
      {
         Dispatch = dispatch
         OnChange = FormChanged
         Action =
            props.Action
            |> Option.defaultValue (Form.View.Action.SubmitOnly "Submit")
         Validation = Form.View.ValidateOnSubmit
      }
      props.Form
      state.FormModel
