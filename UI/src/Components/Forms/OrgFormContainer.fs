module Bank.Org.Forms.FormContainer

open Feliz
open Feliz.UseElmish
open Elmish
open Fable.Form.Simple

open Fable.Form.Simple.Pico
open Lib.SharedTypes
open Bank.Org.Domain
open UIDomain.Org

type State<'Values> = {
   FormModel: Form.View.Model<'Values>
   CommandInProgress: CorrelationId option
}

type Msg<'Values> =
   | FormChanged of Form.View.Model<'Values>
   | Submit of
      Org *
      OrgCommand *
      AsyncOperationStatus<Result<OrgCommandReceipt, Err>>
   | ErrorReceivedViaSignalR of Err

let init (values: 'Values) () =
   {
      FormModel = Form.View.idle values
      CommandInProgress = None
   },
   Cmd.none

let update
   (onSubmit: OrgCommandReceipt -> unit)
   (msg: Msg<'Values>)
   (state: State<'Values>)
   =
   match msg with
   | FormChanged formModel -> { state with FormModel = formModel }, Cmd.none
   | Submit(org, command, Started) ->
      let submit = async {
         let! res = OrgService.submitCommand org command
         return Submit(org, command, Finished res)
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
let OrgFormContainer
   (props:
      {|
         InitialValues: 'Values
         Form: Form.Form<'Values, Msg<'Values>, IReactProperty>
         OnSubmit: OrgCommandReceipt -> unit
         Action: Form.View.Action<Msg<'Values>> option
         Session: Bank.Employee.Domain.UserSession
      |})
   =
   let state, dispatch =
      React.useElmish (init props.InitialValues, update props.OnSubmit, [||])

   SignalREventProvider.useEventSubscription {
      ComponentName = "OrgFormContainer"
      OrgId = Some props.Session.OrgId
      EventTypes = [
         SignalREventProvider.EventType.Account
         SignalREventProvider.EventType.Employee
         SignalREventProvider.EventType.Org
      ]
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
