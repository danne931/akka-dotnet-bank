module Bank.Employee.Forms.FormContainer

open Feliz
open Feliz.UseElmish
open Elmish
open Fable.Form.Simple

open Fable.Form.Simple.Pico
open Lib.SharedTypes
open Bank.Employee.Domain
open UIDomain.Employee

type State<'Values> = {
   FormModel: Form.View.Model<'Values>
   CommandInProgress: CorrelationId option
}

type Msg<'Values> =
   | FormChanged of Form.View.Model<'Values>
   | Submit of
      Employee *
      EmployeeCommand *
      AsyncOperationStatus<Result<EmployeeCommandReceipt, Err>>
   | ErrorReceivedViaSignalR of Err

let init (values: 'Values) () =
   {
      FormModel = Form.View.idle values
      CommandInProgress = None
   },
   Cmd.none

let update
   (onSubmit: EmployeeCommandReceipt -> unit)
   (msg: Msg<'Values>)
   (state: State<'Values>)
   =
   match msg with
   | FormChanged formModel -> { state with FormModel = formModel }, Cmd.none
   | Submit(employee, command, Started) ->
      let submit = async {
         let! res = EmployeeService.submitCommand employee command
         return Submit(employee, command, Finished res)
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
      {
         state with
            CommandInProgress = None
            FormModel.State = Form.View.State.Error(err.HumanFriendly)
      },
      Alerts.toastCommand err

[<ReactComponent>]
let EmployeeFormContainer
   (props:
      {|
         InitialValues: 'Values
         Form: Form.Form<'Values, Msg<'Values>, IReactProperty>
         Action: Form.View.Action<Msg<'Values>> option
         OnSubmit: EmployeeCommandReceipt -> unit
         Session: UserSession
      |})
   =
   let state, dispatch =
      React.useElmish (init props.InitialValues, update props.OnSubmit, [||])

   // Listen for errors in Employee and Account actors related to the submitted
   // command CorrelationId.
   // Ex: An employee purchase is submitted via an Employee actor.
   //     -> The purchase amount puts the daily accrued card amaount over the limit
   //        so a validation error is broadcasted over SignalR from the employee
   //        actor.
   //
   // Ex: An employee purchase is submitted via an Employee actor.
   //     -> The employee actor sends a debit command to the Account actor.
   //     -> The account balance is not sufficient to fulfill the purchase so a
   //        validation error is broadcasted over SignalR from the Account
   //        actor.
   SignalREventProvider.useEventSubscription {
      ComponentName = "EmployeeFormContainer"
      OrgId = Some props.Session.OrgId
      EventTypes = [
         SignalREventProvider.EventType.Employee
         SignalREventProvider.EventType.Account
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
