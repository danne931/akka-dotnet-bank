module Bank.Forms.FormContainer

open Feliz
open Feliz.UseElmish
open Elmish
open Fable.Form.Simple
open Fable.Form.Simple.Pico

open Lib.SharedTypes
open Bank.Employee.Domain
open Bank.Account.Domain
open Bank.Org.Domain
open SignalRBroadcast

[<RequireQualifiedAccess>]
type FormCommand =
   | Account of AccountCommand
   | Employee of EmployeeCommand
   | Org of OrgCommand

   member x.Envelope =
      match x with
      | FormCommand.Account cmd -> cmd.Envelope
      | FormCommand.Employee cmd -> cmd.Envelope
      | FormCommand.Org cmd -> cmd.Envelope

[<RequireQualifiedAccess>]
type FormEntity =
   | Account of Account
   | Employee of Employee
   | Org of Org

[<RequireQualifiedAccess>]
type FormEntityId = Account of AccountId
//| Employee of EmployeeId
//| Org of OrgId

[<RequireQualifiedAccess>]
type FormSubmitReceipt =
   | Account of UIDomain.Account.AccountCommandReceipt
   | Employee of UIDomain.Employee.EmployeeCommandReceipt
   | Org of UIDomain.Org.OrgCommandReceipt

type CommandInProgress = {
   Envelope: Envelope
   FormSubmitReceipt: FormSubmitReceipt option
}

type State<'Values> = {
   FormModel: Form.View.Model<'Values>
   CommandInProgress: CommandInProgress option
   EventsReceivedViaSignalR: Set<CorrelationId>
}

type Msg<'Values> =
   | FormChanged of Form.View.Model<'Values>
   | Submit of
      FormEntity *
      FormCommand *
      AsyncOperationStatus<Result<FormSubmitReceipt, Err>>
   | GetAndSubmit of FormEntityId * FormCommand
   | SignalREventReceived of CorrelationId
   | ErrorReceivedViaSignalR of Err
   | CheckForEventConfirmation of FormSubmitReceipt * attemptNumber: int
   | Noop

let submitCommand (entity: FormEntity) (cmd: FormCommand) = async {
   match entity, cmd with
   | FormEntity.Org org, FormCommand.Org cmd ->
      let! res = OrgService.submitCommand org cmd
      return res |> Result.map FormSubmitReceipt.Org
   | FormEntity.Account account, FormCommand.Account cmd ->
      let! res = AccountService.submitCommand account cmd
      return res |> Result.map FormSubmitReceipt.Account
   | FormEntity.Employee employee, FormCommand.Employee cmd ->
      let! res = EmployeeService.submitCommand employee cmd
      return res |> Result.map FormSubmitReceipt.Employee
   | _ ->
      return "Entity/Command combo incorrect." |> Err.UnexpectedError |> Error
}

let init (values: 'Values) () =
   {
      FormModel = Form.View.idle values
      CommandInProgress = None
      EventsReceivedViaSignalR = Set.empty
   },
   Cmd.none

let update
   (waitForActorProcessingCommand: bool)
   (handlePollingConfirmation: EventPersistedConfirmation -> unit)
   (onSubmit: FormSubmitReceipt -> unit)
   (msg: Msg<'Values>)
   (state: State<'Values>)
   =
   match msg with
   | FormChanged formModel -> { state with FormModel = formModel }, Cmd.none
   | GetAndSubmit(entityId, command) ->
      let getEntity = async {
         match entityId with
         | FormEntityId.Account accountId ->
            let! res = AccountService.getAccount accountId

            match res with
            | Ok(Some account) ->
               return Msg.Submit(FormEntity.Account account, command, Started)
            | Ok None ->
               let err =
                  accountId
                  |> AccountStateTransitionError.AccountNotFound
                  |> Err.AccountStateTransitionError
                  |> Error

               return
                  Msg.Submit(
                     FormEntity.Account Account.empty,
                     command,
                     Finished err
                  )
            | Error err ->
               return
                  Msg.Submit(
                     FormEntity.Account Account.empty,
                     command,
                     Finished(Error err)
                  )
      }

      {
         state with
            FormModel = Form.View.setLoading state.FormModel
      },
      Cmd.fromAsync getEntity
   | Submit(entity, command, Started) ->
      let submit = async {
         let! res = submitCommand entity command
         return Submit(entity, command, Finished res)
      }

      {
         state with
            FormModel = Form.View.setLoading state.FormModel
            CommandInProgress =
               Some {
                  Envelope = command.Envelope
                  FormSubmitReceipt = None
               }
      },
      Cmd.fromAsync submit
   | Submit(_, cmd, Finished(Ok receipt)) ->
      // HTTP request returned 200. Command accepted by network.  Wait
      // for account, employee or org actor to process the command into
      // an event and send out a persistence confirmation via SignalR.
      if not waitForActorProcessingCommand then
         onSubmit receipt
         { state with CommandInProgress = None }, Cmd.none
      else
         // Checking this fixes a scenario where a SignalR event
         // was dispatched from the actor & received before
         // receiving a network response.
         let signalREventReceived =
            state.EventsReceivedViaSignalR
            |> Set.contains cmd.Envelope.CorrelationId

         if signalREventReceived then
            onSubmit receipt
            { state with CommandInProgress = None }, Cmd.none
         else
            let progress =
               state.CommandInProgress
               |> Option.map (fun c -> {
                  Envelope = c.Envelope
                  FormSubmitReceipt = Some receipt
               })

            let delayedMsg = Msg.CheckForEventConfirmation(receipt, 1)

            {
               state with
                  CommandInProgress = progress
            },
            Cmd.fromTimeout 3000 delayedMsg
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
   | SignalREventReceived correlationId ->
      match state.CommandInProgress with
      | Some cmd when cmd.Envelope.CorrelationId = correlationId ->
         let state = {
            state with
               EventsReceivedViaSignalR =
                  state.EventsReceivedViaSignalR |> Set.add correlationId
         }

         match cmd.FormSubmitReceipt with
         // SignalR event received before network response finished.
         | None -> state, Cmd.none
         // SignalR event received after network response.
         | Some receipt ->
            onSubmit receipt
            { state with CommandInProgress = None }, Cmd.none
      | _ -> state, Cmd.none
   // Verify the CommandInProgress was persisted.
   // If a SignalR event doesn't dispatch a Msg.SignalREventReceived within
   // a few seconds of the initial network request to process the command then
   // assume the SignalR message or connection was dropped & revert to
   // polling.
   | CheckForEventConfirmation(receipt, attemptNumber) ->
      let correlationId, confirmation =
         match receipt with
         | FormSubmitReceipt.Account r ->
            r.Envelope.CorrelationId,
            EventPersistedConfirmation.Account {
               Account = r.PendingState
               EventPersisted = r.PendingEvent
               Date = System.DateTime.Now
            }
         | FormSubmitReceipt.Employee r ->
            r.Envelope.CorrelationId,
            EventPersistedConfirmation.Employee {
               Employee = r.PendingState
               EventPersisted = r.PendingEvent
               Date = System.DateTime.Now
            }
         | FormSubmitReceipt.Org r ->
            r.Envelope.CorrelationId,
            EventPersistedConfirmation.Org {
               Org = r.PendingState
               EventPersisted = r.PendingEvent
               Date = System.DateTime.Now
            }

      let checkAgainMsg =
         Msg.CheckForEventConfirmation(receipt, attemptNumber + 1)

      if attemptNumber > 10 then
         Log.error
            "Could not confirm event was processed after several attempts."

         state, Cmd.none
      else
         match state.CommandInProgress with
         | Some cmd when cmd.Envelope.CorrelationId = correlationId ->
            let getReadModel = async {
               let! confirmationMaybe =
                  TransactionService.isEventPersistenceConfirmed correlationId

               match confirmationMaybe with
               | Error e ->
                  Log.error $"Error checking for txn confirmation. {e}"
                  return Msg.Noop
               | Ok false ->
                  do! Async.Sleep 2500
                  return checkAgainMsg
               | Ok true ->
                  handlePollingConfirmation confirmation
                  return Msg.Noop
            }

            state, Cmd.fromAsync getReadModel
         | _ -> state, Cmd.none
   | Noop -> state, Cmd.none

[<ReactComponent>]
let FormContainer
   (props:
      {|
         InitialValues: 'Values
         Form: Form.Form<'Values, Msg<'Values>, IReactProperty>
         Action: Form.View.Action<Msg<'Values>> option
         OnSubmit: FormSubmitReceipt -> unit
         Session: UserSession
         ComponentName: string
         UseEventSubscription: (SignalREventProvider.EventType list) option
      |})
   =
   let signalRDispatch = React.useContext SignalREventProvider.dispatchContext

   // Did not receive a SignalR event in time upon form submission so reverted to polling.
   // If the polling confirmation succeeds then update the SignalR context
   // to mimick a SignalR EventPersisted event being received.
   let handlePollingConfirmation (conf: EventPersistedConfirmation) =
      match conf with
      | EventPersistedConfirmation.Account c ->
         signalRDispatch (SignalREventProvider.Msg.AccountEventPersisted c)
      | EventPersistedConfirmation.Org c ->
         signalRDispatch (SignalREventProvider.Msg.OrgEventPersisted c)
      | EventPersistedConfirmation.Employee c ->
         signalRDispatch (SignalREventProvider.Msg.EmployeeEventPersisted c)

   let state, dispatch =
      React.useElmish (
         init props.InitialValues,
         update
            props.UseEventSubscription.IsSome
            handlePollingConfirmation
            props.OnSubmit,
         [||]
      )

   match props.UseEventSubscription with
   | Some eventTypes ->
      SignalREventProvider.useEventSubscription {
         ComponentName = props.ComponentName
         OrgId = Some props.Session.OrgId
         EventTypes = eventTypes
         OnError =
            React.useCallbackRef (fun err ->
               match state.FormModel.State, state.CommandInProgress with
               | Form.View.State.Loading, (Some cmd) when
                  cmd.Envelope.CorrelationId = err.CorrelationId
                  ->
                  dispatch (Msg.ErrorReceivedViaSignalR err.Error)
               | _ -> ())
         OnPersist =
            fun conf ->
               match conf with
               | EventPersistedConfirmation.Account conf ->
                  let _, evt = AccountEnvelope.unwrap conf.EventPersisted
                  dispatch (Msg.SignalREventReceived evt.CorrelationId)
               | EventPersistedConfirmation.Org conf ->
                  let _, evt = OrgEnvelope.unwrap conf.EventPersisted
                  dispatch (Msg.SignalREventReceived evt.CorrelationId)
               | EventPersistedConfirmation.Employee conf ->
                  let _, evt = EmployeeEnvelope.unwrap conf.EventPersisted
                  dispatch (Msg.SignalREventReceived evt.CorrelationId)
      }
   | None -> ()

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
