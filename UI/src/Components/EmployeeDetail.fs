module EmployeeDetail

open Feliz
open Feliz.UseElmish
open Elmish
open Elmish.SweetAlert

open Bank.Employee.Domain
open UIDomain.Employee
open Dropdown
open Bank.Employee.Forms.EmployeeRoleForm
open Lib.SharedTypes

type State = {
   PendingRole: Role option
   IsEditingRole: bool
   EmployeeInviteApprovalProgress:
      Deferred<Result<CommandApprovalProgressWithRule option, Err>>
}

type Msg =
   | GetEmployeePendingInviteApproval of
      CommandApprovalProgressId *
      AsyncOperationStatus<Result<CommandApprovalProgressWithRule option, Err>>
   | OpenRoleEdit
   | CancelRoleEdit
   | SetPendingRole of Role option
   | ConfirmRole of EmployeeCommandReceipt
   | ResendInviteNotification of
      Employee *
      AsyncOperationStatus<Result<unit, Err>>
   | ShowInviteCancellationConfirmation of UserSession * Employee
   | ConfirmInviteCancellation of
      UserSession *
      Employee *
      AsyncOperationStatus<Result<EmployeeCommandReceipt, Err>>
   | RestoreAccess of
      UserSession *
      Employee *
      AsyncOperationStatus<Result<EmployeeCommandReceipt, Err>>
   | ApproveAccess of
      UserSession *
      Employee *
      CommandApprovalProgressId *
      AsyncOperationStatus<Result<EmployeeCommandReceipt, Err>>
   | DisapproveAccess of
      UserSession *
      Employee *
      CommandApprovalProgressId *
      AsyncOperationStatus<Result<EmployeeCommandReceipt, Err>>
   | DismissInviteCancellation

let init (employee: Employee) =
   {
      PendingRole = None
      IsEditingRole = false
      EmployeeInviteApprovalProgress = Deferred.Idle
   },
   match employee.Status with
   | EmployeeStatus.PendingInviteApproval approval ->
      Cmd.ofMsg
      <| GetEmployeePendingInviteApproval(approval.ProgressId, Started)
   | _ -> Cmd.none

let closeRoleSelect state = {
   state with
      IsEditingRole = false
      PendingRole = None
}

let update (notifyParentOnUpdate: EmployeeCommandReceipt -> unit) msg state =
   match msg with
   | GetEmployeePendingInviteApproval(progressId, Started) ->
      let get = async {
         let! res =
            EmployeeService.getCommandApprovalProgressWithRule progressId

         return Msg.GetEmployeePendingInviteApproval(progressId, Finished res)
      }

      {
         state with
            EmployeeInviteApprovalProgress = Deferred.InProgress
      },
      Cmd.fromAsync get
   | GetEmployeePendingInviteApproval(_, Finished(Ok res)) ->
      {
         state with
            EmployeeInviteApprovalProgress = Deferred.Resolved(Ok res)
      },
      Cmd.none
   | GetEmployeePendingInviteApproval(_, Finished(Error err)) ->
      {
         state with
            EmployeeInviteApprovalProgress = Deferred.Resolved(Error err)
      },
      Alerts.toastCommand err
   | OpenRoleEdit -> { state with IsEditingRole = true }, Cmd.none
   | CancelRoleEdit -> closeRoleSelect state, Cmd.none
   | SetPendingRole role -> { state with PendingRole = role }, Cmd.none
   | ConfirmRole receipt ->
      notifyParentOnUpdate receipt
      closeRoleSelect state, Cmd.none
   | ResendInviteNotification(employee, Started) ->
      let send = async {
         let! res = EmployeeService.resendEmployeeInvitation employee
         return Msg.ResendInviteNotification(employee, Finished res)
      }

      state, Cmd.fromAsync send
   | ResendInviteNotification(employee, Finished(Ok _)) ->
      state, Alerts.toastSuccessCommand $"Invite resent to {employee.Name}"
   | ResendInviteNotification(employee, Finished(Error _)) ->
      state,
      Alerts.toastCommand
      <| Err.NetworkError(exn $"Issue resending invite to {employee.Email}")
   | ShowInviteCancellationConfirmation(session, employee) ->
      let confirm =
         ConfirmAlert(
            "",
            function
            | ConfirmAlertResult.Confirmed ->
               Msg.ConfirmInviteCancellation(session, employee, Started)
            | ConfirmAlertResult.Dismissed _ -> Msg.DismissInviteCancellation
         )
            .Title("Are you sure you want to cancel this invitation?")
            .Type(AlertType.Question)
            .ShowCloseButton(true)

      state, SweetAlert.Run confirm
   | ConfirmInviteCancellation(session, employee, Started) ->
      let cancel = async {
         let cmd =
            CancelInvitationCommand.create
               employee.CompositeId
               (InitiatedById session.EmployeeId)
               { Reason = None }
            |> EmployeeCommand.CancelInvitation

         let! res = EmployeeService.submitCommand employee cmd
         return ConfirmInviteCancellation(session, employee, Finished res)
      }

      state, Cmd.fromAsync cancel
   | ConfirmInviteCancellation(_, employee, Finished(Ok receipt)) ->
      notifyParentOnUpdate receipt
      state, Alerts.toastSuccessCommand $"Cancelling invite for {employee.Name}"
   | ConfirmInviteCancellation(_, employee, Finished(Error err)) ->
      Log.error (string err)

      state,
      Alerts.toastCommand
      <| Err.NetworkError(exn $"Issue cancelling invite for {employee.Name}")
   | DismissInviteCancellation -> state, Cmd.none
   | ApproveAccess(session, employee, cmdApprovalProgressId, Started) ->
      let approve = async {
         let cmd =
            CommandApprovalProgress.AcquireCommandApproval.create
               employee.CompositeId
               {
                  ApprovedBy = InitiatedById session.EmployeeId
                  CommandId = cmdApprovalProgressId
                  CommandType = ApprovableCommandType.InviteEmployee
               }
            |> EmployeeCommand.AcquireCommandApproval

         let! res = EmployeeService.submitCommand employee cmd

         return
            Msg.ApproveAccess(
               session,
               employee,
               cmdApprovalProgressId,
               Finished res
            )
      }

      state, Cmd.fromAsync approve
   | ApproveAccess(_, employee, _, Finished(Ok receipt)) ->
      notifyParentOnUpdate receipt

      state,
      Alerts.toastSuccessCommand $"Access approval acquired for {employee.Name}"
   | ApproveAccess(_, _, _, Finished(Error err)) ->
      state, Alerts.toastCommand err
   | DisapproveAccess(session, employee, cmdApprovalProgressId, Started) ->
      let approve = async {
         let cmd =
            CommandApprovalProgress.DeclineCommandApproval.create
               employee.CompositeId
               {
                  DeclinedBy = InitiatedById session.EmployeeId
                  CommandId = cmdApprovalProgressId
                  CommandType = ApprovableCommandType.InviteEmployee
               }
            |> EmployeeCommand.DeclineCommandApproval

         let! res = EmployeeService.submitCommand employee cmd

         return
            Msg.DisapproveAccess(
               session,
               employee,
               cmdApprovalProgressId,
               Finished res
            )
      }

      state, Cmd.fromAsync approve
   | DisapproveAccess(_, employee, _, Finished(Ok receipt)) ->
      notifyParentOnUpdate receipt

      state, Alerts.toastSuccessCommand $"Access denied for {employee.Name}"
   | DisapproveAccess(_, _, _, Finished(Error err)) ->
      state, Alerts.toastCommand err
   | RestoreAccess(session, employee, Started) ->
      let send = async {
         let cmd =
            RestoreAccessCommand.create
               employee.CompositeId
               (InitiatedById session.EmployeeId)
               {
                  Name = employee.Name
                  Reference = None
               }
            |> EmployeeCommand.RestoreAccess

         let! res = EmployeeService.submitCommand employee cmd
         return Msg.RestoreAccess(session, employee, Finished res)
      }

      state, Cmd.fromAsync send
   | RestoreAccess(_, employee, Finished(Ok receipt)) ->
      notifyParentOnUpdate receipt
      state, Alerts.toastSuccessCommand $"Access restored for {employee.Name}"
   | RestoreAccess(_, _, Finished(Error err)) -> state, Alerts.toastCommand err

[<ReactComponent>]
let EmployeeDetailComponent
   (session: UserSession)
   (employee: Employee)
   (notifyParentOnUpdate: EmployeeCommandReceipt -> unit)
   =
   let state, dispatch =
      React.useElmish (init employee, update notifyParentOnUpdate, [||])

   let dropdownMenuOptions =
      match employee.Status with
      | EmployeeStatus.Active ->
         Some [
            {
               Text = "Edit Role"
               OnClick = fun _ -> dispatch Msg.OpenRoleEdit
               IsSelected = state.IsEditingRole
            }
         ]
      | EmployeeStatus.PendingInviteApproval _ ->
         match state.EmployeeInviteApprovalProgress with
         | Deferred.Resolved(Ok(Some progress)) when
            CommandApprovalProgressWithRule.mayApproveOrDeny
               progress
               session.EmployeeId
            ->
            Some [
               {
                  Text = "Approve Invite"
                  OnClick =
                     fun _ ->
                        dispatch
                        <| Msg.ApproveAccess(
                           session,
                           employee,
                           progress.CommandProgressId,
                           Started
                        )
                  IsSelected = false
               }

               {
                  Text = "Disapprove Invite"
                  OnClick =
                     fun _ ->
                        dispatch
                        <| Msg.DisapproveAccess(
                           session,
                           employee,
                           progress.CommandProgressId,
                           Started
                        )
                  IsSelected = false
               }
            ]
         | _ -> None
      | EmployeeStatus.PendingInviteConfirmation _ ->
         Some [
            {
               Text = "Cancel Invite"
               OnClick =
                  fun _ ->
                     dispatch
                     <| Msg.ShowInviteCancellationConfirmation(
                        session,
                        employee
                     )
               IsSelected = false
            }
            {
               Text = "Resend Invite"
               OnClick =
                  fun _ ->
                     dispatch <| Msg.ResendInviteNotification(employee, Started)
               IsSelected = false
            }
         ]
      | EmployeeStatus.Closed ->
         Some [
            {
               IsSelected = false
               Text = "Restore Access"
               OnClick =
                  fun _ ->
                     dispatch <| Msg.RestoreAccess(session, employee, Started)
            }
         ]
      | _ -> None

   React.fragment [
      classyNode Html.div [ "employee-detail-top-section" ] [
         Html.div [
            Html.p employee.Name

            Html.p [ attr.role "employee-tag"; attr.text employee.Role.Display ]

            Html.p [
               attr.role "employee-tag"
               attr.text employee.Status.Display

               match employee.Status with
               | EmployeeStatus.InitialEmptyState
               | EmployeeStatus.Active -> ()
               | EmployeeStatus.PendingInviteApproval _
               | EmployeeStatus.PendingRestoreAccessApproval
               | EmployeeStatus.PendingInviteConfirmation _ ->
                  attr.classes [ "pending" ]
               | EmployeeStatus.Closed
               | EmployeeStatus.ReadyForDelete -> attr.classes [ "closed" ]
            ]
         ]

         Html.div [ Html.small (string employee.Email) ]

         match state.EmployeeInviteApprovalProgress with
         | Deferred.Resolved(Ok(Some progress)) ->
            let approvedByCnt =
               progress.ApprovedBy
               |> Option.map _.Length
               |> Option.defaultValue 0

            let approvalRequiredFrom =
               progress.ApprovedBy
               |> Option.defaultValue []
               |> fun approvedBy ->
                  List.except approvedBy progress.PermittedApprovers
                  |> List.fold
                        (fun acc approver ->
                           if acc = "" then
                              approver.Name
                           else
                              $"{acc}, {approver.Name}")
                        ""

            let approvalRequiredFrom =
               if approvalRequiredFrom <> "" then
                  $"Employee invite requires approval from {approvalRequiredFrom}."
               else
                  ""

            Html.blockquote
               $"""
               {approvedByCnt} out of {progress.PermittedApprovers.Length} approvals obtained.
               {approvalRequiredFrom}
               """
         | _ -> ()

         match dropdownMenuOptions with
         | Some options ->
            DropdownComponent {|
               Direction = DropdownDirection.LTR
               ShowCaret = false
               Button = None
               Items = options
            |}
         | None -> ()
      ]

      Html.hr []

      if state.IsEditingRole then
         EmployeeRoleFormComponent
            (fun () -> Msg.CancelRoleEdit |> dispatch)
            (Some >> Msg.SetPendingRole >> dispatch)
            (Msg.ConfirmRole >> dispatch)
            employee

      state.PendingRole
      |> Option.defaultValue employee.Role
      |> EmployeePermissions.render
   ]
