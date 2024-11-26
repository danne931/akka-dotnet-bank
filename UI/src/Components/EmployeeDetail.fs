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
      EmployeeId *
      AsyncOperationStatus<Result<CommandApprovalProgressWithRule option, Err>>
   | OpenRoleEdit
   | CancelRoleEdit
   | SetPendingRole of Role option
   | ConfirmRole of EmployeeCommandReceipt
   | ResendInviteNotification of
      Employee *
      AsyncOperationStatus<Result<unit, Err>>
   | ShowInviteCancellationConfirmation of Employee
   | ConfirmInviteCancellation of
      Employee *
      AsyncOperationStatus<Result<EmployeeCommandReceipt, Err>>
   | RestoreAccess of
      Employee *
      AsyncOperationStatus<Result<EmployeeCommandReceipt, Err>>
   | ApproveAccess of
      Employee *
      CommandApprovalProgressId *
      AsyncOperationStatus<Result<EmployeeCommandReceipt, Err>>
   | DisapproveAccess of
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
   | EmployeeStatus.PendingInviteApproval ->
      Cmd.ofMsg
      <| GetEmployeePendingInviteApproval(employee.EmployeeId, Started)
   | _ -> Cmd.none

let closeRoleSelect state = {
   state with
      IsEditingRole = false
      PendingRole = None
}

let update
   (notifyParentOnUpdate: EmployeeCommandReceipt -> unit)
   (session: UserSession)
   msg
   state
   =
   match msg with
   | GetEmployeePendingInviteApproval(employeeId, Started) ->
      let get = async {
         let! res =
            EmployeeService.getCommandApprovalProgressWithRule
               employeeId
               ApprovableCommandType.InviteEmployee

         return Msg.GetEmployeePendingInviteApproval(employeeId, Finished res)
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
   | ShowInviteCancellationConfirmation employee ->
      let confirm =
         ConfirmAlert(
            "",
            function
            | ConfirmAlertResult.Confirmed ->
               Msg.ConfirmInviteCancellation(employee, Started)
            | ConfirmAlertResult.Dismissed _ -> Msg.DismissInviteCancellation
         )
            .Title("Are you sure you want to cancel this invitation?")
            .Type(AlertType.Question)
            .ShowCloseButton(true)

      state, SweetAlert.Run confirm
   | ConfirmInviteCancellation(employee, Started) ->
      let cancel = async {
         let cmd =
            CancelInvitationCommand.create
               employee.CompositeId
               (InitiatedById session.EmployeeId)
               { Reason = None }
            |> EmployeeCommand.CancelInvitation

         let! res = EmployeeService.submitCommand employee cmd
         return ConfirmInviteCancellation(employee, Finished res)
      }

      state, Cmd.fromAsync cancel
   | ConfirmInviteCancellation(employee, Finished(Ok receipt)) ->
      notifyParentOnUpdate receipt
      state, Alerts.toastSuccessCommand $"Cancelling invite for {employee.Name}"
   | ConfirmInviteCancellation(employee, Finished(Error err)) ->
      Log.error (string err)

      state,
      Alerts.toastCommand
      <| Err.NetworkError(exn $"Issue cancelling invite for {employee.Name}")
   | DismissInviteCancellation -> state, Cmd.none
   | ApproveAccess(employee, cmdApprovalProgressId, Started) ->
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
         return Msg.ApproveAccess(employee, cmdApprovalProgressId, Finished res)
      }

      state, Cmd.fromAsync approve
   | ApproveAccess(employee, _, Finished(Ok receipt)) ->
      notifyParentOnUpdate receipt

      state,
      Alerts.toastSuccessCommand $"Access approval acquired for {employee.Name}"
   | ApproveAccess(_, _, Finished(Error err)) -> state, Alerts.toastCommand err
   | DisapproveAccess(employee, cmdApprovalProgressId, Started) ->
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
            Msg.DisapproveAccess(employee, cmdApprovalProgressId, Finished res)
      }

      state, Cmd.fromAsync approve
   | DisapproveAccess(employee, _, Finished(Ok receipt)) ->
      notifyParentOnUpdate receipt

      state, Alerts.toastSuccessCommand $"Access denied for {employee.Name}"
   | DisapproveAccess(_, _, Finished(Error err)) ->
      state, Alerts.toastCommand err
   | RestoreAccess(employee, Started) ->
      let send = async {
         let cmd =
            RestoreAccessCommand.create
               employee.CompositeId
               (InitiatedById session.EmployeeId)
               { Reference = None }
            |> EmployeeCommand.RestoreAccess

         let! res = EmployeeService.submitCommand employee cmd
         return Msg.RestoreAccess(employee, Finished res)
      }

      state, Cmd.fromAsync send
   | RestoreAccess(employee, Finished(Ok receipt)) ->
      notifyParentOnUpdate receipt
      state, Alerts.toastSuccessCommand $"Access restored for {employee.Name}"
   | RestoreAccess(_, Finished(Error err)) -> state, Alerts.toastCommand err

[<ReactComponent>]
let EmployeeDetailComponent
   (session: UserSession)
   (employee: Employee)
   (notifyParentOnUpdate: EmployeeCommandReceipt -> unit)
   =
   let state, dispatch =
      React.useElmish (init employee, update notifyParentOnUpdate session, [||])

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
      | EmployeeStatus.PendingInviteApproval ->
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
                     dispatch <| Msg.ShowInviteCancellationConfirmation employee
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
                  fun _ -> dispatch <| Msg.RestoreAccess(employee, Started)
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
               | EmployeeStatus.PendingInviteApproval
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
