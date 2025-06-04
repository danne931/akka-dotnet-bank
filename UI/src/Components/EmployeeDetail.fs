module EmployeeDetail

open Feliz
open Feliz.UseElmish
open Elmish
open Elmish.SweetAlert

open Bank.Employee.Domain
open Bank.Org.Domain
open CommandApproval
open UIDomain.Employee
open UIDomain.Org
open Dropdown
open Bank.Employee.Forms.EmployeeRoleForm
open Lib.SharedTypes

type State = {
   PendingRole: Role option
   IsEditingRole: bool
}

type Msg =
   | OpenRoleEdit
   | CancelRoleEdit
   | SetPendingRole of Role option
   | ConfirmRole of EmployeeCommandReceipt
   | SubmitRoleUpdateForApproval
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
      Org *
      CommandApprovalProgress.T *
      AsyncOperationStatus<Result<OrgCommandReceipt, Err>>
   | DisapproveAccess of
      UserSession *
      Org *
      CommandApprovalProgress.T *
      AsyncOperationStatus<Result<OrgCommandReceipt, Err>>
   | DismissInviteCancellation

let init () =
   {
      PendingRole = None
      IsEditingRole = false
   },
   Cmd.none

let closeRoleSelect state = {
   state with
      IsEditingRole = false
      PendingRole = None
}

let update
   (onEmployeeUpdate: EmployeeCommandReceipt -> unit)
   (orgDispatch: OrgProvider.Msg -> unit)
   msg
   state
   =
   match msg with
   | OpenRoleEdit -> { state with IsEditingRole = true }, Cmd.none
   | CancelRoleEdit -> closeRoleSelect state, Cmd.none
   | SetPendingRole role -> { state with PendingRole = role }, Cmd.none
   | ConfirmRole receipt ->
      onEmployeeUpdate receipt
      closeRoleSelect state, Cmd.none
   | SubmitRoleUpdateForApproval ->
      closeRoleSelect state,
      Alerts.toastSuccessCommand "Role update submitted for approval."
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
               session.AsInitiator
               { Reason = None }
            |> EmployeeCommand.CancelInvitation

         let! res = EmployeeService.submitCommand employee cmd
         return ConfirmInviteCancellation(session, employee, Finished res)
      }

      state, Cmd.fromAsync cancel
   | ConfirmInviteCancellation(_, employee, Finished(Ok receipt)) ->
      onEmployeeUpdate receipt
      state, Alerts.toastSuccessCommand $"Cancelling invite for {employee.Name}"
   | ConfirmInviteCancellation(_, employee, Finished(Error err)) ->
      Log.error (string err)

      state,
      Alerts.toastCommand
      <| Err.NetworkError(exn $"Issue cancelling invite for {employee.Name}")
   | DismissInviteCancellation -> state, Cmd.none
   | ApproveAccess(session, org, progress, Started) ->
      let approve = async {
         let cmd =
            CommandApprovalProgress.AcquireCommandApproval.create session.OrgId {
               RuleId = progress.RuleId
               ApprovedBy = {
                  EmployeeId = session.EmployeeId
                  EmployeeName = session.Name
               }
               Command = progress.CommandToInitiateOnApproval
               ProgressId = progress.ProgressId
            }
            |> OrgCommand.AcquireCommandApproval

         let! res = OrgService.submitCommand org cmd

         return Msg.ApproveAccess(session, org, progress, Finished res)
      }

      state, Cmd.fromAsync approve
   | ApproveAccess(_, _, _, Finished(Ok receipt)) ->
      orgDispatch (OrgProvider.Msg.OrgUpdated receipt.PendingState)
      state, Alerts.toastSuccessCommand "Access approval acquired."
   | ApproveAccess(_, _, _, Finished(Error err)) ->
      state, Alerts.toastCommand err
   | DisapproveAccess(session, org, progress, Started) ->
      let approve = async {
         let cmd =
            CommandApprovalProgress.DeclineCommandApproval.create org.OrgId {
               RuleId = progress.RuleId
               DeclinedBy = {
                  EmployeeId = session.EmployeeId
                  EmployeeName = session.Name
               }
               Command = progress.CommandToInitiateOnApproval
               ProgressId = progress.ProgressId
            }
            |> OrgCommand.DeclineCommandApproval

         let! res = OrgService.submitCommand org cmd

         return Msg.DisapproveAccess(session, org, progress, Finished res)
      }

      state, Cmd.fromAsync approve
   | DisapproveAccess(_, _, _, Finished(Ok receipt)) ->
      orgDispatch (OrgProvider.Msg.OrgUpdated receipt.PendingState)
      state, Alerts.toastSuccessCommand "Access denied."
   | DisapproveAccess(_, _, _, Finished(Error err)) ->
      state, Alerts.toastCommand err
   | RestoreAccess(session, employee, Started) ->
      let send = async {
         let cmd =
            RestoreAccessCommand.create employee.CompositeId session.AsInitiator {
               Name = employee.Name
               Reference = None
               InviteToken = InviteToken.generate ()
            }
            |> EmployeeCommand.RestoreAccess

         let! res = EmployeeService.submitCommand employee cmd
         return Msg.RestoreAccess(session, employee, Finished res)
      }

      state, Cmd.fromAsync send
   | RestoreAccess(_, employee, Finished(Ok receipt)) ->
      onEmployeeUpdate receipt
      state, Alerts.toastSuccessCommand $"Access restored for {employee.Name}"
   | RestoreAccess(_, _, Finished(Error err)) -> state, Alerts.toastCommand err

[<ReactComponent>]
let EmployeeDetailComponent
   (session: UserSession)
   (employee: Employee)
   (org: Org)
   (onEmployeeUpdate: EmployeeCommandReceipt -> unit)
   =
   let orgDispatch = React.useContext OrgProvider.dispatchContext

   let state, dispatch =
      React.useElmish (init, update onEmployeeUpdate orgDispatch, [||])

   let updatedRolePendingApproval =
      employeeRolePendingApproval
         org.CommandApprovalProgress.Values
         employee.EmployeeId

   let employeeInviteProgressOpt =
      match employee.Status with
      | EmployeeStatus.PendingInviteApproval inviteInfo ->
         Option.map2
            (fun rule progress -> rule, progress)
            (org.CommandApprovalRules.TryFind inviteInfo.RuleId)
            (org.CommandApprovalProgress.TryFind inviteInfo.ProgressId)
      | _ -> None

   let dropdownMenuOptions =
      match employee.Status with
      | EmployeeStatus.Active ->
         match updatedRolePendingApproval with
         | None ->
            Some [
               {
                  Text = "Edit Role"
                  OnClick = fun _ -> dispatch Msg.OpenRoleEdit
                  IsSelected = state.IsEditingRole
               }
            ]
         | Some _ -> None
      | EmployeeStatus.PendingInviteApproval _ ->
         employeeInviteProgressOpt
         |> Option.bind (fun (rule, progress) ->
            if
               CommandApprovalProgress.canManageProgress
                  rule
                  progress
                  session.EmployeeId
            then
               Some progress
            else
               None)
         |> Option.map (fun progress -> [
            {
               Text = "Approve Invite"
               OnClick =
                  fun _ ->
                     dispatch
                     <| Msg.ApproveAccess(session, org, progress, Started)
               IsSelected = false
            }

            {
               Text = "Disapprove Invite"
               OnClick =
                  fun _ ->
                     dispatch
                     <| Msg.DisapproveAccess(session, org, progress, Started)
               IsSelected = false
            }
         ])
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

            Html.p [
               attr.role "employee-tag"
               match updatedRolePendingApproval with
               | Some pendingRole ->
                  attr.text $"{employee.Role.Display} -> {pendingRole.Display}"

                  attr.custom (
                     "data-tooltip",
                     $"{pendingRole.Display} role pending approval."
                  )
               | None -> attr.text employee.Role.Display
            ]

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

         match employeeInviteProgressOpt with
         | Some(rule, progress) ->
            let approvalRequiredFrom =
               CommandApprovalProgress.remainingApprovalRequiredBy rule progress
               |> List.fold
                     (fun acc approver ->
                        if acc = "" then
                           approver.DisplayName
                        else
                           $"{acc}, {approver.DisplayName}")
                     ""

            let approvalRequiredFrom =
               if approvalRequiredFrom <> "" then
                  $"Employee invite requires approval from {approvalRequiredFrom}."
               else
                  ""

            Html.blockquote
               $"""
               {progress.ApprovedBy.Length} out of {rule.Approvers.Length} approvals obtained.
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
            employee
            (Msg.ConfirmRole >> dispatch)
            (fun approvalRequest ->
               approvalRequest
               |> OrgCommand.RequestCommandApproval
               |> OrgProvider.Msg.OrgCommand
               |> orgDispatch

               dispatch Msg.SubmitRoleUpdateForApproval)

      state.PendingRole
      |> Option.defaultValue employee.Role
      |> EmployeePermissions.render
   ]
