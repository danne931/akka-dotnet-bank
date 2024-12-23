module ApprovalDashboard

open Feliz
open Feliz.Router
open Fable.FontAwesome
open Elmish
open Feliz.UseElmish
open Elmish.SweetAlert

open Bank.Employee.Domain
open UIDomain.Employee
open Lib.SharedTypes
open Lib.Time

type State = {
   Approvals: Deferred<Result<CommandApprovalProgressWithRule list option, Err>>
}

type Msg =
   | GetCommandApprovals of
      OrgId *
      AsyncOperationStatus<
         Result<CommandApprovalProgressWithRule list option, Err>
       >
   | ShowCommandDeclineConfirmation of
      UserSession *
      CommandApprovalProgressWithRule
   | AcquireCommandApproval of
      UserSession *
      CommandApprovalProgressWithRule *
      AsyncOperationStatus<Result<EmployeeCommandReceipt, Err>>
   | DeclineCommand of
      UserSession *
      CommandApprovalProgressWithRule *
      AsyncOperationStatus<Result<EmployeeCommandReceipt, Err>>
   | DismissConfirmation

let init (orgCtx: OrgProvider.State) () =
   { Approvals = Deferred.Idle },
   match orgCtx with
   | Deferred.Resolved(Ok(Some o)) ->
      Cmd.ofMsg (Msg.GetCommandApprovals(o.Org.OrgId, Started))
   | _ -> Cmd.none

let update msg state =
   match msg with
   | GetCommandApprovals(orgId, Started) ->
      let getApprovals = async {
         let! approvals = EmployeeService.getCommandApprovals orgId
         return Msg.GetCommandApprovals(orgId, Finished approvals)
      }

      {
         state with
            Approvals = Deferred.InProgress
      },
      Cmd.fromAsync getApprovals
   | GetCommandApprovals(_, Finished(Ok res)) ->
      {
         state with
            Approvals = Deferred.Resolved(Ok res)
      },
      Cmd.none
   | GetCommandApprovals(_, Finished(Error err)) ->
      {
         state with
            Approvals = Deferred.Resolved(Error err)
      },
      Cmd.none
   | ShowCommandDeclineConfirmation(session, approvalProgress) ->
      let confirm =
         ConfirmAlert(
            "",
            function
            | ConfirmAlertResult.Confirmed ->
               Msg.DeclineCommand(session, approvalProgress, Started)
            | ConfirmAlertResult.Dismissed _ -> Msg.DismissConfirmation
         )
            .Title(
               $"Are you sure you want to decline this {approvalProgress.Command.Display}?"
            )
            .Type(AlertType.Question)
            .ShowCloseButton(true)

      state, SweetAlert.Run confirm
   | AcquireCommandApproval(session, approval, Started) ->
      let acquireApproval = async {
         let! employeeRes =
            EmployeeService.getEmployee
               approval.Command.OrgId
               (EmployeeId.fromEntityId approval.Command.EntityId)

         match employeeRes with
         | Error err ->
            return
               Msg.AcquireCommandApproval(
                  session,
                  approval,
                  Finished(Error err)
               )
         | Ok None ->
            return
               Msg.AcquireCommandApproval(
                  session,
                  approval,
                  Finished(
                     Error(Err.UnexpectedError "Missing dependency: Employee")
                  )
               )
         | Ok(Some employee) ->
            let cmd =
               CommandApprovalProgress.AcquireCommandApproval.create
                  employee.CompositeId
                  {
                     CommandId = approval.CommandProgressId
                     CommandType = approval.Command.CommandType
                     ApprovedBy = InitiatedById session.EmployeeId
                  }
               |> EmployeeCommand.AcquireCommandApproval

            let! res = EmployeeService.submitCommand employee cmd
            return Msg.AcquireCommandApproval(session, approval, Finished res)
      }

      state, Cmd.fromAsync acquireApproval
   | AcquireCommandApproval(session, approval, Finished(Ok _)) ->
      {
         state with
            Approvals =
               match state.Approvals with
               | Deferred.Resolved(Ok(Some approvals)) ->
                  approvals
                  |> List.map (fun a ->
                     if a.CommandProgressId = approval.CommandProgressId then
                        let approver: CommandApprovalRule.Approver = {
                           Name = session.Name
                           EmployeeId = session.EmployeeId
                        }

                        {
                           a with
                              ApprovedBy =
                                 match a.ApprovedBy with
                                 | None -> Some [ approver ]
                                 | Some approvers ->
                                    Some(approver :: approvers)
                        }
                     else
                        a)
                  |> Some
                  |> Ok
                  |> Deferred.Resolved
               | _ -> state.Approvals
      },
      Cmd.none
   | AcquireCommandApproval(_, _, Finished(Error err)) ->
      Log.error (string err)
      state, Alerts.toastCommand err
   | DeclineCommand(session, approval, Started) ->
      let decline = async {
         let! employeeRes =
            EmployeeService.getEmployee
               approval.Command.OrgId
               (EmployeeId.fromEntityId approval.Command.EntityId)

         match employeeRes with
         | Error err ->
            return Msg.DeclineCommand(session, approval, Finished(Error err))
         | Ok None ->
            return
               Msg.DeclineCommand(
                  session,
                  approval,
                  Finished(
                     Error(Err.UnexpectedError "Missing dependency: Employee")
                  )
               )
         | Ok(Some employee) ->
            let cmd =
               CommandApprovalProgress.DeclineCommandApproval.create
                  employee.CompositeId
                  {
                     DeclinedBy = InitiatedById session.EmployeeId
                     CommandId = approval.CommandProgressId
                     CommandType = approval.Command.CommandType
                  }
               |> EmployeeCommand.DeclineCommandApproval

            let! res = EmployeeService.submitCommand employee cmd
            return Msg.DeclineCommand(session, approval, Finished res)
      }

      state, Cmd.fromAsync decline
   | DeclineCommand(session, approval, Finished(Ok _)) ->
      {
         state with
            Approvals =
               match state.Approvals with
               | Deferred.Resolved(Ok(Some approvals)) ->
                  approvals
                  |> List.map (fun a ->
                     if a.CommandProgressId = approval.CommandProgressId then
                        let declinedBy: CommandApprovalRule.Approver = {
                           Name = session.FirstName + " " + session.LastName
                           EmployeeId = session.EmployeeId
                        }

                        {
                           a with
                              Status = CommandApprovalProgress.Status.Declined
                              DeclinedBy = Some declinedBy
                        }
                     else
                        a)
                  |> Some
                  |> Ok
                  |> Deferred.Resolved
               | _ -> state.Approvals
      },
      Cmd.none
   | DeclineCommand(_, _, Finished(Error err)) ->
      Log.error (string err)
      state, Alerts.toastCommand err
   | DismissConfirmation -> state, Cmd.none

let approversMsg (approvers: CommandApprovalRule.Approver list) =
   approvers
   |> List.fold (fun acc approver -> $"{acc}{approver.Name}, ") ""
   |> _.TrimEnd()
   |> _.TrimEnd(',')

let renderPendingApprovals
   dispatch
   (session: UserSession)
   (approvals: CommandApprovalProgressWithRule list)
   =
   React.fragment [
      for o in approvals do
         let approvedByCnt =
            o.ApprovedBy |> Option.map _.Length |> Option.defaultValue 0

         let approvalsRemaining =
            o.ApprovedBy
            |> Option.map (fun approvedBy ->
               List.except approvedBy o.PermittedApprovers)
            |> Option.defaultValue o.PermittedApprovers

         let mayApproveOrDeny =
            CommandApprovalProgressWithRule.mayApproveOrDeny
               o
               session.EmployeeId

         classyNode Html.article [ "command-pending-approval" ] [
            Html.header [
               classyNode Html.div [ "grid" ] [
                  Html.p o.Command.Display

                  Html.small
                     $"{approvedByCnt} of {o.PermittedApprovers.Length} approvals acquired"

                  Html.a [
                     if not mayApproveOrDeny then
                        attr.classes [ "secondary" ]
                        attr.ariaDisabled true

                     attr.children [ Fa.i [ Fa.Solid.Check ] [] ]

                     attr.href ""

                     attr.onClick (fun e ->
                        e.preventDefault ()

                        if mayApproveOrDeny then
                           dispatch
                           <| Msg.AcquireCommandApproval(session, o, Started))

                     attr.custom ("data-tooltip", "Approve")
                     attr.custom ("data-placement", "bottom")
                  ]

                  Html.a [
                     if not mayApproveOrDeny then
                        attr.classes [ "secondary" ]
                        attr.ariaDisabled true

                     attr.children [ Fa.i [ Fa.Solid.Ban ] [] ]

                     attr.href ""

                     attr.onClick (fun e ->
                        e.preventDefault ()

                        if mayApproveOrDeny then
                           dispatch
                           <| Msg.ShowCommandDeclineConfirmation(session, o))

                     attr.custom ("data-tooltip", "Decline")
                     attr.custom ("data-placement", "bottom")
                  ]
               ]
            ]

            Html.p (
               match o.Command with
               | ApprovableCommand.DomesticTransfer c ->
                  $"{Money.format c.Data.Amount} domestic transfer from {c.Data.Sender.Name} to {c.Data.Recipient.Name} requested by {o.RequestedBy.Name}."
               | ApprovableCommand.InviteEmployee c ->
                  $"Invite employee {c.Data.Name} requested by {o.RequestedBy.Name}"
               | ApprovableCommand.UpdateEmployeeRole c ->
                  $"Update {c.Data.Name}'s role from {c.Data.PriorRole} to {c.Data.Role} requested by {o.RequestedBy.Name}."
               | ApprovableCommand.FulfillPlatformPayment c ->
                  let pay = c.Data.RequestedPayment.BaseInfo
                  $"{Money.format pay.Amount} payment requested to {pay.Payer.OrgName} by {o.RequestedBy.Name}"
               | ApprovableCommand.InternalTransferBetweenOrgs c ->
                  $"{Money.format c.Data.Amount} transfer to {c.Data.Recipient.Name} requested by {o.RequestedBy.Name}."
            )

            match o.Status with
            | CommandApprovalProgress.Status.Approved ->
               Html.small [
                  attr.classes [ "success" ]
                  attr.text
                     $"Approval completed on {DateTime.format o.LastUpdate}"
               ]
            | CommandApprovalProgress.Status.Declined ->
               let declinedBy =
                  o.DeclinedBy
                  |> Option.map _.Name
                  |> Option.defaultValue "Unknown"

               classyNode Html.div [ "approval-count" ] [
                  Html.small [
                     attr.classes [ "debit" ]
                     attr.text
                        $"Declined on {DateTime.format o.LastUpdate} by {declinedBy}"
                  ]
               ]
            | CommandApprovalProgress.Status.Pending ->
               classyNode Html.div [ "approval-count" ] [
                  Html.small "Approvals remaining:"
                  Html.p (approversMsg approvalsRemaining)
               ]

               classyNode Html.div [ "approval-count" ] [
                  Html.small "Approvals acquired:"

                  match o.ApprovedBy with
                  | Some approvers -> Html.p (approversMsg approvers)
                  | None -> Html.p "None"
               ]
         ]
   ]

[<ReactComponent>]
let ApprovalDashboardComponent
   (url: Routes.ApprovalsUrl)
   (session: UserSession)
   =
   let orgCtx = React.useContext OrgProvider.context
   let orgDispatch = React.useContext OrgProvider.dispatchContext

   let state, dispatch = React.useElmish (init orgCtx, update, [| box orgCtx |])

   classyNode Html.div [ "approval-dashboard" ] [
      classyNode Html.main [ "container-fluid" ] [
         classyNode Html.nav [ "link-menu" ] [
            Html.a [
               attr.text "Pending Approvals"
               attr.href ""
               if url <> Routes.ApprovalsUrl.Approvals then
                  attr.className "secondary"

               attr.onClick (fun e ->
                  e.preventDefault ()
                  Router.navigate Routes.ApprovalsUrl.BasePath)
            ]

            Html.a [
               attr.text "Approval Rule Management"
               attr.href ""
               if url <> Routes.ApprovalsUrl.ApprovalRuleManagement then
                  attr.className "secondary"

               attr.onClick (fun e ->
                  e.preventDefault ()

                  Router.navigate
                     Routes.ApprovalsUrl.ApprovalRuleManagementPath)
            ]
         ]

         Html.hr []
         Html.br []

         match url with
         | Routes.ApprovalsUrl.Approvals ->
            match state.Approvals with
            | Deferred.Resolved(Ok(Some approvals)) ->
               renderPendingApprovals dispatch session approvals
            | Deferred.Resolved(Ok None) ->
               Html.p "No commands require approval."
            | _ -> Html.progress []
         | Routes.ApprovalsUrl.ApprovalRuleManagement ->
            Html.p "TODO: Implement approval rule management dashboard"
         | Routes.ApprovalsUrl.NotFound -> Html.p "Uh oh! Unknown URL."
      ]
   ]
