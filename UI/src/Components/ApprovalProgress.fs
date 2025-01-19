module ApprovalProgress

open Feliz
open Fable.FontAwesome
open Elmish
open Feliz.UseElmish
open Elmish.SweetAlert

open Bank.Employee.Domain
open Bank.Org.Domain
open UIDomain.Org
open Lib.SharedTypes
open Lib.Time

type State = {
   Approvals: Deferred<CommandApprovalProgressWithRuleMaybe>
}

type Msg =
   | GetCommandApprovals of
      OrgId *
      AsyncOperationStatus<CommandApprovalProgressWithRuleMaybe>
   | ShowCommandDeclineConfirmation of
      Org *
      UserSession *
      CommandApprovalProgressWithRule
   | AcquireCommandApproval of
      Org *
      UserSession *
      CommandApprovalProgressWithRule *
      AsyncOperationStatus<Result<OrgCommandReceipt, Err>>
   | DeclineCommand of
      Org *
      UserSession *
      CommandApprovalProgressWithRule *
      AsyncOperationStatus<Result<OrgCommandReceipt, Err>>
   | DismissConfirmation

let init orgId () =
   { Approvals = Deferred.Idle },
   Cmd.ofMsg (Msg.GetCommandApprovals(orgId, Started))

let update msg state =
   match msg with
   | GetCommandApprovals(orgId, Started) ->
      let getApprovals = async {
         let! approvals = OrgService.getCommandApprovals orgId
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
   | ShowCommandDeclineConfirmation(org, session, approvalProgress) ->
      let confirm =
         ConfirmAlert(
            "",
            function
            | ConfirmAlertResult.Confirmed ->
               Msg.DeclineCommand(org, session, approvalProgress, Started)
            | ConfirmAlertResult.Dismissed _ -> Msg.DismissConfirmation
         )
            .Title(
               $"Are you sure you want to decline this {approvalProgress.Command.Display}?"
            )
            .Type(AlertType.Question)
            .ShowCloseButton(true)

      state, SweetAlert.Run confirm
   | AcquireCommandApproval(org, session, approval, Started) ->
      let acquireApproval = async {
         let cmd =
            CommandApprovalProgress.AcquireCommandApproval.create org.OrgId {
               RuleId = approval.RuleId
               Command = approval.Command
               ProgressId = approval.CommandProgressId
               ApprovedBy = {
                  Name = session.Name
                  EmployeeId = session.EmployeeId
               }
            }
            |> OrgCommand.AcquireCommandApproval

         let! res = OrgService.submitCommand org cmd
         return Msg.AcquireCommandApproval(org, session, approval, Finished res)
      }

      state, Cmd.fromAsync acquireApproval
   | AcquireCommandApproval(_, session, approval, Finished(Ok _)) ->
      {
         state with
            Approvals =
               match state.Approvals with
               | Deferred.Resolved(Ok(Some approvals)) ->
                  approvals
                  |> Map.change
                        approval.CommandProgressId
                        (Option.map (fun a ->
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
                           }))
                  |> Some
                  |> Ok
                  |> Deferred.Resolved
               | _ -> state.Approvals
      },
      Cmd.none
   | AcquireCommandApproval(_, _, _, Finished(Error err)) ->
      Log.error (string err)
      state, Alerts.toastCommand err
   | DeclineCommand(org, session, approval, Started) ->
      let decline = async {
         let cmd =
            CommandApprovalProgress.DeclineCommandApproval.create org.OrgId {
               RuleId = approval.RuleId
               DeclinedBy = {
                  EmployeeId = session.EmployeeId
                  Name = session.Name
               }
               ProgressId = approval.CommandProgressId
               Command = approval.Command
            }
            |> OrgCommand.DeclineCommandApproval

         let! res = OrgService.submitCommand org cmd
         return Msg.DeclineCommand(org, session, approval, Finished res)
      }

      state, Cmd.fromAsync decline
   | DeclineCommand(_, session, approval, Finished(Ok _)) ->
      {
         state with
            Approvals =
               match state.Approvals with
               | Deferred.Resolved(Ok(Some approvals)) ->
                  approvals
                  |> Map.change
                        approval.CommandProgressId
                        (Option.map (fun a ->
                           let declinedBy: CommandApprovalRule.Approver = {
                              Name = session.Name
                              EmployeeId = session.EmployeeId
                           }

                           {
                              a with
                                 Status =
                                    CommandApprovalProgress.Status.Declined
                                 DeclinedBy = Some declinedBy
                           }))
                  |> Some
                  |> Ok
                  |> Deferred.Resolved
               | _ -> state.Approvals
      },
      Cmd.none
   | DeclineCommand(_, _, _, Finished(Error err)) ->
      Log.error (string err)
      state, Alerts.toastCommand err
   | DismissConfirmation -> state, Cmd.none

let approversMsg (approvers: CommandApprovalRule.Approver list) =
   approvers
   |> List.fold (fun acc approver -> $"{acc}{approver.Name}, ") ""
   |> _.TrimEnd()
   |> _.TrimEnd(',')

[<ReactComponent>]
let ApprovalProgressComponent
   (session: UserSession)
   (org: Org)
   (onOrgUpdate: OrgCommandReceipt -> unit)
   =
   let state, dispatch =
      React.useElmish (init session.OrgId, update, [| box session.OrgId |])

   match state.Approvals with
   | Deferred.Resolved(Ok(Some approvals)) ->
      React.fragment [
         for o in approvals.Values do
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
                              <| Msg.AcquireCommandApproval(
                                 org,
                                 session,
                                 o,
                                 Started
                              ))

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
                              <| Msg.ShowCommandDeclineConfirmation(
                                 org,
                                 session,
                                 o
                              ))

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
               | CommandApprovalProgress.Status.Terminated ->
                  Html.small
                     $"Approval terminated early on {DateTime.format o.LastUpdate}"
            ]
      ]
   | Deferred.Resolved(Ok None) -> Html.p "No commands require approval."
   | _ -> Html.progress []
