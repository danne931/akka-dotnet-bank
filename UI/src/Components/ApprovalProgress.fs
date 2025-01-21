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

// NOTE: This Feliz Elmish component does not manage state.
//       The Feliz Elmish update handler is just being used for
//       async side effects.
type State = { Nothing: bool }

type Msg =
   | ShowCommandDeclineConfirmation of
      Org *
      UserSession *
      CommandApprovalProgress.T
   | AcquireCommandApproval of
      Org *
      UserSession *
      CommandApprovalProgress.T *
      AsyncOperationStatus<Result<OrgCommandReceipt, Err>>
   | DeclineCommand of
      Org *
      UserSession *
      CommandApprovalProgress.T *
      AsyncOperationStatus<Result<OrgCommandReceipt, Err>>
   | DismissConfirmation

let init () = { Nothing = true }, Cmd.none

let update (onOrgUpdate: OrgCommandReceipt -> unit) msg state =
   match msg with
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
               $"Are you sure you want to decline this {approvalProgress.CommandToInitiateOnApproval.Display}?"
            )
            .Type(AlertType.Question)
            .ShowCloseButton(true)

      state, SweetAlert.Run confirm
   | AcquireCommandApproval(org, session, approval, Started) ->
      let acquireApproval = async {
         let cmd =
            CommandApprovalProgress.AcquireCommandApproval.create org.OrgId {
               RuleId = approval.RuleId
               Command = approval.CommandToInitiateOnApproval
               ProgressId = approval.ProgressId
               ApprovedBy = {
                  EmployeeName = session.Name
                  EmployeeId = session.EmployeeId
               }
            }
            |> OrgCommand.AcquireCommandApproval

         let! res = OrgService.submitCommand org cmd
         return Msg.AcquireCommandApproval(org, session, approval, Finished res)
      }

      state, Cmd.fromAsync acquireApproval
   | AcquireCommandApproval(_, _, _, Finished(Ok receipt)) ->
      onOrgUpdate receipt
      state, Cmd.none
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
                  EmployeeName = session.Name
               }
               ProgressId = approval.ProgressId
               Command = approval.CommandToInitiateOnApproval
            }
            |> OrgCommand.DeclineCommandApproval

         let! res = OrgService.submitCommand org cmd
         return Msg.DeclineCommand(org, session, approval, Finished res)
      }

      state, Cmd.fromAsync decline
   | DeclineCommand(_, _, _, Finished(Ok receipt)) ->
      onOrgUpdate receipt
      state, Cmd.none
   | DeclineCommand(_, _, _, Finished(Error err)) ->
      Log.error (string err)
      state, Alerts.toastCommand err
   | DismissConfirmation -> state, Cmd.none

let approversMsg (approvers: EmployeeReference list) =
   approvers
   |> List.fold (fun acc approver -> $"{acc}{approver.EmployeeName}, ") ""
   |> _.TrimEnd()
   |> _.TrimEnd(',')

[<ReactComponent>]
let ApprovalProgressComponent
   (session: UserSession)
   (org: Org)
   (onOrgUpdate: OrgCommandReceipt -> unit)
   =
   let _, dispatch =
      React.useElmish (init, update onOrgUpdate, [| box session.OrgId |])

   let approvals =
      org.CommandApprovalProgress.Values
      |> Seq.choose (fun progress ->
         org.CommandApprovalRules
         |> Map.tryFind progress.RuleId
         |> Option.map (fun rule -> rule, progress))

   if Seq.isEmpty approvals then
      Html.p "No commands require approval."
   else
      React.fragment [
         for rule, progress in approvals do
            let approvalsRemaining =
               List.except progress.ApprovedBy rule.Approvers

            let mayApproveOrDeny =
               CommandApprovalProgress.mayApproveOrDeny
                  rule
                  progress
                  session.EmployeeId

            classyNode Html.article [ "command-pending-approval" ] [
               Html.header [
                  classyNode Html.div [ "grid" ] [
                     Html.p progress.CommandToInitiateOnApproval.Display

                     Html.small
                        $"{progress.ApprovedBy.Length} of {rule.Approvers.Length} approvals acquired"

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
                                 progress,
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
                                 progress
                              ))

                        attr.custom ("data-tooltip", "Decline")
                        attr.custom ("data-placement", "bottom")
                     ]
                  ]
               ]

               Html.p (
                  match progress.CommandToInitiateOnApproval with
                  | ApprovableCommand.DomesticTransfer c ->
                     $"{Money.format c.Data.Amount} domestic transfer from
                     {c.Data.Sender.Name} to {c.Data.Recipient.Name}"
                  | ApprovableCommand.InviteEmployee c ->
                     $"Invite employee {c.Data.Name}"
                  | ApprovableCommand.UpdateEmployeeRole c ->
                     $"Update {c.Data.Name}'s role from {c.Data.PriorRole} to {c.Data.Role}"
                  | ApprovableCommand.FulfillPlatformPayment c ->
                     let pay = c.Data.RequestedPayment.BaseInfo
                     $"{Money.format pay.Amount} payment requested to {pay.Payer.OrgName}"
                  | ApprovableCommand.InternalTransferBetweenOrgs c ->
                     $"{Money.format c.Data.Amount} transfer to {c.Data.Recipient.Name}"

                  + $" requested by {progress.RequestedBy.EmployeeName} on
                  {DateTime.formatShort progress.CreatedAt}."
               )

               match progress.Status with
               | CommandApprovalProgress.Status.Approved ->
                  Html.small [
                     attr.classes [ "success" ]
                     attr.text
                        $"Approval completed on {DateTime.formatShort progress.LastUpdate}"
                  ]
               | CommandApprovalProgress.Status.Declined ->
                  let declinedBy =
                     progress.DeclinedBy
                     |> Option.map _.EmployeeName
                     |> Option.defaultValue "Unknown"

                  classyNode Html.div [ "approval-count" ] [
                     Html.small [
                        attr.classes [ "debit" ]
                        attr.text
                           $"Declined on {DateTime.formatShort progress.LastUpdate} by {declinedBy}"
                     ]
                  ]
               | CommandApprovalProgress.Status.Pending ->
                  classyNode Html.div [ "approval-count" ] [
                     Html.small "Approvals remaining:"
                     Html.p (approversMsg approvalsRemaining)
                  ]

                  classyNode Html.div [ "approval-count" ] [
                     Html.small "Approvals acquired:"

                     match progress.ApprovedBy with
                     | [] -> Html.p "None"
                     | approvers -> Html.p (approversMsg approvers)
                  ]
               | CommandApprovalProgress.Status.Terminated _ ->
                  Html.small
                     $"Approval terminated early on
                  {DateTime.formatShort progress.LastUpdate}"
            ]
      ]
