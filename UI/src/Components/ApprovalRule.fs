module ApprovalRuleManagement

open Feliz
open Elmish
open Feliz.UseElmish
open Elmish.SweetAlert
open Feliz.Router

open Bank.Employee.Domain
open Bank.Org.Domain
open UIDomain.Org
open Lib.SharedTypes
open Dropdown
open Bank.Employee.Forms.CommandApprovalRule
open CommandApproval

type State = {
   Admins: Deferred<Result<Employee list option, Err>>
   IsCreateRuleOpen: bool
}

type DeleteRuleMessage = {
   Session: UserSession
   Org: Org
   Rule: CommandApprovalRule
   DeletionRequiresApproval: CommandApprovalRule option
}

type ConfigureRuleSubmittedMessage = {
   Session: UserSession
   Receipt: OrgCommandReceipt
   OriginatedFromRuleEdit: bool
}

type Msg =
   | GetAdmins of
      OrgId *
      AsyncOperationStatus<Result<Employee list option, Err>>
   | ToggleCreateRuleOpen
   | ShowDeleteRuleConfirmation of DeleteRuleMessage
   | DismissDeleteRuleConfirmation
   | ConfirmDeleteRule of
      DeleteRuleMessage *
      AsyncOperationStatus<Result<OrgCommandReceipt, Err>>
   | ManageRuleNotAllowed of ApprovableCommandType
   | ConfigureRuleSubmitted of ConfigureRuleSubmittedMessage

let init () =
   {
      Admins = Deferred.Idle
      IsCreateRuleOpen = false
   },
   Cmd.none

let update (orgDispatch: OrgProvider.Msg -> unit) msg state =
   match msg with
   | GetAdmins(orgId, Started) ->
      let getAdmins = async {
         let! admins =
            EmployeeService.getEmployees orgId {
               Roles = Some [ Role.Admin ]
               EmployeeIds = None
               Status = Some EmployeeStatus.Active
            }

         return Msg.GetAdmins(orgId, Finished admins)
      }

      {
         state with
            Admins = Deferred.InProgress
      },
      Cmd.fromAsync getAdmins
   | GetAdmins(_, Finished(Ok res)) ->
      {
         state with
            Admins = Deferred.Resolved(Ok res)
      },
      Cmd.none
   | GetAdmins(_, Finished(Error err)) ->
      {
         state with
            Admins = Deferred.Resolved(Error err)
      },
      Cmd.none
   | ToggleCreateRuleOpen ->
      {
         state with
            IsCreateRuleOpen = not state.IsCreateRuleOpen
      },
      Cmd.none
   | ShowDeleteRuleConfirmation msg ->
      let associatedApprovalCnt =
         msg.Org.CommandApprovalProgress
         |> Map.filter (fun _ p ->
            p.CommandToInitiateOnApproval.CommandType = msg.Rule.CommandType)
         |> Map.count


      let info =
         $"There is currently {associatedApprovalCnt} {msg.Rule.CommandType.Display}
         commands which will initiate "

      let title, info =
         if msg.DeletionRequiresApproval.IsSome then
            "Deletion of this rule requires approval. Continue?",
            info + "upon approval."
         else
            "Are you sure you want to delete this rule?", info + "immediately."

      let confirm =
         ConfirmAlert(
            (if associatedApprovalCnt > 0 then info else ""),
            function
            | ConfirmAlertResult.Confirmed ->
               Msg.ConfirmDeleteRule(msg, Started)
            | ConfirmAlertResult.Dismissed _ ->
               Msg.DismissDeleteRuleConfirmation
         )
            .Title(title)
            .Type(AlertType.Question)
            .ShowCloseButton(true)

      state, SweetAlert.Run confirm
   | DismissDeleteRuleConfirmation -> state, Cmd.none
   | ConfirmDeleteRule(msg, Started) ->
      let delete = async {
         let initiator = {
            EmployeeId = msg.Session.EmployeeId
            EmployeeName = msg.Session.Name
         }

         let cmd =
            match msg.DeletionRequiresApproval with
            | Some ruleForEditingApprovalRules ->
               let approvableCommand =
                  ManageApprovalRuleInput.Delete(msg.Rule, initiator)
                  |> ManageApprovalRuleCommand.create
                  |> ManageApprovalRule
                  |> ApprovableCommand.PerCommand

               CommandApprovalProgress.RequestCommandApproval.fromApprovableCommand
                  msg.Session
                  ruleForEditingApprovalRules
                  approvableCommand
               |> OrgCommand.RequestCommandApproval
            | None ->
               CommandApprovalRule.DeleteApprovalRuleCommand.create {
                  RuleId = msg.Rule.RuleId
                  OrgId = msg.Rule.OrgId
                  CommandType = msg.Rule.CommandType
                  DeletedBy = initiator
               }
               |> OrgCommand.DeleteApprovalRule

         let! res = OrgService.submitCommand msg.Org cmd
         return ConfirmDeleteRule(msg, Finished res)
      }

      state, Cmd.fromAsync delete
   | ConfirmDeleteRule(msg, Finished(Ok receipt)) ->
      orgDispatch (OrgProvider.Msg.OrgUpdated receipt.PendingState)

      let info =
         if msg.DeletionRequiresApproval.IsSome then
            $"Deletion of {msg.Rule.CommandType.Display} rule pending approval."
         else
            $"Deleted {msg.Rule.CommandType.Display} rule."

      state, Alerts.toastSuccessCommand info
   | ConfirmDeleteRule(msg, Finished(Error err)) ->
      Log.error (string err)

      state,
      Alerts.toastCommand
      <| Err.NetworkError(
         exn $"Issue deleting rule for {msg.Rule.CommandType.Display}"
      )
   | ManageRuleNotAllowed cmdType ->
      let alert =
         $"You may not edit or delete {cmdType.Display} rule since management of this rule is pending approval."
         |> Err.UnexpectedError
         |> Alerts.toastCommand

      state, Cmd.batch [ alert; Cmd.navigate Routes.ApprovalsUrl.BasePath ]
   | ConfigureRuleSubmitted msg ->
      let ruleConfigRequiresApproval =
         CommandApprovalRule.ruleManagementRequiresApproval
            (InitiatedById msg.Session.EmployeeId)
            msg.Receipt.PendingState.CommandApprovalRules
         |> _.IsSome

      orgDispatch (OrgProvider.Msg.OrgUpdated msg.Receipt.PendingState)

      let cmd =
         if ruleConfigRequiresApproval then
            Alerts.toastSuccessCommand
               "Rule configuration submitted for approval."
         else
            Alerts.toastSuccessCommand "Rule configured."

      let state =
         if msg.OriginatedFromRuleEdit then
            state
         else
            { state with IsCreateRuleOpen = false }

      state, cmd

// Fetches admins which are used for selecting rule approvers.
let fetchAdminsIfNecessary (state: State) dispatch orgId =
   match state.Admins with
   | Deferred.Resolved(Ok(Some _)) -> ()
   | _ -> dispatch <| Msg.GetAdmins(orgId, Started)

let renderMoney (amount: decimal) =
   Html.span [
      attr.classes [ "success" ]
      attr.text $" {Money.format amount} "
   ]

[<ReactComponent>]
let EditApprovalRuleComponent
   (state: State)
   dispatch
   (session: UserSession)
   (rule: CommandApprovalRule)
   (org: Org)
   =
   let ruleToEdit, setRuleToEdit =
      React.useState<CommandApprovalRule option> None

   let deleteMsg = {
      Session = session
      Org = org
      Rule = rule
      DeletionRequiresApproval =
         CommandApprovalRule.ruleManagementRequiresApproval
            (InitiatedById session.EmployeeId)
            org.CommandApprovalRules
   }

   let isManageRuleNotAllowed =
      CommandApprovalProgress.managementOfRuleIsPendingApproval
         org.CommandApprovalProgress
         rule.CommandType

   React.fragment [
      Html.a [ attr.href "" ]

      match ruleToEdit with
      | Some rule ->
         Html.h6 $"Editing {rule.CommandType.Display} Command Approval:"

         match state.Admins with
         | Deferred.Resolved(Ok(Some admins)) ->
            CommandApprovalRuleEditFormComponent
               (fun () -> setRuleToEdit None)
               (fun receipt ->
                  setRuleToEdit None

                  dispatch
                  <| Msg.ConfigureRuleSubmitted {
                     Session = session
                     Receipt = receipt
                     OriginatedFromRuleEdit = true
                  })
               session
               org
               (admins |> List.map (fun a -> a.EmployeeId, a) |> Map.ofList)
               rule
         | _ -> Html.progress []
      | None ->
         classyNode Html.div [ "grid"; "nav-container" ] [
            Html.small [
               attr.classes [ "rule-command-type" ]
               attr.text rule.CommandType.Display
            ]

            DropdownComponent(
               {|
                  Direction = DropdownDirection.RTL
                  ShowCaret = false
                  Button = None
                  Items = [
                     {
                        Text = "Edit"
                        OnClick =
                           fun _ ->
                              if isManageRuleNotAllowed then
                                 Msg.ManageRuleNotAllowed rule.CommandType
                                 |> dispatch
                              else
                                 setRuleToEdit (Some rule)

                                 fetchAdminsIfNecessary
                                    state
                                    dispatch
                                    session.OrgId
                        IsSelected = ruleToEdit.IsSome
                     }
                     {
                        Text = "Delete"
                        OnClick =
                           fun _ ->
                              if isManageRuleNotAllowed then
                                 Msg.ManageRuleNotAllowed rule.CommandType
                                 |> dispatch
                              else
                                 Msg.ShowDeleteRuleConfirmation deleteMsg
                                 |> dispatch
                        IsSelected = false
                     }
                  ]
               |}
            )
         ]

         match rule.Criteria with
         | ApprovalCriteria.PerCommand ->
            Html.small "For every request require approval from:"
         | ApprovalCriteria.AmountDailyLimit limit ->
            Html.small
               "If transaction amount plus the daily accrued amount (per employee) is >="

            renderMoney limit
            Html.small "require approval from:"
         | ApprovalCriteria.AmountPerCommand range ->
            Html.small $"If amount "

            match range.LowerBound, range.UpperBound with
            | Some low, Some high ->
               React.fragment [
                  Html.small ">="
                  renderMoney low
                  Html.small "and <"
                  renderMoney high
               ]
            | Some low, None ->
               React.fragment [ Html.small ">="; renderMoney low ]
            | None, Some high ->
               React.fragment [ Html.small "<"; renderMoney high ]
            | None, None -> Html.none

            Html.small "require approval from:"

         Html.p (displayApprovers rule.Approvers)
   ]

let renderCreateRule (state: State) dispatch (session: UserSession) (org: Org) =
   classyNode Html.article [ "approval-rule" ] [
      if state.IsCreateRuleOpen then
         Html.h6 $"Configure Approval Rule:"

         match state.Admins with
         | Deferred.Resolved(Ok(Some admins)) ->
            CommandApprovalRuleCreateFormComponent
               (fun () -> dispatch Msg.ToggleCreateRuleOpen)
               (fun receipt ->
                  dispatch
                  <| Msg.ConfigureRuleSubmitted {
                     Session = session
                     Receipt = receipt
                     OriginatedFromRuleEdit = false
                  })
               session
               org
               (admins |> List.map (fun a -> a.EmployeeId, a) |> Map.ofList)
         | _ -> Html.progress []
      else
         classyNode Html.div [ "approval-rule-create" ] [
            Html.button [
               attr.classes [ "outline" ]
               attr.onClick (fun e ->
                  e.preventDefault ()
                  dispatch Msg.ToggleCreateRuleOpen

                  fetchAdminsIfNecessary state dispatch session.OrgId)
               attr.text "Configure Approval Rule"
            ]
         ]
   ]

[<ReactComponent>]
let ApprovalRuleManagementDashboardComponent (session: UserSession) (org: Org) =
   let orgId = session.OrgId
   let orgDispatch = React.useContext OrgProvider.dispatchContext

   let state, dispatch =
      React.useElmish (init, update orgDispatch, [| box orgId |])

   let rules = org.CommandApprovalRules

   React.fragment [
      if rules.IsEmpty then
         Html.p "No approval rules configured."
         renderCreateRule state dispatch session org
      else
         let rules =
            rules.Values
            |> Seq.sortBy (_.Criteria >> ApprovalCriteria.sortBy)
            |> Seq.sortBy (fun r ->
               match r.CommandType with
               | ApprovableCommandType.ApprovableAmountBased FulfillPlatformPaymentCommandType ->
                  0
               | ApprovableCommandType.ApprovableAmountBased DomesticTransferCommandType ->
                  1
               | ApprovableCommandType.ApprovableAmountBased InternalTransferBetweenOrgsCommandType ->
                  2
               | ApprovableCommandType.ApprovablePerCommand InviteEmployeeCommandType ->
                  3
               | ApprovableCommandType.ApprovablePerCommand UnlockCardCommandType ->
                  4
               | ApprovableCommandType.ApprovablePerCommand UpdateEmployeeRoleCommandType ->
                  5
               | ApprovableCommandType.ApprovablePerCommand ManageApprovalRuleCommandType ->
                  6)

         for rule in rules do
            classyNode Html.article [ "approval-rule" ] [
               EditApprovalRuleComponent state dispatch session rule org
            ]

         renderCreateRule state dispatch session org
   ]
