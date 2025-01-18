module ApprovalRuleManagement

open Feliz
open Elmish
open Feliz.UseElmish

open Bank.Employee.Domain
open Bank.Org.Domain
open UIDomain.Org
open Lib.SharedTypes
open Dropdown
open Bank.Employee.Forms.CommandApprovalRule

type State = {
   Admins: Deferred<Result<Employee list option, Err>>
   // Load rules on component init but do not store the rules here.
   // The rules are propagated to the Org.CommandApprovalRules context
   // which will enable reuse of the Org StateTransitions
   // (see ../../../Org.Domain/Org.fs) during rule creation/editing.
   Rules: Deferred<Result<unit, Err>>
   IsCreateRuleOpen: bool
}

type Msg =
   | GetCommandApprovalRules of
      OrgId *
      AsyncOperationStatus<Result<CommandApprovalRule.T list option, Err>>
   | GetAdmins of
      OrgId *
      AsyncOperationStatus<Result<Employee list option, Err>>
   | ToggleCreateRuleOpen

let init (orgId: OrgId) () =
   {
      Admins = Deferred.Idle
      Rules = Deferred.Idle
      IsCreateRuleOpen = false
   },
   Cmd.ofMsg (Msg.GetCommandApprovalRules(orgId, Started))

let update (orgDispatch: OrgProvider.Msg -> unit) msg state =
   match msg with
   | GetCommandApprovalRules(orgId, Started) ->
      let getApprovals = async {
         let! rules = OrgService.getCommandApprovalRules orgId
         return Msg.GetCommandApprovalRules(orgId, Finished rules)
      }

      {
         state with
            Rules = Deferred.InProgress
      },
      Cmd.fromAsync getApprovals
   | GetCommandApprovalRules(_, Finished(Ok res)) ->
      match res with
      | Some rules ->
         orgDispatch (OrgProvider.Msg.CommandApprovalRulesLoaded rules)
      | _ -> ()

      {
         state with
            Rules = Deferred.Resolved(Ok())
      },
      Cmd.none
   | GetCommandApprovalRules(_, Finished(Error err)) ->
      {
         state with
            Rules = Deferred.Resolved(Error err)
      },
      Cmd.none
   | GetAdmins(orgId, Started) ->
      let getAdmins = async {
         let! admins =
            EmployeeService.getEmployees orgId {
               Roles = Some [ Role.Admin ]
               EmployeeIds = None
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

let fetchAdminsIfNecessary (state: State) dispatch orgId =
   match state.Admins with
   | Deferred.Resolved(Ok(Some _)) -> ()
   | _ -> dispatch <| Msg.GetAdmins(orgId, Started)

let renderMoney (amount: decimal) =
   Html.span [
      attr.classes [ "success" ]
      attr.text $" {Money.format amount} "
   ]

let formatApprovers (approvers: CommandApprovalRule.Approver list) =
   approvers
   |> List.fold (fun acc approver -> $"{acc}{approver.Name}, ") ""
   |> _.Remove(-2)

[<ReactComponent>]
let EditApprovalRuleComponent
   (state: State)
   dispatch
   (session: UserSession)
   (rule: CommandApprovalRule.T)
   (org: Org)
   (onOrgUpdate: OrgCommandReceipt -> unit)
   =
   let ruleToEdit, setRuleToEdit =
      React.useState<CommandApprovalRule.T option> None

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
                  onOrgUpdate receipt)
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
                              setRuleToEdit (Some rule)

                              fetchAdminsIfNecessary
                                 state
                                 dispatch
                                 session.OrgId
                        IsSelected = ruleToEdit.IsSome
                     }
                  ]
               |}
            )
         ]

         match rule.Criteria with
         | CommandApprovalRule.Criteria.PerCommand ->
            Html.small "For every command require approval from:"
         | CommandApprovalRule.Criteria.AmountDailyLimit limit ->
            Html.small
               "If transaction amount plus the daily accrued amount is >="

            renderMoney limit
            Html.small "require approval from:"
         | CommandApprovalRule.Criteria.AmountPerCommand range ->
            Html.small $"If amount "

            match range.LowerBound, range.UpperBound with
            | Some low, Some high ->
               React.fragment [
                  Html.small ">="
                  renderMoney low
                  Html.small "and <="
                  renderMoney high
               ]
            | Some low, None ->
               React.fragment [ Html.small ">="; renderMoney low ]
            | None, Some high ->
               React.fragment [ Html.small "<="; renderMoney high ]
            | None, None -> Html.none

            Html.small "require approval from:"

         Html.p (formatApprovers rule.Approvers)
   ]

let renderCreateRule
   (state: State)
   dispatch
   (session: UserSession)
   (org: Org)
   (onOrgUpdate: OrgCommandReceipt -> unit)
   =
   classyNode Html.article [ "approval-rule" ] [
      if state.IsCreateRuleOpen then
         Html.h6 $"Configure Command Approval Rule:"

         match state.Admins with
         | Deferred.Resolved(Ok(Some admins)) ->
            CommandApprovalRuleCreateFormComponent
               (fun () -> dispatch Msg.ToggleCreateRuleOpen)
               (fun receipt ->
                  dispatch Msg.ToggleCreateRuleOpen
                  onOrgUpdate receipt)
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
let ApprovalRuleManagementDashboardComponent
   (session: UserSession)
   (org: Org)
   (onOrgUpdate: OrgCommandReceipt -> unit)
   =
   let orgId = session.OrgId
   let orgDispatch = React.useContext OrgProvider.dispatchContext

   let state, dispatch =
      React.useElmish (init orgId, update orgDispatch, [| box orgId |])

   let rules = org.CommandApprovalRules

   React.fragment [
      match state.Rules, rules.IsEmpty with
      | Deferred.Resolved(Ok()), false ->
         let rules =
            rules.Values
            |> Seq.sortBy (fun r ->
               match r.Criteria with
               | CommandApprovalRule.Criteria.AmountPerCommand range ->
                  match range.LowerBound, range.UpperBound with
                  | None, Some high -> 0, None, Some high
                  | Some low, Some high -> 1, Some low, Some high
                  | Some low, None -> 2, Some low, None
                  // NOTE: Case should not occur.
                  // Consider making a type which enforces either
                  // lower or upper being Some.
                  | None, None -> 3, None, None
               | CommandApprovalRule.Criteria.AmountDailyLimit _ ->
                  4, None, None
               | CommandApprovalRule.Criteria.PerCommand -> 5, None, None)
            |> Seq.sortBy (fun r ->
               match r.CommandType with
               | ApprovableCommandType.FulfillPlatformPayment -> 0
               | ApprovableCommandType.DomesticTransfer -> 1
               | ApprovableCommandType.InternalTransferBetweenOrgs -> 2
               | ApprovableCommandType.InviteEmployee -> 3
               | ApprovableCommandType.UpdateEmployeeRole -> 4)

         for rule in rules do
            classyNode Html.article [ "approval-rule" ] [
               EditApprovalRuleComponent
                  state
                  dispatch
                  session
                  rule
                  org
                  onOrgUpdate
            ]

         renderCreateRule state dispatch session org onOrgUpdate
      | Deferred.Resolved(Ok()), true ->
         Html.p "No command approval rules configured."
         renderCreateRule state dispatch session org onOrgUpdate
      | _ -> Html.progress []
   ]
