module ApprovalRuleManagement

open Feliz
open Elmish
open Feliz.UseElmish

open Bank.Employee.Domain
open Lib.SharedTypes
open Dropdown
open Bank.Employee.Forms.CommandApprovalRule

type State = {
   Admins: Deferred<Result<Employee list option, Err>>
   Rules: Deferred<Result<CommandApprovalRule.T list option, Err>>
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

let update msg state =
   match msg with
   | GetCommandApprovalRules(orgId, Started) ->
      let getApprovals = async {
         let! rules = EmployeeService.getCommandApprovalRules orgId
         return Msg.GetCommandApprovalRules(orgId, Finished rules)
      }

      {
         state with
            Rules = Deferred.InProgress
      },
      Cmd.fromAsync getApprovals
   | GetCommandApprovalRules(_, Finished(Ok res)) ->
      {
         state with
            Rules = Deferred.Resolved(Ok res)
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
               ignore
               session
               (admins |> List.map (fun a -> a.EmployeeId, a) |> Map.ofList)
               rule
         | _ -> Html.progress []
      | None ->
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
                           fetchAdminsIfNecessary state dispatch session.OrgId
                     IsSelected = ruleToEdit.IsSome
                  }
               ]
            |}
         )

         match rule.Criteria with
         | CommandApprovalRule.Criteria.PerCommand ->
            Html.small
               $"For every {rule.CommandType.Display} command require
               approval from:"
         | CommandApprovalRule.Criteria.AmountDailyLimit limit ->
            Html.small
               $"If daily accrued amount of {rule.CommandType.Display} plus the
               transaction amount is >="

            renderMoney limit
            Html.small "require approval from:"
         | CommandApprovalRule.Criteria.AmountPerCommand range ->
            Html.small $"If {rule.CommandType.Display} amount "

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

let renderCreateRule (state: State) dispatch (session: UserSession) =
   classyNode Html.article [ "approval-rule" ] [
      if state.IsCreateRuleOpen then
         Html.h6 $"Configure Command Approval Rule:"

         match state.Admins with
         | Deferred.Resolved(Ok(Some admins)) ->
            CommandApprovalRuleCreateFormComponent
               (fun () -> dispatch Msg.ToggleCreateRuleOpen)
               ignore
               session
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
let ApprovalRuleManagementDashboardComponent (session: UserSession) =
   let orgId = session.OrgId
   let state, dispatch = React.useElmish (init orgId, update, [| box orgId |])

   React.fragment [
      match state.Rules with
      | Deferred.Resolved(Ok(Some rules)) ->
         let rules =
            rules
            |> List.sortBy (fun r ->
               match r.Criteria with
               | CommandApprovalRule.Criteria.AmountPerCommand range ->
                  match range.LowerBound, range.UpperBound with
                  | None, Some _ -> 0
                  | Some _, Some _ -> 1
                  | Some _, None -> 2
                  // NOTE: Case should not occur.
                  // Consider making a type which enforces either
                  // lower or upper being Some.
                  | None, None -> 3
               | CommandApprovalRule.Criteria.AmountDailyLimit _ -> 4
               | CommandApprovalRule.Criteria.PerCommand -> 5)
            |> List.sortBy (fun r ->
               match r.CommandType with
               | ApprovableCommandType.FulfillPlatformPayment -> 0
               | ApprovableCommandType.DomesticTransfer -> 1
               | ApprovableCommandType.InternalTransferBetweenOrgs -> 2
               | ApprovableCommandType.InviteEmployee -> 3
               | ApprovableCommandType.UpdateEmployeeRole -> 4)

         for rule in rules do
            classyNode Html.article [ "approval-rule" ] [
               EditApprovalRuleComponent state dispatch session rule
            ]

         renderCreateRule state dispatch session
      | Deferred.Resolved(Ok None) ->
         Html.p "No command approval rules configured."
         renderCreateRule state dispatch session
      | _ -> Html.progress []
   ]
