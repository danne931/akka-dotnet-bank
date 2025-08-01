module Bank.Employee.Forms.EmployeeRoleForm

open Feliz
open Fable.Form.Simple

open Fable.Form.Simple.Pico
open Bank.Org.Domain
open Bank.Employee.Domain
open Bank.Account.Domain
open UIDomain.Employee
open UIDomain.Org
open Bank.Forms.FormContainer
open Lib.SharedTypes
open PurchaseLimitForm
open AccountProfileForm
open CommandApproval

type Values = {
   Role: string
   DailyPurchaseLimit: string
   MonthlyPurchaseLimit: string
   LinkedAccountId: string
}

let private options = [
   string Role.Admin, Role.Admin.Display
   string Role.CardOnly, Role.CardOnly.Display
   string Role.Scholar, Role.Scholar.Display
]

let employeeRoleSelect (onSelect: Role -> unit) =
   Form.selectField {
      Parser = Role.fromStringUnsafe >> Ok
      Value = _.Role
      Update =
         fun newValue values ->
            onSelect (Role.fromStringUnsafe newValue)
            { values with Role = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Select a role:"
         Placeholder = ""
         Options = options
      }
   }

let private form
   (session: UserSession)
   (employee: Employee)
   (accounts: Map<AccountId, Account>)
   (onRoleSelect: Role -> unit)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let onSubmit
      (role: Role)
      (cardInfo: EmployeeInviteSupplementaryCardInfo option)
      =
      let cmd =
         UpdateRoleCommand.create employee.CompositeId session.AsInitiator {
            EmployeeName = employee.Name
            PriorRole = employee.Role
            Role = role
            CardInfo = cardInfo
         }
         |> EmployeeCommand.UpdateRole
         |> FormCommand.Employee

      let entity = FormEntity.Employee employee

      match role with
      | Role.Scholar ->
         let msg =
            $"All active cards will be closed for {employee.Name}. "
            + "Are you sure you want change role to Scholar?"

         Msg.ShowSubmitConfirmation(msg, entity, cmd)
      | _ -> Msg.Submit(entity, cmd, Started)

   employeeRoleSelect onRoleSelect
   |> Form.andThen (fun role ->
      match role with
      | Role.CardOnly when not employee.HasCard ->
         Form.succeed
            (fun accountId dailyPurchaseLimit monthlyPurchaseLimit ->
               let cardInfo =
                  Some {
                     LinkedAccountId = accountId
                     DailyPurchaseLimit = dailyPurchaseLimit
                     MonthlyPurchaseLimit = monthlyPurchaseLimit
                     CardType = CardType.Debit
                  }

               onSubmit role cardInfo)
         |> Form.append (
            accountSelect
               (Some "Select an account to link the card to:")
               accounts
            |> Form.mapValues {
               Value = fun a -> { AccountId = a.LinkedAccountId }
               Update = fun a b -> { b with LinkedAccountId = a.AccountId }
            }
         )
         |> Form.append (
            dailyPurchaseLimitField
            |> Form.mapValues {
               Value = fun a -> { Amount = a.DailyPurchaseLimit }
               Update = fun a b -> { b with DailyPurchaseLimit = a.Amount }
            }
         )
         |> Form.append (
            monthlyPurchaseLimitField
            |> Form.mapValues {
               Value = fun a -> { Amount = a.MonthlyPurchaseLimit }
               Update =
                  fun a b -> {
                     b with
                        MonthlyPurchaseLimit = a.Amount
                  }
            }
         )
      | _ -> Form.succeed (onSubmit role None))

[<ReactComponent>]
let EmployeeRoleFormComponent
   (onCancel: unit -> unit)
   (onSelect: Role -> unit)
   (employee: Employee)
   (onSubmit: EmployeeCommandReceipt -> unit)
   (onSubmitForApproval: CommandApprovalProgress.RequestCommandApproval -> unit)
   =
   let session = React.useContext UserSessionProvider.context
   let orgCtx = React.useContext OrgProvider.context
   let selectedRole, setSelectedRole = React.useState employee.Role

   let onSelect (role: Role) =
      setSelectedRole role
      onSelect role

   let formProps: Values = {
      Role = string employee.Role
      DailyPurchaseLimit = string Constants.DAILY_PURCHASE_LIMIT_DEFAULT
      MonthlyPurchaseLimit = string Constants.MONTHLY_PURCHASE_LIMIT_DEFAULT
      LinkedAccountId = ""
   }

   match orgCtx, session with
   | Deferred.Resolved(Ok(Some org)), Deferred.Resolved(Ok session) ->
      let roleChangeRequiresApproval =
         CommandApprovalRule.commandTypeRequiresApproval
            (ApprovableCommandType.ApprovablePerCommand
               UpdateEmployeeRoleCommandType)
            (InitiatedById session.EmployeeId)
            org.Org.CommandApprovalRules

      let customAction _ =
         Form.View.Action.Custom(fun state _ ->
            if selectedRole = employee.Role then
               Form.View.cancelButton onCancel state
            else
               match roleChangeRequiresApproval with
               | None ->
                  Form.View.submitAndCancelButton "Save Role" onCancel state
               | Some _ ->
                  Form.View.submitAndCancelButton
                     "Request Approval for Role Update"
                     onCancel
                     state)

      FormContainer {|
         InitialValues = formProps
         Form = form session employee org.CheckingAccounts onSelect
         Action = Some customAction
         Session = session
         ComponentName = "EmployeeRoleForm"
         UseEventSubscription = None
         OnSubmit =
            function
            | FormSubmitReceipt.Employee receipt ->
               match roleChangeRequiresApproval, receipt.PendingCommand with
               | Some rule, EmployeeCommand.UpdateRole cmd ->
                  let cmd =
                     cmd |> UpdateEmployeeRole |> ApprovableCommand.PerCommand

                  CommandApprovalProgress.RequestCommandApproval.fromApprovableCommand
                     session
                     rule
                     cmd
                  |> onSubmitForApproval
               | _ -> onSubmit receipt
            | _ -> ()
      |}
   | _ -> Html.progress []
