module Bank.Employee.Forms.CommandApprovalRule

open Feliz
open Fable.Form.Simple
open System
open Validus

open Bank.Org.Domain
open Bank.Employee.Domain
open Lib.SharedTypes
open Lib.Validators
open UIDomain.Org
open Fable.Form.Simple.Pico
open CommandApproval
open Bank.Forms.FormContainer

type ApproverValues = { EmployeeId: string; Name: string }

type Values = {
   Approvers: ApproverValues list
   CommandType: string
   AmountBasedCriteriaType: string
   DailyLimit: string
   RangeLowerBound: string
   RangeUpperBound: string
}

let private ANY_ADMIN_APPROVER = {
   EmployeeId = string Initiator.System.Id
   Name = "Any Admin"
}

let fieldDailyLimit =
   Form.textField {
      Parser =
         amountValidatorFromString "Daily limit"
         >> validationErrorsHumanFriendly
      Value = _.DailyLimit
      Update =
         fun newValue values -> {
            values with
               DailyLimit =
                  Form.Util.formattedPositiveDecimal newValue
                  |> Option.defaultValue values.DailyLimit
         }
      Error = fun _ -> None
      Attributes = {
         Label = "Daily Limit Per Employee:"
         Placeholder = "Daily limit"
         HtmlAttributes = []
      }
   }

let rangeLowerBoundParser =
   amountValidatorFromString "Lower bound" >> validationErrorsHumanFriendly

let rangeUpperBoundParser =
   amountValidatorFromString "Upper bound" >> validationErrorsHumanFriendly

let rangeLowerField (values: Values) =
   Form.textField {
      Parser =
         fun amt -> validate {
            let! lower = rangeLowerBoundParser amt

            do!
               match rangeUpperBoundParser values.RangeUpperBound with
               | Ok upper ->
                  if lower >= upper then
                     Error "Lower bound must be less than upper bound"
                  else
                     Ok()
               | Error _ -> Ok()

            return lower
         }
      Value = _.RangeLowerBound
      Update =
         fun newValue values -> {
            values with
               RangeLowerBound = newValue
         }
      Error = fun _ -> None
      Attributes = {
         Label = "Lower Bound:"
         Placeholder = "Lower bound"
         HtmlAttributes = []
      }
   }

let rangeUpperField =
   Form.textField {
      Parser = rangeUpperBoundParser
      Value = _.RangeUpperBound
      Update =
         fun newValue values -> {
            values with
               RangeUpperBound = newValue
         }
      Error = fun _ -> None
      Attributes = {
         Label = "Upper Bound:"
         Placeholder = "Upper bound"
         HtmlAttributes = []
      }
   }

let private fieldApprovableCommandType =
   Form.selectField {
      Parser =
         fun input ->
            match ApprovableCommandType.fromString input with
            | Some f -> Ok f
            | None -> Error $"Invalid approvable command type {input}"
      Value = fun values -> values.CommandType
      Update = fun newValue values -> { values with CommandType = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Command Type:"
         Placeholder = ""
         Options =
            let manageApprovalRule =
               ApprovableCommandType.ApprovablePerCommand
                  ManageApprovalRuleCommandType

            let employeeInvite =
               ApprovableCommandType.ApprovablePerCommand
                  InviteEmployeeCommandType

            let employeeRole =
               ApprovableCommandType.ApprovablePerCommand
                  UpdateEmployeeRoleCommandType

            let cardUnlock =
               ApprovableCommandType.ApprovablePerCommand UnlockCardCommandType

            let platformPayment =
               ApprovableCommandType.ApprovableAmountBased
                  FulfillPlatformPaymentCommandType

            let internalTransferBetweenOrgs =
               ApprovableCommandType.ApprovableAmountBased
                  InternalTransferBetweenOrgsCommandType

            let domesticTransfer =
               ApprovableCommandType.ApprovableAmountBased
                  DomesticTransferCommandType

            [
               string employeeInvite, employeeInvite.Display
               string employeeRole, employeeRole.Display
               string cardUnlock, cardUnlock.Display
               string platformPayment, platformPayment.Display
               string internalTransferBetweenOrgs,
               internalTransferBetweenOrgs.Display
               string domesticTransfer, domesticTransfer.Display
               string manageApprovalRule, manageApprovalRule.Display
            ]
      }
   }

type private AmountBasedCriteriaType =
   | AmountDailyLimit
   | AmountPerCommand

let private fieldAmountBasedCriteria =
   Form.radioField {
      Parser =
         fun input ->
            match input with
            | "AmountDailyLimit" -> Ok AmountBasedCriteriaType.AmountDailyLimit
            | "AmountPerCommand" -> Ok AmountBasedCriteriaType.AmountPerCommand
            | _ -> Error "Unknown amount based criteria type"
      Value = fun values -> values.AmountBasedCriteriaType
      Update =
         fun newValue values -> {
            values with
               AmountBasedCriteriaType = newValue
         }
      Error = fun _ -> None
      Attributes = {
         Label = "Type of amount based criteria:"
         Options = [
            "AmountDailyLimit", "Daily Limit"
            "AmountPerCommand", "Amount Per Command"
         ]
      }
   }

let private approverItemForm
   (admins: Map<EmployeeId, Employee>)
   (values: Values)
   (index: int)
   : Form.Form<ApproverValues, _, _>
   =
   let optionIsApprover employeeId =
      values.Approvers
      |> List.tryFindIndex (fun a -> a.EmployeeId = string employeeId)
      |> Option.map (fun foundInd -> foundInd = index)
      |> Option.defaultValue false

   let optionIsUnselectedAdmin employeeId =
      values.Approvers
      |> List.exists (fun a -> a.EmployeeId = string employeeId)
      |> not

   let admins =
      admins.Values |> Seq.map (fun em -> string em.EmployeeId, em.Name)

   let options =
      admins
      |> Seq.filter (fun (emId, _) ->
         optionIsApprover emId || optionIsUnselectedAdmin emId)
      |> Seq.toList

   let options =
      (string ANY_ADMIN_APPROVER.EmployeeId, string ANY_ADMIN_APPROVER.Name)
      :: options

   let fieldApproverSelect =
      Form.selectField {
         Parser = Ok
         Value = fun values -> values.EmployeeId
         Update = fun newValue values -> { values with EmployeeId = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Approver:"
            Placeholder = "Select an approver"
            Options = options
         }
      }

   Form.succeed id |> Form.append fieldApproverSelect

let private approverListForm
   (employees: Map<EmployeeId, Employee>)
   : Form.Form<Values, CommandApprover list, IReactProperty>
   =
   Form.succeed (
      List.map (fun (emId: string) ->
         if emId = ANY_ADMIN_APPROVER.EmployeeId then
            CommandApprover.AnyAdmin
         else
            let emId = EmployeeId(Guid.Parse emId)
            let em = Map.find emId employees

            CommandApprover.Admin {
               EmployeeId = emId
               EmployeeName = em.Name
            })
   )
   |> Form.append (
      Form.meta (fun values ->
         Form.list
            {
               Default = { EmployeeId = ""; Name = "1" }
               Value = fun values -> values.Approvers
               Update =
                  fun newValue values -> { values with Approvers = newValue }
               Attributes = {
                  Label = ""
                  Add =
                     // Disallow creating another list item if no more
                     // potential approvers available
                     if values.Approvers.Length < employees.Count then
                        Some "Add another approver"
                     else
                        None
                  Delete =
                     // Ensure 1 approver input always exists
                     if values.Approvers.Length <= 1 then None else Some ""
               }
            }
            (approverItemForm employees values))
   )

let private amountBasedCriteriaForm (criteriaType: AmountBasedCriteriaType) =
   match criteriaType with
   | AmountBasedCriteriaType.AmountPerCommand ->
      Form.meta (fun values ->
         Form.succeed (fun lowerBound upperBound ->
            let criteria =
               ApprovalCriteria.AmountPerCommand {
                  LowerBound = lowerBound
                  UpperBound = upperBound
               }

            criteria)
         |> Form.append (Form.optional (rangeLowerField values))
         |> Form.append (Form.optional rangeUpperField))
      |> Form.group
   | AmountBasedCriteriaType.AmountDailyLimit ->
      Form.succeed ApprovalCriteria.AmountDailyLimit
      |> Form.append fieldDailyLimit

// Create/Edit a command approval rule or request approval
// for creating/editing a command approval rule.
let ruleConfigureOrRequestApproval
   (session: UserSession)
   (org: Org)
   (rule: CommandApprovalRule)
   =
   let initiator = session.AsInitiator

   let ruleForManagingApprovalRules =
      CommandApprovalRule.ruleManagementRequiresApproval
         initiator.Id
         org.CommandApprovalRules

   match ruleForManagingApprovalRules with
   | None ->
      CommandApprovalRule.ConfigureApprovalRuleCommand.create
         session.OrgId
         initiator
         rule
      |> OrgCommand.ConfigureApprovalRule
   | Some ruleForManagingApprovalRules ->
      let approvableCmd =
         ManageApprovalRuleInput.CreateOrEdit(rule, initiator)
         |> ManageApprovalRuleCommand.create
         |> ManageApprovalRule
         |> ApprovableCommand.PerCommand

      CommandApprovalProgress.RequestCommandApproval.fromApprovableCommand
         session
         ruleForManagingApprovalRules
         approvableCmd
      |> OrgCommand.RequestCommandApproval

let private ruleCreateForm
   (org: Org)
   (employees: Map<EmployeeId, Employee>)
   (session: UserSession)
   : Form.Form<Values, Msg<_>, IReactProperty>
   =
   Form.succeed (fun (cmdType, criteria, approvers) ->
      let cmd =
         ruleConfigureOrRequestApproval session org {
            RuleId = Guid.NewGuid() |> CommandApprovalRuleId
            OrgId = session.OrgId
            CommandType = cmdType
            Criteria = criteria
            Approvers = approvers
         }
         |> FormCommand.Org

      Msg.Submit(FormEntity.Org org, cmd, Started))
   |> Form.append (
      fieldApprovableCommandType
      |> Form.andThen (fun cmdType ->
         match cmdType with
         | ApprovableCommandType.ApprovablePerCommand _ ->
            Form.succeed (fun approvers ->
               cmdType, ApprovalCriteria.PerCommand, approvers)
            |> Form.append (approverListForm employees)
         | ApprovableCommandType.ApprovableAmountBased _ ->
            fieldAmountBasedCriteria
            |> Form.andThen (fun criteriaType ->
               Form.succeed (fun criteria approvers ->
                  cmdType, criteria, approvers)
               |> Form.append (amountBasedCriteriaForm criteriaType)
               |> Form.append (approverListForm employees)))
   )

let private ruleEditForm
   (org: Org)
   (employees: Map<EmployeeId, Employee>)
   (session: UserSession)
   (rule: CommandApprovalRule)
   : Form.Form<Values, Msg<_>, IReactProperty>
   =
   Form.succeed
      (fun (criteria: ApprovalCriteria, approvers: CommandApprover list) ->
         let cmd =
            ruleConfigureOrRequestApproval session org {
               RuleId = rule.RuleId
               OrgId = rule.OrgId
               CommandType = rule.CommandType
               Criteria = criteria
               Approvers = approvers
            }
            |> FormCommand.Org

         Msg.Submit(FormEntity.Org org, cmd, Started))
   |> Form.append (
      match rule.CommandType with
      | ApprovableCommandType.ApprovablePerCommand _ ->
         Form.succeed (fun approvers -> rule.Criteria, approvers)
         |> Form.append (approverListForm employees)
      | ApprovableCommandType.ApprovableAmountBased _ ->
         let amountBasedCriteriaType =
            match rule.Criteria with
            | ApprovalCriteria.AmountDailyLimit _ ->
               AmountBasedCriteriaType.AmountDailyLimit
            | _ -> AmountBasedCriteriaType.AmountPerCommand

         Form.succeed (fun criteria approvers -> criteria, approvers)
         |> Form.append (amountBasedCriteriaForm amountBasedCriteriaType)
         |> Form.append (approverListForm employees)
   )

let customFormSubmit onCancel =
   Form.View.Action.Custom(fun state _ ->
      Form.View.submitAndCancelButton "Save" onCancel state)

[<ReactComponent>]
let CommandApprovalRuleEditFormComponent
   (onCancel: unit -> unit)
   (onSubmit: OrgCommandReceipt -> unit)
   (session: UserSession)
   (org: Org)
   (employees: Map<EmployeeId, Employee>)
   (rule: CommandApprovalRule)
   =
   let initValues = {
      CommandType = string rule.CommandType
      Approvers =
         rule.Approvers
         |> List.map (function
            | CommandApprover.AnyAdmin -> ANY_ADMIN_APPROVER
            | CommandApprover.Admin a -> {
               EmployeeId = string a.EmployeeId
               Name = a.EmployeeName
              })
      AmountBasedCriteriaType = ""
      DailyLimit = ""
      RangeLowerBound = ""
      RangeUpperBound = ""
   }

   let initValues =
      match rule.Criteria with
      | ApprovalCriteria.AmountDailyLimit limit -> {
         initValues with
            AmountBasedCriteriaType =
               string AmountBasedCriteriaType.AmountDailyLimit
            DailyLimit = string limit
        }
      | ApprovalCriteria.AmountPerCommand range -> {
         initValues with
            AmountBasedCriteriaType =
               string AmountBasedCriteriaType.AmountPerCommand
            RangeLowerBound =
               range.LowerBound |> Option.map string |> Option.defaultValue ""
            RangeUpperBound =
               range.UpperBound |> Option.map string |> Option.defaultValue ""
        }
      | ApprovalCriteria.PerCommand -> initValues

   FormContainer {|
      InitialValues = initValues
      Form = ruleEditForm org employees session rule
      OnSubmit =
         function
         | FormSubmitReceipt.Org receipt -> onSubmit receipt
         | _ -> ()
      Action = Some(customFormSubmit onCancel)
      Session = session
      ComponentName = "CommandApprovalRuleEditForm"
      UseEventSubscription = Some [ SignalREventProvider.EventType.Org ]
   |}

[<ReactComponent>]
let CommandApprovalRuleCreateFormComponent
   (onCancel: unit -> unit)
   (onSubmit: OrgCommandReceipt -> unit)
   (session: UserSession)
   (org: Org)
   (employees: Map<EmployeeId, Employee>)
   =
   FormContainer {|
      InitialValues = {
         CommandType =
            InviteEmployeeCommandType
            |> ApprovableCommandType.ApprovablePerCommand
            |> string
         Approvers = [ { EmployeeId = ""; Name = "" } ]
         AmountBasedCriteriaType = string ApprovalCriteria.PerCommand
         DailyLimit = ""
         RangeLowerBound = ""
         RangeUpperBound = ""
      }
      Form = ruleCreateForm org employees session
      OnSubmit =
         function
         | FormSubmitReceipt.Org receipt -> onSubmit receipt
         | _ -> ()
      Action = Some(customFormSubmit onCancel)
      Session = session
      ComponentName = "CommandApprovalRuleCreateForm"
      UseEventSubscription = Some [ SignalREventProvider.EventType.Org ]
   |}
