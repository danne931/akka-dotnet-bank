module Bank.Account.Forms.ConfigureAutomaticTransferTargetBalanceRule

open Feliz
open Fable.Form.Simple
open System
open Validus

open Fable.Form.Simple.Pico
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Employee.Domain
open Lib.SharedTypes
open AutomaticTransfer
open UIDomain.Account
open Bank.Account.Forms.ConfigureAutomaticTransferFormContainer

type Values = {
   TargetAccountId: string
   ManagingPartnerAccountId: string
   TargetBalance: string
   HasRange: bool
   RangeLowerBound: string
   RangeUpperBound: string
}

let targetBalanceParser =
   Form.Util.positiveAmountParser "Target balance"
   >> validationErrorsHumanFriendly

let rangeLowerBoundParser =
   Form.Util.positiveAmountParser "Lower bound" >> validationErrorsHumanFriendly

let rangeUpperBoundParser =
   Form.Util.positiveAmountParser "Upper bound" >> validationErrorsHumanFriendly

let targetBalanceField =
   Form.textField {
      Parser = targetBalanceParser
      Value = _.TargetBalance
      Update = fun newValue values -> { values with TargetBalance = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Target Balance:"
         Placeholder = "Target balance"
         HtmlAttributes = []
      }
   }

let rangeLowerField (values: Values) =
   Form.textField {
      Parser =
         fun amt -> validate {
            let! lower = rangeLowerBoundParser amt

            do!
               match targetBalanceParser values.TargetBalance with
               | Ok target ->
                  if lower < target then
                     Ok()
                  else
                     Error "Lower bound should be less than target balance"
               | Error _ -> Ok()

            do!
               match rangeUpperBoundParser values.RangeUpperBound with
               | Ok upper ->
                  if lower < upper then
                     Ok()
                  else
                     Error "Lower bound should be less than upper bound"
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

let rangeUpperField (values: Values) =
   Form.textField {
      Parser =
         fun amt -> validate {
            let! upper = rangeUpperBoundParser amt

            do!
               match targetBalanceParser values.TargetBalance with
               | Ok target ->
                  if upper > target then
                     Ok()
                  else
                     Error "Upper bound should be greater than target balance"
               | Error _ -> Ok()

            return upper
         }
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

let rangeCheckbox =
   Form.checkboxField {
      Parser = Ok
      Value = _.HasRange
      Update = fun newVal values -> { values with HasRange = newVal }
      Error = fun _ -> None
      Attributes = { Text = "Range" }
   }

let form
   (accounts: Map<AccountId, Account>)
   (existingRule: TargetBalanceRule option)
   : Form.Form<Values, Result<FormResult, Err>, IReactProperty>
   =
   let existingTargetAccountId =
      existingRule |> Option.map _.TargetAccount.AccountId

   let fieldTargetAccountSelect (values: Values) =
      let optionIsPartner (a: Account) =
         string a.AccountId = values.ManagingPartnerAccountId

      let isSelectable (a: Account) =
         match existingTargetAccountId with
         | None -> a.AutoTransferRule.IsNone && not (optionIsPartner a)
         // Updating the target of a rule which has already been persisted
         // is not allowed.  The user needs to delete the existing rule if
         // they want to change the target account of a rule.
         | Some id -> id = a.AccountId

      Form.selectField {
         Parser =
            Form.Util.accountParser accounts "target account"
            >> validationErrorsHumanFriendly
         Value = fun values -> values.TargetAccountId
         Update =
            fun newValue values -> {
               values with
                  TargetAccountId = newValue
            }
         Error = fun _ -> None
         Attributes = {
            Label = "Manage the balance of"
            Placeholder = ""
            Options = Form.Util.accountSelectOptions isSelectable accounts
         }
      }

   let fieldManagingPartnerAccountSelect (values: Values) =
      Form.selectField {
         Parser =
            Form.Util.accountParser accounts "managing partner account"
            >> validationErrorsHumanFriendly
         Value = fun values -> values.ManagingPartnerAccountId
         Update =
            fun newValue values -> {
               values with
                  ManagingPartnerAccountId = newValue
            }
         Error = fun _ -> None
         Attributes = {
            Label = "Add or remove money from"
            Placeholder = "Select an account"
            Options =
               Form.Util.accountSelectOptions
                  (fun a -> string a.AccountId <> values.TargetAccountId)
                  accounts
         }
      }

   let renderCalculation
      (selectedAccounts: Account * Account)
      (targetAccountBalance: PositiveAmount)
      (range: TargetBalanceRange option)
      =
      let target, partner = selectedAccounts

      let rule = {
         TargetAccountBalance = targetAccountBalance
         TargetBalanceRange = range
         TargetAccount = {
            Name = target.Name
            AccountId = target.AccountId
            ParentAccountId = target.ParentAccountId
            OrgId = target.OrgId
         }
         ManagingPartnerAccount = {
            Name = partner.Name
            AccountId = partner.AccountId
            ParentAccountId = partner.ParentAccountId
            OrgId = partner.OrgId
         }
      }

      Ok {
         Target = target
         Rule = AutomaticTransferRule.TargetBalance rule
      }

   Form.succeed renderCalculation
   |> Form.append (
      Form.meta (fun values ->
         Form.succeed (fun target partner -> target, partner)
         |> Form.append (fieldTargetAccountSelect values)
         |> Form.append (fieldManagingPartnerAccountSelect values))
      |> Form.group
   )
   |> Form.append targetBalanceField
   |> Form.append (
      rangeCheckbox
      |> Form.andThen (fun hasRange ->
         if hasRange then
            Form.meta (fun values ->
               Form.succeed (fun lowerBound upperBound ->
                  Some {
                     LowerBound = lowerBound
                     UpperBound = upperBound
                  })
               |> Form.append (rangeLowerField values)
               |> Form.append (rangeUpperField values))
            |> Form.group
         else
            Form.succeed None)
   )

let renderCalculationDisplay (target: Account) (rule: TargetBalanceRule) =
   let targetName = rule.TargetAccount.Name
   let partnerName = rule.ManagingPartnerAccount.Name
   let targetBalance = PositiveAmount.get rule.TargetAccountBalance
   let currentBalance = target.AvailableBalance

   React.fragment [
      Html.small (Frequency.Schedule CronSchedule.Daily).Display

      Html.p (
         let note =
            let note =
               $"Restore {targetName} from {Money.format currentBalance} to {Money.format targetBalance}"

            match rule.TargetBalanceRange with
            | None -> note + "."
            | Some range ->
               let lower =
                  range.LowerBound |> PositiveAmount.get |> Money.format

               let upper =
                  range.UpperBound |> PositiveAmount.get |> Money.format

               $"{note} if it's outside the target range of {lower} - {upper}."

         if targetBalance > currentBalance then
            $"{note} Add money from {partnerName}."
         else
            $"{note} Move excess money to {partnerName}."
      )

      Html.small "Estimated 1st transfer"
      Html.hr []

      match TargetBalanceRule.computeTransfer rule currentBalance with
      | None -> Html.p "No initial transfer needed."
      | Some t ->
         let amount = t.Amount |> PositiveAmount.get |> Money.format

         let isTargetMoneyFlowOut =
            t.Sender.AccountId = rule.TargetAccount.AccountId

         classyNode Html.div [ "grid" ] [
            Html.p targetName
            Html.p [
               attr.classes [
                  if isTargetMoneyFlowOut then "debit" else "credit"
               ]
               attr.text (if isTargetMoneyFlowOut then $"-{amount}" else amount)
            ]
         ]

         classyNode Html.div [ "grid" ] [
            Html.p partnerName
            Html.p [
               attr.classes [
                  if isTargetMoneyFlowOut then "credit" else "debit"
               ]
               attr.text (if isTargetMoneyFlowOut then amount else $"-{amount}")
            ]
         ]
   ]

[<ReactComponent>]
let ConfigureAutoTransferTargetBalanceRuleFormComponent
   (onSubmit: AccountCommandReceipt -> unit)
   (session: UserSession)
   (accounts: Map<AccountId, Account>)
   (ruleToEdit: (Guid * TargetBalanceRule) option)
   =
   let existingRule = ruleToEdit |> Option.map snd

   let existingTargetAccountId =
      existingRule |> Option.map _.TargetAccount.AccountId

   let targetBalanceRange = existingRule |> Option.bind _.TargetBalanceRange

   let initValues = {
      TargetBalance =
         existingRule
         |> Option.map (_.TargetAccountBalance >> PositiveAmount.get >> string)
         |> Option.defaultValue ""
      TargetAccountId =
         Form.Util.defaultTargetAccountId existingTargetAccountId accounts
      ManagingPartnerAccountId =
         existingRule
         |> Option.map (_.ManagingPartnerAccount.AccountId >> string)
         |> Option.defaultValue ""
      HasRange = targetBalanceRange.IsSome
      RangeLowerBound =
         targetBalanceRange
         |> Option.map (_.LowerBound >> PositiveAmount.get >> string)
         |> Option.defaultValue ""
      RangeUpperBound =
         targetBalanceRange
         |> Option.map (_.UpperBound >> PositiveAmount.get >> string)
         |> Option.defaultValue ""
   }

   ConfigureAutoTransferRuleFormContainer {|
      Accounts = accounts
      Session = session
      ExistingRule =
         ruleToEdit
         |> Option.map (fun (ruleId, rule) ->
            ruleId, AutomaticTransferRule.TargetBalance rule)
      OnSubmitSuccess = onSubmit
      RenderCalculationDisplay =
         fun formResult ->
            match formResult.Rule with
            | AutomaticTransferRule.TargetBalance r ->
               renderCalculationDisplay formResult.Target r
            | _ -> Html.none
      Values = initValues
      Form = form accounts existingRule
      Action = None
   |}
