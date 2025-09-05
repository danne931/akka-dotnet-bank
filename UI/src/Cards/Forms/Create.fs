module Bank.Employee.Forms.CreateCard

open Feliz
open Fable.Form.Simple
open System

open Fable.Form.Simple.Pico
open Bank.Account.Domain
open Bank.Employee.Domain
open UIDomain.Employee
open Lib.SharedTypes
open AccountProfileForm
open PurchaseLimitForm
open Bank.Forms.FormContainer

type Values = {
   Nickname: string
   CardType: string
   DailyPurchaseLimit: string
   MonthlyPurchaseLimit: string
   LinkedAccountId: string
   IsVirtual: bool
}

let fieldNickname =
   Form.textField {
      Parser = Ok
      Value = fun values -> values.Nickname
      Update = fun newValue values -> { values with Nickname = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Nickname:"
         Placeholder = "e.g. Overtime Dinner Card"
         HtmlAttributes = []
      }
   }

let selectCardType =
   Form.selectField {
      Parser = CardType.fromStringUnsafe >> Ok
      Value = fun values -> values.CardType
      Update = fun newValue values -> { values with CardType = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = "Card type:"
         Placeholder = ""
         Options = [
            //string CardType.Credit, "Credit"
            string CardType.Debit, "Debit"
         ]
      }
   }

(*
let fieldVirtualCard =
   Form.checkboxField {
      Parser = Ok
      Value = fun values -> values.IsVirtual
      Update = fun newValue values -> {
         values with
            IsVirtual = newValue
      }
      Error = fun _ -> None
      Attributes = { Text = "Virtual Card" }
   }

let fieldPhysicalCard =
   Form.checkboxField {
      Parser = Ok
      Value = fun values -> not values.IsVirtual
      Update = fun newValue values -> {
         values with
            IsVirtual = not newValue
      }
      Error = fun _ -> None
      Attributes = { Text = "Physical Card" }
   }
*)

let private form
   (session: UserSession)
   (employee: Employee)
   (accounts: Map<AccountId, Account>)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let onSubmit
      cardType
      selectedAccountId
      nickname
      (*isVirtual*)
      dailyPurchaseLimit
      monthlyPurchaseLimit
      =
      let cmd =
         CreateCardCommand.create {
            PersonName = employee.Name
            CardNickname = Some nickname
            CardId = Guid.NewGuid() |> CardId
            DailyPurchaseLimit = Some dailyPurchaseLimit
            MonthlyPurchaseLimit = Some monthlyPurchaseLimit
            Virtual = true //isVirtual
            CardType = cardType
            OrgId = employee.OrgId
            AccountId = selectedAccountId
            EmployeeId = employee.EmployeeId
            InitiatedBy = session.AsInitiator
            OriginatedFromEmployeeOnboarding = None
         }
         |> EmployeeCommand.CreateCard
         |> FormCommand.Employee

      Msg.Submit(FormEntity.Employee employee, cmd, Started)

   Form.succeed onSubmit
   |> Form.append selectCardType
   |> Form.append (
      accountSelect (Some "Select an account to link the card to:") accounts
      |> Form.mapValues {
         Value = fun a -> { AccountId = a.LinkedAccountId }
         Update = fun a b -> { b with LinkedAccountId = a.AccountId }
      }
   )
   |> Form.append fieldNickname
   (*
   |> Form.append (
      Form.succeed (fun isVirtual _ -> isVirtual)
      |> Form.append fieldVirtualCard
      |> Form.append fieldPhysicalCard 
      |> Form.group
   )
   *)
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

[<ReactComponent>]
let CreateCardFormComponent
   (onSubmit: EmployeeCommandReceipt -> unit)
   (employee: Employee)
   =
   let sessionCtx = React.useContext UserSessionProvider.context
   let orgCtx = React.useContext OrgProvider.context

   let formProps: Values = {
      CardType = string CardType.Debit
      Nickname = ""
      DailyPurchaseLimit = string Constants.DAILY_PURCHASE_LIMIT_DEFAULT
      MonthlyPurchaseLimit = string Constants.MONTHLY_PURCHASE_LIMIT_DEFAULT
      LinkedAccountId = ""
      IsVirtual = true
   }

   match orgCtx, sessionCtx with
   | Deferred.Resolved(Ok(Some org)), Deferred.Resolved(Ok session) ->
      FormContainer {|
         InitialValues = formProps
         Form = form session employee org.CheckingAccounts
         Action =
            Some(fun _ ->
               Form.View.Action.Custom(fun state _ ->
                  Form.View.submitButton "Create Card" state))
         OnSubmit =
            function
            | FormSubmitReceipt.Employee receipt -> onSubmit receipt
            | _ -> ()
         Session = session
         ComponentName = "CardCreateForm"
         UseEventSubscription = Some [ SignalREventProvider.EventType.Employee ]
      |}
   | _ -> Html.progress []
