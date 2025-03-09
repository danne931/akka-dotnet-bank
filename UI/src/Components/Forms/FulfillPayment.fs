module Bank.Account.Forms.PaymentFulfillment

open Feliz
open Fable.Form.Simple
open System

open Fable.Form.Simple.Pico
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Employee.Domain
open UIDomain.Org
open UIDomain.Account
open Bank.Forms.FormContainer
open Lib.SharedTypes
open CommandApproval

type Values = { AccountIdSourceOfFunds: string }

let formFulfillPlatformPayment
   (payerAccounts: Map<AccountId, Account>)
   (payment: PlatformPayment)
   (initiatedBy: Initiator)
   : Form.Form<Values, Msg<Values>, IReactProperty>
   =
   let fundingSourceOptions =
      payerAccounts
      |> Map.toList
      |> List.map (fun (accountId, a) ->
         string accountId, $"{a.Name} ({Money.format a.Balance})")
      |> List.sortBy snd

   let fieldFundingSourceSelect =
      Form.selectField {
         Parser = Ok
         Value = _.AccountIdSourceOfFunds
         Update =
            fun newValue values -> {
               values with
                  AccountIdSourceOfFunds = newValue
            }
         Error = fun _ -> None
         Attributes = {
            Label = "Pay with account:"
            Placeholder = "No account selected"
            Options = fundingSourceOptions
         }
      }

   let onSubmit (selectedFundingSourceAccountId: string) =
      let selectedAccountId =
         selectedFundingSourceAccountId |> Guid.Parse |> AccountId

      let cmd =
         FulfillPlatformPaymentCommand.create initiatedBy {
            PaymentMethod = selectedAccountId
            RequestedPayment = {
               PlatformPaymentRequested.fromPayment payment with
                  BaseInfo.InitiatedById = initiatedBy.Id
            }
         }
         |> AccountCommand.FulfillPlatformPayment
         |> FormCommand.Account

      Msg.GetAndSubmit(FormEntityId.Account selectedAccountId, cmd)

   Form.succeed onSubmit |> Form.append fieldFundingSourceSelect

[<ReactComponent>]
let PaymentFulfillmentFormComponent
   (session: UserSession)
   (payerAccounts: Map<AccountId, Account>)
   (rules: Map<CommandApprovalRuleId, CommandApprovalRule>)
   (payment: Payment)
   (onSubmit: AccountCommandReceipt -> unit)
   (onSubmitForApproval: CommandApprovalProgress.RequestCommandApproval -> unit)
   =
   let dailyAccrual, setDailyAccrual =
      React.useState<Deferred<Result<CommandApprovalDailyAccrual, Err>>>
         Deferred.InProgress

   React.useEffectOnce (fun () ->
      async {
         let! res =
            OrgService.getTodaysAccrualMetricsByInitiatedBy
               session.OrgId
               session.AsInitiator.Id

         match res with
         | Error e -> Log.error $"Error getting employee accrual metrics {e}"
         | _ -> ()

         setDailyAccrual (Deferred.Resolved res)
      }
      |> Async.StartImmediate)

   let payerAccounts =
      payerAccounts
      |> Map.filter (fun _ a -> a.Depository = AccountDepository.Checking)

   let defaultSourceAccountId =
      payerAccounts
      |> Map.toSeq
      |> Seq.tryHead
      |> Option.map (snd >> _.AccountId >> string)
      |> Option.defaultValue ""

   let initValues = {
      AccountIdSourceOfFunds = defaultSourceAccountId
   }

   React.fragment [
      match payment with
      | Payment.ThirdParty _ -> Html.p "Not implemented."
      | Payment.Platform payment ->
         Html.select [
            attr.onChange (fun (_: string) -> ())
            attr.value "platform"

            attr.children [
               Html.option [
                  attr.value "platform"
                  attr.text "Pay with Platform"
                  attr.disabled true
               ]

               Html.option [
                  attr.value "ach"
                  attr.text "Pay with ACH"
                  attr.disabled true
               ]
            ]
         ]

         match dailyAccrual with
         | Deferred.Resolved(Ok employeeAccrual) ->
            FormContainer {|
               InitialValues = initValues
               Form =
                  formFulfillPlatformPayment
                     payerAccounts
                     payment
                     session.AsInitiator
               Action = Some(Form.View.Action.SubmitOnly "Submit Payment")
               Session = session
               ComponentName = "FulfillPaymentForm"
               UseEventSubscription =
                  Some [
                     // Listen for payment fulfilled
                     SignalREventProvider.EventType.Account
                     // Listen for command approval request
                     SignalREventProvider.EventType.Org
                  ]
               OnSubmit =
                  function
                  | FormSubmitReceipt.Account receipt ->
                     match receipt.PendingCommand with
                     | AccountCommand.FulfillPlatformPayment cmd ->
                        let cmd =
                           cmd
                           |> FulfillPlatformPayment
                           |> ApprovableCommand.AmountBased

                        let requiresApproval =
                           CommandApprovalRule.commandRequiresApproval
                              cmd
                              employeeAccrual
                              rules

                        match requiresApproval with
                        | Some rule ->
                           CommandApprovalProgress.RequestCommandApproval.fromApprovableCommand
                              session
                              rule
                              cmd
                           |> onSubmitForApproval
                        | _ -> onSubmit receipt
                     | _ -> ()
                  | _ -> ()
            |}
         | _ -> Html.progress []
   ]
