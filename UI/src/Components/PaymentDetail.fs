module PaymentDetail

open System
open Feliz
open Feliz.UseElmish
open Elmish
open Elmish.SweetAlert
open Feliz.Router

open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Transfer.Domain
open Bank.Org.Domain
open CommandApproval
open Bank.Account.Forms.PaymentFulfillment
open UIDomain.Account
open UIDomain.Org
open Lib.Time
open Lib.SharedTypes

type State = { IsFulfillingPayment: bool }

[<RequireQualifiedAccess>]
type Confirmation =
   | CancelPayment of Payment
   | DeclinePayment of Payment

type Msg =
   | TogglePaymentFulfillment
   | DismissConfirmation
   | ShowConfirmation of UserSession * Confirmation
   | ConfirmCancelPaymentRequest of
      UserSession *
      reason: string option *
      Payment *
      AsyncOperationStatus<Result<AccountCommandReceipt, Err>>
   | ConfirmDeclinePaymentRequest of
      UserSession *
      reason: string option *
      Payment *
      AsyncOperationStatus<Result<AccountCommandReceipt, Err>>
   | SubmitPaymentForApproval

let init () =
   { IsFulfillingPayment = false }, Cmd.none

let getAccount (payment: Payment) =
   payment
   |> Payment.baseInfo
   |> fun p -> p.Payee.AccountId
   |> AccountService.getAccount

let update (notifyParentOnUpdate: AccountCommandReceipt -> unit) msg state =
   match msg with
   | TogglePaymentFulfillment ->
      {
         state with
            IsFulfillingPayment = not state.IsFulfillingPayment
      },
      Cmd.none
   | DismissConfirmation -> state, Cmd.none
   | ShowConfirmation(session, confirmation) ->
      let confAction =
         match confirmation with
         | Confirmation.DeclinePayment _ -> "decline"
         | Confirmation.CancelPayment _ -> "cancel"

      let confirm =
         InputAlert(
            function
            | InputAlertResult.Confirmed reason ->
               let reason =
                  if String.IsNullOrWhiteSpace reason then
                     None
                  else
                     Some reason

               match confirmation with
               | Confirmation.DeclinePayment p ->
                  Msg.ConfirmDeclinePaymentRequest(session, reason, p, Started)
               | Confirmation.CancelPayment p ->
                  Msg.ConfirmCancelPaymentRequest(session, reason, p, Started)
            | InputAlertResult.Dismissed _ -> Msg.DismissConfirmation
         )
            .Title(
               $"Are you sure you want to {confAction} this payment request?"
            )
            .Type(AlertType.Question)
            .InputType(InputAlertType.TextArea)
            .Placeholder($"Reason to {confAction}")
            .ShowCloseButton(true)

      state, SweetAlert.Run confirm
   | ConfirmCancelPaymentRequest(session, reason, payment, Started) ->
      let cancel = async {
         let! accountOpt = getAccount payment

         match payment, accountOpt with
         | Payment.ThirdParty _, _ ->
            let err =
               NotImplementedException("third party payment")
               |> Err.NotImplementedError
               |> Error

            return
               ConfirmCancelPaymentRequest(
                  session,
                  reason,
                  payment,
                  Finished err
               )
         | Payment.Platform p, Ok(Some account) ->
            let cmd =
               let initiator = InitiatedById session.EmployeeId

               CancelPlatformPaymentCommand.create initiator {
                  RequestedPayment = {
                     PlatformPaymentRequested.fromPayment p with
                        BaseInfo.InitiatedById = initiator
                  }
                  Reason = reason
               }
               |> AccountCommand.CancelPlatformPayment

            let! res = AccountService.submitCommand account cmd

            return
               ConfirmCancelPaymentRequest(
                  session,
                  reason,
                  payment,
                  Finished res
               )
         | _ ->
            let err = Err.UnexpectedError "Error" |> Error

            return
               ConfirmCancelPaymentRequest(
                  session,
                  reason,
                  payment,
                  Finished err
               )
      }

      state, Cmd.fromAsync cancel
   | ConfirmCancelPaymentRequest(_, _, p, Finished(Ok receipt)) ->
      notifyParentOnUpdate receipt

      state,
      Alerts.toastSuccessCommand
         $"Cancelled payment request to {Payment.payer p}"
   | ConfirmCancelPaymentRequest(_, _, _, Finished(Error err)) ->
      Log.error (string err)
      state, Alerts.toastCommand err
   | ConfirmDeclinePaymentRequest(session, reason, payment, Started) ->
      let decline = async {
         let! accountOpt = getAccount payment

         match payment, accountOpt with
         | Payment.ThirdParty _, _ ->
            let err =
               NotImplementedException("third party payment")
               |> Err.NotImplementedError
               |> Error

            return
               ConfirmDeclinePaymentRequest(
                  session,
                  reason,
                  payment,
                  Finished err
               )
         | Payment.Platform p, Ok(Some account) ->
            let cmd =
               let initiator = InitiatedById session.EmployeeId

               DeclinePlatformPaymentCommand.create initiator {
                  RequestedPayment = {
                     PlatformPaymentRequested.fromPayment p with
                        BaseInfo.InitiatedById = initiator
                  }
                  Reason = reason
               }
               |> AccountCommand.DeclinePlatformPayment

            let! res = AccountService.submitCommand account cmd

            return
               ConfirmDeclinePaymentRequest(
                  session,
                  reason,
                  payment,
                  Finished res
               )
         | _ ->
            let err = Err.UnexpectedError "Error" |> Error

            return
               ConfirmDeclinePaymentRequest(
                  session,
                  reason,
                  payment,
                  Finished err
               )
      }

      state, Cmd.fromAsync decline
   | ConfirmDeclinePaymentRequest(_, _, p, Finished(Ok receipt)) ->
      notifyParentOnUpdate receipt
      let payee = (Payment.baseInfo p).Payee.OrgName
      state, Alerts.toastSuccessCommand $"Declined payment request from {payee}"
   | ConfirmDeclinePaymentRequest(_, _, _, Finished(Error err)) ->
      Log.error (string err)
      state, Alerts.toastCommand err
   | SubmitPaymentForApproval ->
      { IsFulfillingPayment = false },
      Cmd.batch [
         Alerts.toastSuccessCommand "Submitted payment for approval."
         Cmd.navigate Routes.PaymentUrl.BasePath
      ]

[<ReactComponent>]
let PaymentDetailComponent
   (session: UserSession)
   (payment: Payment)
   (org: OrgWithAccountProfiles)
   (notifyParentOnUpdate: AccountCommandReceipt -> unit)
   =
   let orgDispatch = React.useContext OrgProvider.dispatchContext

   let state, dispatch =
      React.useElmish (init, update notifyParentOnUpdate, [||])

   let baseInfo = Payment.baseInfo payment
   let isPaymentOutgoing = session.OrgId = baseInfo.Payee.OrgId
   let accounts = org.Accounts

   let paymentPendingApproval =
      paymentFulfillmentPendingApproval
         org.Org.CommandApprovalProgress.Values
         baseInfo.Id

   let approvalRemainingCnt (progress: CommandApprovalProgress.T) =
      org.Org.CommandApprovalRules.TryFind progress.RuleId
      |> Option.map (fun rule ->
         CommandApprovalProgress.remainingApprovalRequiredBy rule progress
         |> _.Length
         |> string)

   let canManagePayment =
      Payment.canManage payment && paymentPendingApproval.IsNone

   classyNode Html.div [ "payment-detail" ] [
      Html.div [
         Html.h5 [
            attr.style [ style.marginBottom 0 ]
            attr.text (Money.format baseInfo.Amount)
         ]
         Html.div [
            Html.small [
               match paymentPendingApproval with
               | Some progress ->
                  let cnt =
                     approvalRemainingCnt progress |> Option.defaultValue "all"

                  attr.text (Payment.statusDisplay payment + " -> Paid")

                  attr.custom (
                     "data-tooltip",
                     $"Updates to Paid when {cnt} approvals acquired."
                  )

                  attr.custom ("data-placement", "right")
               | None -> attr.text (Payment.statusDisplay payment)
            ]
         ]
      ]

      Html.hr []

      Html.br []

      if isPaymentOutgoing then
         Html.div [
            Html.small "Request sent to:"
            Html.p [
               attr.style [ style.marginLeft 10; style.display.inlineElement ]
               attr.text (Payment.payer payment)
            ]
         ]

         Html.div [
            Html.small "Destination Account:"
            Html.p [
               attr.style [ style.marginLeft 10; style.display.inlineElement ]

               attr.text (
                  accounts
                  |> Map.tryFind baseInfo.Payee.AccountId
                  |> Option.map _.FullName
                  |> Option.defaultValue baseInfo.Payee.OrgName
               )
            ]
         ]

         Html.div [
            Html.small "Memo:"
            Html.p [
               attr.style [ style.marginLeft 10; style.display.inlineElement ]
               attr.text baseInfo.Memo
            ]
         ]
      else
         Html.div [
            Html.small "Request sent from:"
            Html.p [
               attr.style [ style.marginLeft 10; style.display.inlineElement ]
               attr.text baseInfo.Payee.OrgName
            ]
         ]

         Html.div [
            Html.small "Memo:"
            Html.p [
               attr.style [ style.marginLeft 10; style.display.inlineElement ]
               attr.text baseInfo.Memo
            ]
         ]

      Html.br []

      classyNode Html.div [ "grid" ] [
         Html.div [
            Html.small "Created on:"
            Html.p (DateTime.format baseInfo.CreatedAt)
         ]

         Html.div [
            Html.small (
               if
                  baseInfo.Expiration.ToUniversalTime().Date > System.DateTime.UtcNow.Date
               then
                  "Due:"
               else
                  "Expired on:"
            )
            Html.p (DateTime.format baseInfo.Expiration)
         ]
      ]

      if state.IsFulfillingPayment then
         PaymentFulfillmentFormComponent
            session
            accounts
            org.Org.CommandApprovalRules
            payment
            (fun receipt ->
               dispatch Msg.TogglePaymentFulfillment
               notifyParentOnUpdate receipt)
            (fun cmdApprovalRequest ->
               cmdApprovalRequest
               |> OrgCommand.RequestCommandApproval
               |> OrgProvider.Msg.OrgCommand
               |> orgDispatch

               dispatch Msg.SubmitPaymentForApproval)
      else
         match canManagePayment, isPaymentOutgoing with
         | true, true ->
            Html.button [
               attr.classes [ "outline" ]
               attr.text "Cancel payment request"
               attr.onClick (fun _ ->
                  (session, Confirmation.CancelPayment payment)
                  |> Msg.ShowConfirmation
                  |> dispatch)
            ]
         | true, false ->
            classyNode Html.div [ "grid" ] [
               Html.button [
                  attr.classes [ "outline" ]
                  attr.text "Decline to pay"
                  attr.onClick (fun _ ->
                     (session, Confirmation.DeclinePayment payment)
                     |> Msg.ShowConfirmation
                     |> dispatch)
               ]

               Html.button [
                  attr.classes [ "outline" ]
                  attr.text "Fulfill payment"
                  attr.onClick (fun _ -> dispatch Msg.TogglePaymentFulfillment)
               ]
            ]
         | false, false ->
            match paymentPendingApproval |> Option.map approvalRemainingCnt with
            | Some cnt ->
               Html.p
                  $"Payment will be sent when {cnt} more approvals acquired."

               if session.Role = Role.Admin then
                  Html.button [
                     attr.classes [ "outline" ]
                     attr.text "View Payment Approval Progress"
                     attr.onClick (fun _ ->
                        Router.navigate Routes.ApprovalsUrl.BasePath)
                  ]
            | None -> ()
         | _ -> ()
   ]
