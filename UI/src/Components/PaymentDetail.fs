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

type State = {
   IsFulfillingPayment: bool
} with

   static member Init = { IsFulfillingPayment = false }

[<RequireQualifiedAccess>]
type Confirmation =
   | CancelPayment of Payment
   | DeclinePayment of Payment

type Msg =
   | TogglePaymentFulfillment
   | DismissConfirmation
   | ShowConfirmation of Initiator * Confirmation
   | ConfirmCancelPaymentRequest of
      Initiator *
      reason: string option *
      Payment *
      AsyncOperationStatus<Result<AccountCommandReceipt, Err>>
   | ConfirmDeclinePaymentRequest of
      Initiator *
      reason: string option *
      Payment *
      AsyncOperationStatus<Result<AccountCommandReceipt, Err>>
   | SubmitPaymentForApproval

let init () = State.Init, Cmd.none

let getAccount (payment: Payment) =
   AccountService.getAccount payment.BaseInfo.Payee.AccountId

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
   | ConfirmCancelPaymentRequest(initiator, reason, payment, Started) ->
      let cancel = async {
         let! accountOpt = getAccount payment

         match payment, accountOpt with
         | Payment.ThirdParty _, _ ->
            let err =
               NotImplementedException "third party payment"
               |> Err.NotImplementedError
               |> Error

            return
               ConfirmCancelPaymentRequest(
                  initiator,
                  reason,
                  payment,
                  Finished err
               )
         | Payment.Platform p, Ok(Some account) ->
            let cmd =
               CancelPlatformPaymentCommand.create initiator {
                  BaseInfo = PlatformPaymentBaseInfo.fromPayment p
                  Reason = reason
               }
               |> AccountCommand.CancelPlatformPayment

            let! res = AccountService.submitCommand account cmd

            return
               ConfirmCancelPaymentRequest(
                  initiator,
                  reason,
                  payment,
                  Finished res
               )
         | _ ->
            let err = Err.UnexpectedError "Error" |> Error

            return
               ConfirmCancelPaymentRequest(
                  initiator,
                  reason,
                  payment,
                  Finished err
               )
      }

      state, Cmd.fromAsync cancel
   | ConfirmCancelPaymentRequest(_, _, p, Finished(Ok receipt)) ->
      notifyParentOnUpdate receipt

      state,
      Alerts.toastSuccessCommand $"Cancelled payment request to {p.Payer}"
   | ConfirmCancelPaymentRequest(_, _, _, Finished(Error err)) ->
      Log.error (string err)
      state, Alerts.toastCommand err
   | ConfirmDeclinePaymentRequest(initiator, reason, payment, Started) ->
      let decline = async {
         let! accountOpt = getAccount payment

         match payment, accountOpt with
         | Payment.ThirdParty _, _ ->
            let err =
               NotImplementedException "third party payment"
               |> Err.NotImplementedError
               |> Error

            return
               ConfirmDeclinePaymentRequest(
                  initiator,
                  reason,
                  payment,
                  Finished err
               )
         | Payment.Platform p, Ok(Some account) ->
            let cmd =
               DeclinePlatformPaymentCommand.create initiator {
                  BaseInfo = PlatformPaymentBaseInfo.fromPayment p
                  Reason = reason
               }
               |> AccountCommand.DeclinePlatformPayment

            let! res = AccountService.submitCommand account cmd

            return
               ConfirmDeclinePaymentRequest(
                  initiator,
                  reason,
                  payment,
                  Finished res
               )
         | _ ->
            let err = Err.UnexpectedError "Error" |> Error

            return
               ConfirmDeclinePaymentRequest(
                  initiator,
                  reason,
                  payment,
                  Finished err
               )
      }

      state, Cmd.fromAsync decline
   | ConfirmDeclinePaymentRequest(_, _, p, Finished(Ok receipt)) ->
      notifyParentOnUpdate receipt
      let payee = p.BaseInfo.Payee.OrgName
      state, Alerts.toastSuccessCommand $"Declined payment request from {payee}"
   | ConfirmDeclinePaymentRequest(_, _, _, Finished(Error err)) ->
      Log.error (string err)
      state, Alerts.toastCommand err
   | SubmitPaymentForApproval ->
      State.Init,
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

   let baseInfo = payment.BaseInfo
   let isPaymentRequestOutgoing = session.OrgId = baseInfo.Payee.OrgId
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

   let canManagePayment = payment.CanManage && paymentPendingApproval.IsNone

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

                  attr.text (Payment.statusDisplay payment + " -> Fulfilled")

                  attr.custom (
                     "data-tooltip",
                     $"Updates to Fulfilled when {cnt} approvals acquired."
                  )

                  attr.custom ("data-placement", "right")
               | None -> attr.text (Payment.statusDisplay payment)
            ]
         ]
      ]

      Html.hr []

      Html.br []

      if isPaymentRequestOutgoing then
         Html.div [
            Html.small "Request sent to:"
            Html.p [
               attr.style [ style.marginLeft 10; style.display.inlineElement ]
               attr.text payment.Payer
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
            Html.small "Requested on:"
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


      match payment.Status with
      | PaymentRequestStatus.Fulfilled f ->
         Html.button [
            attr.classes [ "outline" ]
            attr.text "View Transfer"
            attr.onClick (fun _ ->
               Routes.TransactionsUrl.queryPath {
                  TransactionBrowserQuery.empty with
                     Transaction =
                        f.TransferId
                        |> TransferId.toCorrelationId
                        |> TransactionId
                        |> Some
               }
               |> Router.navigate)
         ]
      | _ ->
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
            match canManagePayment, isPaymentRequestOutgoing with
            | true, true ->
               Html.button [
                  attr.classes [ "outline" ]
                  attr.text "Cancel payment request"
                  attr.onClick (fun _ ->
                     (session.AsInitiator, Confirmation.CancelPayment payment)
                     |> Msg.ShowConfirmation
                     |> dispatch)
               ]
            | true, false ->
               classyNode Html.div [ "grid" ] [
                  Html.button [
                     attr.classes [ "outline" ]
                     attr.text "Decline to pay"
                     attr.onClick (fun _ ->
                        (session.AsInitiator,
                         Confirmation.DeclinePayment payment)
                        |> Msg.ShowConfirmation
                        |> dispatch)
                  ]

                  Html.button [
                     attr.classes [ "outline" ]
                     attr.text "Fulfill payment"
                     attr.onClick (fun _ ->
                        dispatch Msg.TogglePaymentFulfillment)
                  ]
               ]
            | false, false ->
               match
                  paymentPendingApproval |> Option.map approvalRemainingCnt
               with
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
