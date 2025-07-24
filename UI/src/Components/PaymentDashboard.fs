module PaymentDashboard

open Feliz
open Feliz.UseElmish
open Elmish
open Feliz.Router
open Fable.FontAwesome

open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Payment.Domain
open Bank.Org.Domain
open CommandApproval
open UIDomain.Account
open UIDomain.Org
open Lib.Time
open Lib.SharedTypes
open PaymentDetail
open Bank.Account.Forms.PaymentRequest

type State = { Payments: Deferred<PaymentsMaybe> }

[<RequireQualifiedAccess>]
type PaymentActionView =
   | RequestPayment
   | ViewPayment of PaymentRequestId

let private actionNav (action: PaymentActionView option) =
   let path =
      match action with
      | Some PaymentActionView.RequestPayment ->
         Routes.PaymentUrl.RequestPaymentPath
      | Some(PaymentActionView.ViewPayment payId) ->
         Routes.PaymentUrl.selectedPath payId
      | None -> [| Routes.PaymentUrl.BasePath |]

   Router.navigate path

type Msg =
   | LoadPayments of AsyncOperationStatus<PaymentsMaybe>
   | PaymentCommandProcessing of AccountCommandReceipt

let init () =
   { Payments = Deferred.Idle }, Cmd.ofMsg (LoadPayments Started)

let updatePaymentStatus
   (targetId: PaymentRequestId)
   (status: PaymentRequestStatus)
   (payment: PaymentRequest)
   : PaymentRequest
   =
   if payment.SharedDetails.Id = targetId then
      match payment with
      | PaymentRequest.Platform p ->
         PaymentRequest.Platform { p with SharedDetails.Status = status }
      | PaymentRequest.ThirdParty p ->
         PaymentRequest.ThirdParty { p with SharedDetails.Status = status }
   else
      payment

let update orgId msg state =
   match msg with
   | LoadPayments Started ->
      let loadPayments = async {
         let! res = AccountService.getPayments orgId
         return LoadPayments(Finished res)
      }

      { Payments = Deferred.InProgress }, Cmd.fromAsync loadPayments
   | LoadPayments(Finished(Ok(Some payments))) ->
      {
         state with
            Payments = Deferred.Resolved(Ok(Some payments))
      },
      Cmd.none
   | LoadPayments(Finished(Ok None)) ->
      {
         state with
            Payments = Deferred.Resolved(Ok None)
      },
      Cmd.none
   | LoadPayments(Finished(Error err)) ->
      {
         state with
            Payments = Deferred.Resolved(Error err)
      },
      Cmd.none
   | PaymentCommandProcessing receipt ->
      {
         state with
            Payments =
               (Deferred.map << Result.map << Option.map)
                  (fun paymentSummary ->
                     match receipt.PendingEvent with
                     | AccountEvent.PaymentRequested p ->
                        let payment = PaymentRequested.toPaymentRequest p

                        {
                           paymentSummary with
                              OutgoingRequests =
                                 payment :: paymentSummary.OutgoingRequests
                        }
                     | AccountEvent.PaymentRequestCancelled e -> {
                        paymentSummary with
                           OutgoingRequests =
                              paymentSummary.OutgoingRequests
                              |> List.map (
                                 updatePaymentStatus
                                    e.Data.SharedDetails.Id
                                    PaymentRequestStatus.Cancelled
                              )
                       }
                     | AccountEvent.InternalTransferBetweenOrgsPending e ->
                        match e.Data.BaseInfo.FromPaymentRequest with
                        | Some paymentId ->
                           let status =
                              PaymentRequestStatus.Fulfilled {
                                 TransferId = e.Data.BaseInfo.TransferId
                                 FulfilledAt = e.Timestamp
                              }

                           {
                              paymentSummary with
                                 IncomingRequests =
                                    paymentSummary.IncomingRequests
                                    |> List.map (
                                       updatePaymentStatus paymentId status
                                    )
                           }
                        | None -> paymentSummary
                     | AccountEvent.PaymentRequestDeclined e -> {
                        paymentSummary with
                           IncomingRequests =
                              paymentSummary.IncomingRequests
                              |> List.map (
                                 updatePaymentStatus
                                    e.Data.SharedDetails.Id
                                    PaymentRequestStatus.Declined
                              )
                       }
                     | _ -> paymentSummary)
                  state.Payments
      },
      match receipt.PendingEvent with
      | AccountEvent.PaymentRequested e ->
         Alerts.toastSuccessCommand
            $"Payment requested from {e.Data.PayerName}."
      | _ -> Cmd.none

let selectedPayment
   (payments: Deferred<PaymentsMaybe>)
   (selectedId: PaymentRequestId)
   : PaymentRequest option
   =
   match payments with
   | Deferred.Resolved(Ok(Some payments)) ->
      payments.IncomingRequests
      |> List.tryFind (fun p -> p.SharedDetails.Id = selectedId)
      |> Option.orElse (
         payments.OutgoingRequests
         |> List.tryFind (fun p -> p.SharedDetails.Id = selectedId)
      )
   | _ -> None

let renderIncomingTableRow
   (progress: CommandApprovalProgress.T seq)
   (payment: PaymentRequest)
   (selectedId: PaymentRequestId option)
   =
   let sharedDetails = payment.SharedDetails
   let paymentId = sharedDetails.Id
   let statusDisplay = Payment.statusDisplay payment

   let paymentPendingApproval =
      paymentFulfillmentPendingApproval progress paymentId

   Html.tr [
      attr.key (string paymentId)

      match selectedId with
      | Some id when id = paymentId -> attr.classes [ "selected" ]
      | _ -> ()

      attr.onClick (fun _ ->
         paymentId |> PaymentActionView.ViewPayment |> Some |> actionNav)

      attr.children [
         Html.td (DateTime.formatShort sharedDetails.DueAt)

         Html.td sharedDetails.Payee.OrgName

         Html.td (Money.format sharedDetails.Amount)

         Html.td [
            match paymentPendingApproval with
            | None -> attr.text statusDisplay
            | Some _ ->
               attr.text (statusDisplay + " -> Fulfilled")

               attr.custom (
                  "data-tooltip",
                  "Updates to Fulfilled when all approvals acquired."
               )
         ]
      ]
   ]

let renderIncomingTable
   (progress: CommandApprovalProgress.T seq)
   (payments: PaymentRequest list)
   (selectedId: PaymentRequestId option)
   =
   Html.table [
      attr.classes [ "clickable-table" ]
      attr.role "grid"
      attr.children [
         Html.thead [
            Html.tr [
               Html.th [ attr.scope "col"; attr.text "Due" ]

               Html.th [ attr.scope "col"; attr.text "From" ]

               Html.th [ attr.scope "col"; attr.text "Amount" ]

               Html.th [ attr.scope "col"; attr.text "Status" ]
            ]
         ]

         Html.tbody [
            let payments = payments |> List.sortBy _.DisplayPriority

            for payment in payments ->
               renderIncomingTableRow progress payment selectedId
         ]
      ]
   ]

let renderTableRow
   (payment: PaymentRequest)
   (selectedId: PaymentRequestId option)
   (org: OrgWithAccountProfiles)
   =
   let sharedDetails = payment.SharedDetails
   let paymentId = sharedDetails.Id

   Html.tr [
      attr.key (string paymentId)

      match selectedId with
      | Some id when id = paymentId -> attr.classes [ "selected" ]
      | _ -> ()

      attr.onClick (fun _ ->
         paymentId |> PaymentActionView.ViewPayment |> Some |> actionNav)

      attr.children [
         Html.td (DateTime.formatShort sharedDetails.CreatedAt)

         Html.td (
            match payment.SharedDetails.Status with
            | PaymentRequestStatus.Requested ->
               DateTime.futureTimeUIFriendly payment.SharedDetails.DueAt
            | _ -> "-"
         )

         match payment with
         | PaymentRequest.Platform p -> Html.td p.Payer.OrgName
         | PaymentRequest.ThirdParty p -> Html.td p.Payer.Name

         Html.td (Money.format sharedDetails.Amount)

         Html.td (Payment.statusDisplay payment)

         org.Accounts.TryFind sharedDetails.Payee.AccountId
         |> Option.map _.FullName
         |> Option.defaultValue ""
         |> Html.td

         Html.td (
            match payment.SharedDetails.RecurrenceSettings with
            | None -> "-"
            | Some settings -> settings.Pattern.Display
         )
      ]
   ]

let renderTable
   (payments: PaymentRequest list)
   (selectedId: PaymentRequestId option)
   (org: OrgWithAccountProfiles)
   =
   Html.table [
      attr.classes [ "clickable-table" ]
      attr.role "grid"
      attr.children [
         Html.thead [
            Html.tr [
               Html.th [ attr.scope "col"; attr.text "Requested" ]

               Html.th [ attr.scope "col"; attr.text "Due" ]

               Html.th [ attr.scope "col"; attr.text "To" ]

               Html.th [ attr.scope "col"; attr.text "Amount" ]

               Html.th [ attr.scope "col"; attr.text "Status" ]

               Html.th [ attr.scope "col"; attr.text "Account" ]

               Html.th [ attr.scope "col"; attr.text "Schedule" ]
            ]
         ]

         Html.tbody [
            let payments = payments |> List.sortBy _.DisplayPriority

            for payment in payments -> renderTableRow payment selectedId org
         ]
      ]
   ]

// NOTE: Currently fetching all payment requests at once rather than utilizing
// pagination so just going to compute the analytics in the browser rather than
// making a database query.  Good enough for now.
let renderAnalytics (payments: PaymentRequestSummary) =
   let renderAmounts moneyTotal (countTotal: int) (title: string) =
      Html.article [
         Html.h5 [
            attr.text (Money.format moneyTotal)
            attr.style [ style.marginBottom 0 ]
         ]

         classyNode Html.div [ "grid" ] [
            Html.small [ attr.style [ style.marginBottom 0 ]; attr.text title ]

            Html.p [
               attr.text countTotal
               attr.style [ style.marginBottom 0 ]
               attr.classes [ "count-total" ]
            ]
         ]
      ]

   let incoming = payments.AnalyticsIncoming
   let outgoing = payments.AnalyticsOutgoing

   classyNode Html.div [ "grid"; "payment-analytics" ] [
      renderAmounts incoming.UnpaidMoney incoming.UnpaidCount "Incoming Unpaid"
      renderAmounts
         incoming.OverdueMoney
         incoming.OverdueCount
         "Incoming Overdue"
      renderAmounts outgoing.UnpaidMoney outgoing.UnpaidCount "Outgoing Unpaid"
      renderAmounts
         outgoing.OverdueMoney
         outgoing.OverdueCount
         "Outgoing Overdue"
   ]

[<ReactComponent>]
let PaymentDashboardComponent (url: Routes.PaymentUrl) (session: UserSession) =
   let state, dispatch = React.useElmish (init, update session.OrgId, [||])
   let orgCtx = React.useContext OrgProvider.context

   let selectedPaymentRequestId =
      match url with
      | Routes.PaymentUrl.ViewPayment payId -> Some payId
      | _ -> None

   let close _ = actionNav None

   classyNode Html.div [ "payment-dashboard" ] [
      classyNode Html.main [ "container-fluid" ] [
         classyNode Html.div [ "grid" ] [
            Html.section [
               classyNode Html.div [ "title-with-button-container" ] [
                  Html.h4 [ attr.text "Payments" ]
                  Html.button [
                     attr.children [
                        Fa.i [ Fa.Solid.Pray ] []
                        Html.span "Request Payment"
                     ]

                     attr.onClick (fun _ ->
                        actionNav (Some PaymentActionView.RequestPayment))
                  ]
               ]

               Html.progress [
                  attr.custom ("data-transactions-loader", "")
                  if Deferred.resolved state.Payments then
                     attr.value 100
               ]

               Html.div [
                  match orgCtx, state.Payments with
                  | _, Resolved(Error _) ->
                     Html.small "Uh oh. Error getting payments."
                  | _, Resolved(Ok None) -> Html.small "No payments."
                  | Resolved(Ok(Some org)), Resolved(Ok(Some payments)) ->
                     renderAnalytics payments

                     Html.h6 "Incoming Requests"

                     if payments.IncomingRequests.IsEmpty then
                        Html.small "No incoming payment requests."
                     else
                        renderIncomingTable
                           org.Org.CommandApprovalProgress.Values
                           payments.IncomingRequests
                           selectedPaymentRequestId

                     Html.h6 "Outgoing Requests"

                     if payments.OutgoingRequests.IsEmpty then
                        Html.small "No outgoing payment requests."
                     else
                        renderTable
                           payments.OutgoingRequests
                           selectedPaymentRequestId
                           org
                  | _ -> ()
               ]
            ]

            match url with
            | Routes.PaymentUrl.RequestPayment ->
               classyNode Html.article [ "form-wrapper" ] [
                  Html.h6 "Payment Request"

                  CloseButton.render close

                  match orgCtx with
                  | Deferred.Resolved(Ok(Some o)) ->
                     PaymentRequestFormComponent
                        session
                        o.Org
                        o.Accounts
                        (Msg.PaymentCommandProcessing >> dispatch >> close)
                  | _ -> Html.progress []
               ]
               |> ScreenOverlay.Portal
            | Routes.PaymentUrl.ViewPayment payId ->
               classyNode Html.article [ "form-wrapper" ] [
                  Html.h6 "Payment"

                  CloseButton.render close

                  match orgCtx, state.Payments with
                  | Deferred.Resolved(Ok(Some org)), Deferred.Resolved _ ->
                     match selectedPayment state.Payments payId with
                     | Some payment ->
                        PaymentDetailComponent
                           session
                           payment
                           org
                           (Msg.PaymentCommandProcessing >> dispatch >> close)
                     | _ -> Html.p $"No payment found for {payId}"
                  | _ -> Html.progress []
               ]
               |> ScreenOverlay.Portal
            | _ -> ()
         ]
      ]
   ]
