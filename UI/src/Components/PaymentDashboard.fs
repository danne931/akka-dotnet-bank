module PaymentDashboard

open Feliz
open Feliz.UseElmish
open Elmish
open Feliz.Router
open Fable.FontAwesome

open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Transfer.Domain
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
   | ViewPayment of PaymentId

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

let updatePlatformPaymentStatus
   (targetId: PaymentId)
   (status: PlatformPaymentStatus)
   (payment: PlatformPayment)
   : PlatformPayment
   =
   if payment.BaseInfo.Id = targetId then
      { payment with Status = status }
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
                     | AccountEvent.PlatformPaymentRequested p ->
                        let payment =
                           PlatformPaymentRequested.toPayment p
                           |> Payment.Platform

                        {
                           paymentSummary with
                              OutgoingRequests =
                                 payment :: paymentSummary.OutgoingRequests
                        }
                     | AccountEvent.PlatformPaymentCancelled e ->
                        let updateStatus =
                           updatePlatformPaymentStatus
                              e.Data.BaseInfo.Id
                              PlatformPaymentStatus.Cancelled

                        {
                           paymentSummary with
                              OutgoingRequests =
                                 paymentSummary.OutgoingRequests
                                 |> List.map (function
                                    | Payment.Platform p ->
                                       Payment.Platform(updateStatus p)
                                    // NOT yet implemented
                                    | Payment.ThirdParty p ->
                                       Payment.ThirdParty p)
                        }
                     | AccountEvent.PlatformPaymentPending e ->
                        let updateStatus =
                           updatePlatformPaymentStatus
                              e.Data.BaseInfo.Id
                              PlatformPaymentStatus.PaymentPending

                        {
                           paymentSummary with
                              IncomingRequests =
                                 paymentSummary.IncomingRequests
                                 |> List.map updateStatus
                        }
                     | AccountEvent.PlatformPaymentDeclined e ->
                        let updateStatus =
                           updatePlatformPaymentStatus
                              e.Data.BaseInfo.Id
                              PlatformPaymentStatus.Declined

                        {
                           paymentSummary with
                              IncomingRequests =
                                 paymentSummary.IncomingRequests
                                 |> List.map updateStatus
                        }
                     | _ -> paymentSummary)
                  state.Payments
      },
      Cmd.none

let selectedPayment
   (payments: Deferred<PaymentsMaybe>)
   (selectedId: PaymentId)
   : Payment option
   =
   match payments with
   | Deferred.Resolved(Ok(Some payments)) ->
      payments.IncomingRequests
      |> List.tryFind (fun p -> p.BaseInfo.Id = selectedId)
      |> Option.map Payment.Platform
      |> Option.orElse (
         payments.OutgoingRequests
         |> List.tryFind (fun p -> (Payment.baseInfo p).Id = selectedId)
      )
   | _ -> None

let renderIncomingTableRow
   (progress: CommandApprovalProgress.T seq)
   (payment: PlatformPayment)
   (selectedId: PaymentId option)
   =
   let info = payment.BaseInfo
   let paymentId = info.Id
   let statusDisplay = Payment.statusDisplay (Payment.Platform payment)

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
         Html.td (DateTime.formatShort info.Expiration)

         Html.td info.Payee.OrgName

         Html.td (Money.format info.Amount)

         Html.td [
            match paymentPendingApproval with
            | None -> attr.text statusDisplay
            | Some _ ->
               attr.text (statusDisplay + " -> Paid")

               attr.custom (
                  "data-tooltip",
                  "Updates to Paid when all approvals acquired."
               )
         ]
      ]
   ]

let renderIncomingTable
   (progress: CommandApprovalProgress.T seq)
   (payments: PlatformPayment list)
   (selectedId: PaymentId option)
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
            let payments =
               payments
               |> List.sortBy (Payment.Platform >> Payment.displayPriority)

            for payment in payments ->
               renderIncomingTableRow progress payment selectedId
         ]
      ]
   ]

let renderTableRow
   (payment: Payment)
   (selectedId: PaymentId option)
   (org: OrgWithAccountProfiles)
   =
   let paymentBaseInfo = Payment.baseInfo payment
   let paymentId = paymentBaseInfo.Id

   Html.tr [
      attr.key (string paymentId)

      match selectedId with
      | Some id when id = paymentId -> attr.classes [ "selected" ]
      | _ -> ()

      attr.onClick (fun _ ->
         paymentId |> PaymentActionView.ViewPayment |> Some |> actionNav)

      attr.children [
         Html.td (DateTime.formatShort paymentBaseInfo.CreatedAt)

         match payment with
         | Payment.Platform p -> Html.td p.Payer.OrgName
         | Payment.ThirdParty p -> Html.td p.Payer.Name

         Html.td (Money.format paymentBaseInfo.Amount)

         Html.td (Payment.statusDisplay payment)

         org.Accounts.TryFind paymentBaseInfo.Payee.AccountId
         |> Option.map _.FullName
         |> Option.defaultValue ""
         |> Html.td
      ]
   ]

let renderTable
   (payments: Payment list)
   (selectedId: PaymentId option)
   (org: OrgWithAccountProfiles)
   =
   Html.table [
      attr.classes [ "clickable-table" ]
      attr.role "grid"
      attr.children [
         Html.thead [
            Html.tr [
               Html.th [ attr.scope "col"; attr.text "Created on" ]

               Html.th [ attr.scope "col"; attr.text "To" ]

               Html.th [ attr.scope "col"; attr.text "Amount" ]

               Html.th [ attr.scope "col"; attr.text "Status" ]

               Html.th [ attr.scope "col"; attr.text "Account" ]
            ]
         ]

         Html.tbody [
            let payments = payments |> List.sortBy Payment.displayPriority

            for payment in payments -> renderTableRow payment selectedId org
         ]
      ]
   ]

[<ReactComponent>]
let PaymentDashboardComponent (url: Routes.PaymentUrl) (session: UserSession) =
   let state, dispatch = React.useElmish (init, update session.OrgId, [||])
   let orgCtx = React.useContext OrgProvider.context


   let selectedPaymentId =
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
                     Html.h6 "Incoming Requests"

                     if payments.IncomingRequests.IsEmpty then
                        Html.small "No incoming payment requests."
                     else
                        renderIncomingTable
                           org.Org.CommandApprovalProgress.Values
                           payments.IncomingRequests
                           selectedPaymentId

                     Html.h6 "Outgoing Requests"

                     if payments.OutgoingRequests.IsEmpty then
                        Html.small "No outgoing payment requests."
                     else
                        renderTable
                           payments.OutgoingRequests
                           selectedPaymentId
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
