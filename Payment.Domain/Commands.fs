namespace Bank.Payment.Domain

open Validus

open Lib.SharedTypes

type RequestPaymentCommand = Command<PaymentRequested>

module RequestPaymentCommand =
   let create (initiatedBy: Initiator) (data: PaymentRequested) =
      let payee = data.SharedDetails.Payee

      Command.create
         (ParentAccountId.toEntityId payee.ParentAccountId)
         payee.OrgId
         (data.SharedDetails.Id |> PaymentRequestId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: RequestPaymentCommand)
      : ValidationResult<BankEvent<PaymentRequested>>
      =
      validate {
         let shared = cmd.Data.SharedDetails
         let! _ = Check.String.notEmpty "memo" shared.Memo

         let! _ =
            Lib.Validators.datePresentOrFutureValidator
               "due date"
               shared.Expiration

         match cmd.Data with
         | Platform info ->
            let payerOrgId = OrgId.get info.Payer.OrgId
            let payeeOrgId = OrgId.get shared.Payee.OrgId

            let! _ =
               Lib.Validators.amountValidator "Payment amount" shared.Amount

            let! _ =
               Check.Guid.notEquals
                  payerOrgId
                  "Payer org = Payee org"
                  payeeOrgId

            return BankEvent.create<PaymentRequested> cmd
         | ThirdParty info ->
            // TODO: validate info.SecurePaymentFormUrl
            // TODO: validate info.Payer.Email
            let! _ =
               Lib.Validators.amountValidator "Payment amount" shared.Amount

            return BankEvent.create<PaymentRequested> cmd
      }

type CancelPaymentRequestCommand = Command<PaymentRequestCancelled>

module CancelPaymentRequestCommand =
   let create (initiatedBy: Initiator) (data: PaymentRequestCancelled) =
      let payee = data.SharedDetails.Payee

      Command.create
         (ParentAccountId.toEntityId payee.ParentAccountId)
         payee.OrgId
         (data.SharedDetails.Id |> PaymentRequestId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: CancelPaymentRequestCommand)
      : ValidationResult<BankEvent<PaymentRequestCancelled>>
      =
      BankEvent.create<PaymentRequestCancelled> cmd |> Ok

type DeclinePaymentRequestCommand = Command<PaymentRequestDeclined>

module DeclinePaymentRequestCommand =
   let create (initiatedBy: Initiator) (data: PaymentRequestDeclined) =
      let payee = data.SharedDetails.Payee

      Command.create
         (ParentAccountId.toEntityId payee.ParentAccountId)
         payee.OrgId
         (data.SharedDetails.Id |> PaymentRequestId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: DeclinePaymentRequestCommand)
      : ValidationResult<BankEvent<PaymentRequestDeclined>>
      =
      BankEvent.create<PaymentRequestDeclined> cmd |> Ok
