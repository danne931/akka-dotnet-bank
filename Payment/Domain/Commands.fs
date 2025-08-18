namespace Bank.Payment.Domain

open Validus
open System

open Lib.SharedTypes

type RequestPaymentCommand = Command<PaymentRequested>

module RequestPaymentCommand =
   let create (initiatedBy: Initiator) (data: PaymentRequested) =
      let payee = data.SharedDetails.Payee

      Command.create
         payee.ParentAccountId.AsEntityId
         payee.OrgId
         (CorrelationId data.SharedDetails.Id.Value)
         initiatedBy
         data

   let fromRecurring
      (initiatedBy: Initiator)
      (previousPaymentRequest: PaymentRequested)
      (dueAt: DateTime)
      =
      let updateRecurringSchedule schedule =
         schedule
         |> Option.map (fun info -> {
            info with
               OriginPaymentId = previousPaymentRequest.SharedDetails.Id
               Settings.PaymentsRequestedCount =
                  info.Settings.PaymentsRequestedCount + 1
         })

      let sharedForNewPaymentRequest = {
         previousPaymentRequest.SharedDetails with
            Id = Guid.NewGuid() |> PaymentRequestId
            DueAt = dueAt
      }

      let pay =
         match previousPaymentRequest with
         | PaymentRequested.Platform p ->
            PaymentRequested.Platform {
               p with
                  SharedDetails = sharedForNewPaymentRequest
                  RecurringPaymentReference =
                     updateRecurringSchedule p.RecurringPaymentReference
            }
         | PaymentRequested.ThirdParty p ->
            PaymentRequested.ThirdParty {
               p with
                  SharedDetails = sharedForNewPaymentRequest
                  RecurringPaymentReference =
                     updateRecurringSchedule p.RecurringPaymentReference
            }

      create initiatedBy pay

   let toEvent
      (cmd: RequestPaymentCommand)
      : ValidationResult<BankEvent<PaymentRequested>>
      =
      validate {
         let shared = cmd.Data.SharedDetails
         let! _ = Check.String.notEmpty "memo" shared.Memo

         let dueAt = Lib.Time.DateTime.asEndOfDayUtc shared.DueAt

         let! _ = Lib.Validators.datePresentOrFutureValidator "due date" dueAt

         let! recurringPaymentReference =
            match cmd.Data.RecurringPaymentReference with
            | Some info ->
               match info.Settings.Termination with
               | RecurringPaymentSchedule.RecurrenceTerminationCondition.EndDate date ->
                  Lib.Validators.dateInFutureValidator
                     "Recurring payment termination end date"
                     date
                  |> Result.map (fun date ->
                     Some {
                        info with
                           Settings.Termination =
                              Lib.Time.DateTime.asEndOfDayUtc date
                              |> RecurringPaymentSchedule.RecurrenceTerminationCondition.EndDate
                     })
               | _ -> Ok cmd.Data.RecurringPaymentReference
            | _ -> Ok None

         match cmd.Data with
         | Platform info ->
            let! _ =
               Lib.Validators.amountValidator "Payment amount" shared.Amount

            let! _ =
               Check.Guid.notEquals
                  info.Payer.OrgId.Value
                  "Payer org = Payee org"
                  shared.Payee.OrgId.Value

            let info = {
               info with
                  SharedDetails.DueAt = dueAt
                  RecurringPaymentReference = recurringPaymentReference
            }

            let cmd = { cmd with Data = Platform info }
            return BankEvent.create<PaymentRequested> cmd
         | ThirdParty info ->
            let! _ =
               Lib.Validators.amountValidator "Payment amount" shared.Amount

            let info = {
               info with
                  SharedDetails.DueAt = dueAt
                  RecurringPaymentReference = recurringPaymentReference
            }

            let cmd = { cmd with Data = ThirdParty info }

            return BankEvent.create<PaymentRequested> cmd
      }

type CancelPaymentRequestCommand = Command<PaymentRequestCancelled>

module CancelPaymentRequestCommand =
   let create (initiatedBy: Initiator) (data: PaymentRequestCancelled) =
      let payee = data.SharedDetails.Payee

      Command.create
         payee.ParentAccountId.AsEntityId
         payee.OrgId
         (CorrelationId data.SharedDetails.Id.Value)
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
         payee.ParentAccountId.AsEntityId
         payee.OrgId
         (CorrelationId data.SharedDetails.Id.Value)
         initiatedBy
         data

   let toEvent
      (cmd: DeclinePaymentRequestCommand)
      : ValidationResult<BankEvent<PaymentRequestDeclined>>
      =
      BankEvent.create<PaymentRequestDeclined> cmd |> Ok
