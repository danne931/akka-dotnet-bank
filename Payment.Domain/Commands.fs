namespace Bank.Payment.Domain

open Validus
open System

open Lib.SharedTypes
open Lib.Validators

type PlatformPaymentRequestInput = {
   BaseInfo: PlatformPaymentBaseInfo
   Expiration: DateTime option
   Memo: string
}

type RequestPlatformPaymentCommand = Command<PlatformPaymentRequestInput>

module RequestPlatformPaymentCommand =
   let create (initiatedBy: Initiator) (data: PlatformPaymentRequestInput) =
      Command.create
         (ParentAccountId.toEntityId data.BaseInfo.Payee.ParentAccountId)
         data.BaseInfo.Payee.OrgId
         (data.BaseInfo.Id |> PaymentRequestId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: RequestPlatformPaymentCommand)
      : ValidationResult<BankEvent<PlatformPaymentRequested>>
      =
      validate {
         let info = cmd.Data.BaseInfo
         let payerOrg = OrgId.get info.Payer.OrgId
         let payeeOrg = OrgId.get info.Payee.OrgId

         let expiration =
            cmd.Data.Expiration
            |> Option.defaultValue (DateTime.UtcNow.AddDays 30)

         let! _ = amountValidator "Payment amount" info.Amount
         let! expiration = dateInFutureValidator "Payment expiration" expiration
         let! _ = Check.Guid.notEquals payerOrg "Payer org = Payee org" payeeOrg
         let! memo = Check.String.notEmpty "memo" cmd.Data.Memo

         return
            BankEvent.create2<
               PlatformPaymentRequestInput,
               PlatformPaymentRequested
             >
               cmd
               {
                  Expiration = expiration
                  BaseInfo = info
                  Memo = memo
               }
      }

type CancelPlatformPaymentRequestCommand =
   Command<PlatformPaymentRequestCancelled>

module CancelPlatformPaymentRequestCommand =
   let create (initiatedBy: Initiator) (data: PlatformPaymentRequestCancelled) =
      let payee = data.BaseInfo.Payee

      Command.create
         (ParentAccountId.toEntityId payee.ParentAccountId)
         payee.OrgId
         (data.BaseInfo.Id |> PaymentRequestId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: CancelPlatformPaymentRequestCommand)
      : ValidationResult<BankEvent<PlatformPaymentRequestCancelled>>
      =
      BankEvent.create<PlatformPaymentRequestCancelled> cmd |> Ok

type DeclinePlatformPaymentRequestCommand =
   Command<PlatformPaymentRequestDeclined>

module DeclinePlatformPaymentRequestCommand =
   let create (initiatedBy: Initiator) (data: PlatformPaymentRequestDeclined) =
      let payee = data.BaseInfo.Payee

      Command.create
         (ParentAccountId.toEntityId payee.ParentAccountId)
         payee.OrgId
         (data.BaseInfo.Id |> PaymentRequestId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: DeclinePlatformPaymentRequestCommand)
      : ValidationResult<BankEvent<PlatformPaymentRequestDeclined>>
      =
      BankEvent.create<PlatformPaymentRequestDeclined> cmd |> Ok
