namespace Bank.Transfer.Domain

open System
open Validus

open Lib.SharedTypes
open Lib.Validators

type TransferInput = {
   Recipient: TransferRecipient
   Date: DateTime
   Amount: decimal
   Reference: string option
}

type TransferCommand = Command<TransferInput>

module TransferCommand =
   let create accountId (data: TransferInput) =
      Command.create accountId (guid ()) data

   let toEvent
      (cmd: TransferCommand)
      : ValidationResult<BankEvent<TransferPending>>
      =
      validate {
         let input = cmd.Data
         let! _ = amountValidator "Transfer amount" input.Amount
         let! _ = dateNotDefaultValidator "Transfer date" input.Date

         return
            BankEvent.create<TransferInput, TransferPending> cmd {
               Recipient = input.Recipient
               Date = input.Date
               DebitedAmount = input.Amount
               Reference = input.Reference
               Status = TransferProgress.Outgoing
            }
      }

type TransferProgressInput = {
   Recipient: TransferRecipient
   Date: DateTime
   Amount: decimal
   Status: TransferProgress
}

type UpdateTransferProgressCommand = Command<TransferProgressInput>

module UpdateTransferProgressCommand =
   let create accountId correlationId (data: TransferProgressInput) =
      Command.create accountId correlationId data

   let toEvent
      (cmd: UpdateTransferProgressCommand)
      : ValidationResult<BankEvent<TransferProgressUpdate>>
      =
      let input = cmd.Data

      Ok
      <| BankEvent.create<TransferProgressInput, TransferProgressUpdate> cmd {
         Recipient = input.Recipient
         Date = input.Date
         DebitedAmount = input.Amount
         Status = input.Status
      }

type ApproveTransferInput = {
   Recipient: TransferRecipient
   Date: DateTime
   Amount: decimal
}

type ApproveTransferCommand = Command<ApproveTransferInput>

module ApproveTransferCommand =
   let create accountId correlationId (data: ApproveTransferInput) =
      Command.create accountId correlationId data

   let toEvent
      (cmd: ApproveTransferCommand)
      : ValidationResult<BankEvent<TransferApproved>>
      =
      let input = cmd.Data

      Ok
      <| BankEvent.create<ApproveTransferInput, TransferApproved> cmd {
         Recipient = input.Recipient
         Date = input.Date
         DebitedAmount = input.Amount
      }

type RejectTransferInput = {
   Recipient: TransferRecipient
   Date: DateTime
   Amount: decimal
   Reason: TransferDeclinedReason
}

type RejectTransferCommand = Command<RejectTransferInput>

module RejectTransferCommand =
   let create accountId correlationId (data: RejectTransferInput) =
      Command.create accountId correlationId data

   let toEvent
      (cmd: RejectTransferCommand)
      : ValidationResult<BankEvent<TransferRejected>>
      =
      let input = cmd.Data
      // Updates status of transfer recipient when a transfer is declined
      // due to an account not existing or becoming closed.
      let updatedRecipientStatus =
         match input.Reason with
         | InvalidAccountInfo -> RecipientRegistrationStatus.InvalidAccount
         | AccountClosed -> RecipientRegistrationStatus.Closed
         | _ -> input.Recipient.Status

      Ok
      <| BankEvent.create<RejectTransferInput, TransferRejected> cmd {
         Recipient = {
            input.Recipient with
               Status = updatedRecipientStatus
         }
         Date = input.Date
         DebitedAmount = input.Amount
         Reason = input.Reason
      }

type DepositTransferInput = { Amount: decimal; Origin: string }
type DepositTransferCommand = Command<DepositTransferInput>

module DepositTransferCommand =
   let create accountId correlationId (data: DepositTransferInput) =
      Command.create accountId correlationId data

   let toEvent
      (cmd: DepositTransferCommand)
      : ValidationResult<BankEvent<TransferDeposited>>
      =
      Ok
      <| BankEvent.create<DepositTransferInput, TransferDeposited> cmd {
         DepositedAmount = cmd.Data.Amount
         Origin = cmd.Data.Origin
      }

type RegisterTransferRecipientInput = { Recipient: TransferRecipient }
type RegisterTransferRecipientCommand = Command<RegisterTransferRecipientInput>

module RegisterTransferRecipientCommand =
   let create accountId (data: RegisterTransferRecipientInput) =
      Command.create accountId (guid ()) data

module TransferRecipientEvent =
   let recipientValidation (cmd: RegisterTransferRecipientCommand) = validate {
      let recipient = cmd.Data.Recipient

      let! _ =
         transferRecipientIdValidator
            (string cmd.EntityId)
            recipient.Identification

      and! _ = accountNumberValidator recipient.Identification

      and! _ = firstNameValidator recipient.FirstName
      and! _ = lastNameValidator recipient.LastName
      return cmd
   }

   let local
      (cmd: RegisterTransferRecipientCommand)
      : ValidationResult<BankEvent<RegisteredInternalTransferRecipient>>
      =
      validate {
         let! _ = recipientValidation cmd
         let recipient = cmd.Data.Recipient

         return
            BankEvent.create<RegisterTransferRecipientInput, RegisteredInternalTransferRecipient>
               cmd
               {
                  AccountNumber = recipient.Identification
                  LastName = recipient.LastName
                  FirstName = recipient.FirstName
               }
      }

   let domestic
      (cmd: RegisterTransferRecipientCommand)
      : ValidationResult<BankEvent<RegisteredDomesticTransferRecipient>>
      =
      validate {
         let! _ = recipientValidation cmd
         and! _ = routingNumberValidator cmd.Data.Recipient.RoutingNumber
         let recipient = cmd.Data.Recipient

         return
            BankEvent.create<RegisterTransferRecipientInput, RegisteredDomesticTransferRecipient>
               cmd
               {
                  LastName = recipient.LastName
                  FirstName = recipient.FirstName
                  AccountNumber = recipient.Identification
                  RoutingNumber = recipient.RoutingNumber
               }
      }

type RegisterInternalSenderInput = { Sender: InternalTransferSender }
type RegisterInternalSenderCommand = Command<RegisterInternalSenderInput>

module RegisterInternalSenderCommand =
   let create accountId (data: RegisterInternalSenderInput) =
      Command.create accountId (guid ()) data

   let toEvent
      (cmd: RegisterInternalSenderCommand)
      : ValidationResult<BankEvent<InternalSenderRegistered>>
      =
      Ok
      <| BankEvent.create<RegisterInternalSenderInput, InternalSenderRegistered>
         cmd
         { TransferSender = cmd.Data.Sender }

type DeactivateInternalRecipientInput = {
   RecipientId: Guid
   RecipientName: string
}

type DeactivateInternalRecipientCommand =
   Command<DeactivateInternalRecipientInput>

module DeactivateInternalRecipientCommand =
   let create accountId (data: DeactivateInternalRecipientInput) =
      Command.create accountId (guid ()) data

   let toEvent
      (cmd: DeactivateInternalRecipientCommand)
      : ValidationResult<BankEvent<InternalRecipientDeactivated>>
      =
      Ok
      <| BankEvent.create<DeactivateInternalRecipientInput, InternalRecipientDeactivated>
         cmd
         {
            RecipientId = cmd.Data.RecipientId
            RecipientName = cmd.Data.RecipientName
         }

module TransferTransactionToCommand =
   let progress (txn: TransferTransaction) (status: TransferProgress) =
      UpdateTransferProgressCommand.create txn.SenderAccountId txn.TransactionId {
         Recipient = txn.Recipient
         Date = txn.Date
         Amount = txn.Amount
         Status = status
      }

   let approve (txn: TransferTransaction) =
      ApproveTransferCommand.create txn.SenderAccountId txn.TransactionId {
         Recipient = txn.Recipient
         Date = txn.Date
         Amount = txn.Amount
      }

   let reject (txn: TransferTransaction) (reason: TransferDeclinedReason) =
      RejectTransferCommand.create txn.SenderAccountId txn.TransactionId {
         Recipient = txn.Recipient
         Date = txn.Date
         Amount = txn.Amount
         Reason = reason
      }
