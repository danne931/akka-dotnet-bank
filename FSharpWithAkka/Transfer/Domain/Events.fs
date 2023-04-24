namespace Bank.Transfer.Domain

open System

open Lib.Types

type DebitedTransfer = {
   Recipient: TransferRecipient
   Date: DateTime
   DebitedAmount: decimal
   Reference: string
}

module TransferEvent =
   let create (cmd: TransferCommand) = {
      EntityId = cmd.EntityId
      Timestamp = cmd.Timestamp
      Data = {
         Recipient = cmd.Recipient
         Date = cmd.Date
         DebitedAmount = cmd.Amount
         Reference = cmd.Reference
      }
   }

type RegisteredInternalTransferRecipient = {
   LastName: string
   FirstName: string
   AccountNumber: string
}

type RegisteredDomesticTransferRecipient = {
   LastName: string
   FirstName: string
   RoutingNumber: string option
   AccountNumber: string
}

type RegisteredInternationalTransferRecipient = {
   LastName: string
   FirstName: string
   Identification: string
   IdentificationStrategy: RecipientAccountIdentificationStrategy
   Currency: string option
}

module RegisterInternalTransferRecipientEvent =
   let create (cmd: RegisterTransferRecipientCommand) = {
      EntityId = cmd.EntityId
      Timestamp = cmd.Timestamp
      Data = {
         AccountNumber = cmd.Recipient.Identification
         LastName = cmd.Recipient.LastName
         FirstName = cmd.Recipient.FirstName
      }
   }

module RegisterDomesticTransferRecipientEvent =
   let create (cmd: RegisterTransferRecipientCommand) = {
      EntityId = cmd.EntityId
      Timestamp = cmd.Timestamp
      Data = {
         LastName = cmd.Recipient.LastName
         FirstName = cmd.Recipient.FirstName
         AccountNumber = cmd.Recipient.Identification
         RoutingNumber = cmd.Recipient.RoutingNumber
      }
   }

module RegisterInternationalTransferRecipientEvent =
   let create (cmd: RegisterTransferRecipientCommand) = {
      EntityId = cmd.EntityId
      Timestamp = cmd.Timestamp
      Data = {
         LastName = cmd.Recipient.LastName
         FirstName = cmd.Recipient.FirstName
         Identification = cmd.Recipient.Identification
         IdentificationStrategy = cmd.Recipient.IdentificationStrategy
         Currency = cmd.Recipient.Currency
      }
   }


type TransferRecipientEvent =
   | RegisteredInternalTransferRecipient of
      BankEvent<RegisteredInternalTransferRecipient>
   | RegisteredDomesticTransferRecipient of
      BankEvent<RegisteredDomesticTransferRecipient>
   | RegisteredInternationalTransferRecipient of
      BankEvent<RegisteredInternationalTransferRecipient>

module RegisterTransferRecipientEvent =
   let eventToRecipient (evt: TransferRecipientEvent) =
      match evt with
      | RegisteredInternalTransferRecipient e -> {
         FirstName = e.Data.FirstName
         LastName = e.Data.LastName
         AccountEnvironment = RecipientAccountEnvironment.Internal
         Identification = e.Data.AccountNumber
         IdentificationStrategy =
            RecipientAccountIdentificationStrategy.AccountId
         Currency = None
         RoutingNumber = None
        }
      | RegisteredDomesticTransferRecipient e -> {
         FirstName = e.Data.FirstName
         LastName = e.Data.LastName
         AccountEnvironment = RecipientAccountEnvironment.Domestic
         Identification = e.Data.AccountNumber
         IdentificationStrategy =
            RecipientAccountIdentificationStrategy.AccountId
         Currency = None
         RoutingNumber = e.Data.RoutingNumber
        }
      | RegisteredInternationalTransferRecipient e -> {
         FirstName = e.Data.FirstName
         LastName = e.Data.LastName
         AccountEnvironment = RecipientAccountEnvironment.International
         Identification = e.Data.Identification
         IdentificationStrategy = e.Data.IdentificationStrategy
         Currency = e.Data.Currency
         RoutingNumber = None
        }
