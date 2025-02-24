namespace Bank.Transfer.Domain

open Validus
open System

open Lib.SharedTypes
open Lib.Validators
open AutomaticTransfer

type InternalTransferInput = {
   Memo: string option
   Amount: decimal
   Recipient: InternalTransferRecipient
   Sender: InternalTransferSender
   ScheduledDateSeedOverride: DateTime option
}

type ScheduleInternalTransferInput = {
   TransferInput: InternalTransferInput
   ScheduledDate: DateTime
}

type InternalTransferWithinOrgCommand = Command<InternalTransferInput>

module InternalTransferWithinOrgCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      (initiator: Initiator)
      (data: InternalTransferInput)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         (CorrelationId.create ())
         initiator
         data

   let toEvent
      (cmd: InternalTransferWithinOrgCommand)
      : ValidationResult<BankEvent<InternalTransferWithinOrgPending>>
      =
      validate {
         let info = cmd.Data
         let! _ = amountValidator "Transfer amount" info.Amount

         return
            BankEvent.create2<
               InternalTransferInput,
               InternalTransferWithinOrgPending
             >
               cmd
               {
                  BaseInfo = {
                     TransferId =
                        cmd.CorrelationId |> CorrelationId.get |> TransferId
                     InitiatedBy = cmd.InitiatedBy
                     Recipient = info.Recipient
                     Amount = info.Amount
                     ScheduledDate =
                        info.ScheduledDateSeedOverride
                        |> Option.defaultValue cmd.Timestamp
                     Sender = info.Sender
                  }
               }
      }

type CompleteInternalTransferWithinOrgCommand =
   Command<InternalTransferWithinOrgCompleted>

module CompleteInternalTransferWithinOrgCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      correlationId
      (initiator: Initiator)
      (data: InternalTransferWithinOrgCompleted)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiator
         data

   let toEvent
      (cmd: CompleteInternalTransferWithinOrgCommand)
      : ValidationResult<BankEvent<InternalTransferWithinOrgCompleted>>
      =
      BankEvent.create<InternalTransferWithinOrgCompleted> cmd |> Ok

type FailInternalTransferWithinOrgCommand =
   Command<InternalTransferWithinOrgFailed>

module FailInternalTransferWithinOrgCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      correlationId
      (initiator: Initiator)
      (data: InternalTransferWithinOrgFailed)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiator
         data

   let toEvent
      (cmd: FailInternalTransferWithinOrgCommand)
      : ValidationResult<BankEvent<InternalTransferWithinOrgFailed>>
      =
      BankEvent.create<InternalTransferWithinOrgFailed> cmd |> Ok

type InternalTransferBetweenOrgsCommand = Command<InternalTransferInput>

module InternalTransferBetweenOrgsCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      (initiator: Initiator)
      (data: InternalTransferInput)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         (CorrelationId.create ())
         initiator
         data

   let toEvent
      (cmd: InternalTransferBetweenOrgsCommand)
      : ValidationResult<BankEvent<InternalTransferBetweenOrgsPending>>
      =
      validate {
         let info = cmd.Data
         let! _ = amountValidator "Transfer amount" info.Amount

         return
            BankEvent.create2<
               InternalTransferInput,
               InternalTransferBetweenOrgsPending
             >
               cmd
               {
                  Memo = info.Memo
                  BaseInfo = {
                     TransferId =
                        cmd.CorrelationId |> CorrelationId.get |> TransferId
                     InitiatedBy = cmd.InitiatedBy
                     Recipient = info.Recipient
                     Amount = info.Amount
                     ScheduledDate =
                        info.ScheduledDateSeedOverride
                        |> Option.defaultValue cmd.Timestamp
                     Sender = info.Sender
                  }
               }
      }

type ScheduleInternalTransferBetweenOrgsCommand =
   Command<ScheduleInternalTransferInput>

module ScheduleInternalTransferBetweenOrgsCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      (initiator: Initiator)
      (data: ScheduleInternalTransferInput)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         (CorrelationId.create ())
         initiator
         data

   let toEvent
      (cmd: ScheduleInternalTransferBetweenOrgsCommand)
      : ValidationResult<BankEvent<InternalTransferBetweenOrgsScheduled>>
      =
      validate {
         let info = cmd.Data.TransferInput
         let! _ = amountValidator "Transfer amount" info.Amount

         let! _ =
            datePresentOrFutureValidator "Transfer date" cmd.Data.ScheduledDate

         return
            BankEvent.create2<
               ScheduleInternalTransferInput,
               InternalTransferBetweenOrgsScheduled
             >
               cmd
               {
                  Memo = info.Memo
                  BaseInfo = {
                     TransferId =
                        cmd.CorrelationId |> CorrelationId.get |> TransferId
                     InitiatedBy = cmd.InitiatedBy
                     Recipient = info.Recipient
                     Amount = info.Amount
                     ScheduledDate = cmd.Data.ScheduledDate
                     Sender = info.Sender
                  }
               }
      }

   let initiateTransferCommand
      (data: InternalTransferBetweenOrgsScheduled)
      : InternalTransferBetweenOrgsCommand
      =
      let info = data.BaseInfo

      let cmd =
         InternalTransferBetweenOrgsCommand.create
            (info.Sender.AccountId, info.Sender.OrgId)
            info.InitiatedBy
            {
               Amount = info.Amount
               Sender = info.Sender
               Recipient = info.Recipient
               Memo = data.Memo
               ScheduledDateSeedOverride = None
            }

      {
         cmd with
            CorrelationId = info.TransferId |> TransferId.get |> CorrelationId
      }

type CompleteInternalTransferBetweenOrgsCommand =
   Command<InternalTransferBetweenOrgsCompleted>

module CompleteInternalTransferBetweenOrgsCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      correlationId
      (initiator: Initiator)
      (data: InternalTransferBetweenOrgsCompleted)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiator
         data

   let toEvent
      (cmd: CompleteInternalTransferBetweenOrgsCommand)
      : ValidationResult<BankEvent<InternalTransferBetweenOrgsCompleted>>
      =
      BankEvent.create<InternalTransferBetweenOrgsCompleted> cmd |> Ok

type FailInternalTransferBetweenOrgsCommand =
   Command<InternalTransferBetweenOrgsFailed>

module FailInternalTransferBetweenOrgsCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      correlationId
      (initiator: Initiator)
      (data: InternalTransferBetweenOrgsFailed)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiator
         data

   let toEvent
      (cmd: FailInternalTransferBetweenOrgsCommand)
      : ValidationResult<BankEvent<InternalTransferBetweenOrgsFailed>>
      =
      BankEvent.create<InternalTransferBetweenOrgsFailed> cmd |> Ok

type DepositInternalTransferWithinOrgCommand =
   Command<InternalTransferWithinOrgDeposited>

module DepositInternalTransferWithinOrgCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      correlationId
      (initiatedBy: Initiator)
      (data: InternalTransferWithinOrgDeposited)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: DepositInternalTransferWithinOrgCommand)
      : ValidationResult<BankEvent<InternalTransferWithinOrgDeposited>>
      =
      BankEvent.create<InternalTransferWithinOrgDeposited> cmd |> Ok

type DepositInternalTransferBetweenOrgsCommand =
   Command<InternalTransferBetweenOrgsDeposited>

module DepositInternalTransferBetweenOrgsCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      correlationId
      (initiatedBy: Initiator)
      (data: InternalTransferBetweenOrgsDeposited)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: DepositInternalTransferBetweenOrgsCommand)
      : ValidationResult<BankEvent<InternalTransferBetweenOrgsDeposited>>
      =
      BankEvent.create<InternalTransferBetweenOrgsDeposited> cmd |> Ok

type NicknameDomesticTransferRecipientCommand =
   Command<NicknamedDomesticTransferRecipient>

module NicknameDomesticTransferRecipientCommand =
   let create
      (orgId: OrgId)
      (initiatedBy: Initiator)
      (data: NicknamedDomesticTransferRecipient)
      =
      Command.create
         (OrgId.toEntityId orgId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: NicknameDomesticTransferRecipientCommand)
      : ValidationResult<BankEvent<NicknamedDomesticTransferRecipient>>
      =
      BankEvent.create<NicknamedDomesticTransferRecipient> cmd |> Ok

type DomesticTransferRecipientInput = {
   AccountId: AccountId
   LastName: string
   FirstName: string
   AccountNumber: string
   RoutingNumber: string
   Depository: DomesticRecipientAccountDepository
   PaymentNetwork: PaymentNetwork
}

type RegisterDomesticTransferRecipientCommand =
   Command<DomesticTransferRecipientInput>

module RegisterDomesticTransferRecipientCommand =
   let create
      (orgId: OrgId)
      (initiatedBy: Initiator)
      (data: DomesticTransferRecipientInput)
      =
      Command.create
         (OrgId.toEntityId orgId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: RegisterDomesticTransferRecipientCommand)
      : ValidationResult<BankEvent<RegisteredDomesticTransferRecipient>>
      =
      validate {
         let! accountNumber =
            AccountNumber.fromString "Account Number" cmd.Data.AccountNumber

         and! routingNumber =
            RoutingNumber.fromString "Routing Number" cmd.Data.RoutingNumber

         and! firstName = firstNameValidator cmd.Data.FirstName
         and! lastName = lastNameValidator cmd.Data.LastName

         let recipient = {
            FirstName = firstName
            LastName = lastName
            Nickname = None
            AccountNumber = accountNumber
            RoutingNumber = routingNumber
            Status = RecipientRegistrationStatus.Confirmed
            SenderOrgId = cmd.OrgId
            RecipientAccountId = cmd.Data.AccountId
            Depository = cmd.Data.Depository
            PaymentNetwork = cmd.Data.PaymentNetwork
            CreatedAt = cmd.Timestamp
         }

         return
            BankEvent.create2<
               DomesticTransferRecipientInput,
               RegisteredDomesticTransferRecipient
             >
               cmd
               { Recipient = recipient }
      }

type EditDomesticTransferRecipientInput = {
   RecipientWithoutAppliedUpdates: DomesticTransferRecipient
   LastName: string
   FirstName: string
   AccountNumber: string
   RoutingNumber: string
   Depository: DomesticRecipientAccountDepository
   PaymentNetwork: PaymentNetwork
}

type EditDomesticTransferRecipientCommand =
   Command<EditDomesticTransferRecipientInput>

module EditDomesticTransferRecipientCommand =
   let create
      (orgId: OrgId)
      (initiatedBy: Initiator)
      (data: EditDomesticTransferRecipientInput)
      =
      Command.create
         (OrgId.toEntityId orgId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: EditDomesticTransferRecipientCommand)
      : ValidationResult<BankEvent<EditedDomesticTransferRecipient>>
      =
      validate {
         let! accountNumber =
            AccountNumber.fromString "Account Number" cmd.Data.AccountNumber

         and! routingNumber =
            RoutingNumber.fromString "Routing Number" cmd.Data.RoutingNumber

         and! firstName = firstNameValidator cmd.Data.FirstName
         and! lastName = lastNameValidator cmd.Data.LastName

         let recipient = {
            cmd.Data.RecipientWithoutAppliedUpdates with
               FirstName = firstName
               LastName = lastName
               AccountNumber = accountNumber
               RoutingNumber = routingNumber
               Depository = cmd.Data.Depository
               PaymentNetwork = cmd.Data.PaymentNetwork
         }

         return
            BankEvent.create2<
               EditDomesticTransferRecipientInput,
               EditedDomesticTransferRecipient
             >
               cmd
               { Recipient = recipient }
      }

type FailDomesticTransferRecipientCommand =
   Command<DomesticTransferRecipientFailed>

module FailDomesticTransferRecipientCommand =
   let create
      (orgId: OrgId)
      (initiatedBy: Initiator)
      (data: DomesticTransferRecipientFailed)
      =
      Command.create
         (OrgId.toEntityId orgId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: FailDomesticTransferRecipientCommand)
      : ValidationResult<BankEvent<DomesticTransferRecipientFailed>>
      =
      BankEvent.create<DomesticTransferRecipientFailed> cmd |> Ok

type DomesticTransferRetryConfirmsRecipientCommand =
   Command<DomesticTransferRetryConfirmsRecipient>

module DomesticTransferRetryConfirmsRecipientCommand =
   let create
      (orgId: OrgId)
      (initiatedBy: Initiator)
      (data: DomesticTransferRetryConfirmsRecipient)
      =
      Command.create
         (OrgId.toEntityId orgId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: DomesticTransferRetryConfirmsRecipientCommand)
      : ValidationResult<BankEvent<DomesticTransferRetryConfirmsRecipient>>
      =
      BankEvent.create<DomesticTransferRetryConfirmsRecipient> cmd |> Ok

type DomesticTransferInput = {
   Amount: decimal
   Sender: DomesticTransferSender
   Recipient: DomesticTransferRecipient
   Memo: string option
   ScheduledDateSeedOverride: DateTime option
}

type ScheduleDomesticTransferInput = {
   TransferInput: DomesticTransferInput
   ScheduledDate: DateTime
}

type DomesticTransferCommand = Command<DomesticTransferInput>

module DomesticTransferCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      (correlationId: CorrelationId)
      (initiatedBy: Initiator)
      (data: DomesticTransferInput)
      : DomesticTransferCommand
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: DomesticTransferCommand)
      : ValidationResult<BankEvent<DomesticTransferPending>>
      =
      validate {
         let input = cmd.Data
         let! _ = amountValidator "Transfer amount" input.Amount

         return
            BankEvent.create2<DomesticTransferInput, DomesticTransferPending>
               cmd
               {
                  BaseInfo = {
                     TransferId =
                        cmd.CorrelationId |> CorrelationId.get |> TransferId
                     InitiatedBy = cmd.InitiatedBy
                     ScheduledDate =
                        input.ScheduledDateSeedOverride
                        |> Option.defaultValue cmd.Timestamp
                     Amount = input.Amount
                     Sender = input.Sender
                     Recipient = input.Recipient
                     Memo = input.Memo
                  }
               }
      }

type ScheduleDomesticTransferCommand = Command<ScheduleDomesticTransferInput>

module ScheduleDomesticTransferCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      (initiatedBy: Initiator)
      (data: ScheduleDomesticTransferInput)
      : ScheduleDomesticTransferCommand
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: ScheduleDomesticTransferCommand)
      : ValidationResult<BankEvent<DomesticTransferScheduled>>
      =
      validate {
         let input = cmd.Data.TransferInput
         let! _ = amountValidator "Transfer amount" input.Amount

         let! _ =
            datePresentOrFutureValidator "Transfer date" cmd.Data.ScheduledDate

         return
            BankEvent.create2<
               ScheduleDomesticTransferInput,
               DomesticTransferScheduled
             >
               cmd
               {
                  BaseInfo = {
                     TransferId =
                        cmd.CorrelationId |> CorrelationId.get |> TransferId
                     InitiatedBy = cmd.InitiatedBy
                     ScheduledDate = cmd.Data.ScheduledDate
                     Amount = input.Amount
                     Sender = input.Sender
                     Recipient = input.Recipient
                     Memo = input.Memo
                  }
               }
      }

   let initiateTransferCommand
      (data: DomesticTransferScheduled)
      : DomesticTransferCommand
      =
      let info = data.BaseInfo

      DomesticTransferCommand.create
         (info.Sender.AccountId, info.Sender.OrgId)
         (info.TransferId |> TransferId.get |> CorrelationId)
         info.InitiatedBy
         {
            Amount = info.Amount
            Sender = info.Sender
            Recipient = info.Recipient
            Memo = info.Memo
            ScheduledDateSeedOverride = None
         }

type CompleteDomesticTransferCommand = Command<DomesticTransferCompleted>

module CompleteDomesticTransferCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      correlationId
      (initiatedBy: Initiator)
      (data: DomesticTransferCompleted)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: CompleteDomesticTransferCommand)
      : ValidationResult<BankEvent<DomesticTransferCompleted>>
      =
      BankEvent.create<DomesticTransferCompleted> cmd |> Ok

type FailDomesticTransferCommand = Command<DomesticTransferFailed>

module FailDomesticTransferCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      correlationId
      (initiatedBy: Initiator)
      (data: DomesticTransferFailed)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: FailDomesticTransferCommand)
      : ValidationResult<BankEvent<DomesticTransferFailed>>
      =
      BankEvent.create<DomesticTransferFailed> cmd |> Ok

type UpdateDomesticTransferProgressCommand =
   Command<DomesticTransferProgressUpdate>

module UpdateDomesticTransferProgressCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      correlationId
      (initiatedBy: Initiator)
      (data: DomesticTransferProgressUpdate)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: UpdateDomesticTransferProgressCommand)
      : ValidationResult<BankEvent<DomesticTransferProgressUpdate>>
      =
      BankEvent.create<DomesticTransferProgressUpdate> cmd |> Ok

module DomesticTransferToCommand =
   /// Received a "InProgress" progress response from domestic transfer service.
   let progress (txn: DomesticTransfer) (progress: DomesticTransferInProgress) =
      UpdateDomesticTransferProgressCommand.create
         (txn.Sender.AccountId, txn.Sender.OrgId)
         (txn.TransferId |> TransferId.get |> CorrelationId)
         txn.InitiatedBy
         {
            BaseInfo = {
               TransferId = txn.TransferId
               InitiatedBy = txn.InitiatedBy
               Sender = txn.Sender
               Recipient = txn.Recipient
               ScheduledDate = txn.ScheduledDate
               Amount = txn.Amount
               Memo = txn.Memo
            }
            InProgressInfo = progress
         }

   /// Received a "Complete" progress response from domestic transfer service.
   let complete (txn: DomesticTransfer) =
      CompleteDomesticTransferCommand.create
         (txn.Sender.AccountId, txn.Sender.OrgId)
         (txn.TransferId |> TransferId.get |> CorrelationId)
         txn.InitiatedBy
         {
            BaseInfo = {
               TransferId = txn.TransferId
               InitiatedBy = txn.InitiatedBy
               Sender = txn.Sender
               Recipient = txn.Recipient
               ScheduledDate = txn.ScheduledDate
               Amount = txn.Amount
               Memo = txn.Memo
            }
            // Will be overwritten during the account actor state transition
            // upon detecting a previously failed transfer by txn.TransferId.
            FromRetry = None
         }

   /// Received a "Failed" response from domestic transfer service.
   let fail (txn: DomesticTransfer) (reason: DomesticTransferFailReason) =
      FailDomesticTransferCommand.create
         (txn.Sender.AccountId, txn.Sender.OrgId)
         (txn.TransferId |> TransferId.get |> CorrelationId)
         txn.InitiatedBy
         {
            BaseInfo = {
               TransferId = txn.TransferId
               InitiatedBy = txn.InitiatedBy
               Sender = txn.Sender
               Recipient = txn.Recipient
               ScheduledDate = txn.ScheduledDate
               Amount = txn.Amount
               Memo = txn.Memo
            }
            Reason = reason
         }

   let retry (txn: DomesticTransfer) =
      Command.create
         (AccountId.toEntityId txn.Sender.AccountId)
         txn.Sender.OrgId
         (txn.TransferId |> TransferId.get |> CorrelationId)
         txn.InitiatedBy
         {
            Amount = txn.Amount
            Sender = txn.Sender
            Recipient = txn.Recipient
            Memo = txn.Memo
            ScheduledDateSeedOverride = None
         }

type PlatformPaymentRequestInput = {
   BaseInfo: PlatformPaymentBaseInfo
   Expiration: DateTime option
   Memo: string
}

type RequestPlatformPaymentCommand = Command<PlatformPaymentRequestInput>

module RequestPlatformPaymentCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      (initiatedBy: Initiator)
      (data: PlatformPaymentRequestInput)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         (data.BaseInfo.Id |> PaymentId.get |> CorrelationId)
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

type CancelPlatformPaymentInput = {
   RequestedPayment: PlatformPaymentRequested
   Reason: string option
}

type CancelPlatformPaymentCommand = Command<CancelPlatformPaymentInput>

module CancelPlatformPaymentCommand =
   let create (initiatedBy: Initiator) (data: CancelPlatformPaymentInput) =
      let payee = data.RequestedPayment.BaseInfo.Payee

      Command.create
         (AccountId.toEntityId payee.AccountId)
         payee.OrgId
         (data.RequestedPayment.BaseInfo.Id |> PaymentId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: CancelPlatformPaymentCommand)
      : ValidationResult<BankEvent<PlatformPaymentCancelled>>
      =
      BankEvent.create2<CancelPlatformPaymentInput, PlatformPaymentCancelled>
         cmd
         {
            Reason = cmd.Data.Reason
            BaseInfo = cmd.Data.RequestedPayment.BaseInfo
         }
      |> Ok


type DeclinePlatformPaymentInput = {
   RequestedPayment: PlatformPaymentRequested
   Reason: string option
}

type DeclinePlatformPaymentCommand = Command<DeclinePlatformPaymentInput>

module DeclinePlatformPaymentCommand =
   let create (initiatedBy: Initiator) (data: DeclinePlatformPaymentInput) =
      let payee = data.RequestedPayment.BaseInfo.Payee

      Command.create
         (AccountId.toEntityId payee.AccountId)
         payee.OrgId
         (data.RequestedPayment.BaseInfo.Id |> PaymentId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: DeclinePlatformPaymentCommand)
      : ValidationResult<BankEvent<PlatformPaymentDeclined>>
      =
      BankEvent.create2<DeclinePlatformPaymentInput, PlatformPaymentDeclined>
         cmd
         {
            Reason = cmd.Data.Reason
            BaseInfo = cmd.Data.RequestedPayment.BaseInfo
         }
      |> Ok

type FulfillPlatformPaymentInput = {
   RequestedPayment: PlatformPaymentRequested
   PaymentMethod: AccountId
}

type FulfillPlatformPaymentCommand = Command<FulfillPlatformPaymentInput>

module FulfillPlatformPaymentCommand =
   let create (initiatedBy: Initiator) (data: FulfillPlatformPaymentInput) =
      let payer = data.RequestedPayment.BaseInfo.Payer

      Command.create
         (AccountId.toEntityId data.PaymentMethod)
         payer.OrgId
         (data.RequestedPayment.BaseInfo.Id |> PaymentId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: FulfillPlatformPaymentCommand)
      : ValidationResult<BankEvent<PlatformPaymentPaid>>
      =
      BankEvent.create2<FulfillPlatformPaymentInput, PlatformPaymentPaid> cmd {
         BaseInfo = cmd.Data.RequestedPayment.BaseInfo
         PaymentMethod = PaymentMethod.Platform cmd.Data.PaymentMethod
      }
      |> Ok

type DepositPlatformPaymentCommand = Command<PlatformPaymentDeposited>

module DepositPlatformPaymentCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      (initiatedBy: Initiator)
      (data: PlatformPaymentDeposited)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         (data.BaseInfo.Id |> PaymentId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: DepositPlatformPaymentCommand)
      : ValidationResult<BankEvent<PlatformPaymentDeposited>>
      =
      BankEvent.create<PlatformPaymentDeposited> cmd |> Ok

type ConfigureAutoTransferRuleInput = {
   RuleIdToUpdate: Guid option
   Rule: AutomaticTransferRule
}

type ConfigureAutoTransferRuleCommand = Command<ConfigureAutoTransferRuleInput>

module ConfigureAutoTransferRuleCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      (initiatedBy: Initiator)
      (data: ConfigureAutoTransferRuleInput)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: ConfigureAutoTransferRuleCommand)
      : ValidationResult<BankEvent<AutomaticTransferRuleConfigured>>
      =
      BankEvent.create2<
         ConfigureAutoTransferRuleInput,
         AutomaticTransferRuleConfigured
       >
         cmd
         {
            Config = {
               Id =
                  cmd.Data.RuleIdToUpdate
                  |> Option.defaultValue (Guid.NewGuid())
               Info = cmd.Data.Rule
            }
         }
      |> Ok

type DeleteAutoTransferRuleCommand = Command<AutomaticTransferRuleDeleted>

module DeleteAutoTransferRuleCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      (initiatedBy: Initiator)
      (data: AutomaticTransferRuleDeleted)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: DeleteAutoTransferRuleCommand)
      : ValidationResult<BankEvent<AutomaticTransferRuleDeleted>>
      =
      Ok <| BankEvent.create<AutomaticTransferRuleDeleted> cmd

type InternalAutoTransferCommand =
   Command<AutomaticTransfer.AutoTransferDerivedFromRule>

module InternalAutoTransferCommand =
   let create (data: AutomaticTransfer.AutoTransferDerivedFromRule) =
      let t = data.Transfer

      Command.create
         (AccountId.toEntityId t.Sender.AccountId)
         t.Sender.OrgId
         (CorrelationId.create ())
         {
            Id = InitiatedById Constants.SYSTEM_USER_ID
            Name = "System"
         }
         data

   let toEvent
      (cmd: InternalAutoTransferCommand)
      : ValidationResult<BankEvent<InternalAutomatedTransferPending>>
      =
      validate {
         let info = cmd.Data
         let t = info.Transfer

         return
            BankEvent.create2<
               AutomaticTransfer.AutoTransferDerivedFromRule,
               InternalAutomatedTransferPending
             >
               cmd
               {
                  Rule = cmd.Data.Rule
                  BaseInfo = {
                     TransferId =
                        cmd.CorrelationId |> CorrelationId.get |> TransferId
                     InitiatedBy = cmd.InitiatedBy
                     Recipient = t.Recipient
                     Amount = PositiveAmount.get t.Amount
                     ScheduledDate = cmd.Timestamp
                     Sender = t.Sender
                  }
               }
      }

type CompleteInternalAutoTransferCommand =
   Command<InternalAutomatedTransferCompleted>

module CompleteInternalAutoTransferCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      correlationId
      (initiatedBy: Initiator)
      (data: InternalAutomatedTransferCompleted)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: CompleteInternalAutoTransferCommand)
      : ValidationResult<BankEvent<InternalAutomatedTransferCompleted>>
      =
      BankEvent.create<InternalAutomatedTransferCompleted> cmd |> Ok

type FailInternalAutoTransferCommand = Command<InternalAutomatedTransferFailed>

module FailInternalAutoTransferCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      correlationId
      (initiatedBy: Initiator)
      (data: InternalAutomatedTransferFailed)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: FailInternalAutoTransferCommand)
      : ValidationResult<BankEvent<InternalAutomatedTransferFailed>>
      =
      BankEvent.create<InternalAutomatedTransferFailed> cmd |> Ok

type DepositInternalAutoTransferCommand =
   Command<InternalAutomatedTransferDeposited>

module DepositInternalAutoTransferCommand =
   let create
      (accountId: AccountId, orgId: OrgId)
      correlationId
      (initiatedBy: Initiator)
      (data: InternalAutomatedTransferDeposited)
      =
      Command.create
         (AccountId.toEntityId accountId)
         orgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: DepositInternalAutoTransferCommand)
      : ValidationResult<BankEvent<InternalAutomatedTransferDeposited>>
      =
      BankEvent.create<InternalAutomatedTransferDeposited> cmd |> Ok
