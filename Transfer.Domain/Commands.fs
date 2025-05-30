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
   /// Indicates whether this transfer originated from a scheduled job.
   OriginatedFromSchedule: bool
}

type ScheduleInternalTransferInput = {
   TransferInput: InternalTransferInput
   ScheduledDate: DateTime
}

type InternalTransferWithinOrgCommand = Command<InternalTransferInput>

module InternalTransferWithinOrgCommand =
   let create (initiator: Initiator) (data: InternalTransferInput) =
      Command.create
         (ParentAccountId.toEntityId data.Sender.ParentAccountId)
         data.Sender.OrgId
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
                     Memo = info.Memo
                  }
               }
      }

type FailInternalTransferWithinOrgCommand =
   Command<InternalTransferWithinOrgFailed>

module FailInternalTransferWithinOrgCommand =
   let create
      correlationId
      (initiator: Initiator)
      (data: InternalTransferWithinOrgFailed)
      =
      Command.create
         (ParentAccountId.toEntityId data.BaseInfo.Sender.ParentAccountId)
         data.BaseInfo.Sender.OrgId
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
   let create (initiator: Initiator) (data: InternalTransferInput) =
      Command.create
         (ParentAccountId.toEntityId data.Sender.ParentAccountId)
         data.Sender.OrgId
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
                  FromSchedule = info.OriginatedFromSchedule
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
                     Memo = info.Memo
                  }
               }
      }

type ScheduleInternalTransferBetweenOrgsCommand =
   Command<ScheduleInternalTransferInput>

module ScheduleInternalTransferBetweenOrgsCommand =
   let create (initiator: Initiator) (data: ScheduleInternalTransferInput) =
      Command.create
         (ParentAccountId.toEntityId data.TransferInput.Sender.ParentAccountId)
         data.TransferInput.Sender.OrgId
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
                  BaseInfo = {
                     TransferId =
                        cmd.CorrelationId |> CorrelationId.get |> TransferId
                     InitiatedBy = cmd.InitiatedBy
                     Recipient = info.Recipient
                     Amount = info.Amount
                     ScheduledDate = cmd.Data.ScheduledDate
                     Sender = info.Sender
                     Memo = info.Memo
                  }
               }
      }

type FailInternalTransferBetweenOrgsCommand =
   Command<InternalTransferBetweenOrgsFailed>

module FailInternalTransferBetweenOrgsCommand =
   let create
      correlationId
      (initiator: Initiator)
      (data: InternalTransferBetweenOrgsFailed)
      =
      Command.create
         (ParentAccountId.toEntityId data.BaseInfo.Sender.ParentAccountId)
         data.BaseInfo.Sender.OrgId
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
      correlationId
      (initiatedBy: Initiator)
      (data: InternalTransferWithinOrgDeposited)
      =
      Command.create
         (ParentAccountId.toEntityId data.BaseInfo.Sender.ParentAccountId)
         data.BaseInfo.Sender.OrgId
         correlationId
         initiatedBy
         data

   let fromPending
      (evt: BankEvent<InternalTransferWithinOrgPending>)
      : DepositInternalTransferWithinOrgCommand
      =
      let info = evt.Data.BaseInfo

      Command.create
         (ParentAccountId.toEntityId info.Sender.ParentAccountId)
         evt.OrgId
         evt.CorrelationId
         evt.InitiatedBy
         { BaseInfo = info }

   let toEvent
      (cmd: DepositInternalTransferWithinOrgCommand)
      : ValidationResult<BankEvent<InternalTransferWithinOrgDeposited>>
      =
      BankEvent.create<InternalTransferWithinOrgDeposited> cmd |> Ok

type DepositInternalTransferBetweenOrgsCommand =
   Command<InternalTransferBetweenOrgsDeposited>

module DepositInternalTransferBetweenOrgsCommand =
   let create
      correlationId
      (initiatedBy: Initiator)
      (data: InternalTransferBetweenOrgsDeposited)
      =
      Command.create
         (ParentAccountId.toEntityId data.BaseInfo.Recipient.ParentAccountId)
         data.BaseInfo.Recipient.OrgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: DepositInternalTransferBetweenOrgsCommand)
      : ValidationResult<BankEvent<InternalTransferBetweenOrgsDeposited>>
      =
      BankEvent.create<InternalTransferBetweenOrgsDeposited> cmd |> Ok

type DomesticTransferRecipientInput = {
   AccountId: AccountId
   LastName: string
   FirstName: string
   AccountNumber: string
   RoutingNumber: string
   Depository: DomesticRecipientAccountDepository
   PaymentNetwork: PaymentNetwork
   Sender: {|
      OrgId: OrgId
      ParentAccountId: ParentAccountId
   |}
}

type RegisterDomesticTransferRecipientCommand =
   Command<DomesticTransferRecipientInput>

module RegisterDomesticTransferRecipientCommand =
   let create (initiatedBy: Initiator) (data: DomesticTransferRecipientInput) =
      Command.create
         (ParentAccountId.toEntityId data.Sender.ParentAccountId)
         data.Sender.OrgId
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
      (parentAccountId: ParentAccountId)
      (orgId: OrgId)
      (initiatedBy: Initiator)
      (data: EditDomesticTransferRecipientInput)
      =
      Command.create
         (ParentAccountId.toEntityId parentAccountId)
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
      (parentAccountId: ParentAccountId)
      (orgId: OrgId)
      (initiatedBy: Initiator)
      (data: DomesticTransferRecipientFailed)
      =
      Command.create
         (ParentAccountId.toEntityId parentAccountId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: FailDomesticTransferRecipientCommand)
      : ValidationResult<BankEvent<DomesticTransferRecipientFailed>>
      =
      BankEvent.create<DomesticTransferRecipientFailed> cmd |> Ok

type NicknameDomesticTransferRecipientCommand =
   Command<NicknamedDomesticTransferRecipient>

module NicknameDomesticTransferRecipientCommand =
   let create
      (orgId: OrgId)
      (parentAccountId: ParentAccountId)
      (initiatedBy: Initiator)
      (data: NicknamedDomesticTransferRecipient)
      =
      Command.create
         (ParentAccountId.toEntityId parentAccountId)
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: NicknameDomesticTransferRecipientCommand)
      : ValidationResult<BankEvent<NicknamedDomesticTransferRecipient>>
      =
      BankEvent.create<NicknamedDomesticTransferRecipient> cmd |> Ok

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
   OriginatedFromSchedule: bool
}

type ScheduleDomesticTransferInput = {
   TransferInput: DomesticTransferInput
   ScheduledDate: DateTime
}

type DomesticTransferCommand = Command<DomesticTransferInput>

module DomesticTransferCommand =
   let create
      (correlationId: CorrelationId)
      (initiatedBy: Initiator)
      (data: DomesticTransferInput)
      : DomesticTransferCommand
      =
      Command.create
         (ParentAccountId.toEntityId data.Sender.ParentAccountId)
         data.Sender.OrgId
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
                  FromSchedule = input.OriginatedFromSchedule
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
      (initiatedBy: Initiator)
      (data: ScheduleDomesticTransferInput)
      : ScheduleDomesticTransferCommand
      =
      Command.create
         (ParentAccountId.toEntityId data.TransferInput.Sender.ParentAccountId)
         data.TransferInput.Sender.OrgId
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

type CompleteDomesticTransferCommand = Command<DomesticTransferCompleted>

module CompleteDomesticTransferCommand =
   let create
      correlationId
      (initiatedBy: Initiator)
      (data: DomesticTransferCompleted)
      =
      Command.create
         (ParentAccountId.toEntityId data.BaseInfo.Sender.ParentAccountId)
         data.BaseInfo.Sender.OrgId
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
      correlationId
      (initiatedBy: Initiator)
      (data: DomesticTransferFailed)
      =
      Command.create
         (ParentAccountId.toEntityId data.BaseInfo.Sender.ParentAccountId)
         data.BaseInfo.Sender.OrgId
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
      correlationId
      (initiatedBy: Initiator)
      (data: DomesticTransferProgressUpdate)
      =
      Command.create
         (ParentAccountId.toEntityId data.BaseInfo.Sender.ParentAccountId)
         data.BaseInfo.Sender.OrgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: UpdateDomesticTransferProgressCommand)
      : ValidationResult<BankEvent<DomesticTransferProgressUpdate>>
      =
      BankEvent.create<DomesticTransferProgressUpdate> cmd |> Ok

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
         (ParentAccountId.toEntityId payee.ParentAccountId)
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
      let payer = data.RequestedPayment.BaseInfo.Payer

      Command.create
         (ParentAccountId.toEntityId payer.ParentAccountId)
         payer.OrgId
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
   PaymentMethod: PaymentMethod
}

type FulfillPlatformPaymentCommand = Command<FulfillPlatformPaymentInput>

module FulfillPlatformPaymentCommand =
   let create (initiatedBy: Initiator) (data: FulfillPlatformPaymentInput) =
      let payer = data.RequestedPayment.BaseInfo.Payer

      Command.create
         (ParentAccountId.toEntityId payer.ParentAccountId)
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
         PaymentMethod = cmd.Data.PaymentMethod
      }
      |> Ok

type DepositPlatformPaymentCommand = Command<PlatformPaymentDeposited>

module DepositPlatformPaymentCommand =
   let create (initiatedBy: Initiator) (data: PlatformPaymentDeposited) =
      let payee = data.BaseInfo.Payee

      Command.create
         (ParentAccountId.toEntityId payee.ParentAccountId)
         payee.OrgId
         (data.BaseInfo.Id |> PaymentId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: DepositPlatformPaymentCommand)
      : ValidationResult<BankEvent<PlatformPaymentDeposited>>
      =
      BankEvent.create<PlatformPaymentDeposited> cmd |> Ok

type RefundPlatformPaymentCommand = Command<PlatformPaymentRefunded>

module RefundPlatformPaymentCommand =
   let create (initiatedBy: Initiator) (data: PlatformPaymentRefunded) =
      let payer = data.BaseInfo.Payer

      Command.create
         (ParentAccountId.toEntityId payer.ParentAccountId)
         payer.OrgId
         (data.BaseInfo.Id |> PaymentId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: RefundPlatformPaymentCommand)
      : ValidationResult<BankEvent<PlatformPaymentRefunded>>
      =
      BankEvent.create<PlatformPaymentRefunded> cmd |> Ok

type ConfigureAutoTransferRuleInput = {
   RuleIdToUpdate: Guid option
   Rule: AutomaticTransferRule
   AccountId: AccountId
}

type ConfigureAutoTransferRuleCommand = Command<ConfigureAutoTransferRuleInput>

module ConfigureAutoTransferRuleCommand =
   let create
      (parentAccountId: ParentAccountId, orgId: OrgId)
      (initiatedBy: Initiator)
      (data: ConfigureAutoTransferRuleInput)
      =
      Command.create
         (ParentAccountId.toEntityId parentAccountId)
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
            AccountId = cmd.Data.AccountId
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
      (parentAccountId: ParentAccountId, orgId: OrgId)
      (initiatedBy: Initiator)
      (data: AutomaticTransferRuleDeleted)
      =
      Command.create
         (ParentAccountId.toEntityId parentAccountId)
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
         (ParentAccountId.toEntityId t.Sender.ParentAccountId)
         t.Sender.OrgId
         (CorrelationId.create ())
         Initiator.System
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
                     Memo = None
                  }
               }
      }

type FailInternalAutoTransferCommand = Command<InternalAutomatedTransferFailed>

module FailInternalAutoTransferCommand =
   let create
      correlationId
      (initiatedBy: Initiator)
      (data: InternalAutomatedTransferFailed)
      =
      let sender = data.BaseInfo.Sender

      Command.create
         (ParentAccountId.toEntityId sender.ParentAccountId)
         sender.OrgId
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
      correlationId
      (initiatedBy: Initiator)
      (data: InternalAutomatedTransferDeposited)
      =
      let recipient = data.BaseInfo.Recipient

      Command.create
         (ParentAccountId.toEntityId recipient.ParentAccountId)
         recipient.OrgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: DepositInternalAutoTransferCommand)
      : ValidationResult<BankEvent<InternalAutomatedTransferDeposited>>
      =
      BankEvent.create<InternalAutomatedTransferDeposited> cmd |> Ok
