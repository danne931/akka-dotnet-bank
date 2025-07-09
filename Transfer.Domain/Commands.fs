namespace Bank.Transfer.Domain

open Validus
open System

open Lib.SharedTypes
open Lib.Validators
open AutomaticTransfer

type InternalTransferWithinOrgInput = {
   Memo: string option
   Amount: decimal
   Recipient: InternalTransferRecipient
   Sender: InternalTransferSender
   ScheduledDateSeedOverride: DateTime option
   /// Indicates whether this transfer originated from a scheduled job.
   OriginatedFromSchedule: bool
}

type InternalTransferWithinOrgCommand = Command<InternalTransferWithinOrgInput>

module InternalTransferWithinOrgCommand =
   let create (initiator: Initiator) (data: InternalTransferWithinOrgInput) =
      Command.create
         (ParentAccountId.toEntityId data.Sender.ParentAccountId)
         data.Sender.OrgId
         (CorrelationId.create ())
         initiator
         data

   let toEvent
      (cmd: InternalTransferWithinOrgCommand)
      : ValidationResult<BankEvent<InternalTransferWithinOrgDeducted>>
      =
      validate {
         let info = cmd.Data
         let! _ = amountValidator "Transfer amount" info.Amount

         return
            BankEvent.create2<
               InternalTransferWithinOrgInput,
               InternalTransferWithinOrgDeducted
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

type DepositInternalTransferWithinOrgCommand =
   Command<InternalTransferWithinOrgDeposited>

module DepositInternalTransferWithinOrgCommand =
   let create
      correlationId
      (initiatedBy: Initiator)
      (data: InternalTransferWithinOrgDeposited)
      =
      let recipient = data.BaseInfo.Recipient

      Command.create
         (ParentAccountId.toEntityId recipient.ParentAccountId)
         recipient.OrgId
         correlationId
         initiatedBy
         data

   let fromPending
      (evt: BankEvent<InternalTransferWithinOrgDeducted>)
      : DepositInternalTransferWithinOrgCommand
      =
      let info = evt.Data.BaseInfo

      Command.create
         (ParentAccountId.toEntityId info.Recipient.ParentAccountId)
         evt.OrgId
         evt.CorrelationId
         evt.InitiatedBy
         { BaseInfo = info }

   let toEvent
      (cmd: DepositInternalTransferWithinOrgCommand)
      : ValidationResult<BankEvent<InternalTransferWithinOrgDeposited>>
      =
      BankEvent.create<InternalTransferWithinOrgDeposited> cmd |> Ok

type InternalTransferBetweenOrgsInput = {
   Memo: string option
   Amount: decimal
   Recipient: InternalTransferRecipient
   Sender: InternalTransferSender
   ScheduledDateSeedOverride: DateTime option
   OriginatedFromPaymentRequest: PaymentId option
   /// Indicates whether this transfer originated from a scheduled job.
   OriginatedFromSchedule: bool
}

type InternalTransferBetweenOrgsCommand =
   Command<InternalTransferBetweenOrgsInput>

module InternalTransferBetweenOrgsCommand =
   let create (initiator: Initiator) (data: InternalTransferBetweenOrgsInput) =
      let sender = data.Sender

      Command.create
         (ParentAccountId.toEntityId sender.ParentAccountId)
         sender.OrgId
         (CorrelationId.create ())
         initiator
         data

   let fromPaymentRequest
      (initiator: Initiator)
      (payment: PlatformPayment)
      selectedAccountId
      =
      let info = payment.BaseInfo
      let payer = payment.Payer

      let transferInput: InternalTransferBetweenOrgsInput = {
         ScheduledDateSeedOverride = None
         Amount = payment.BaseInfo.Amount
         Sender = {
            OrgId = payer.OrgId
            ParentAccountId = payer.ParentAccountId
            AccountId = selectedAccountId
            Name = payer.OrgName
         }
         Recipient = {
            OrgId = info.Payee.OrgId
            ParentAccountId = info.Payee.ParentAccountId
            AccountId = info.Payee.AccountId
            Name = info.Payee.OrgName
         }
         Memo = Some info.Memo
         OriginatedFromSchedule = false
         OriginatedFromPaymentRequest = Some info.Id
      }

      create initiator transferInput

   let toEvent
      (cmd: InternalTransferBetweenOrgsCommand)
      : ValidationResult<BankEvent<InternalTransferBetweenOrgsPending>>
      =
      validate {
         let info = cmd.Data
         let! _ = amountValidator "Transfer amount" info.Amount

         return
            BankEvent.create2<
               InternalTransferBetweenOrgsInput,
               InternalTransferBetweenOrgsPending
             >
               cmd
               {
                  FromSchedule = info.OriginatedFromSchedule
                  BaseInfo = {
                     FromPaymentRequest = cmd.Data.OriginatedFromPaymentRequest
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

type ScheduleInternalTransferBetweenOrgsInput = {
   TransferInput: InternalTransferBetweenOrgsInput
   ScheduledDate: DateTime
}

type ScheduleInternalTransferBetweenOrgsCommand =
   Command<ScheduleInternalTransferBetweenOrgsInput>

module ScheduleInternalTransferBetweenOrgsCommand =
   let create
      (initiator: Initiator)
      (data: ScheduleInternalTransferBetweenOrgsInput)
      =
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
               ScheduleInternalTransferBetweenOrgsInput,
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
                     FromPaymentRequest = info.OriginatedFromPaymentRequest
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

type SettleInternalTransferBetweenOrgsCommand =
   Command<InternalTransferBetweenOrgsSettled>

module SettleInternalTransferBetweenOrgsCommand =
   let create
      correlationId
      (initiatedBy: Initiator)
      (data: InternalTransferBetweenOrgsSettled)
      =
      Command.create
         (ParentAccountId.toEntityId data.BaseInfo.Sender.ParentAccountId)
         data.BaseInfo.Sender.OrgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: SettleInternalTransferBetweenOrgsCommand)
      : ValidationResult<BankEvent<InternalTransferBetweenOrgsSettled>>
      =
      BankEvent.create<InternalTransferBetweenOrgsSettled> cmd |> Ok

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

         let scheduledDate =
            input.ScheduledDateSeedOverride |> Option.defaultValue cmd.Timestamp

         return
            BankEvent.create2<DomesticTransferInput, DomesticTransferPending>
               cmd
               {
                  FromSchedule = input.OriginatedFromSchedule
                  ExpectedSettlementDate = scheduledDate.AddDays 5
                  BaseInfo = {
                     TransferId =
                        cmd.CorrelationId |> CorrelationId.get |> TransferId
                     InitiatedBy = cmd.InitiatedBy
                     ScheduledDate = scheduledDate
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

         let! scheduledDate =
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
                     ScheduledDate = scheduledDate
                     Amount = input.Amount
                     Sender = input.Sender
                     Recipient = input.Recipient
                     Memo = input.Memo
                  }
                  ExpectedSettlementDate = scheduledDate.AddDays 5
               }
      }

type SettleDomesticTransferCommand = Command<DomesticTransferSettled>

module SettleDomesticTransferCommand =
   let create
      correlationId
      (initiatedBy: Initiator)
      (data: DomesticTransferSettled)
      =
      Command.create
         (ParentAccountId.toEntityId data.BaseInfo.Sender.ParentAccountId)
         data.BaseInfo.Sender.OrgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: SettleDomesticTransferCommand)
      : ValidationResult<BankEvent<DomesticTransferSettled>>
      =
      BankEvent.create<DomesticTransferSettled> cmd |> Ok

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
   Command<DomesticTransferProgressUpdated>

module UpdateDomesticTransferProgressCommand =
   let create
      correlationId
      (initiatedBy: Initiator)
      (data: DomesticTransferProgressUpdated)
      =
      Command.create
         (ParentAccountId.toEntityId data.BaseInfo.Sender.ParentAccountId)
         data.BaseInfo.Sender.OrgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: UpdateDomesticTransferProgressCommand)
      : ValidationResult<BankEvent<DomesticTransferProgressUpdated>>
      =
      BankEvent.create<DomesticTransferProgressUpdated> cmd |> Ok

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

type CancelPlatformPaymentCommand = Command<PlatformPaymentRequestCancelled>

module CancelPlatformPaymentCommand =
   let create (initiatedBy: Initiator) (data: PlatformPaymentRequestCancelled) =
      let payee = data.BaseInfo.Payee

      Command.create
         (ParentAccountId.toEntityId payee.ParentAccountId)
         payee.OrgId
         (data.BaseInfo.Id |> PaymentId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: CancelPlatformPaymentCommand)
      : ValidationResult<BankEvent<PlatformPaymentRequestCancelled>>
      =
      BankEvent.create<PlatformPaymentRequestCancelled> cmd |> Ok

type DeclinePlatformPaymentCommand = Command<PlatformPaymentRequestDeclined>

module DeclinePlatformPaymentCommand =
   let create (initiatedBy: Initiator) (data: PlatformPaymentRequestDeclined) =
      let payee = data.BaseInfo.Payee

      Command.create
         (ParentAccountId.toEntityId payee.ParentAccountId)
         payee.OrgId
         (data.BaseInfo.Id |> PaymentId.get |> CorrelationId)
         initiatedBy
         data

   let toEvent
      (cmd: DeclinePlatformPaymentCommand)
      : ValidationResult<BankEvent<PlatformPaymentRequestDeclined>>
      =
      BankEvent.create<PlatformPaymentRequestDeclined> cmd |> Ok

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
      let sender = data.Transfer.Sender

      Command.create
         (ParentAccountId.toEntityId sender.ParentAccountId)
         sender.OrgId
         (CorrelationId.create ())
         Initiator.System
         data

   let toEvent
      (cmd: InternalAutoTransferCommand)
      : ValidationResult<BankEvent<InternalAutomatedTransferDeducted>>
      =
      validate {
         let info = cmd.Data
         let t = info.Transfer

         return
            BankEvent.create2<
               AutomaticTransfer.AutoTransferDerivedFromRule,
               InternalAutomatedTransferDeducted
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
