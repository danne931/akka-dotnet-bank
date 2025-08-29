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
         data.Sender.ParentAccountId.AsEntityId
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
                     TransferId = TransferId cmd.CorrelationId.Value
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
         recipient.ParentAccountId.AsEntityId
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
         info.Recipient.ParentAccountId.AsEntityId
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
   OriginatedFromPaymentRequest: PaymentRequestId option
   /// Indicates whether this transfer originated from a scheduled job.
   OriginatedFromSchedule: bool
}

type InternalTransferBetweenOrgsCommand =
   Command<InternalTransferBetweenOrgsInput>

module InternalTransferBetweenOrgsCommand =
   let create (initiator: Initiator) (data: InternalTransferBetweenOrgsInput) =
      let sender = data.Sender

      Command.create
         sender.ParentAccountId.AsEntityId
         sender.OrgId
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
               InternalTransferBetweenOrgsInput,
               InternalTransferBetweenOrgsPending
             >
               cmd
               {
                  FromSchedule = info.OriginatedFromSchedule
                  BaseInfo = {
                     FromPaymentRequest = cmd.Data.OriginatedFromPaymentRequest
                     TransferId = TransferId cmd.CorrelationId.Value
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
         data.TransferInput.Sender.ParentAccountId.AsEntityId
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
                     TransferId = TransferId cmd.CorrelationId.Value
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
         data.BaseInfo.Sender.ParentAccountId.AsEntityId
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
         data.BaseInfo.Recipient.ParentAccountId.AsEntityId
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
         data.BaseInfo.Sender.ParentAccountId.AsEntityId
         data.BaseInfo.Sender.OrgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: SettleInternalTransferBetweenOrgsCommand)
      : ValidationResult<BankEvent<InternalTransferBetweenOrgsSettled>>
      =
      BankEvent.create<InternalTransferBetweenOrgsSettled> cmd |> Ok

type CounterpartyInput = {
   CounterpartyId: CounterpartyId
   PartnerBankCounterpartyId: PartnerBankCounterpartyId
   LastName: string
   FirstName: string
   AccountNumber: string
   RoutingNumber: string
   Address: Address
   Depository: CounterpartyAccountDepository
   PaymentNetwork: PaymentNetwork
   Sender: {|
      OrgId: OrgId
      ParentAccountId: ParentAccountId
   |}
}

type RegisterCounterpartyCommand = Command<CounterpartyInput>

module RegisterCounterpartyCommand =
   let create (initiatedBy: Initiator) (data: CounterpartyInput) =
      Command.create
         data.Sender.ParentAccountId.AsEntityId
         data.Sender.OrgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: RegisterCounterpartyCommand)
      : ValidationResult<BankEvent<RegisteredCounterparty>>
      =
      validate {
         let! accountNumber =
            AccountNumber.fromString "Account Number" cmd.Data.AccountNumber

         and! routingNumber =
            RoutingNumber.fromString "Routing Number" cmd.Data.RoutingNumber

         and! firstName = firstNameValidator cmd.Data.FirstName
         and! lastName = lastNameValidator cmd.Data.LastName

         let counterparty = {
            FirstName = firstName
            LastName = lastName
            Nickname = None
            AccountNumber = accountNumber
            RoutingNumber = routingNumber
            OrgId = cmd.OrgId
            CounterpartyId = cmd.Data.CounterpartyId
            PartnerBankCounterpartyId = cmd.Data.PartnerBankCounterpartyId
            Address = cmd.Data.Address
            Depository = cmd.Data.Depository
            PaymentNetwork = cmd.Data.PaymentNetwork
            CreatedAt = cmd.Timestamp
         }

         return
            BankEvent.create2<CounterpartyInput, RegisteredCounterparty> cmd {
               Counterparty = counterparty
            }
      }

type EditCounterpartyInput = {
   CounterpartyWithoutAppliedUpdates: Counterparty
   LastName: string
   FirstName: string
   AccountNumber: string
   RoutingNumber: string
   Depository: CounterpartyAccountDepository
   PaymentNetwork: PaymentNetwork
}

type EditCounterpartyCommand = Command<EditCounterpartyInput>

module EditCounterpartyCommand =
   let create
      (parentAccountId: ParentAccountId)
      (orgId: OrgId)
      (initiatedBy: Initiator)
      (data: EditCounterpartyInput)
      =
      Command.create
         parentAccountId.AsEntityId
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: EditCounterpartyCommand)
      : ValidationResult<BankEvent<EditedCounterparty>>
      =
      validate {
         let! accountNumber =
            AccountNumber.fromString "Account Number" cmd.Data.AccountNumber

         and! routingNumber =
            RoutingNumber.fromString "Routing Number" cmd.Data.RoutingNumber

         and! firstName = firstNameValidator cmd.Data.FirstName
         and! lastName = lastNameValidator cmd.Data.LastName

         let counterparty = {
            cmd.Data.CounterpartyWithoutAppliedUpdates with
               FirstName = firstName
               LastName = lastName
               AccountNumber = accountNumber
               RoutingNumber = routingNumber
               Depository = cmd.Data.Depository
               PaymentNetwork = cmd.Data.PaymentNetwork
         }

         return
            BankEvent.create2<EditCounterpartyInput, EditedCounterparty> cmd {
               Counterparty = counterparty
            }
      }

type NicknameCounterpartyCommand = Command<NicknamedCounterparty>

module NicknameCounterpartyCommand =
   let create
      (orgId: OrgId)
      (parentAccountId: ParentAccountId)
      (initiatedBy: Initiator)
      (data: NicknamedCounterparty)
      =
      Command.create
         parentAccountId.AsEntityId
         orgId
         (CorrelationId.create ())
         initiatedBy
         data

   let toEvent
      (cmd: NicknameCounterpartyCommand)
      : ValidationResult<BankEvent<NicknamedCounterparty>>
      =
      BankEvent.create<NicknamedCounterparty> cmd |> Ok

type DomesticTransferInput = {
   Amount: decimal
   Originator: DomesticTransferOriginatorReference
   Counterparty: Counterparty
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
         data.Originator.ParentAccountId.AsEntityId
         data.Originator.OrgId
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
                     TransferId = TransferId cmd.CorrelationId.Value
                     InitiatedBy = cmd.InitiatedBy
                     ScheduledDate = scheduledDate
                     Amount = input.Amount
                     Originator = input.Originator
                     Counterparty = input.Counterparty
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
         data.TransferInput.Originator.ParentAccountId.AsEntityId
         data.TransferInput.Originator.OrgId
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
                     TransferId = TransferId cmd.CorrelationId.Value
                     InitiatedBy = cmd.InitiatedBy
                     ScheduledDate = scheduledDate
                     Amount = input.Amount
                     Originator = input.Originator
                     Counterparty = input.Counterparty
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
         data.BaseInfo.Originator.ParentAccountId.AsEntityId
         data.BaseInfo.Originator.OrgId
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
         data.BaseInfo.Originator.ParentAccountId.AsEntityId
         data.BaseInfo.Originator.OrgId
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
         data.BaseInfo.Originator.ParentAccountId.AsEntityId
         data.BaseInfo.Originator.OrgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: UpdateDomesticTransferProgressCommand)
      : ValidationResult<BankEvent<DomesticTransferProgressUpdated>>
      =
      BankEvent.create<DomesticTransferProgressUpdated> cmd |> Ok

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
         parentAccountId.AsEntityId
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
         parentAccountId.AsEntityId
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
         sender.ParentAccountId.AsEntityId
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
                     TransferId = TransferId cmd.CorrelationId.Value
                     InitiatedBy = cmd.InitiatedBy
                     Recipient = t.Recipient
                     Amount = t.Amount.Value
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
         recipient.ParentAccountId.AsEntityId
         recipient.OrgId
         correlationId
         initiatedBy
         data

   let toEvent
      (cmd: DepositInternalAutoTransferCommand)
      : ValidationResult<BankEvent<InternalAutomatedTransferDeposited>>
      =
      BankEvent.create<InternalAutomatedTransferDeposited> cmd |> Ok
