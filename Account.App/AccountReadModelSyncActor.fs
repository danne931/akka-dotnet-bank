[<RequireQualifiedAccess>]
module AccountReadModelSyncActor

open System

open Lib.SharedTypes
open Lib.Types
open Lib.Postgres
open Bank.Account.Domain
open Bank.Transfer.Domain
open Bank.Payment.Domain
open AccountSqlMapper
open PaymentSqlMapper
open TransferSqlMapper
open Lib.ReadModelSyncActor

module aeSqlMapper = AccountEventSqlMapper
module aeSqlWriter = AccountEventSqlMapper.SqlWriter
module aeFields = AccountEventSqlMapper.Fields

module paeSqlMapper = ParentAccountEventSqlMapper
module paeSqlWriter = ParentAccountEventSqlMapper.SqlWriter
module paeFields = ParentAccountEventSqlMapper.Fields

type private SqlParams = (string * SqlValue) list list

type SqlParamsDerivedFromAccountEvents = {
   AccountCreate: SqlParams
   AccountUpdate: SqlParams
   AccountEvent: SqlParams
   ParentAccountEvent: SqlParams
   Payment: SqlParams
   PaymentUpdate: SqlParams
   PlatformPayment: SqlParams
   Transfer: SqlParams
   InternalTransferWithinOrg: SqlParams
   InternalTransferBetweenOrgs: SqlParams
   DomesticTransfer: SqlParams
   DomesticTransferUpdate: SqlParams
   PartnerBankInitialized: SqlParams
   BillingCycle: SqlParams
   DomesticTransferRecipient: SqlParams
   DomesticTransferRecipientUpdate: SqlParams
}

let private paymentRequestBaseSqlParams (p: PaymentRequested) =
   let requestType =
      match p with
      | PaymentRequested.Platform _ -> "Platform"
      | PaymentRequested.ThirdParty _ -> "ThirdParty"

   let shared = p.SharedDetails

   [
      "paymentId", PaymentSqlWriter.paymentId shared.Id

      "payeeOrgId", PaymentSqlWriter.payeeOrgId shared.Payee.OrgId

      "payeeAccountId", PaymentSqlWriter.payeeAccountId shared.Payee.AccountId

      "payeeParentAccountId",
      PaymentSqlWriter.payeeParentAccountId shared.Payee.ParentAccountId

      "amount", PaymentSqlWriter.amount shared.Amount
      "requestType", Sql.string requestType
      "expiration", PaymentSqlWriter.expiration shared.Expiration
      "memo", PaymentSqlWriter.memo shared.Memo
   ]

let private internalTransferWithinOrgBaseSqlParams
   (o: BaseInternalTransferWithinOrgInfo)
   =
   [
      "transferId", TransferSqlWriter.transferId o.TransferId

      "initiatedById", TransferSqlWriter.initiatedById o.InitiatedBy.Id

      "senderOrgId", TransferSqlWriter.orgId o.Sender.OrgId

      "senderAccountId", TransferSqlWriter.accountId o.Sender.AccountId

      "recipientOrgId", TransferSqlWriter.orgId o.Recipient.OrgId

      "recipientAccountId", TransferSqlWriter.accountId o.Recipient.AccountId

      "scheduledAt", TransferSqlWriter.scheduledAt o.ScheduledDate

      "amount", TransferSqlWriter.amount o.Amount
   ]

let private internalTransferBetweenOrgsBaseSqlParams
   (o: BaseInternalTransferBetweenOrgsInfo)
   =
   [
      "transferId", TransferSqlWriter.transferId o.TransferId

      "initiatedById", TransferSqlWriter.initiatedById o.InitiatedBy.Id

      "senderOrgId", TransferSqlWriter.orgId o.Sender.OrgId

      "senderAccountId", TransferSqlWriter.accountId o.Sender.AccountId

      "recipientOrgId", TransferSqlWriter.orgId o.Recipient.OrgId

      "recipientAccountId", TransferSqlWriter.accountId o.Recipient.AccountId

      "scheduledAt", TransferSqlWriter.scheduledAt o.ScheduledDate

      "amount", TransferSqlWriter.amount o.Amount
   ]

let private domesticTransferBaseSqlParams (o: BaseDomesticTransferInfo) = [
   "transferId", TransferSqlWriter.transferId o.TransferId

   "initiatedById", TransferSqlWriter.initiatedById o.InitiatedBy.Id

   "senderOrgId", TransferSqlWriter.orgId o.Sender.OrgId

   "senderAccountId", TransferSqlWriter.accountId o.Sender.AccountId

   "scheduledAt", TransferSqlWriter.scheduledAt o.ScheduledDate

   "amount", TransferSqlWriter.amount o.Amount

   "recipientAccountId",
   TransferSqlWriter.accountId o.Recipient.RecipientAccountId

   "memo", TransferSqlWriter.memo o.Memo
]

module private AccountBalanceReducer =
   let reserveFunds accountId txnAmount acc =
      let qParams = [
         "accountId", AccountSqlWriter.accountId accountId
         "pendingDeductionsMoneyDelta", Sql.money txnAmount
         "pendingDeductionsCountDelta", Sql.int 1
      ]

      {
         acc with
            AccountUpdate = qParams :: acc.AccountUpdate
      }

   let releaseReservedFunds accountId txnAmount acc =
      let qParams = [
         "accountId", AccountSqlWriter.accountId accountId
         "pendingDeductionsMoneyDelta", Sql.money -txnAmount
         "pendingDeductionsCountDelta", Sql.int -1
      ]

      {
         acc with
            AccountUpdate = qParams :: acc.AccountUpdate
      }

   let settleFunds accountId txnAmount acc =
      let qParams = [
         "accountId", AccountSqlWriter.accountId accountId
         "balanceDelta", Sql.money -txnAmount
         "pendingDeductionsMoneyDelta", Sql.money -txnAmount
         "pendingDeductionsCountDelta", Sql.int -1
      ]

      {
         acc with
            AccountUpdate = qParams :: acc.AccountUpdate
      }

   let depositFunds accountId txnAmount acc =
      let qParams = [
         "accountId", AccountSqlWriter.accountId accountId
         "balanceDelta", Sql.money txnAmount
      ]

      {
         acc with
            AccountUpdate = qParams :: acc.AccountUpdate
      }

let private internalTransferWithinOrgStatusReducer
   (acc: SqlParamsDerivedFromAccountEvents)
   (status: InternalTransferWithinOrgStatus)
   (info: BaseInternalTransferWithinOrgInfo)
   (isAutomated: bool)
   =
   let qParams =
      internalTransferWithinOrgBaseSqlParams info
      @ [
         "status", TransferSqlWriter.InternalWithinOrg.status status
         "statusDetail", TransferSqlWriter.InternalWithinOrg.statusDetail status
         "isAutomated",
         TransferSqlWriter.InternalWithinOrg.isAutomated isAutomated
      ]

   {
      acc with
         InternalTransferWithinOrg = qParams :: acc.InternalTransferWithinOrg
   }

let private internalTransferBetweenOrgsStatusReducer
   (acc: SqlParamsDerivedFromAccountEvents)
   (status: InternalTransferBetweenOrgsStatus)
   (info: BaseInternalTransferBetweenOrgsInfo)
   =
   let qParams =
      internalTransferBetweenOrgsBaseSqlParams info
      @ [
         "status", TransferSqlWriter.InternalBetweenOrgs.status status
         "statusDetail",
         TransferSqlWriter.InternalBetweenOrgs.statusDetail status
      ]

   {
      acc with
         InternalTransferBetweenOrgs =
            qParams :: acc.InternalTransferBetweenOrgs
   }

let private domesticRecipientReducer
   (acc: SqlParamsDerivedFromAccountEvents)
   (recipient: DomesticTransferRecipient)
   =
   let qParams = [
      "recipientAccountId",
      TransferSqlWriter.DomesticRecipient.recipientAccountId
         recipient.RecipientAccountId
      "senderOrgId",
      TransferSqlWriter.DomesticRecipient.senderOrgId recipient.SenderOrgId
      "lastName",
      TransferSqlWriter.DomesticRecipient.lastName recipient.LastName
      "firstName",
      TransferSqlWriter.DomesticRecipient.firstName recipient.FirstName
      "accountNumber",
      TransferSqlWriter.DomesticRecipient.accountNumber recipient.AccountNumber
      "routingNumber",
      TransferSqlWriter.DomesticRecipient.routingNumber recipient.RoutingNumber
      "status", TransferSqlWriter.DomesticRecipient.status recipient.Status
      "depository",
      TransferSqlWriter.DomesticRecipient.depository recipient.Depository
      "paymentNetwork",
      TransferSqlWriter.DomesticRecipient.paymentNetwork
         recipient.PaymentNetwork
      "nickname",
      TransferSqlWriter.DomesticRecipient.nickname recipient.Nickname
   ]

   {
      acc with
         DomesticTransferRecipient = qParams :: acc.DomesticTransferRecipient
   }

let private domesticTransferStatusReducer
   (acc: SqlParamsDerivedFromAccountEvents)
   (status: DomesticTransferProgress)
   (transferId: TransferId)
   =
   let qParams = [
      "transferId", TransferSqlWriter.transferId transferId
      "status", TransferSqlWriter.Domestic.status status
      "statusDetail", TransferSqlWriter.Domestic.statusDetail status
   ]

   {
      acc with
         DomesticTransferUpdate = qParams :: acc.DomesticTransferUpdate
   }

let private accountEventReducer
   (acc: SqlParamsDerivedFromAccountEvents)
   (evt: AccountEvent)
   (envelope: Envelope)
   =
   let sqlParams = [
      "eventId", aeSqlWriter.eventId envelope.Id

      "accountId", aeSqlWriter.accountId evt.AccountId

      "orgId", aeSqlWriter.orgId envelope.OrgId

      "parentAccountId",
      envelope.EntityId
      |> ParentAccountId.fromEntityId
      |> aeSqlWriter.parentAccountId

      "correlationId", aeSqlWriter.correlationId envelope.CorrelationId

      "initiatedById", aeSqlWriter.initiatedById envelope.InitiatedBy.Id

      "name", aeSqlWriter.name envelope.EventName
      "timestamp", aeSqlWriter.timestamp envelope.Timestamp
      "event", aeSqlWriter.event evt
   ]

   let amountOpt, moneyFlowOpt, sourceOpt = AccountEvent.moneyTransaction evt

   let sqlParams =
      sqlParams
      @ [
         "amount", aeSqlWriter.amount amountOpt
         "moneyFlow", aeSqlWriter.moneyFlow moneyFlowOpt
         "source", aeSqlWriter.source sourceOpt

         "cardId",
         aeSqlWriter.cardId (
            match evt with
            | AccountEvent.DebitRefunded e ->
               Some e.Data.EmployeePurchaseReference.CardId
            | AccountEvent.DebitSettled e ->
               Some e.Data.EmployeePurchaseReference.CardId
            | AccountEvent.DebitFailed e ->
               Some e.Data.EmployeePurchaseReference.CardId
            | _ -> None
         )
      ]

   {
      acc with
         AccountEvent = sqlParams :: acc.AccountEvent
   }

let private parentAccountEventReducer
   (acc: SqlParamsDerivedFromAccountEvents)
   (evt: ParentAccountEvent)
   (envelope: Envelope)
   =
   let sqlParams = [
      "eventId", paeSqlWriter.eventId envelope.Id

      "orgId", paeSqlWriter.orgId envelope.OrgId

      "parentAccountId",
      envelope.EntityId
      |> ParentAccountId.fromEntityId
      |> paeSqlWriter.parentAccountId

      "correlationId", paeSqlWriter.correlationId envelope.CorrelationId

      "initiatedById", paeSqlWriter.initiatedById envelope.InitiatedBy.Id

      "name", paeSqlWriter.name envelope.EventName
      "timestamp", paeSqlWriter.timestamp envelope.Timestamp
      "event", paeSqlWriter.event evt
   ]

   {
      acc with
         ParentAccountEvent = sqlParams :: acc.ParentAccountEvent
   }

let sqlParamReducer
   (acc: SqlParamsDerivedFromAccountEvents)
   (evt: AccountEvent)
   : SqlParamsDerivedFromAccountEvents
   =
   let evt, envelope = AccountEnvelope.unwrap evt
   let accountId = evt.AccountId

   let acc =
      match evt with
      | AccountEvent.ParentAccount evt ->
         parentAccountEventReducer acc evt envelope
      | _ -> accountEventReducer acc evt envelope

   match evt with
   | AccountEvent.ParentAccount(ParentAccountEvent.RegisteredDomesticTransferRecipient e) ->
      domesticRecipientReducer acc e.Data.Recipient
   | AccountEvent.ParentAccount(ParentAccountEvent.EditedDomesticTransferRecipient e) ->
      domesticRecipientReducer acc e.Data.Recipient
   | AccountEvent.ParentAccount(ParentAccountEvent.NicknamedDomesticTransferRecipient e) ->
      let qParams = [
         "recipientAccountId",
         TransferSqlWriter.DomesticRecipient.recipientAccountId
            e.Data.RecipientId
         "nickname",
         TransferSqlWriter.DomesticRecipient.nickname e.Data.Nickname
      ]

      {
         acc with
            DomesticTransferRecipientUpdate =
               qParams :: acc.DomesticTransferRecipientUpdate
      }
   | AccountEvent.InitializedPrimaryCheckingAccount e ->
      let parentAccountQueryParams = [
         "orgId", PartnerBankSqlMapper.SqlWriter.orgId e.OrgId
         "parentAccountId",
         PartnerBankSqlMapper.SqlWriter.parentAccountId e.Data.ParentAccountId
         "routingNumber",
         PartnerBankSqlMapper.SqlWriter.routingNumber
            e.Data.PartnerBankRoutingNumber
         "accountNumber",
         PartnerBankSqlMapper.SqlWriter.accountNumber
            e.Data.PartnerBankAccountNumber
         "status",
         PartnerBankSqlMapper.SqlWriter.status ParentAccountStatus.Active
      ]

      let accountQueryParams = [
         "accountId",
         AccountSqlWriter.accountId e.Data.PrimaryChecking.AccountId
         "accountNumber",
         AccountSqlWriter.accountNumber e.Data.PrimaryChecking.AccountNumber
         "routingNumber",
         AccountSqlWriter.routingNumber e.Data.PrimaryChecking.RoutingNumber
         "orgId", AccountSqlWriter.orgId e.OrgId
         "parentAccountId",
         AccountSqlWriter.parentAccountId (
            ParentAccountId.fromEntityId e.EntityId
         )
         "name", AccountSqlWriter.name e.Data.PrimaryChecking.Name
         "depository", AccountSqlWriter.depository AccountDepository.Checking
         "currency", AccountSqlWriter.currency Currency.USD
         "balance", AccountSqlWriter.balance 0m
         "pendingDeductionsMoney",
         AccountSqlWriter.pendingDeductionsMoney PendingDeductions.Zero
         "pendingDeductionsCount",
         AccountSqlWriter.pendingDeductionsCount PendingDeductions.Zero
         "status", AccountSqlWriter.status AccountStatus.Active
         "autoTransferRule", AccountSqlWriter.autoTransferRule None
         "autoTransferRuleFrequency",
         AccountSqlWriter.autoTransferRuleFrequency None
      ]

      {
         acc with
            PartnerBankInitialized =
               parentAccountQueryParams :: acc.PartnerBankInitialized
            AccountCreate = accountQueryParams :: acc.AccountCreate
      }
   | AccountEvent.CreatedVirtualAccount e ->
      let qParams = [
         "accountId", AccountSqlWriter.accountId e.Data.AccountId
         "accountNumber", AccountSqlWriter.accountNumber e.Data.AccountNumber
         "routingNumber", AccountSqlWriter.routingNumber e.Data.RoutingNumber
         "orgId", AccountSqlWriter.orgId e.OrgId
         "parentAccountId",
         AccountSqlWriter.parentAccountId (
            ParentAccountId.fromEntityId e.EntityId
         )
         "name", AccountSqlWriter.name e.Data.Name
         "depository", AccountSqlWriter.depository e.Data.Depository
         "currency", AccountSqlWriter.currency e.Data.Currency
         "balance", AccountSqlWriter.balance 0m
         "pendingDeductionsMoney",
         AccountSqlWriter.pendingDeductionsMoney PendingDeductions.Zero
         "pendingDeductionsCount",
         AccountSqlWriter.pendingDeductionsCount PendingDeductions.Zero
         "status", AccountSqlWriter.status AccountStatus.Active
         "autoTransferRule", AccountSqlWriter.autoTransferRule None
         "autoTransferRuleFrequency",
         AccountSqlWriter.autoTransferRuleFrequency None
      ]

      {
         acc with
            AccountCreate = qParams :: acc.AccountCreate
      }
   | AccountEvent.AccountClosed _ ->
      let qParams = [
         "accountId", AccountSqlWriter.accountId evt.AccountId
         "status", AccountSqlWriter.status AccountStatus.Closed
      ]

      {
         acc with
            AccountUpdate = qParams :: acc.AccountUpdate
      }
   | AccountEvent.DepositedCash e ->
      AccountBalanceReducer.depositFunds accountId e.Data.Amount acc
   // DebitPending is Deferred in the actor rather than Persisted so
   // will not see this event consumed by the read journal.
   | AccountEvent.DebitPending _ -> acc
   | AccountEvent.DebitSettled e ->
      let qParams = [
         "accountId", AccountSqlWriter.accountId accountId
         "balanceDelta", Sql.money -e.Data.Amount
      ]

      {
         acc with
            AccountUpdate = qParams :: acc.AccountUpdate
      }
   | AccountEvent.DebitFailed e ->
      AccountBalanceReducer.releaseReservedFunds accountId e.Data.Amount acc
   | AccountEvent.DebitRefunded e ->
      AccountBalanceReducer.depositFunds accountId e.Data.Amount acc
   | AccountEvent.InternalTransferWithinOrgDeducted e ->
      let info = e.Data.BaseInfo

      let transferParams =
         internalTransferWithinOrgBaseSqlParams info
         @ [
            "memo", TransferSqlWriter.memo None
            "transferCategory",
            TransferSqlWriter.transferCategory
               TransferCategory.InternalWithinOrg
         ]

      let acc =
         internalTransferWithinOrgStatusReducer
            {
               acc with
                  Transfer = transferParams :: acc.Transfer
            }
            InternalTransferWithinOrgStatus.Pending
            e.Data.BaseInfo
            false

      let accountParams = [
         "accountId", AccountSqlWriter.accountId accountId
         "balanceDelta", Sql.money -info.Amount
      ]

      {
         acc with
            AccountUpdate = accountParams :: acc.AccountUpdate
      }
   | AccountEvent.InternalTransferWithinOrgDeposited e ->
      let acc =
         internalTransferWithinOrgStatusReducer
            acc
            InternalTransferWithinOrgStatus.Settled
            e.Data.BaseInfo
            false

      let accountParams = [
         "accountId", AccountSqlWriter.accountId accountId
         "balanceDelta", Sql.money e.Data.BaseInfo.Amount
      ]

      {
         acc with
            AccountUpdate = accountParams :: acc.AccountUpdate
      }
   | AccountEvent.InternalAutomatedTransferDeducted e ->
      let info = e.Data.BaseInfo

      let transferParams =
         internalTransferWithinOrgBaseSqlParams info
         @ [
            "memo", TransferSqlWriter.memo None
            "transferCategory",
            TransferSqlWriter.transferCategory
               TransferCategory.InternalAutomatedWithinOrg
         ]

      let acc =
         internalTransferWithinOrgStatusReducer
            {
               acc with
                  Transfer = transferParams :: acc.Transfer
            }
            InternalTransferWithinOrgStatus.Pending
            e.Data.BaseInfo
            true

      let accountParams = [
         "accountId", AccountSqlWriter.accountId accountId
         "balanceDelta", Sql.money -info.Amount
      ]

      {
         acc with
            AccountUpdate = accountParams :: acc.AccountUpdate
      }
   | AccountEvent.InternalAutomatedTransferDeposited e ->
      let acc =
         internalTransferWithinOrgStatusReducer
            acc
            InternalTransferWithinOrgStatus.Settled
            e.Data.BaseInfo
            true

      let accountParams = [
         "accountId", AccountSqlWriter.accountId accountId
         "balanceDelta", Sql.money e.Data.BaseInfo.Amount
      ]

      {
         acc with
            AccountUpdate = accountParams :: acc.AccountUpdate
      }
   | AccountEvent.InternalTransferBetweenOrgsScheduled e ->
      let info = e.Data.BaseInfo

      let transferParams =
         internalTransferBetweenOrgsBaseSqlParams info
         @ [
            "memo", TransferSqlWriter.memo info.Memo
            "transferCategory",
            TransferSqlWriter.transferCategory
               TransferCategory.InternalBetweenOrgs
         ]


      internalTransferBetweenOrgsStatusReducer
         {
            acc with
               Transfer = transferParams :: acc.Transfer
         }
         InternalTransferBetweenOrgsStatus.Scheduled
         info
   | AccountEvent.InternalTransferBetweenOrgsPending e ->
      let info = e.Data.BaseInfo

      let transferParams =
         internalTransferBetweenOrgsBaseSqlParams info
         @ [
            "memo", TransferSqlWriter.memo info.Memo
            "transferCategory",
            TransferSqlWriter.transferCategory
               TransferCategory.InternalBetweenOrgs
         ]

      internalTransferBetweenOrgsStatusReducer
         {
            acc with
               Transfer = transferParams :: acc.Transfer
         }
         InternalTransferBetweenOrgsStatus.Pending
         info
      |> AccountBalanceReducer.reserveFunds accountId info.Amount
   | AccountEvent.InternalTransferBetweenOrgsSettled e ->
      let info = e.Data.BaseInfo

      let acc =
         internalTransferBetweenOrgsStatusReducer
            acc
            InternalTransferBetweenOrgsStatus.Settled
            info
         |> AccountBalanceReducer.settleFunds accountId info.Amount

      match info.FromPaymentRequest with
      | Some paymentId ->
         let status =
            PaymentRequestStatus.Fulfilled {
               TransferId = info.TransferId
               FulfilledAt = e.Timestamp
            }

         let pParams = [
            "paymentId", PaymentSqlWriter.paymentId paymentId
            "status", PaymentSqlWriter.status status
            "statusDetail", PaymentSqlWriter.statusDetail status
            "fulfilledByTransferId",
            PaymentSqlWriter.fulfilledByTransferId status
            "fulfilledAt", PaymentSqlWriter.fulfilledAt status
         ]

         {
            acc with
               PaymentUpdate = pParams :: acc.PaymentUpdate
         }
      | None -> acc
   | AccountEvent.InternalTransferBetweenOrgsFailed e ->
      let info = e.Data.BaseInfo

      internalTransferBetweenOrgsStatusReducer
         acc
         (InternalTransferBetweenOrgsStatus.Failed e.Data.Reason)
         info
      |> AccountBalanceReducer.releaseReservedFunds accountId info.Amount
   | AccountEvent.InternalTransferBetweenOrgsDeposited e ->
      let info = e.Data.BaseInfo

      internalTransferBetweenOrgsStatusReducer
         acc
         InternalTransferBetweenOrgsStatus.Deposited
         info
      |> AccountBalanceReducer.depositFunds accountId info.Amount
   | AccountEvent.DomesticTransferScheduled e ->
      let info = e.Data.BaseInfo
      let status = DomesticTransferProgress.Scheduled

      let transferParams =
         domesticTransferBaseSqlParams info
         @ [
            "transferCategory",
            TransferSqlWriter.transferCategory TransferCategory.Domestic
         ]

      let domesticTransferParams =
         domesticTransferBaseSqlParams info
         @ [
            "status", TransferSqlWriter.Domestic.status status
            "statusDetail", TransferSqlWriter.Domestic.statusDetail status
            "expectedSettlementDate",
            TransferSqlWriter.Domestic.expectedSettlementDate
               e.Data.ExpectedSettlementDate
         ]

      {
         acc with
            Transfer = transferParams :: acc.Transfer
            DomesticTransfer = domesticTransferParams :: acc.DomesticTransfer
      }
   | AccountEvent.DomesticTransferPending e ->
      let info = e.Data.BaseInfo
      let status = DomesticTransferProgress.WaitingForTransferServiceAck

      let transferParams =
         domesticTransferBaseSqlParams info
         @ [
            "transferCategory",
            TransferSqlWriter.transferCategory TransferCategory.Domestic
         ]

      let domesticTransferParams =
         domesticTransferBaseSqlParams info
         @ [

            "status", TransferSqlWriter.Domestic.status status
            "statusDetail", TransferSqlWriter.Domestic.statusDetail status
            "expectedSettlementDate",
            TransferSqlWriter.Domestic.expectedSettlementDate
               e.Data.ExpectedSettlementDate
         ]

      {
         acc with
            Transfer = transferParams :: acc.Transfer
            DomesticTransfer = domesticTransferParams :: acc.DomesticTransfer
      }
      |> AccountBalanceReducer.reserveFunds accountId info.Amount
   | AccountEvent.DomesticTransferProgress e ->
      let status = DomesticTransferProgress.ThirdParty e.Data.InProgressInfo

      let qParams = [
         "transferId", TransferSqlWriter.transferId e.Data.BaseInfo.TransferId
         "status", TransferSqlWriter.Domestic.status status
         "statusDetail", TransferSqlWriter.Domestic.statusDetail status
      ]

      let qParams =
         match e.Data.NewExpectedSettlementDate with
         | Some date ->
            ("expectedSettlementDate",
             TransferSqlWriter.Domestic.expectedSettlementDate date)
            :: qParams
         | None -> qParams

      {
         acc with
            DomesticTransferUpdate = qParams :: acc.DomesticTransferUpdate
      }
   | AccountEvent.DomesticTransferFailed e ->
      let info = e.Data.BaseInfo

      let acc =
         domesticTransferStatusReducer
            acc
            (DomesticTransferProgress.Failed e.Data.Reason)
            info.TransferId
         |> AccountBalanceReducer.releaseReservedFunds accountId info.Amount

      let updatedRecipientStatus =
         match e.Data.Reason with
         | DomesticTransferFailReason.ThirdParty DomesticTransferThirdPartyFailReason.RecipientAccountNotActive ->
            Some RecipientRegistrationStatus.Closed
         | DomesticTransferFailReason.ThirdParty DomesticTransferThirdPartyFailReason.RecipientAccountInvalidInfo ->
            Some RecipientRegistrationStatus.InvalidAccount
         | _ -> None

      match updatedRecipientStatus with
      | Some status ->
         let qParams = [
            "recipientAccountId",
            TransferSqlWriter.DomesticRecipient.recipientAccountId
               info.Recipient.RecipientAccountId

            "status", TransferSqlWriter.DomesticRecipient.status status
         ]

         {
            acc with
               DomesticTransferRecipientUpdate =
                  qParams :: acc.DomesticTransferRecipientUpdate
         }
      | None -> acc
   | AccountEvent.DomesticTransferSettled e ->
      let info = e.Data.BaseInfo

      let acc =
         domesticTransferStatusReducer
            acc
            DomesticTransferProgress.Settled
            info.TransferId
         |> AccountBalanceReducer.settleFunds accountId info.Amount

      match e.Data.FromRetry with
      | Some(DomesticTransferFailReason.ThirdParty DomesticTransferThirdPartyFailReason.RecipientAccountInvalidInfo) ->
         let qParams = [
            "recipientAccountId",
            TransferSqlWriter.DomesticRecipient.recipientAccountId
               info.Recipient.RecipientAccountId

            "status",
            TransferSqlWriter.DomesticRecipient.status
               RecipientRegistrationStatus.Confirmed
         ]

         {
            acc with
               DomesticTransferRecipientUpdate =
                  qParams :: acc.DomesticTransferRecipientUpdate
         }
      | _ -> acc
   | AccountEvent.PaymentRequested e ->
      let shared = e.Data.SharedDetails
      let status = PaymentRequestStatus.Requested

      let payParams =
         [
            "initiatedById", PaymentSqlWriter.initiatedById e.InitiatedBy.Id
            "status", PaymentSqlWriter.status status
            "statusDetail", PaymentSqlWriter.statusDetail status
         ]
         @ paymentRequestBaseSqlParams e.Data

      let acc = {
         acc with
            Payment = payParams :: acc.Payment
      }

      match e.Data with
      | Platform info ->
         let payer = info.Payer

         let platformPayParams = [
            "paymentId", PaymentSqlWriter.paymentId shared.Id
            "payerOrgId", PaymentSqlWriter.Platform.payerOrgId payer.OrgId
            "payerParentAccountId",
            PaymentSqlWriter.Platform.payerParentAccountId payer.ParentAccountId
         ]

         {
            acc with
               PlatformPayment = platformPayParams :: acc.PlatformPayment
         }
      | ThirdParty info ->
         // TODO
         acc
   | AccountEvent.PaymentRequestCancelled e ->
      let status = PaymentRequestStatus.Cancelled

      let pParams = [
         "paymentId", PaymentSqlWriter.paymentId e.Data.SharedDetails.Id
         "status", PaymentSqlWriter.status status
         "statusDetail", PaymentSqlWriter.statusDetail status
      ]

      {
         acc with
            PaymentUpdate = pParams :: acc.PaymentUpdate
      }
   | AccountEvent.PaymentRequestDeclined e ->
      let status = PaymentRequestStatus.Declined

      let pParams = [
         "paymentId", PaymentSqlWriter.paymentId e.Data.SharedDetails.Id
         "status", PaymentSqlWriter.status status
         "statusDetail", PaymentSqlWriter.statusDetail status
      ]

      {
         acc with
            PaymentUpdate = pParams :: acc.PaymentUpdate
      }
   | AccountEvent.MaintenanceFeeDebited e ->
      let qParams = [
         "parentAccountId",
         ParentAccountId.fromEntityId e.EntityId
         |> PartnerBankSqlMapper.SqlWriter.parentAccountId

         "lastBillingCycleDate",
         Some e.Data.BillingDate
         |> PartnerBankSqlMapper.SqlWriter.lastBillingCycleDate
      ]

      let accountParams = [
         "accountId", AccountSqlWriter.accountId e.Data.AccountId
         "balanceDelta", Sql.money -e.Data.Amount
      ]

      {
         acc with
            BillingCycle = qParams :: acc.BillingCycle
            AccountUpdate = accountParams :: acc.AccountUpdate
      }
   | AccountEvent.MaintenanceFeeSkipped e ->
      let qParams = [
         "parentAccountId",
         ParentAccountId.fromEntityId e.EntityId
         |> PartnerBankSqlMapper.SqlWriter.parentAccountId

         "lastBillingCycleDate",
         Some e.Data.BillingDate
         |> PartnerBankSqlMapper.SqlWriter.lastBillingCycleDate
      ]

      {
         acc with
            BillingCycle = qParams :: acc.BillingCycle
      }

   | AccountEvent.AutoTransferRuleConfigured e ->
      let qParams = [
         "accountId", AccountSqlWriter.accountId e.Data.AccountId
         "autoTransferRule",
         AccountSqlWriter.autoTransferRule (Some e.Data.Config)
         "autoTransferRuleFrequency",
         e.Data.Config.Info
         |> AutomaticTransfer.frequencyFromAutoTransferRule
         |> Some
         |> AccountSqlWriter.autoTransferRuleFrequency
      ]

      {
         acc with
            AccountUpdate = qParams :: acc.AccountUpdate
      }

   | AccountEvent.AutoTransferRuleDeleted e ->
      let qParams = [
         "accountId", AccountSqlWriter.accountId e.Data.AccountId
         "autoTransferRule", AccountSqlWriter.autoTransferRule None
         "autoTransferRuleFrequency",
         AccountSqlWriter.autoTransferRuleFrequency None
      ]

      {
         acc with
            AccountUpdate = qParams :: acc.AccountUpdate
      }

let upsertReadModels (accountEvents: AccountEvent list) =
   let sqlParamsDerivedFromAccountEvents =
      accountEvents
      |> List.sortByDescending (AccountEnvelope.unwrap >> snd >> _.Timestamp)
      |> List.fold sqlParamReducer {
         AccountCreate = []
         AccountUpdate = []
         AccountEvent = []
         ParentAccountEvent = []
         PartnerBankInitialized = []
         BillingCycle = []
         Payment = []
         PaymentUpdate = []
         PlatformPayment = []
         Transfer = []
         InternalTransferWithinOrg = []
         InternalTransferBetweenOrgs = []
         DomesticTransfer = []
         DomesticTransferUpdate = []
         DomesticTransferRecipient = []
         DomesticTransferRecipientUpdate = []
      }

   pgTransaction [
      $"""
      INSERT into {PartnerBankSqlMapper.table}
         ({PartnerBankSqlMapper.Fields.parentAccountId},
          {PartnerBankSqlMapper.Fields.accountNumber},
          {PartnerBankSqlMapper.Fields.routingNumber},
          {PartnerBankSqlMapper.Fields.status},
          {PartnerBankSqlMapper.Fields.orgId})
      VALUES
         (@parentAccountId,
          @accountNumber,
          @routingNumber,
          @status::{PartnerBankSqlMapper.TypeCast.status},
          @orgId)
      ON CONFLICT ({PartnerBankSqlMapper.Fields.parentAccountId})
      DO NOTHING;
      """,
      sqlParamsDerivedFromAccountEvents.PartnerBankInitialized

      $"""
      UPDATE {PartnerBankSqlMapper.table}
      SET {PartnerBankSqlMapper.Fields.lastBillingCycleDate} = @lastBillingCycleDate
      WHERE {PartnerBankSqlMapper.Fields.parentAccountId} = @parentAccountId;
      """,
      sqlParamsDerivedFromAccountEvents.BillingCycle

      $"""
      INSERT into {AccountSqlMapper.table}
         ({AccountFields.accountId},
          {AccountFields.accountNumber},
          {AccountFields.routingNumber},
          {AccountFields.orgId},
          {AccountFields.parentAccountId},
          {AccountFields.name},
          {AccountFields.depository},
          {AccountFields.balance},
          {AccountFields.pendingDeductionsMoney},
          {AccountFields.pendingDeductionsCount},
          {AccountFields.currency},
          {AccountFields.status},
          {AccountFields.autoTransferRule},
          {AccountFields.autoTransferRuleFrequency})
      VALUES
         (@accountId,
          @accountNumber,
          @routingNumber,
          @orgId,
          @parentAccountId,
          @name,
          @depository::{AccountTypeCast.depository},
          @balance,
          @pendingDeductionsMoney,
          @pendingDeductionsCount,
          @currency,
          @status::{AccountTypeCast.status},
          @autoTransferRule,
          @autoTransferRuleFrequency::{AccountTypeCast.autoTransferRuleFrequency})
      ON CONFLICT ({AccountFields.accountId})
      DO NOTHING;
      """,
      sqlParamsDerivedFromAccountEvents.AccountCreate

      $"""
      UPDATE {AccountSqlMapper.table}
      SET
         {AccountFields.balance} = {AccountFields.balance} + COALESCE(@balanceDelta, 0::money),
         {AccountFields.pendingDeductionsMoney} =
            {AccountFields.pendingDeductionsMoney} + COALESCE(@pendingDeductionsMoneyDelta, 0::money),
         {AccountFields.pendingDeductionsCount} =
            {AccountFields.pendingDeductionsCount} + COALESCE(@pendingDeductionsCountDelta, 0::int),
         {AccountFields.status} = COALESCE(@status::{AccountTypeCast.status}, {AccountFields.status}),
         {AccountFields.autoTransferRule} = COALESCE(@autoTransferRule, {AccountFields.autoTransferRule}),
         {AccountFields.autoTransferRuleFrequency} =
            COALESCE(
               @autoTransferRuleFrequency::{AccountTypeCast.autoTransferRuleFrequency},
               {AccountFields.autoTransferRuleFrequency}
            )
      WHERE {AccountFields.accountId} = @accountId;
      """,
      sqlParamsDerivedFromAccountEvents.AccountUpdate
      |> List.map (
         Lib.Postgres.addCoalescableParamsForUpdate [
            "balanceDelta"
            "pendingDeductionsMoneyDelta"
            "pendingDeductionsCountDelta"
            "status"
            "autoTransferRule"
            "autoTransferRuleFrequency"
         ]
      )

      $"""
      INSERT into {aeSqlMapper.table}
         ({aeFields.eventId},
          {aeFields.accountId},
          {aeFields.orgId},
          {aeFields.parentAccountId},
          {aeFields.correlationId},
          {aeFields.initiatedById},
          {aeFields.cardId},
          {aeFields.name},
          {aeFields.timestamp},
          {aeFields.event},
          {aeFields.amount},
          {aeFields.source},
          {aeFields.moneyFlow})
      VALUES
         (@eventId,
          @accountId,
          @orgId,
          @parentAccountId,
          @correlationId,
          @initiatedById,
          @cardId,
          @name,
          @timestamp,
          @event,
          @amount,
          @source,
          @moneyFlow::{aeSqlMapper.TypeCast.moneyFlow})
      ON CONFLICT ({aeFields.eventId})
      DO NOTHING;
      """,
      sqlParamsDerivedFromAccountEvents.AccountEvent

      $"""
      INSERT into {paeSqlMapper.table}
         ({aeFields.eventId},
          {aeFields.orgId},
          {aeFields.parentAccountId},
          {aeFields.correlationId},
          {aeFields.initiatedById},
          {aeFields.name},
          {aeFields.timestamp},
          {aeFields.event})
      VALUES
         (@eventId,
          @orgId,
          @parentAccountId,
          @correlationId,
          @initiatedById,
          @name,
          @timestamp,
          @event)
      ON CONFLICT ({paeFields.eventId})
      DO NOTHING;
      """,
      sqlParamsDerivedFromAccountEvents.ParentAccountEvent

      let paymentTable = PaymentSqlMapper.Table.payment

      $"""
      INSERT into {paymentTable}
         ({PaymentFields.paymentId},
          {PaymentFields.initiatedById},
          {PaymentFields.amount},
          {PaymentFields.status},
          {PaymentFields.statusDetail},
          {PaymentFields.memo},
          {PaymentFields.requestType},
          {PaymentFields.expiration},
          {PaymentFields.payeeOrgId},
          {PaymentFields.payeeAccountId},
          {PaymentFields.payeeParentAccountId})
      VALUES
         (@paymentId,
          @initiatedById,
          @amount,
          @status::{PaymentTypeCast.status},
          @statusDetail,
          @memo,
          @requestType::{PaymentTypeCast.requestType},
          @expiration,
          @payeeOrgId,
          @payeeAccountId,
          @payeeParentAccountId)
      ON CONFLICT ({PaymentFields.paymentId})
      DO NOTHING;
      """,
      sqlParamsDerivedFromAccountEvents.Payment

      $"""
      INSERT into {PaymentSqlMapper.Table.platformPayment}
         ({PaymentFields.paymentId},
          {PaymentFields.Platform.payerOrgId},
          {PaymentFields.Platform.payerParentAccountId})
      VALUES
         (@paymentId,
          @payerOrgId,
          @payerParentAccountId)
      ON CONFLICT ({PaymentFields.paymentId})
      DO NOTHING;
      """,
      sqlParamsDerivedFromAccountEvents.PlatformPayment

      $"""
      INSERT into {TransferSqlMapper.Table.transfer}
         ({TransferFields.transferId},
          {TransferFields.initiatedById},
          {TransferFields.amount},
          {TransferFields.transferCategory},
          {TransferFields.scheduledAt},
          {TransferFields.senderOrgId},
          {TransferFields.senderAccountId},
          {TransferFields.memo})
      VALUES
         (@transferId,
          @initiatedById,
          @amount,
          @transferCategory::{TransferTypeCast.transferCategory},
          @scheduledAt,
          @senderOrgId,
          @senderAccountId,
          @memo)
      ON CONFLICT ({TransferFields.transferId})
      DO NOTHING;
      """,
      sqlParamsDerivedFromAccountEvents.Transfer

      $"""
      INSERT into {TransferSqlMapper.Table.internalTransferWithinOrg}
         ({TransferFields.transferId},
          {TransferFields.InternalWithinOrg.status},
          {TransferFields.InternalWithinOrg.statusDetail},
          {TransferFields.InternalWithinOrg.isAutomated},
          {TransferFields.InternalWithinOrg.recipientOrgId},
          {TransferFields.InternalWithinOrg.recipientAccountId})
      VALUES
         (@transferId,
          @status::{TransferTypeCast.internalTransferWithinOrgStatus},
          @statusDetail,
          @isAutomated,
          @recipientOrgId,
          @recipientAccountId)
      ON CONFLICT ({TransferFields.transferId})
      DO UPDATE SET
         {TransferFields.InternalWithinOrg.status} = @status::{TransferTypeCast.internalTransferWithinOrgStatus},
         {TransferFields.InternalWithinOrg.statusDetail} = @statusDetail;
      """,
      sqlParamsDerivedFromAccountEvents.InternalTransferWithinOrg

      $"""
      INSERT into {TransferSqlMapper.Table.internalTransferBetweenOrgs}
         ({TransferFields.transferId},
          {TransferFields.InternalBetweenOrgs.status},
          {TransferFields.InternalBetweenOrgs.statusDetail},
          {TransferFields.InternalBetweenOrgs.recipientOrgId},
          {TransferFields.InternalBetweenOrgs.recipientAccountId})
      VALUES
         (@transferId,
          @status::{TransferTypeCast.internalTransferBetweenOrgsStatus},
          @statusDetail,
          @recipientOrgId,
          @recipientAccountId)
      ON CONFLICT ({TransferFields.transferId})
      DO UPDATE SET
         {TransferFields.InternalBetweenOrgs.status} = @status::{TransferTypeCast.internalTransferBetweenOrgsStatus},
         {TransferFields.InternalBetweenOrgs.statusDetail} = @statusDetail;
      """,
      sqlParamsDerivedFromAccountEvents.InternalTransferBetweenOrgs

      $"""
      INSERT into {TransferSqlMapper.Table.domesticRecipient}
         ({TransferFields.DomesticRecipient.recipientAccountId},
          {TransferFields.DomesticRecipient.senderOrgId},
          {TransferFields.DomesticRecipient.firstName},
          {TransferFields.DomesticRecipient.lastName},
          {TransferFields.DomesticRecipient.nickname},
          {TransferFields.DomesticRecipient.routingNumber},
          {TransferFields.DomesticRecipient.accountNumber},
          {TransferFields.DomesticRecipient.status},
          {TransferFields.DomesticRecipient.depository},
          {TransferFields.DomesticRecipient.paymentNetwork})
      VALUES
         (@recipientAccountId,
          @senderOrgId,
          @firstName,
          @lastName,
          @nickname,
          @routingNumber,
          @accountNumber,
          @status::{TransferTypeCast.domesticRecipientStatus},
          @depository::{TransferTypeCast.domesticRecipientAccountDepository},
          @paymentNetwork::{TransferTypeCast.paymentNetwork})
      ON CONFLICT ({TransferFields.DomesticRecipient.recipientAccountId})
      DO UPDATE SET
         {TransferFields.DomesticRecipient.firstName} = @firstName,
         {TransferFields.DomesticRecipient.lastName} = @lastName,
         {TransferFields.DomesticRecipient.nickname} = @nickname,
         {TransferFields.DomesticRecipient.routingNumber} = @routingNumber,
         {TransferFields.DomesticRecipient.accountNumber} = @accountNumber,
         {TransferFields.DomesticRecipient.status} =
            @status::{TransferTypeCast.domesticRecipientStatus},
         {TransferFields.DomesticRecipient.depository} =
            @depository::{TransferTypeCast.domesticRecipientAccountDepository},
         {TransferFields.DomesticRecipient.paymentNetwork} =
            @paymentNetwork::{TransferTypeCast.paymentNetwork};
      """,
      sqlParamsDerivedFromAccountEvents.DomesticTransferRecipient

      $"""
      UPDATE {TransferSqlMapper.Table.domesticRecipient}
      SET 
         {TransferFields.DomesticRecipient.nickname} =
            COALESCE(@nickname, {TransferFields.DomesticRecipient.nickname}),
         {TransferFields.DomesticRecipient.status} =
            COALESCE(
               @status::{TransferTypeCast.domesticRecipientStatus},
               {TransferFields.DomesticRecipient.status}
            )
      WHERE {TransferFields.DomesticRecipient.recipientAccountId} = @recipientAccountId;
      """,
      sqlParamsDerivedFromAccountEvents.DomesticTransferRecipientUpdate
      |> List.map (
         Lib.Postgres.addCoalescableParamsForUpdate [ "nickname"; "status" ]
      )

      $"""
      INSERT into {TransferSqlMapper.Table.domesticTransfer}
         ({TransferFields.transferId},
          {TransferFields.Domestic.expectedSettlementDate},
          {TransferFields.Domestic.status},
          {TransferFields.Domestic.statusDetail},
          {TransferFields.Domestic.recipientAccountId})
      VALUES
         (@transferId,
          @expectedSettlementDate,
          @status::{TransferTypeCast.domesticTransferStatus},
          @statusDetail,
          @recipientAccountId)
      ON CONFLICT ({TransferFields.transferId})
      DO UPDATE SET
         {TransferFields.Domestic.status} = @status::{TransferTypeCast.domesticTransferStatus},
         {TransferFields.Domestic.statusDetail} = @statusDetail;
      """,
      sqlParamsDerivedFromAccountEvents.DomesticTransfer

      $"""
      UPDATE {TransferSqlMapper.Table.domesticTransfer}
      SET
         {TransferFields.Domestic.expectedSettlementDate} = COALESCE(@expectedSettlementDate, {TransferFields.Domestic.expectedSettlementDate}),
         {TransferFields.Domestic.status} = @status::{TransferTypeCast.domesticTransferStatus},
         {TransferFields.Domestic.statusDetail} = @statusDetail
      WHERE {TransferFields.transferId} = @transferId;
      """,
      sqlParamsDerivedFromAccountEvents.DomesticTransferUpdate
      |> List.map (
         Lib.Postgres.addCoalescableParamsForUpdate [ "expectedSettlementDate" ]
      )

      $"""
      UPDATE {paymentTable}
      SET
         {PaymentFields.status} = @status::{PaymentTypeCast.status},
         {PaymentFields.statusDetail} = @statusDetail,
         {PaymentFields.fulfilledByTransferId} =
            COALESCE(@fulfilledByTransferId, {paymentTable}.{PaymentFields.fulfilledByTransferId}),
         {PaymentFields.fulfilledAt} =
            COALESCE(@fulfilledAt, {paymentTable}.{PaymentFields.fulfilledAt})
      WHERE {PaymentFields.paymentId} = @paymentId;
      """,
      sqlParamsDerivedFromAccountEvents.PaymentUpdate
      |> List.map (
         Lib.Postgres.addCoalescableParamsForUpdate [
            "fulfilledByTransferId"
            "fulfilledAt"
         ]
      )
   ]

let initProps
   (chunking: StreamChunkingEnvConfig)
   (restartSettings: Akka.Streams.RestartSettings)
   (retryPersistenceAfter: TimeSpan)
   =
   actorProps<ParentAccountSnapshot, AccountEvent>
   <| ReadModelSyncConfig.DefaultMode {
      Chunking = chunking
      RestartSettings = restartSettings
      RetryPersistenceAfter = retryPersistenceAfter
      UpsertReadModels = upsertReadModels
      EventJournalTag = Constants.AKKA_ACCOUNT_JOURNAL
   }
