[<RequireQualifiedAccess>]
module AccountReadModelSyncActor

open System
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes
open Lib.Types
open Lib.Postgres
open Bank.Account.Domain
open Bank.Transfer.Domain
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

let private platformPaymentBaseSqlParams (p: PlatformPaymentBaseInfo) = [
   "paymentId", PaymentSqlWriter.paymentId p.Id

   "initiatedById", PaymentSqlWriter.initiatedById p.InitiatedById

   "payeeOrgId", PaymentSqlWriter.payeeOrgId p.Payee.OrgId

   "payeeAccountId", PaymentSqlWriter.payeeAccountId p.Payee.AccountId

   "payeeParentAccountId",
   PaymentSqlWriter.payeeParentAccountId p.Payee.ParentAccountId

   "payerOrgId", PaymentSqlWriter.Platform.payerOrgId p.Payer.OrgId

   "payerParentAccountId",
   PaymentSqlWriter.Platform.payerParentAccountId p.Payer.ParentAccountId

   "payByAccount", PaymentSqlWriter.Platform.payByAccount None
]

let private internalTransferBaseSqlParams (o: BaseInternalTransferInfo) = [
   "transferId", TransferSqlWriter.transferId o.TransferId

   "initiatedById", TransferSqlWriter.initiatedById o.InitiatedBy.Id

   "senderOrgId", TransferSqlWriter.senderOrgId o.Sender.OrgId

   "senderAccountId", TransferSqlWriter.senderAccountId o.Sender.AccountId

   "scheduledAt", TransferSqlWriter.scheduledAt o.ScheduledDate

   "amount", TransferSqlWriter.amount o.Amount

   "recipientAccountId",
   TransferSqlWriter.Internal.recipientAccountId o.Recipient.AccountId

   "recipientOrgId", TransferSqlWriter.Internal.recipientOrgId o.Recipient.OrgId
]

let private domesticTransferBaseSqlParams (o: BaseDomesticTransferInfo) = [
   "transferId", TransferSqlWriter.transferId o.TransferId

   "initiatedById", TransferSqlWriter.initiatedById o.InitiatedBy.Id

   "senderOrgId", TransferSqlWriter.senderOrgId o.Sender.OrgId

   "senderAccountId", TransferSqlWriter.senderAccountId o.Sender.AccountId

   "scheduledAt", TransferSqlWriter.scheduledAt o.ScheduledDate

   "amount", TransferSqlWriter.amount o.Amount

   "recipientAccountId",
   TransferSqlWriter.Domestic.recipientAccountId o.Recipient.RecipientAccountId

   "memo", TransferSqlWriter.memo o.Memo
]

type SqlParamsDerivedFromAccountEvents = {
   AccountEvent: (string * SqlValue) list list
   ParentAccountEvent: (string * SqlValue) list list
   Payment: (string * SqlValue) list list
   PlatformPayment: (string * SqlValue) list list
   Transfer: (string * SqlValue) list list
   InternalTransfer: (string * SqlValue) list list
   DomesticTransfer: (string * SqlValue) list list
   PartnerBankInitialized: (string * SqlValue) list list
   BillingCycle: (string * SqlValue) list list
}

let private internalTransferStatusReducer
   (acc: SqlParamsDerivedFromAccountEvents)
   (status: InternalTransferStatus)
   (info: BaseInternalTransferInfo)
   =
   let qParams =
      internalTransferBaseSqlParams info
      @ [
         "status", TransferSqlWriter.Internal.status status
         "statusDetail", TransferSqlWriter.Internal.statusDetail status
      ]

   {
      acc with
         InternalTransfer = qParams :: acc.InternalTransfer
   }

let private domesticTransferStatusReducer
   (acc: SqlParamsDerivedFromAccountEvents)
   (status: DomesticTransferProgress)
   (info: BaseDomesticTransferInfo)
   =
   let qParams =
      domesticTransferBaseSqlParams info
      @ [
         "status", TransferSqlWriter.Domestic.status status
         "statusDetail", TransferSqlWriter.Domestic.statusDetail status
      ]

   {
      acc with
         DomesticTransfer = qParams :: acc.DomesticTransfer
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
            | AccountEvent.DebitedAccount e ->
               Some e.Data.EmployeePurchaseReference.CardId
            | AccountEvent.RefundedDebit e ->
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

   let acc =
      match evt with
      | AccountEvent.ParentAccount evt ->
         parentAccountEventReducer acc evt envelope
      | _ -> accountEventReducer acc evt envelope

   match evt with
   | AccountEvent.InitializedPrimaryCheckingAccount e ->
      let qParams = [
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

      {
         acc with
            PartnerBankInitialized = qParams :: acc.PartnerBankInitialized
      }
   | AccountEvent.ParentAccount(ParentAccountEvent.BillingCycleStarted e) ->
      let qParams = [
         "parentAccountId",
         PartnerBankSqlMapper.SqlWriter.parentAccountId (
            ParentAccountId.fromEntityId e.EntityId
         )

         "lastBillingCycleDate",
         PartnerBankSqlMapper.SqlWriter.lastBillingCycleDate (Some e.Timestamp)
      ]

      {
         acc with
            BillingCycle = qParams :: acc.BillingCycle
      }
   | AccountEvent.InternalTransferWithinOrgPending e ->
      let info = e.Data.BaseInfo

      let transferParams =
         internalTransferBaseSqlParams info
         @ [
            "memo", TransferSqlWriter.memo None
            "transferCategory",
            TransferSqlWriter.transferCategory
               TransferCategory.InternalWithinOrg
         ]

      let status = InternalTransferStatus.Pending

      let internalTransferParams =
         internalTransferBaseSqlParams info
         @ [
            "status", TransferSqlWriter.Internal.status status
            "statusDetail", TransferSqlWriter.Internal.statusDetail status
         ]

      {
         acc with
            Transfer = transferParams :: acc.Transfer
            InternalTransfer = internalTransferParams :: acc.InternalTransfer
      }
   | AccountEvent.InternalTransferWithinOrgFailed e ->
      internalTransferStatusReducer
         acc
         (InternalTransferStatus.Failed e.Data.Reason)
         e.Data.BaseInfo
   | AccountEvent.InternalTransferWithinOrgDeposited e ->
      internalTransferStatusReducer
         acc
         InternalTransferStatus.Deposited
         e.Data.BaseInfo
   | AccountEvent.InternalAutomatedTransferPending e ->
      let info = e.Data.BaseInfo

      let transferParams =
         internalTransferBaseSqlParams info
         @ [
            "memo", TransferSqlWriter.memo None
            "transferCategory",
            TransferSqlWriter.transferCategory
               TransferCategory.InternalAutomatedWithinOrg
         ]

      let status = InternalTransferStatus.Pending

      let internalTransferParams =
         internalTransferBaseSqlParams info
         @ [
            "status", TransferSqlWriter.Internal.status status
            "statusDetail", TransferSqlWriter.Internal.statusDetail status
         ]

      {
         acc with
            Transfer = transferParams :: acc.Transfer
            InternalTransfer = internalTransferParams :: acc.InternalTransfer
      }
   | AccountEvent.InternalAutomatedTransferFailed e ->
      internalTransferStatusReducer
         acc
         (InternalTransferStatus.Failed e.Data.Reason)
         e.Data.BaseInfo
   | AccountEvent.InternalAutomatedTransferDeposited e ->
      internalTransferStatusReducer
         acc
         InternalTransferStatus.Deposited
         e.Data.BaseInfo
   | AccountEvent.InternalTransferBetweenOrgsScheduled e ->
      let info = e.Data.BaseInfo

      let transferParams =
         internalTransferBaseSqlParams info
         @ [
            "memo", TransferSqlWriter.memo e.Data.BaseInfo.Memo
            "transferCategory",
            TransferSqlWriter.transferCategory
               TransferCategory.InternalBetweenOrgs
         ]

      let status = InternalTransferStatus.Scheduled

      let internalTransferParams =
         internalTransferBaseSqlParams info
         @ [
            "status", TransferSqlWriter.Internal.status status
            "statusDetail", TransferSqlWriter.Internal.statusDetail status
         ]

      {
         acc with
            Transfer = transferParams :: acc.Transfer
            InternalTransfer = internalTransferParams :: acc.InternalTransfer
      }
   | AccountEvent.InternalTransferBetweenOrgsPending e ->
      let info = e.Data.BaseInfo

      let transferParams =
         internalTransferBaseSqlParams info
         @ [
            "memo", TransferSqlWriter.memo e.Data.BaseInfo.Memo
            "transferCategory",
            TransferSqlWriter.transferCategory
               TransferCategory.InternalBetweenOrgs
         ]

      let status = InternalTransferStatus.Pending

      let internalTransferParams =
         internalTransferBaseSqlParams info
         @ [
            "status", TransferSqlWriter.Internal.status status
            "statusDetail", TransferSqlWriter.Internal.statusDetail status
         ]

      {
         acc with
            Transfer = transferParams :: acc.Transfer
            InternalTransfer = internalTransferParams :: acc.InternalTransfer
      }
   | AccountEvent.InternalTransferBetweenOrgsFailed e ->
      internalTransferStatusReducer
         acc
         (InternalTransferStatus.Failed e.Data.Reason)
         e.Data.BaseInfo
   | AccountEvent.InternalTransferBetweenOrgsDeposited e ->
      internalTransferStatusReducer
         acc
         InternalTransferStatus.Deposited
         e.Data.BaseInfo
   | AccountEvent.DomesticTransferScheduled e ->
      let info = e.Data.BaseInfo

      let transferParams =
         domesticTransferBaseSqlParams info
         @ [
            "transferCategory",
            TransferSqlWriter.transferCategory TransferCategory.Domestic
         ]

      let status = DomesticTransferProgress.Scheduled

      let domesticTransferParams =
         domesticTransferBaseSqlParams info
         @ [
            "status", TransferSqlWriter.Domestic.status status
            "statusDetail", TransferSqlWriter.Domestic.statusDetail status
         ]

      {
         acc with
            Transfer = transferParams :: acc.Transfer
            DomesticTransfer = domesticTransferParams :: acc.DomesticTransfer
      }
   | AccountEvent.DomesticTransferPending e ->
      let info = e.Data.BaseInfo

      let transferParams =
         domesticTransferBaseSqlParams info
         @ [
            "transferCategory",
            TransferSqlWriter.transferCategory TransferCategory.Domestic
         ]

      let status = DomesticTransferProgress.WaitingForTransferServiceAck

      let domesticTransferParams =
         domesticTransferBaseSqlParams info
         @ [
            "status", TransferSqlWriter.Domestic.status status
            "statusDetail", TransferSqlWriter.Domestic.statusDetail status
         ]

      {
         acc with
            Transfer = transferParams :: acc.Transfer
            DomesticTransfer = domesticTransferParams :: acc.DomesticTransfer
      }
   | AccountEvent.DomesticTransferProgress e ->
      domesticTransferStatusReducer
         acc
         (DomesticTransferProgress.InProgress e.Data.InProgressInfo)
         e.Data.BaseInfo
   | AccountEvent.DomesticTransferFailed e ->
      domesticTransferStatusReducer
         acc
         (DomesticTransferProgress.Failed e.Data.Reason)
         e.Data.BaseInfo
   | AccountEvent.DomesticTransferCompleted e ->
      domesticTransferStatusReducer
         acc
         DomesticTransferProgress.Completed
         e.Data.BaseInfo
   | AccountEvent.PlatformPaymentRequested e ->
      let pInfo = e.Data.BaseInfo

      let payParams =
         platformPaymentBaseSqlParams pInfo
         @ [
            "amount", PaymentSqlWriter.amount pInfo.Amount
            "paymentType", Sql.string "Platform"
            "expiration", PaymentSqlWriter.expiration e.Data.Expiration
            "memo", PaymentSqlWriter.memo e.Data.Memo
         ]

      let status = PlatformPaymentStatus.Unpaid

      let platformPayParams =
         [
            "status", PaymentSqlWriter.Platform.status status
            "statusDetail", PaymentSqlWriter.Platform.statusDetail status
         ]
         @ platformPaymentBaseSqlParams pInfo

      {
         acc with
            Payment = payParams :: acc.Payment
            PlatformPayment = platformPayParams :: acc.PlatformPayment
      }
   | AccountEvent.PlatformPaymentCancelled e ->
      let status = PlatformPaymentStatus.Cancelled

      let pParams =
         [
            "status", PaymentSqlWriter.Platform.status status
            "statusDetail", PaymentSqlWriter.Platform.statusDetail status
         ]
         @ platformPaymentBaseSqlParams e.Data.BaseInfo

      {
         acc with
            PlatformPayment = pParams :: acc.PlatformPayment
      }
   | AccountEvent.PlatformPaymentPaid e ->
      let status = PlatformPaymentStatus.Paid

      let pParams =
         [
            "status", PaymentSqlWriter.Platform.status status
            "statusDetail", PaymentSqlWriter.Platform.statusDetail status
         ]
         @ platformPaymentBaseSqlParams e.Data.BaseInfo

      let pParams =
         match e.Data.PaymentMethod with
         | PaymentMethod.Platform accountId ->
            pParams
            |> List.map (fun (field, sqlValue) ->
               if field = "payByAccount" then
                  field,
                  PaymentSqlWriter.Platform.payByAccount (Some accountId)
               else
                  field, sqlValue)
         | PaymentMethod.ThirdParty tp ->
            // TODO: Implement third party payment methods
            pParams

      {
         acc with
            PlatformPayment = pParams :: acc.PlatformPayment
      }
   | AccountEvent.PlatformPaymentDeposited e ->
      let status = PlatformPaymentStatus.Deposited

      let pParams =
         [
            "status", PaymentSqlWriter.Platform.status status
            "statusDetail", PaymentSqlWriter.Platform.statusDetail status
         ]
         @ platformPaymentBaseSqlParams e.Data.BaseInfo

      {
         acc with
            PlatformPayment = pParams :: acc.PlatformPayment
      }
   | AccountEvent.PlatformPaymentDeclined e ->
      let status = PlatformPaymentStatus.Declined

      let pParams =
         [
            "status", PaymentSqlWriter.Platform.status status
            "statusDetail", PaymentSqlWriter.Platform.statusDetail status
         ]
         @ platformPaymentBaseSqlParams e.Data.BaseInfo

      {
         acc with
            PlatformPayment = pParams :: acc.PlatformPayment
      }
   | _ -> acc

let sqlParamsFromAccount (account: Account) : (string * SqlValue) list = [
   "id", AccountSqlWriter.accountId account.AccountId
   "accountNumber", AccountSqlWriter.accountNumber account.AccountNumber
   "routingNumber", AccountSqlWriter.routingNumber account.RoutingNumber
   "orgId", AccountSqlWriter.orgId account.OrgId
   "parentAccountId", AccountSqlWriter.parentAccountId account.ParentAccountId
   "name", AccountSqlWriter.name account.Name
   "depository", AccountSqlWriter.depository account.Depository
   "balance", AccountSqlWriter.balance account.Balance
   "currency", AccountSqlWriter.currency account.Currency
   "status", AccountSqlWriter.status account.Status
   "autoTransferRule",
   AccountSqlWriter.autoTransferRule account.AutoTransferRule

   "autoTransferRuleFrequency",
   account.AutoTransferRule
   |> Option.map (_.Info >> AutomaticTransfer.frequencyFromAutoTransferRule)
   |> AccountSqlWriter.autoTransferRuleFrequency
]

let upsertReadModels
   (parentAccounts: ParentAccountSnapshot list, accountEvents: AccountEvent list)
   =
   let accountsIndexed =
      parentAccounts
      |> List.map (_.VirtualAccounts.Values >> Seq.toList)
      |> List.collect id
      |> List.fold
            (fun acc account -> Map.add account.AccountId account acc)
            Map.empty<AccountId, Account>

   let updatedAccounts =
      accountEvents
      |> List.fold
            (fun acc evt ->
               match accountsIndexed |> Map.tryFind evt.AccountId with
               | Some account -> acc |> Map.add evt.AccountId account
               | None -> acc)
            Map.empty<AccountId, Account>
      |> _.Values
      |> Seq.toList

   let accountSqlParams = List.map sqlParamsFromAccount updatedAccounts

   let sqlParamsDerivedFromAccountEvents =
      accountEvents
      |> List.sortByDescending (AccountEnvelope.unwrap >> snd >> _.Timestamp)
      |> List.fold sqlParamReducer {
         AccountEvent = []
         ParentAccountEvent = []
         PartnerBankInitialized = []
         BillingCycle = []
         Payment = []
         PlatformPayment = []
         Transfer = []
         InternalTransfer = []
         DomesticTransfer = []
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
          {AccountFields.currency},
          {AccountFields.status},
          {AccountFields.autoTransferRule},
          {AccountFields.autoTransferRuleFrequency})
      VALUES
         (@id,
          @accountNumber,
          @routingNumber,
          @orgId,
          @parentAccountId,
          @name,
          @depository::{AccountTypeCast.depository},
          @balance,
          @currency,
          @status::{AccountTypeCast.status},
          @autoTransferRule,
          @autoTransferRuleFrequency::{AccountTypeCast.autoTransferRuleFrequency})
      ON CONFLICT ({AccountFields.accountId})
      DO UPDATE SET
         {AccountFields.balance} = @balance,
         {AccountFields.status} = @status::{AccountTypeCast.status},
         {AccountFields.autoTransferRule} = @autoTransferRule,
         {AccountFields.autoTransferRuleFrequency} = @autoTransferRuleFrequency::{AccountTypeCast.autoTransferRuleFrequency};
      """,
      accountSqlParams

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

      $"""
      INSERT into {PaymentSqlMapper.Table.payment}
         ({PaymentFields.paymentId},
          {PaymentFields.initiatedById},
          {PaymentFields.amount},
          {PaymentFields.memo},
          {PaymentFields.paymentType},
          {PaymentFields.expiration},
          {PaymentFields.payeeOrgId},
          {PaymentFields.payeeAccountId},
          {PaymentFields.payeeParentAccountId})
      VALUES
         (@paymentId,
          @initiatedById,
          @amount,
          @memo,
          @paymentType::{PaymentTypeCast.paymentType},
          @expiration,
          @payeeOrgId,
          @payeeAccountId,
          @payeeParentAccountId)
      ON CONFLICT ({PaymentFields.paymentId})
      DO NOTHING;
      """,
      sqlParamsDerivedFromAccountEvents.Payment

      let paymentTable = PaymentSqlMapper.Table.platformPayment
      let payByField = PaymentFields.Platform.payByAccount

      $"""
      INSERT into {paymentTable}
         ({PaymentFields.paymentId},
          {PaymentFields.Platform.status},
          {PaymentFields.Platform.statusDetail},
          {PaymentFields.Platform.payerOrgId},
          {PaymentFields.Platform.payerParentAccountId})
      VALUES
         (@paymentId,
          @status::{PaymentTypeCast.platformPaymentStatus},
          @statusDetail,
          @payerOrgId,
          @payerParentAccountId)
      ON CONFLICT ({PaymentFields.paymentId})
      DO UPDATE SET
         {PaymentFields.Platform.status} = @status::{PaymentTypeCast.platformPaymentStatus},
         {PaymentFields.Platform.statusDetail} = @statusDetail,
         {payByField} = COALESCE(@payByAccount, {paymentTable}.{payByField});
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
      INSERT into {TransferSqlMapper.Table.internalTransfer}
         ({TransferFields.transferId},
          {TransferFields.Internal.status},
          {TransferFields.Internal.statusDetail},
          {TransferFields.Internal.recipientOrgId},
          {TransferFields.Internal.recipientAccountId})
      VALUES
         (@transferId,
          @status::{TransferTypeCast.internalTransferStatus},
          @statusDetail,
          @recipientOrgId,
          @recipientAccountId)
      ON CONFLICT ({TransferFields.transferId})
      DO UPDATE SET
         {TransferFields.Internal.status} = @status::{TransferTypeCast.internalTransferStatus},
         {TransferFields.Internal.statusDetail} = @statusDetail;
      """,
      sqlParamsDerivedFromAccountEvents.InternalTransfer

      $"""
      INSERT into {TransferSqlMapper.Table.domesticTransfer}
         ({TransferFields.transferId},
          {TransferFields.Domestic.status},
          {TransferFields.Domestic.statusDetail},
          {TransferFields.Domestic.recipientAccountId})
      VALUES
         (@transferId,
          @status::{TransferTypeCast.domesticTransferStatus},
          @statusDetail,
          @recipientAccountId)
      ON CONFLICT ({TransferFields.transferId})
      DO UPDATE SET
         {TransferFields.Domestic.status} = @status::{TransferTypeCast.domesticTransferStatus},
         {TransferFields.Domestic.statusDetail} = @statusDetail;
      """,
      sqlParamsDerivedFromAccountEvents.DomesticTransfer
   ]

let initProps
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (chunking: StreamChunking)
   (restartSettings: Akka.Streams.RestartSettings)
   (retryPersistenceAfter: TimeSpan)
   =
   actorProps<ParentAccountSnapshot, AccountEvent> (
      {
         GetAggregateIdFromEvent =
            AccountEnvelope.unwrap >> snd >> _.EntityId >> EntityId.get
         GetAggregate =
            fun parentAccountId -> task {
               let aref = getAccountRef (ParentAccountId parentAccountId)

               let! (parentAccountOpt: ParentAccountSnapshot option) =
                  aref <? AccountMessage.GetAccount

               return parentAccountOpt
            }
         Chunking = chunking
         RestartSettings = restartSettings
         RetryPersistenceAfter = retryPersistenceAfter
         UpsertReadModels = upsertReadModels
         EventJournalTag = Constants.AKKA_ACCOUNT_JOURNAL
      }
   )
