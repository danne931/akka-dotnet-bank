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
open TransactionSqlMapper
open PaymentSqlMapper
open TransferSqlMapper
open Lib.ReadModelSyncActor

let private platformPaymentBaseSqlParams (p: PlatformPaymentBaseInfo) = [
   "paymentId", PaymentSqlWriter.paymentId p.Id

   "initiatedById", PaymentSqlWriter.initiatedById p.InitiatedById

   "payeeOrgId", PaymentSqlWriter.payeeOrgId p.Payee.OrgId

   "payeeAccountId", PaymentSqlWriter.payeeAccountId p.Payee.AccountId

   "payerOrgId", PaymentSqlWriter.Platform.payerOrgId p.Payer.OrgId

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
   Transaction: (string * SqlValue) list list
   Payment: (string * SqlValue) list list
   PlatformPayment: (string * SqlValue) list list
   Transfer: (string * SqlValue) list list
   InternalTransfer: (string * SqlValue) list list
   DomesticTransfer: (string * SqlValue) list list
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

let sqlParamReducer
   (acc: SqlParamsDerivedFromAccountEvents)
   (evt: AccountEvent)
   : SqlParamsDerivedFromAccountEvents
   =
   let evt, envelope = AccountEnvelope.unwrap evt

   let transactionSqlParams = [
      "eventId", TransactionSqlWriter.eventId envelope.Id

      "accountId",
      envelope.EntityId
      |> AccountId.fromEntityId
      |> TransactionSqlWriter.accountId

      "orgId", TransactionSqlWriter.orgId envelope.OrgId

      "correlationId", TransactionSqlWriter.correlationId envelope.CorrelationId

      "initiatedById",
      TransactionSqlWriter.initiatedById envelope.InitiatedBy.Id

      "name", TransactionSqlWriter.name envelope.EventName
      "timestamp", TransactionSqlWriter.timestamp envelope.Timestamp
      "event", TransactionSqlWriter.event evt
   ]

   let amountOpt, moneyFlowOpt, sourceOpt = AccountEvent.moneyTransaction evt

   let transactionSqlParams =
      transactionSqlParams
      @ [
         "amount", TransactionSqlWriter.amount amountOpt
         "moneyFlow", TransactionSqlWriter.moneyFlow moneyFlowOpt
         "source", TransactionSqlWriter.source sourceOpt

         "cardId",
         TransactionSqlWriter.cardId (
            match evt with
            | AccountEvent.DebitedAccount e ->
               Some e.Data.EmployeePurchaseReference.CardId
            | _ -> None
         )
      ]

   let acc = {
      acc with
         Transaction = transactionSqlParams :: acc.Transaction
   }

   match evt with
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
   | AccountEvent.InternalTransferWithinOrgCompleted e ->
      internalTransferStatusReducer
         acc
         InternalTransferStatus.Completed
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
   | AccountEvent.InternalAutomatedTransferCompleted e ->
      internalTransferStatusReducer
         acc
         InternalTransferStatus.Completed
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
            "memo", TransferSqlWriter.memo e.Data.Memo
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
            "memo", TransferSqlWriter.memo e.Data.Memo
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
   | AccountEvent.InternalTransferBetweenOrgsCompleted e ->
      internalTransferStatusReducer
         acc
         InternalTransferStatus.Completed
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

      let status = DomesticTransferProgress.Outgoing

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

      let platformPayParams =
         ("status",
          PaymentSqlWriter.Platform.status PlatformPaymentStatus.Unpaid)
         :: platformPaymentBaseSqlParams pInfo

      {
         acc with
            Payment = payParams :: acc.Payment
            PlatformPayment = platformPayParams :: acc.PlatformPayment
      }
   | AccountEvent.PlatformPaymentCancelled e ->
      let pParams =
         ("status",
          PaymentSqlWriter.Platform.status PlatformPaymentStatus.Cancelled)
         :: platformPaymentBaseSqlParams e.Data.BaseInfo

      {
         acc with
            PlatformPayment = pParams :: acc.PlatformPayment
      }
   | AccountEvent.PlatformPaymentPaid e ->
      let pParams =
         ("status", PaymentSqlWriter.Platform.status PlatformPaymentStatus.Paid)
         :: platformPaymentBaseSqlParams e.Data.BaseInfo

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
      let pParams =
         ("status",
          PaymentSqlWriter.Platform.status PlatformPaymentStatus.Deposited)
         :: platformPaymentBaseSqlParams e.Data.BaseInfo

      {
         acc with
            PlatformPayment = pParams :: acc.PlatformPayment
      }
   | AccountEvent.PlatformPaymentDeclined e ->
      let pParams =
         ("status",
          PaymentSqlWriter.Platform.status PlatformPaymentStatus.Declined)
         :: platformPaymentBaseSqlParams e.Data.BaseInfo

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
   "name", AccountSqlWriter.name account.Name
   "depository", AccountSqlWriter.depository account.Depository
   "balance", AccountSqlWriter.balance account.Balance
   "currency", AccountSqlWriter.currency account.Currency
   "status", AccountSqlWriter.status account.Status
   "lastBillingCycleDate",
   AccountSqlWriter.lastBillingCycleDate account.LastBillingCycleDate

   "maintenanceFeeQualifyingDepositFound",
   AccountSqlWriter.maintenanceFeeQualifyingDepositFound
      account.MaintenanceFeeCriteria.QualifyingDepositFound

   "maintenanceFeeDailyBalanceThreshold",
   AccountSqlWriter.maintenanceFeeDailyBalanceThreshold
      account.MaintenanceFeeCriteria.DailyBalanceThreshold

   "autoTransferRule",
   AccountSqlWriter.autoTransferRule account.AutoTransferRule

   "autoTransferRuleFrequency",
   account.AutoTransferRule
   |> Option.map (_.Info >> AutomaticTransfer.frequencyFromAutoTransferRule)
   |> AccountSqlWriter.autoTransferRuleFrequency
]

let upsertReadModels
   (accounts: Account list, accountEvents: AccountEvent list)
   =
   let accountSqlParams = List.map sqlParamsFromAccount accounts

   let sqlParamsDerivedFromAccountEvents =
      accountEvents
      |> List.sortByDescending (AccountEnvelope.unwrap >> snd >> _.Timestamp)
      |> List.fold sqlParamReducer {
         Transaction = []
         Payment = []
         PlatformPayment = []
         Transfer = []
         InternalTransfer = []
         DomesticTransfer = []
      }

   pgTransaction [
      $"""
      INSERT into {AccountSqlMapper.table}
         ({AccountFields.accountId},
          {AccountFields.accountNumber},
          {AccountFields.routingNumber},
          {AccountFields.orgId},
          {AccountFields.name},
          {AccountFields.depository},
          {AccountFields.balance},
          {AccountFields.currency},
          {AccountFields.status},
          {AccountFields.lastBillingCycleDate},
          {AccountFields.maintenanceFeeQualifyingDepositFound},
          {AccountFields.maintenanceFeeDailyBalanceThreshold},
          {AccountFields.autoTransferRule},
          {AccountFields.autoTransferRuleFrequency})
      VALUES
         (@id,
          @accountNumber,
          @routingNumber,
          @orgId,
          @name,
          @depository::{AccountTypeCast.depository},
          @balance,
          @currency,
          @status::{AccountTypeCast.status},
          @lastBillingCycleDate,
          @maintenanceFeeQualifyingDepositFound,
          @maintenanceFeeDailyBalanceThreshold,
          @autoTransferRule,
          @autoTransferRuleFrequency::{AccountTypeCast.autoTransferRuleFrequency})
      ON CONFLICT ({AccountFields.accountId})
      DO UPDATE SET
         {AccountFields.balance} = @balance,
         {AccountFields.status} = @status::{AccountTypeCast.status},
         {AccountFields.lastBillingCycleDate} = @lastBillingCycleDate,
         {AccountFields.maintenanceFeeQualifyingDepositFound} = @maintenanceFeeQualifyingDepositFound,
         {AccountFields.maintenanceFeeDailyBalanceThreshold} = @maintenanceFeeDailyBalanceThreshold,
         {AccountFields.autoTransferRule} = @autoTransferRule,
         {AccountFields.autoTransferRuleFrequency} = @autoTransferRuleFrequency::{AccountTypeCast.autoTransferRuleFrequency};
      """,
      accountSqlParams

      $"""
      INSERT into {TransactionSqlMapper.table}
         ({TransactionFields.eventId},
          {TransactionFields.accountId},
          {TransactionFields.orgId},
          {TransactionFields.correlationId},
          {TransactionFields.initiatedById},
          {TransactionFields.cardId},
          {TransactionFields.name},
          {TransactionFields.timestamp},
          {TransactionFields.event},
          {TransactionFields.amount},
          {TransactionFields.source},
          {TransactionFields.moneyFlow})
      VALUES
         (@eventId,
          @accountId,
          @orgId,
          @correlationId,
          @initiatedById,
          @cardId,
          @name,
          @timestamp,
          @event,
          @amount,
          @source,
          @moneyFlow::{TransactionTypeCast.moneyFlow})
      ON CONFLICT ({TransactionFields.eventId})
      DO NOTHING;
      """,
      sqlParamsDerivedFromAccountEvents.Transaction

      if not sqlParamsDerivedFromAccountEvents.Payment.IsEmpty then
         $"""
         INSERT into {PaymentSqlMapper.Table.payment}
            ({PaymentFields.paymentId},
             {PaymentFields.initiatedById},
             {PaymentFields.amount},
             {PaymentFields.memo},
             {PaymentFields.paymentType},
             {PaymentFields.expiration},
             {PaymentFields.payeeOrgId},
             {PaymentFields.payeeAccountId})
         VALUES
            (@paymentId,
             @initiatedById,
             @amount,
             @memo,
             @paymentType::{PaymentTypeCast.paymentType},
             @expiration,
             @payeeOrgId,
             @payeeAccountId)
         ON CONFLICT ({PaymentFields.paymentId})
         DO NOTHING;
         """,
         sqlParamsDerivedFromAccountEvents.Payment

      if not sqlParamsDerivedFromAccountEvents.PlatformPayment.IsEmpty then
         let table = PaymentSqlMapper.Table.platformPayment
         let payByField = PaymentFields.Platform.payByAccount

         $"""
         INSERT into {table}
            ({PaymentFields.paymentId},
             {PaymentFields.Platform.status},
             {PaymentFields.Platform.payerOrgId})
         VALUES
            (@paymentId,
             @status::{PaymentTypeCast.platformPaymentStatus},
             @payerOrgId)
         ON CONFLICT ({PaymentFields.paymentId})
         DO UPDATE SET
            {PaymentFields.Platform.status} = @status::{PaymentTypeCast.platformPaymentStatus},
            {payByField} = COALESCE(@payByAccount, {table}.{payByField});
         """,
         sqlParamsDerivedFromAccountEvents.PlatformPayment

      if not sqlParamsDerivedFromAccountEvents.Transfer.IsEmpty then
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

      if not sqlParamsDerivedFromAccountEvents.InternalTransfer.IsEmpty then
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

      if not sqlParamsDerivedFromAccountEvents.DomesticTransfer.IsEmpty then
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
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   (chunking: StreamChunking)
   (restartSettings: Akka.Streams.RestartSettings)
   (retryPersistenceAfter: TimeSpan)
   =
   actorProps<Account, AccountEvent> (
      {
         GetAggregateIdFromEvent =
            AccountEnvelope.unwrap >> snd >> _.EntityId >> EntityId.get
         GetAggregate =
            fun accountId -> task {
               let aref = getAccountRef (AccountId accountId)

               let! (accountOpt: Account option) =
                  aref <? AccountMessage.GetAccount

               return accountOpt
            }
         Chunking = chunking
         RestartSettings = restartSettings
         RetryPersistenceAfter = retryPersistenceAfter
         UpsertReadModels = upsertReadModels
         EventJournalTag = Constants.AKKA_ACCOUNT_JOURNAL
      }
   )
