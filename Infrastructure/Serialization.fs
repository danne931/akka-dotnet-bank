namespace Bank.Infrastructure

open System
open System.Text.Json
open System.Runtime.Serialization
open Akka.Serialization
open Akka.Actor
open Akka.Persistence.Journal
open Akka.Persistence.Extras
open Akkling.Cluster.Sharding

open Bank.Org.Domain
open CachedOrgSettings
open Bank.Employee.Domain
open Bank.Account.Domain
open PartnerBank.Service.Domain
open CardIssuer.Service.Domain
open BillingStatement
open Lib.SharedTypes
open Lib.CircuitBreaker
open CommandApproval
open Lib.Saga
open Bank.Scheduler
open TransferMessages



// TODO: Replace with protobuf



type OrganizationEventPersistenceAdapter() =
   let envelopeFromJournal (entry: obj) : Envelope =
      let evt: OrgEvent = unbox entry
      let _, envelope = OrgEnvelope.unwrap evt
      envelope

   interface IEventAdapter with
      member _.Manifest(evt: obj) = envelopeFromJournal(evt).EventName

      member _.ToJournal(evt: obj) : obj =
         let envelope = envelopeFromJournal evt

         Tagged(
            evt,
            Set.empty<string>
               .Add(Constants.AKKA_ORG_JOURNAL)
               .Add(envelope.EventName)
         )

      member _.FromJournal(evt: obj, manifest: string) : IEventSequence =
         EventSequence.Single evt

type AccountEventPersistenceAdapter() =
   let envelopeFromJournal (entry: obj) : Envelope =
      let evt: AccountEvent = unbox entry
      let _, envelope = AccountEnvelope.unwrap evt
      envelope

   interface IEventAdapter with
      member _.Manifest(evt: obj) = envelopeFromJournal(evt).EventName

      member _.ToJournal(evt: obj) : obj =
         let envelope = envelopeFromJournal evt

         Tagged(
            evt,
            Set.empty<string>
               .Add(Constants.AKKA_ACCOUNT_JOURNAL)
               .Add(envelope.EventName)
         )

      member _.FromJournal(evt: obj, manifest: string) : IEventSequence =
         EventSequence.Single(evt)

type EmployeeEventPersistenceAdapter() =
   let envelopeFromJournal (entry: obj) : Envelope =
      let evt: EmployeeEvent = unbox entry
      let _, envelope = EmployeeEnvelope.unwrap evt
      envelope

   interface IEventAdapter with
      member _.Manifest(evt: obj) = envelopeFromJournal(evt).EventName

      member _.ToJournal(evt: obj) : obj =
         let envelope = envelopeFromJournal evt

         Tagged(
            evt,
            Set.empty<string>
               .Add(Constants.AKKA_EMPLOYEE_JOURNAL)
               .Add(envelope.EventName)
         )

      member _.FromJournal(evt: obj, manifest: string) : IEventSequence =
         EventSequence.Single evt

type SagaEventPersistenceAdapter() =
   let eventName (entry: obj) : string =
      let evt: AppSaga.AppSagaPersistableEvent = unbox entry

      match evt with
      | SagaPersistableEvent.StartEvent e -> e.Data.Name
      | SagaPersistableEvent.Event e -> e.Data.Name

   interface IEventAdapter with
      member _.Manifest(evt: obj) = eventName evt

      member _.ToJournal(evt: obj) : obj =
         let evtName = eventName evt

         Tagged(
            evt,
            Set.empty<string>.Add(Constants.AKKA_APP_SAGA_JOURNAL).Add(evtName)
         )

      member _.FromJournal(evt: obj, manifest: string) : IEventSequence =
         EventSequence.Single evt

type private OrgShardEnvelope = {
   EntityId: string
   ShardId: string
   Message: OrgMessage
}

type private AccountShardEnvelope = {
   EntityId: string
   ShardId: string
   Message: AccountMessage
}

type private EmployeeShardEnvelope = {
   EntityId: string
   ShardId: string
   Message: EmployeeMessage
}

type private AppSagaShardEnvelope = {
   EntityId: string
   ShardId: string
   Message: AppSaga.AppSagaMessage
}

type BankSerializer(system: ExtendedActorSystem) =
   inherit SerializerWithStringManifest(system)

   static member Name = "bank-serializer"

   override _.Identifier = 931

   override _.Manifest(o: obj) =
      match o with
      | :? ConfirmableMessageEnvelope -> "ConfirmableMessageEnvelope"
      | :? Lib.ReadModelSyncActor.State -> "ReadModelSyncState"
      | :? CommandApprovalDailyAccrual -> "CommandApprovalDailyAccrual"
      | :? CachedOrgSettings -> "CachedOrgSettings"
      | :? OrgSnapshot -> "OrgSnapshot"
      | :? Option<Org> -> "OrgOption"
      | :? OrgEvent -> "OrgEvent"
      | :? OrgMessage -> "OrgMessage"
      | :? KYCMessage -> "KYCMessage"
      | :? PartnerBankServiceMessage -> "PartnerBankServiceMessage"
      | :? CardIssuerMessage -> "CardIssuerServiceMessage"
      | :? EmployeeSnapshot -> "EmployeeSnapshot"
      | :? Option<Employee> -> "EmployeeOption"
      | :? EmployeeMessage -> "EmployeeMessage"
      | :? EmployeeEvent -> "EmployeeEvent"
      | :? AppSaga.SagaAlarmClockMessage -> "SagaAlarmClockMessage"
      | :? AppSaga.AppSagaMessage -> "AppSagaMessage"
      | :? AppSaga.AppSagaPersistableEvent -> "AppSagaEvent"
      | :? AppSaga.Saga -> "AppSaga"
      | :? Option<AppSaga.Saga> -> "AppSagaSnapshot"
      | :? ScheduledTransfersLowBalanceMessage ->
         "ScheduledTransfersLowBalanceMessage"
      | :? AccountLoadTestTypes.AccountLoadTestMessage ->
         "AccountLoadTestMessage"
      | :? AccountSeederMessage -> "AccountSeederMessage"
      | :? SchedulerMessage -> "SchedulerMessage"
      | :? AccountClosureMessage -> "AccountClosureActorMessage"
      | :? List<AccountEvent> -> "AccountEventList"
      | :? ParentAccountSnapshot -> "ParentAccountSnapshot"
      | :? Option<ParentAccountSnapshot> -> "ParentAccountSnapshotOption"
      | :? Option<Account> -> "AccountOption"
      | :? List<Account> -> "AccountList"
      | :? Map<AccountId, Account> -> "AccountMap"
      | :? AccountEvent -> "AccountEvent"
      | :? AccountMessage -> "AccountMessage"
      | :? SignalRActor.Msg -> "SignalRMessage"
      | :? CircuitBreakerState -> "CircuitBreakerActorState"
      | :? CircuitBreakerEvent -> "CircuitBreakerEvent"
      | :? CircuitBreakerMessage -> "CircuitBreakerActorMessage"
      | :? BillingCycleMessage -> "BillingCycleActorMessage"
      | :? AutoTransferMessage -> "AutomaticTransferActorMessage"
      | :? ShardEnvelope as e ->
         match e.Message with
         | :? OrgMessage -> "OrgShardEnvelope"
         | :? AccountMessage -> "AccountShardEnvelope"
         | :? EmployeeMessage -> "EmployeeShardEnvelope"
         | :? AppSaga.AppSagaMessage -> "AppSagaShardEnvelope"
         | _ -> raise <| NotImplementedException()
      | :? EmailMessage.EmailMessage -> "EmailMessage"
      | _ -> raise <| NotImplementedException()

   override _.ToBinary(o: obj) =
      match o with
      // Akka ShardRegionProxy defined in Akka.Hosting does not
      // recognize Akkling ShardEnvelope as Akka ShardingEnvelope
      // so need to explicitly add it for message extraction.
      //
      // This would be unnecessary if I was using Akka.Hosting
      // IRequiredActor<> to dependency inject the account shard
      // region proxy.  However, doing so would lose Akkling's typed
      // actor message benefits when forwarding a message from
      // Akka ShardRegionProxy.
      | :? ShardEnvelope as e ->
         match e.Message with
         | :? OrgMessage ->
            JsonSerializer.SerializeToUtf8Bytes(e, Serialization.jsonOptions)
         | :? AccountMessage ->
            JsonSerializer.SerializeToUtf8Bytes(e, Serialization.jsonOptions)
         | :? EmployeeMessage ->
            JsonSerializer.SerializeToUtf8Bytes(e, Serialization.jsonOptions)
         | :? AppSaga.AppSagaMessage ->
            JsonSerializer.SerializeToUtf8Bytes(e, Serialization.jsonOptions)
         | _ -> raise <| NotImplementedException()
      // AccountEvent, EmployeeEvent, & OrgEvent messages to be persisted
      // are wrapped in Akka.Persistence.Extras ConfirmableMessageEnvelope
      // to ensure failed Persist calls are retried & messages received
      // during backoff period are not lost.
      | :? ConfirmableMessageEnvelope
      // AccountEventPersisted messages sent over DistributedPubSub
      // from Account nodes to AccountLoadTestActor on Web node.
      | :? AccountLoadTestTypes.AccountLoadTestMessage
      // ReadModelSyncActor projection offset snapshot
      | :? Lib.ReadModelSyncActor.State
      // Messages from account nodes to AccountSeederActor cluster singleton
      | :? AccountSeederMessage
      // SchedulingActor message for Quartz job persistence.
      | :? SchedulerMessage
      // Messages from SchedulingActor to BillingCycleActor
      | :? BillingCycleMessage
      // Messages from SchedulingActor to AutomaticTransferSchedulingActor
      | :? AutoTransferMessage
      // Messages from sharded account nodes to AccountClosureActor cluster
      // singleton. Also for messages from SchedulingActor to Account Closure Proxy
      | :? AccountClosureMessage
      // AccountMessage.GetEvents response serialized for message sent
      // from account cluster nodes to Web node.
      | :? List<AccountEvent>
      // AccountMessage.GetAccount response serialized for message sent from
      // account cluster nodes to AccountReadModelSync or Web node diagnostic route.
      | :? Option<ParentAccountSnapshot>
      | :? Option<Account>
      // AccountClosureActor persistence snapshot.
      | :? Map<AccountId, Account>
      | :? List<Account>
      | :? CircuitBreakerState
      // Messages sent over DistributedPubSub to CircuitBreakerActor.
      | :? CircuitBreakerMessage
      // CircuitBreakerActor persistence.
      | :? CircuitBreakerEvent
      // Messages sent over DistributedPubSub to SignalRActor on Web node.
      | :? SignalRActor.Msg
      // Most email sending is done from actors within account nodes.
      // Allow emails to also be sent from web nodes.
      | :? EmailMessage.EmailMessage
      // AccountActor persistence snapshot.
      | :? ParentAccountSnapshot as o ->
         JsonSerializer.SerializeToUtf8Bytes(o, Serialization.jsonOptions)
      // AccountEvent actor message replay
      | :? AccountEvent as e ->
         JsonSerializer.SerializeToUtf8Bytes(e, Serialization.jsonOptions)
      | :? AccountMessage as msg ->
         JsonSerializer.SerializeToUtf8Bytes(msg, Serialization.jsonOptions)
      // EmployeeMessage.GetEmployee response serialized for message sent
      // from employee cluster nodes to Web node.
      | :? Option<Employee>
      // EmployeeActor persistence snapshot.
      | :? EmployeeSnapshot as o ->
         JsonSerializer.SerializeToUtf8Bytes(o, Serialization.jsonOptions)
      // EmployeeEvent actor message replay
      | :? EmployeeEvent as e ->
         JsonSerializer.SerializeToUtf8Bytes(e, Serialization.jsonOptions)
      | :? EmployeeMessage as msg ->
         JsonSerializer.SerializeToUtf8Bytes(msg, Serialization.jsonOptions)
      | :? AppSaga.AppSagaMessage as msg ->
         JsonSerializer.SerializeToUtf8Bytes(msg, Serialization.jsonOptions)
      // Saga event persistence
      | :? AppSaga.AppSagaPersistableEvent
      | :? AppSaga.Saga
      // AppSagaActor persistence snapshot.
      | :? Option<AppSaga.Saga>
      // Messages from SchedulingActor to Saga.App/SagaAlarmClockActor
      | :? AppSaga.SagaAlarmClockMessage
      // Messages from SchedulingActor to ScheduledTransfersLowBalanceWarningActor
      | :? ScheduledTransfersLowBalanceMessage
      // OrgMessage.GetCommandApprovalDailyAccrualByInitiatedBy response
      // serialized for message sent from org cluster nodes to web node
      | :? CommandApprovalDailyAccrual
      // KnowYourCustomer third party api message serialized for RabbitMq
      | :? KYCMessage
      // Partner bank third party api message serialized for RabbitMq
      | :? PartnerBankServiceMessage
      // Card issuer third party api message serialized for RabbitMq
      | :? CardIssuerMessage
      // Cached org settings stored in Akka.DistributedData CRDTs
      | :? CachedOrgSettings
      // OrgMessage.GetOrg response serialized for message sent
      // from org cluster nodes to Web node.
      | :? Option<Org>
      // OrgActor persistence snapshot.
      | :? OrgSnapshot as o ->
         JsonSerializer.SerializeToUtf8Bytes(o, Serialization.jsonOptions)
      // OrgEvent actor message replay
      | :? OrgEvent as e ->
         JsonSerializer.SerializeToUtf8Bytes(e, Serialization.jsonOptions)
      | :? OrgMessage as msg ->
         JsonSerializer.SerializeToUtf8Bytes(msg, Serialization.jsonOptions)
      | _ -> raise <| NotImplementedException()

   (*
      NOTE:
      https://getakka.net/articles/serialization/serialization.html

      It's recommended to throw SerializationException in FromBinary(Byte[], String)
      if the manifest is unknown.  This makes it possible to introduce new message
      types and send them to nodes that don't know about them. This is typically
      needed when performing rolling upgrades, i.e. running a cluster with mixed
      versions for while. SerializationException is treated as a transient problem
      in the TCP based remoting layer.  The problem will be logged and message is
      dropped.  Other exceptions will tear down the TCP connection because it can
      be an indication of corrupt bytes from the underlying transport.
   *)
   override _.FromBinary(bytes: byte[], manifest: string) : obj =
      let deserializeToType =
         match manifest with
         | "ConfirmableMessageEnvelope" -> typeof<ConfirmableMessageEnvelope>
         | "AccountLoadTestMessage" ->
            typeof<AccountLoadTestTypes.AccountLoadTestMessage>
         | "ReadModelSyncState" -> typeof<Lib.ReadModelSyncActor.State>
         | "CommandApprovalDailyAccrual" -> typeof<CommandApprovalDailyAccrual>
         | "CachedOrgSettings" -> typeof<CachedOrgSettings>
         | "OrgSnapshot" -> typeof<OrgSnapshot>
         | "OrgOption" -> typeof<Org option>
         | "OrgEvent" -> typeof<OrgEvent>
         | "OrgMessage" -> typeof<OrgMessage>
         | "OrgShardEnvelope" -> typeof<OrgShardEnvelope>
         | "KYCMessage"
         | "Bank.Org.Domain.KYCMessage, Org.Domain" -> typeof<KYCMessage>
         | "PartnerBankServiceMessage"
         | "PartnerBank.Service.Domain+PartnerBankServiceMessage, Transfer.Domain" ->
            typeof<PartnerBankServiceMessage>
         | "CardIssuerServiceMessage"
         | "CardIssuer.Service.Domain+CardIssuerMessage, Employee.Domain" ->
            typeof<CardIssuerMessage>
         | "EmployeeSnapshot" -> typeof<EmployeeSnapshot>
         | "EmployeeOption" -> typeof<Employee option>
         | "EmployeeEvent" -> typeof<EmployeeEvent>
         | "EmployeeMessage" -> typeof<EmployeeMessage>
         | "EmployeeShardEnvelope" -> typeof<EmployeeShardEnvelope>
         | "ParentAccountSnapshot" -> typeof<ParentAccountSnapshot>
         | "ParentAccountSnapshotOption" ->
            typeof<Option<ParentAccountSnapshot>>
         | "AccountOption" -> typeof<Account option>
         | "AccountMap" -> typeof<Map<AccountId, Account>>
         | "AccountList" -> typeof<Account list>
         | "AccountEvent" -> typeof<AccountEvent>
         | "AccountEventList" -> typeof<AccountEvent list>
         | "AccountMessage" -> typeof<AccountMessage>
         | "AccountShardEnvelope" -> typeof<AccountShardEnvelope>
         | "AppSagaMessage" -> typeof<AppSaga.AppSagaMessage>
         | "AppSagaEvent" -> typeof<AppSaga.AppSagaPersistableEvent>
         | "AppSagaShardEnvelope" -> typeof<AppSagaShardEnvelope>
         | "AppSaga" -> typeof<AppSaga.Saga>
         | "AppSagaSnapshot" -> typeof<AppSaga.Saga option>
         | "SagaAlarmClockMessage" -> typeof<AppSaga.SagaAlarmClockMessage>
         | "ScheduledTransfersLowBalanceMessage" ->
            typeof<ScheduledTransfersLowBalanceMessage>
         | "SignalRMessage" -> typeof<SignalRActor.Msg>
         | "CircuitBreakerEvent" -> typeof<CircuitBreakerEvent>
         | "CircuitBreakerActorMessage" -> typeof<CircuitBreakerMessage>
         | "CircuitBreakerActorState" -> typeof<CircuitBreakerState>
         | "BillingCycleActorMessage" -> typeof<BillingCycleMessage>
         | "AutomaticTransferActorMessage" -> typeof<AutoTransferMessage>
         | "AccountClosureActorMessage" -> typeof<AccountClosureMessage>
         | "SchedulerMessage" -> typeof<SchedulerMessage>
         | "AccountSeederMessage" -> typeof<AccountSeederMessage>
         | "EmailMessage+EmailMessage, Notifications.Domain"
         | "EmailMessage" -> typeof<EmailMessage.EmailMessage>
         | _ -> raise <| SerializationException()

      let deserialized =
         JsonSerializer.Deserialize(
            bytes,
            deserializeToType,
            Serialization.jsonOptions
         )

      match deserialized with
      | :? AccountShardEnvelope as e ->
         // NOTE:
         // Deserialize as AccountShardEnvelope instead
         // of ShardEnvelope so envelope.Message
         // is AccountMessage type rather than JObject.
         // Return a ShardEnvelope type so the message
         // gets routed correctly.
         let envelope: ShardEnvelope = {
            EntityId = e.EntityId
            ShardId = e.ShardId
            Message = e.Message
         }

         envelope
      | :? EmployeeShardEnvelope as e ->
         let envelope: ShardEnvelope = {
            EntityId = e.EntityId
            ShardId = e.ShardId
            Message = e.Message
         }

         envelope
      | :? OrgShardEnvelope as e ->
         let envelope: ShardEnvelope = {
            EntityId = e.EntityId
            ShardId = e.ShardId
            Message = e.Message
         }

         envelope
      | :? AppSagaShardEnvelope as e ->
         let envelope: ShardEnvelope = {
            EntityId = e.EntityId
            ShardId = e.ShardId
            Message = e.Message
         }

         envelope
      | _ -> deserialized
