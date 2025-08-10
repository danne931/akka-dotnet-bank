module BankActorRegistry

open Akkling
open Akkling.Cluster.Sharding
open Akka.Actor
open Akka.Hosting

open Lib.SharedTypes
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open TransferMessages
open CardIssuer.Service.Domain
open PartnerBank.Service.Domain
open BillingStatement
open Bank.Scheduler

[<RequireQualifiedAccess>]
module ActorMarker =
   type Org() = class end

   type OrgReadModelSync() = class end

   type OrgGuaranteedDeliveryProducer() = class end

   /// Singleton consumes partner bank messages off RabbitMq
   type PartnerBankService() = class end

   /// Enqueues partner bank messages into RabbitMq
   type PartnerBankServiceProducer() = class end

   /// Singleton consumes know-your-customer messages off RabbitMq
   type KYCService() = class end

   /// Enqueues know-your-customer messages into RabbitMq
   type KYCServiceProducer() = class end

   /// Singleton consumes card issuer messages off RabbitMq
   type CardIssuerService() = class end

   /// Enqueues card issuer messages into RabbitMq
   type CardIssuerServiceProducer() = class end

   type CircuitBreaker() = class end

   type AccountLoadTest() = class end

   type Auditor() = class end

   type AccountSeeder() = class end

   /// Singleton Proxy to forward emails from web node
   type EmailProxy() = class end

   /// Singleton consumes email messages off RabbitMq
   type Email() = class end

   /// Enqueues email messages into RabbitMq
   type EmailProducer() = class end

   type AccountClosure() = class end

   /// Singleton consumes domestic transfer messages off RabbitMq
   type DomesticTransfer() = class end

   /// Enqueues domestic transfer messages into RabbitMq
   type DomesticTransferProducer() = class end

   type AutoTransferScheduling() = class end

   type ScheduledTransfersLowBalanceWarning() = class end

   type BillingCycle() = class end

   type BillingStatement() = class end

   type Saga() = class end

   type SagaGuaranteedDeliveryProducer() = class end

   type SagaReadModelSync() = class end

   type SagaAlarmClock() = class end

   type Account() = class end

   type AccountGuaranteedDeliveryProducer() = class end

   type AccountReadModelSync() = class end

   type Scheduling() = class end

   type Employee() = class end

   type EmployeeGuaranteedDeliveryProducer() = class end

   type EmployeeReadModelSync() = class end

type IEmailActor =
   abstract EmailActor: unit -> IActorRef<Email.EmailMessage>

type IEmailProxyActor =
   abstract EmailProxyActor: unit -> IActorRef<Email.EmailMessage>

type IOrgActor =
   abstract OrgActor: OrgId -> IEntityRef<OrgMessage>

type IOrgGuaranteedDeliveryActor =
   abstract OrgGuaranteedDeliveryActor:
      unit -> IActorRef<GuaranteedDelivery.Message<OrgMessage>>

type IEmployeeActor =
   abstract EmployeeActor: EmployeeId -> IEntityRef<EmployeeMessage>

type IEmployeeGuaranteedDeliveryActor =
   abstract EmployeeGuaranteedDeliveryActor:
      unit -> IActorRef<GuaranteedDelivery.Message<EmployeeMessage>>

type IAccountActor =
   abstract AccountActor: ParentAccountId -> IEntityRef<AccountMessage>

type IAccountGuaranteedDeliveryActor =
   abstract AccountGuaranteedDeliveryActor:
      unit -> IActorRef<GuaranteedDelivery.Message<AccountMessage>>

type IDomesticTransferActor =
   abstract DomesticTransferActor:
      unit -> IActorRef<DomesticTransferServiceMessage>

type IAccountClosureActor =
   abstract AccountClosureActor: unit -> IActorRef<AccountClosureMessage>

/// Send a message to a cluster sharded saga actor with AtMostOnceDelivery
type ISagaActor =
   abstract SagaActor: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>

/// Send a message to a cluster sharded saga actor with AtLeastOnceDelivery
type ISagaGuaranteedDeliveryActor =
   abstract SagaGuaranteedDeliveryActor:
      unit -> IActorRef<GuaranteedDelivery.Message<AppSaga.AppSagaMessage>>

type IBillingStatementActor =
   abstract BillingStatementActor:
      unit -> IActorRef<BillingStatement.BillingStatementMessage>

type ICardIssuerServiceActor =
   abstract CardIssuerServiceActor: unit -> IActorRef<CardIssuerMessage>

type ISchedulerActor =
   abstract SchedulerActor: unit -> IActorRef<SchedulerMessage>

type ICircuitBreakerActor =
   abstract CircuitBreakerActor:
      unit -> IActorRef<Lib.CircuitBreaker.CircuitBreakerMessage>

type IKYCServiceActor =
   abstract KYCServiceActor: unit -> IActorRef<KYCMessage>

type IPartnerBankServiceActor =
   abstract PartnerBankServiceActor:
      unit -> IActorRef<PartnerBankServiceMessage>

type IBillingCycleActor =
   abstract BillingCycleActor: unit -> IActorRef<BillingCycleMessage>

type BankActorRegistry(system: ActorSystem) =
   let registry = ActorRegistry.For(system)

   interface IEmailActor with
      member _.EmailActor() =
         registry.Get<ActorMarker.EmailProducer>() |> typed

   interface IEmailProxyActor with
      member _.EmailProxyActor() =
         registry.Get<ActorMarker.EmailProxy>() |> typed

   interface IOrgActor with
      member _.OrgActor orgId =
         ActorUtil.getEntityRef
            system
            ActorUtil.ClusterMetadata.orgShardRegion
            (OrgId.get orgId)

   interface IOrgGuaranteedDeliveryActor with
      member _.OrgGuaranteedDeliveryActor() =
         registry.Get<ActorMarker.OrgGuaranteedDeliveryProducer>() |> typed

   interface IEmployeeActor with
      member _.EmployeeActor employeeId =
         ActorUtil.getEntityRef
            system
            ActorUtil.ClusterMetadata.employeeShardRegion
            (EmployeeId.get employeeId)

   interface IEmployeeGuaranteedDeliveryActor with
      member _.EmployeeGuaranteedDeliveryActor() =
         registry.Get<ActorMarker.EmployeeGuaranteedDeliveryProducer>() |> typed

   interface IAccountActor with
      member _.AccountActor parentAccountId =
         ActorUtil.getEntityRef
            system
            ActorUtil.ClusterMetadata.accountShardRegion
            (ParentAccountId.get parentAccountId)

   interface IAccountGuaranteedDeliveryActor with
      member _.AccountGuaranteedDeliveryActor() =
         registry.Get<ActorMarker.AccountGuaranteedDeliveryProducer>() |> typed

   interface IAccountClosureActor with
      member _.AccountClosureActor() =
         registry.Get<ActorMarker.AccountClosure>() |> typed

   interface IDomesticTransferActor with
      member _.DomesticTransferActor() =
         registry.Get<ActorMarker.DomesticTransferProducer>() |> typed

   interface ISagaActor with
      member _.SagaActor corrId =
         ActorUtil.getEntityRef
            system
            ActorUtil.ClusterMetadata.sagaShardRegion
            (CorrelationId.get corrId)

   interface ISagaGuaranteedDeliveryActor with
      member _.SagaGuaranteedDeliveryActor() =
         registry.Get<ActorMarker.SagaGuaranteedDeliveryProducer>() |> typed

   interface IBillingStatementActor with
      member _.BillingStatementActor() =
         registry.Get<ActorMarker.BillingStatement>() |> typed

   interface ICardIssuerServiceActor with
      member _.CardIssuerServiceActor() =
         registry.Get<ActorMarker.CardIssuerServiceProducer>() |> typed

   interface ISchedulerActor with
      member _.SchedulerActor() =
         registry.Get<ActorMarker.Scheduling>() |> typed

   interface ICircuitBreakerActor with
      member _.CircuitBreakerActor() =
         registry.Get<ActorMarker.CircuitBreaker>() |> typed

   interface IKYCServiceActor with
      member _.KYCServiceActor() =
         registry.Get<ActorMarker.KYCServiceProducer>() |> typed

   interface IPartnerBankServiceActor with
      member _.PartnerBankServiceActor() =
         registry.Get<ActorMarker.PartnerBankServiceProducer>() |> typed

   interface IBillingCycleActor with
      member _.BillingCycleActor() =
         registry.Get<ActorMarker.BillingCycle>() |> typed
