namespace Scheduler.Infrastructure

open System
open System.Text.Json
open System.Runtime.Serialization
open Akka.Serialization
open Akka.Actor
open Lib.SharedTypes

open Bank.Account.Domain
open BillingStatement
open Bank.Scheduler
open TransferMessages

// NOTE:
// Using a QuartzMessageEnvelope type for messages serialized with
// Akka.Quartz.Actor until serialization PR merged.  Akka.Quartz.Actor
// is always passing in Object as manifest unless this PR merged:
// https://github.com/akkadotnet/Akka.Quartz.Actor/pull/335

type private QuartzSagaMessageEnvelope = {
   Manifest: string
   Message: {|
      SagaMessage: AppSaga.AppSagaMessage
      CorrelationId: CorrelationId
   |}
}

type private QuartzSagaAlarmClockMessageEnvelope = {
   Manifest: string
   Message: SagaAlarmClockActor.Message
}

type private QuartzAccountMessageEnvelope = {
   Manifest: string
   Message: {|
      AccountMessage: AccountMessage
      AccountId: AccountId
   |}
}

type private QuartzBillingMessageEnvelope = {
   Manifest: string
   Message: BillingCycleMessage
}

type private QuartzBalanceManagementMessageEnvelope = {
   Manifest: string
   Message: AutoTransferMessage
}

type private QuartzScheduledTransfersLowBalanceMessageEnvelope = {
   Manifest: string
   Message: ScheduledTransfersLowBalanceMessage
}

type private QuartzAccountClosureMessageEnvelope = {
   Manifest: string
   Message: AccountClosureMessage
}

type private SchedulingActorMessageEnvelope = {
   Manifest: string
   Message: SchedulerMessage
}

type QuartzSerializer(system: ExtendedActorSystem) =
   inherit Serializer(system)

   static member Name = "quartz-serializer"

   override x.Identifier = 932

   override x.IncludeManifest = true

   override x.ToBinary(o: obj) =
      match o with
      | :? QuartzMessageEnvelope as msg ->
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
   override x.FromBinary(bytes: byte[], manifest: Type) : obj =
      let deserialized =
         try
            JsonSerializer.Deserialize<QuartzMessageEnvelope>(
               bytes,
               Serialization.jsonOptions
            )
         with _ ->
            raise <| SerializationException()

      match deserialized.Manifest with
      | "AccountClosureActorMessage" ->
         let deseri =
            JsonSerializer.Deserialize<QuartzAccountClosureMessageEnvelope>(
               bytes,
               Serialization.jsonOptions
            )

         deseri.Message
      | "BillingCycleActorMessage" ->
         let deseri =
            JsonSerializer.Deserialize<QuartzBillingMessageEnvelope>(
               bytes,
               Serialization.jsonOptions
            )

         deseri.Message
      | "BalanceManagementMessage" ->
         let deseri =
            JsonSerializer.Deserialize<QuartzBalanceManagementMessageEnvelope>(
               bytes,
               Serialization.jsonOptions
            )

         deseri.Message
      | "SchedulingActorMessage" ->
         let deseri =
            JsonSerializer.Deserialize<SchedulingActorMessageEnvelope>(
               bytes,
               Serialization.jsonOptions
            )

         deseri.Message
      | "AccountActorMessage" ->
         let deseri =
            JsonSerializer.Deserialize<QuartzAccountMessageEnvelope>(
               bytes,
               Serialization.jsonOptions
            )

         let entityId = string deseri.Message.AccountId

         let msg: Akkling.Cluster.Sharding.ShardEnvelope = {
            ShardId =
               ActorUtil
                  .ClusterMetadata
                  .accountShardRegion
                  .messageExtractor
                  .ShardId(entityId)
            EntityId = entityId
            Message = deseri.Message.AccountMessage
         }

         msg
      | "SagaAlarmClockActorMessage" ->
         let deseri =
            JsonSerializer.Deserialize<QuartzSagaAlarmClockMessageEnvelope>(
               bytes,
               Serialization.jsonOptions
            )

         deseri.Message
      | "ScheduledTransfersLowBalanceMessage" ->
         let deseri =
            JsonSerializer.Deserialize<
               QuartzScheduledTransfersLowBalanceMessageEnvelope
             >(
               bytes,
               Serialization.jsonOptions
            )

         deseri.Message
      | _ -> raise <| SerializationException()
