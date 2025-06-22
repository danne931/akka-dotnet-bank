[<RequireQualifiedAccess>]
module GuaranteedDelivery

open System
open Akka.Actor
open Akka.Cluster.Sharding
open Akka.Cluster.Sharding.Delivery
open Akka.Delivery
open Akkling

type Message<'Msg> = { EntityId: Guid; Message: 'Msg }

type ClusterShardingProducerActor<'Msg>() as x =
   inherit ReceiveActor()
   let mutable sendNext: IActorRef = ActorRefs.Nobody

   do
      x.Receive<Message<'Msg>>(fun (msg: Message<'Msg>) ->
         sendNext.Tell(ShardingEnvelope(string msg.EntityId, msg.Message)))

   do
      x.Receive<ShardingProducerController.RequestNext<'Msg>>
         (fun (next: ShardingProducerController.RequestNext<'Msg>) ->
            sendNext <- next.SendNextTo)

type ClusterShardingProducerOptions = {
   System: ActorSystem
   ShardRegion: IActorRef
   ProducerName: string
}

let producer<'Msg>
   (opts: ClusterShardingProducerOptions)
   : IActorRef<Message<'Msg>>
   =
   let system = opts.System
   let clusterMemberAddress = Akka.Cluster.Cluster.Get(system).SelfAddress
   let hash = Akka.Util.MurmurHash.StringHash(string clusterMemberAddress)
   let producerId = $"{opts.ProducerName}{hash}"

   let shardingProducerControllerProps =
      ShardingProducerController.Create<'Msg>(
         producerId,
         opts.ShardRegion,
         Akka.Util.Option<Akka.Actor.Props>.None,
         ShardingProducerController.Settings.Create system
      )

   let producerControllerRef =
      system.ActorOf(
         shardingProducerControllerProps,
         $"sharding-producer-controller-{opts.ProducerName}"
      )

   let producer =
      system.ActorOf(
         Props.Create<ClusterShardingProducerActor<'Msg>>(),
         opts.ProducerName
      )

   let startMsg = new ShardingProducerController.Start<'Msg>(producer)

   producerControllerRef.Tell startMsg

   typed producer

/// ShardingConsumerController guarantees processing of messages,
/// even across process restarts, shutdowns or shard rebalancing.
let consumer<'Msg> (system: ActorSystem) (fac: IActorRef -> Props) : Props =
   ShardingConsumerController.Create<'Msg>(
      fac,
      ShardingConsumerController.Settings.Create system
   )

/// Wrap a message in an envelope for Akka.Delivery to deliver to a
/// cluster sharded entity actor via the ClusterShardingProducerActor.
let message (entityId: Guid) (msg: 'Msg) : Message<'Msg> = {
   EntityId = entityId
   Message = msg
}

/// Notify Akka Guaranteed Delivery Controller of successful
/// delivery to a cluster sharded entity actor.
let ack (msg: ConsumerController.Delivery<'Msg>) =
   msg.ConfirmTo.Tell ConsumerController.Confirmed.Instance
