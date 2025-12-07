[<RequireQualifiedAccess>]
module GuaranteedDelivery

open System
open Akka.Actor
open Akka.Cluster.Sharding
open Akka.Cluster.Sharding.Delivery
open Akka.Delivery
open Akkling

type Message<'Msg> = {
   EntityId: Guid
   Message: 'Msg
   Timestamp: DateTime
}

type ClusterShardingProducerActor<'Msg>() as x =
   inherit UntypedActor()

   let mutable sendNext: IActorRef = ActorRefs.Nobody
   let mutable stash: IStash = null
   let mutable currentBehavior = x.Idle

   interface IWithUnboundedStash with
      member _.Stash
         with get () = stash
         and set value = stash <- value

   member x.Active(msg: obj) =
      match msg with
      | :? Message<'Msg> as msg ->
         sendNext.Tell(ShardingEnvelope(string msg.EntityId, msg.Message))
         currentBehavior <- x.Idle
      | :? ShardingProducerController.RequestNext<'Msg> as next ->
         sendNext <- next.SendNextTo
      | _ -> x.Unhandled(msg)

   member x.Idle(msg: obj) =
      match msg with
      | :? Message<'Msg> -> stash.Stash()
      | :? ShardingProducerController.RequestNext<'Msg> as next ->
         sendNext <- next.SendNextTo
         currentBehavior <- x.Active
         stash.UnstashAll()
      | _ -> x.Unhandled(msg)

   override _.OnReceive(msg) = currentBehavior msg

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
   Timestamp = DateTime.UtcNow
}

/// Notify Akka Guaranteed Delivery Controller of successful
/// delivery to a cluster sharded entity actor.
let ack (msg: ConsumerController.Delivery<'Msg>) =
   msg.ConfirmTo.Tell ConsumerController.Confirmed.Instance
