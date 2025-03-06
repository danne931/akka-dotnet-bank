[<RequireQualifiedAccess>]
module DomesticTransferProducerActor

open Akka.Actor
open Akka.Hosting
open Akka.Streams.Amqp.RabbitMq
open Akka.Streams.Amqp.RabbitMq.Dsl
open Akkling
open Akkling.Streams

open ActorUtil
open Bank.Transfer.Domain
open Lib.Types

let private serializeForRabbit
   (system: ActorSystem)
   (msg: DomesticTransferMessage)
   : Result<OutgoingMessage, exn>
   =
   try
      let serializer =
         system.Serialization.FindSerializerForType(
            typeof<DomesticTransferMessage>
         )

      let serializedMsg = serializer.ToBinary msg
      let byteString = Akka.IO.ByteString.FromBytes(serializedMsg)
      let msg = OutgoingMessage(byteString, immediate = true, mandatory = true)
      Ok msg
   with ex ->
      Error ex

// Serialize message for rabbit and pass on for rabbit enqueue
let actorProps system targetRef : Props<DomesticTransferMessage> =
   let handler targetRef (ctx: Actor<DomesticTransferMessage>) =
      let rec loop () = actor {
         let! msg = ctx.Receive()

         match serializeForRabbit system msg with
         | Error e ->
            logError
               ctx
               $"DomesticTransferProducer serialization broken {e.Message}"

            return unhandled ()
         | Ok msg ->
            targetRef <! msg
            return! loop ()
      }

      loop ()

   props (handler targetRef)

let start
   (system: ActorSystem)
   (conn: AmqpConnectionDetails)
   (queueSettings: RabbitQueueSettings)
   : IActorRef<DomesticTransferMessage>
   =
   let spawnActor targetRef =
      spawnAnonymous system (actorProps system targetRef)

   let mat = system.Materializer()

   let sink =
      Lib.Rabbit.createSinkSettings queueSettings.Name conn |> AmqpSink.Create

   Source.actorRef Akka.Streams.OverflowStrategy.DropNew 1000
   |> Source.mapMatValue spawnActor
   |> Source.toMat sink Keep.left
   |> Graph.run mat

let get (system: ActorSystem) : IActorRef<DomesticTransferMessage> =
   typed
   <| ActorRegistry
      .For(system)
      .Get<ActorMetadata.DomesticTransferProducerMarker>()
