module Lib.Rabbit

open System
open Akka.Streams.Amqp.RabbitMq
open Akka.Streams.Amqp.RabbitMq.Dsl
open Akka.Actor
open Akkling
open Akkling.Streams

open Lib.Types

let createConnection
   (settings: RabbitConnectionSettings)
   : AmqpConnectionDetails
   =
   AmqpConnectionDetails
      .Create(settings.Host, settings.Port)
      .WithCredentials(
         AmqpCredentials.Create(settings.Username, settings.Password)
      )
      .WithAutomaticRecoveryEnabled(true)
      .WithNetworkRecoveryInterval(TimeSpan.FromSeconds(1))

let createQueueDeclaration (queueName: string) =
   QueueDeclaration.Create(queueName).WithDurable(true).WithAutoDelete(false)

let createSourceSettings (queueName: string) (conn: AmqpConnectionDetails) =
   NamedQueueSourceSettings
      .Create(conn, queueName)
      .WithDeclarations(createQueueDeclaration queueName)

let createSinkSettings (queueName: string) (conn: AmqpConnectionDetails) =
   AmqpSinkSettings
      .Create(conn)
      .WithRoutingKey(queueName)
      .WithDeclarations(createQueueDeclaration queueName)

// Serialize message for RabbitMq
let private serializeForRabbit
   (system: ActorSystem)
   (msg: 'Message)
   : Result<OutgoingMessage, exn>
   =
   try
      typeof<'Message>
      |> system.Serialization.FindSerializerForType
      |> fun serializer -> serializer.ToBinary msg
      |> Akka.IO.ByteString.FromBytes
      |> fun s -> OutgoingMessage(s, immediate = true, mandatory = true)
      |> Ok
   with ex ->
      Error ex

// Serialize message for rabbit and pass on for rabbit enqueue
let private producerActorProps system targetRef : Props<'Message> =
   let handler targetRef (ctx: Actor<'Message>) =
      let rec loop () = actor {
         let! msg = ctx.Receive()

         match serializeForRabbit system msg with
         | Error e ->
            logError ctx $"Producer serialization broken {e.Message}"

            return unhandled ()
         | Ok msg ->
            targetRef <! msg
            return! loop ()
      }

      loop ()

   props (handler targetRef)

/// Actor enqueues messages of a given message type to RabbitMq
/// for a corresponding rabbit consumer actor to dequeue.
let startProducer
   (system: ActorSystem)
   (conn: AmqpConnectionDetails)
   (queueSettings: RabbitQueueSettings)
   : IActorRef<'Message>
   =
   let spawnActor targetRef =
      spawnAnonymous system (producerActorProps system targetRef)

   let sink = createSinkSettings queueSettings.Name conn |> AmqpSink.Create

   Source.actorRef Akka.Streams.OverflowStrategy.DropNew 1000
   |> Source.mapMatValue spawnActor
   |> Source.toMat sink Keep.left
   |> Graph.run (system.Materializer())
