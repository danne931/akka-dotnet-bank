module Lib.Queue

open System
open System.Threading.Tasks
open Akka.Streams.Amqp.RabbitMq
open Akka.Streams.Amqp.RabbitMq.Dsl
open Akka.Actor
open Akkling
open Akkling.Streams

open Lib.Types
open Lib.SharedTypes
open Lib.CircuitBreaker

type QueueConnectionDetails = AmqpConnectionDetails

let createConnection
   (settings: QueueConnectionEnvConfig)
   : QueueConnectionDetails
   =
   QueueConnectionDetails
      .Create(settings.Host, settings.Port)
      .WithCredentials(
         AmqpCredentials.Create(settings.Username, settings.Password)
      )
      .WithAutomaticRecoveryEnabled(true)
      .WithNetworkRecoveryInterval(TimeSpan.FromSeconds(1.))

let createQueueDeclaration (queueName: string) =
   QueueDeclaration.Create(queueName).WithDurable(true).WithAutoDelete(false)

let createSourceSettings (queueName: string) (conn: QueueConnectionDetails) =
   NamedQueueSourceSettings
      .Create(conn, queueName)
      .WithDeclarations(createQueueDeclaration queueName)

let createSinkSettings (queueName: string) (conn: QueueConnectionDetails) =
   AmqpSinkSettings
      .Create(conn)
      .WithRoutingKey(queueName)
      .WithDeclarations(createQueueDeclaration queueName)

let private serializeForQueue
   (system: ActorSystem)
   (msg: 'QueueMessage)
   : Result<OutgoingMessage, exn>
   =
   try
      typeof<'QueueMessage>
      |> system.Serialization.FindSerializerForType
      |> fun serializer -> serializer.ToBinary msg
      |> Akka.IO.ByteString.FromBytes
      |> fun s -> OutgoingMessage(s, immediate = true, mandatory = true)
      |> Ok
   with ex ->
      Error ex

// Serialize message for rabbit and pass on for rabbit enqueue
let private producerActorProps<'QueueMessage>
   system
   targetRef
   : Props<'QueueMessage>
   =
   let handler targetRef (ctx: Actor<'QueueMessage>) =
      let rec loop () = actor {
         let! msg = ctx.Receive()

         match serializeForQueue system msg with
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
let startProducer<'QueueMessage>
   (system: ActorSystem)
   (conn: QueueConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   : IActorRef<'QueueMessage>
   =
   let spawnActor targetRef =
      let p = producerActorProps<'QueueMessage> system targetRef
      spawnAnonymous system p

   let sinkSettings = createSinkSettings queueSettings.Name conn

   let sink =
      Akka.Streams.Dsl.RestartSink.WithBackoff(
         (fun () -> AmqpSink.Create sinkSettings),
         streamRestartSettings
      )

   Source.actorRef Akka.Streams.OverflowStrategy.DropNew 1000
   |> Source.mapMatValue spawnActor
   |> Source.toMat sink Keep.left
   |> Graph.run (system.Materializer())

// Deserialize the rabbit message envelope into the domain 'QueueMessage
let private deserializeFromQueue<'QueueMessage>
   (system: ActorSystem)
   (msg: CommittableIncomingMessage)
   : Result<'QueueMessage, exn>
   =
   try
      let serializer =
         system.Serialization.FindSerializerForType typeof<'QueueMessage>

      let byteArr = msg.Message.Bytes.ToArray()
      serializer.FromBinary<'QueueMessage> byteArr |> Ok
   with ex ->
      Error ex

type QueueConsumerOptions<'QueueMessage, 'ActionRequest, 'ActionResponse> = {
   Service: CircuitBreakerService
   onCircuitBreakerEvent: CircuitBreakerEvent -> unit
   /// A rabbit message to a protected action request.
   queueMessageToActionRequest:
      Actor<obj> -> 'QueueMessage -> Task<'ActionRequest option>
   /// The action to protect with circuit breaker
   protectedAction:
      Actor<obj> -> 'ActionRequest -> Task<Result<'ActionResponse, Err>>
   /// Allow user to specify stream-based post-processing
   /// for successful 'ActionResponse items.
   onSuccessFlow:
      Akka.Streams.Dsl.Flow<
         Actor<obj> * 'QueueMessage * 'ActionResponse,
         'ActionResponse,
         unit
       > option
}

let private initConsumerStream
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (opts: QueueConsumerOptions<'QueueMessage, 'ActionRequest, 'ActionResponse>)
   (killSwitch: Akka.Streams.SharedKillSwitch)
   (mailbox: Actor<obj>)
   (maxParallel: int)
   =
   let logError, logWarning = logError mailbox, logWarning mailbox

   logInfo
      mailbox
      $"({queueSettings.Name}) Init rabbit consumer stream with buffer size {maxParallel}"

   let source =
      let srcSettings = createSourceSettings queueSettings.Name queueConnection

      let src =
         AmqpSource.CommittableSource(srcSettings, bufferSize = maxParallel)

      Akka.Streams.Dsl.RestartSource.WithBackoff(
         (fun () -> src),
         streamRestartSettings
      )

   let sink =
      Sink.onComplete (function
         | None ->
            logWarning $"Rabbit Consumer Stream Completed ({opts.Service})"
         | Some err ->
            logError
               $"Rabbit Consumer Stream Completed With Error ({opts.Service}): {err}")

   let materializer =
      let settings =
         Akka.Streams.ActorMaterializerSettings
            .Create(mailbox.System)
            .WithSupervisionStrategy(fun e ->
               logError e.Message
               Akka.Streams.Supervision.Directive.Resume)

      mailbox.System.Materializer settings

   source
   |> Source.viaMat (killSwitch.Flow()) Keep.none
   // Deserialize dequeued Rabbit message envelopes into 'QueueMessage
   // and map the 'QueueMessage to a 'ActionRequest object.
   |> Source.asyncMap
      maxParallel
      (fun (committable: CommittableIncomingMessage) -> async {
         if breaker.IsOpen then
            logWarning
               $"CircuitBreakerOpen ({opts.Service}): Will dequeue on HalfOpen"

            do! committable.Nack() |> Async.AwaitTask
            return None
         else
            match deserializeFromQueue mailbox.System committable with
            | Error e ->
               logError $"Deserialization Broken ({opts.Service}): {e}"
               do! committable.Nack() |> Async.AwaitTask
               return None
            | Ok msg ->
               let! actionRequestObject =
                  opts.queueMessageToActionRequest mailbox msg
                  |> Async.AwaitTask

               match actionRequestObject with
               | None ->
                  do! committable.Nack() |> Async.AwaitTask
                  return None
               | Some request -> return Some(committable, msg, request)
      })
   // Filter out items that were nacked due to circuit breaker being open,
   // deserialization errors or errors resolving 'ActionRequest from 'QueueMessage.
   |> Source.choose id
   // Initiate action with circuit breaker integration.  If the action
   // fails then the message will be Nacked and attempted later when
   // the breaker transitions to HalfOpen or Closed.
   |> Source.asyncMap
      maxParallel
      (fun
           (committable: CommittableIncomingMessage,
            queueMessage: 'QueueMessage,
            request: 'ActionRequest) -> async {
         let mutable response
            : (Actor<obj> * 'QueueMessage * 'ActionResponse) option =
            None

         if breaker.IsOpen then
            logWarning
               $"CircuitBreakerOpen ({opts.Service}): Will dequeue on HalfOpen"

            do! committable.Nack() |> Async.AwaitTask
         else
            try
               do!
                  breaker.WithCircuitBreaker
                     (fun (_: Threading.CancellationToken) -> task {
                        match! opts.protectedAction mailbox request with
                        | Ok res ->
                           response <- Some(mailbox, queueMessage, res)
                           return ()
                        // Raise exception to trip the circuit breaker
                        | Error e -> return failwith (string e)
                     })
                  |> Async.AwaitTask

               do! committable.Ack() |> Async.AwaitTask
            with e ->
               do! committable.Nack() |> Async.AwaitTask

               logError
                  $"Error Processing Protected Action ({opts.Service}): {request} {e.Message}"

         return response
      })
   |> Source.choose id
   |> Source.via (
      opts.onSuccessFlow
      |> Option.defaultValue (
         Flow.map (fun (_, _, response) -> response) Flow.id
      )
   )
   |> Source.map (fun _ -> Akka.NotUsed.Instance)
   |> Source.toMat sink Keep.none
   |> Graph.run materializer

type private CircuitBreakerMessage =
   | BreakerHalfOpen
   | BreakerClosed

/// Initialize a stream which dequeues 'QueueMessages off RabbitMq, maps those
/// messages into 'ActionRequests, and then invokes an action for each
/// 'ActionRequest with circuit breaker integration over the action invocation.
let consumerActorProps
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (opts: QueueConsumerOptions<'QueueMessage, 'ActionRequest, 'ActionResponse>)
   : Props<obj>
   =
   let createKillSwitch () =
      KillSwitch.shared $"CircuitBreakerOpen-{opts.Service}"

   let mutable killSwitch = createKillSwitch ()

   let initStream =
      initConsumerStream
         queueConnection
         queueSettings
         streamRestartSettings
         breaker
         opts

   let rec init (ctx: Actor<obj>) = actor {
      let! msg = ctx.Receive()

      breaker.OnHalfOpen(fun () ->
         ctx.Self <! BreakerHalfOpen

         opts.onCircuitBreakerEvent {
            Service = opts.Service
            Status = CircuitBreakerStatus.HalfOpen
            Timestamp = DateTime.UtcNow
         })
      |> ignore

      breaker.OnClose(fun () ->
         ctx.Self <! BreakerClosed

         opts.onCircuitBreakerEvent {
            Service = opts.Service
            Status = CircuitBreakerStatus.Closed
            Timestamp = DateTime.UtcNow
         })
      |> ignore

      breaker.OnOpen(fun () ->
         logWarning
            ctx
            $"CircuitBreakerOpen ({opts.Service}): Pause Processing"

         // Shutdown processing of messages off RabbitMq until
         // BreakerHalfOpen.  When BreakerHalfOpen the stream will
         // be reinitialized with buffer size 1 so will attempt to process
         // just 1 message off the queue.  If successful then the stream
         // will return to processing messages normally.
         killSwitch.Shutdown()

         // Refresh the kill switch for stream reinitialization.
         // Otherwise, it appears the stream will never start back up on
         // CircuitBreakerClosed due to being configured with a killswitch
         // that has already been shut down.
         killSwitch <- createKillSwitch ()

         opts.onCircuitBreakerEvent {
            Service = opts.Service
            Status = CircuitBreakerStatus.Open
            Timestamp = DateTime.UtcNow
         })
      |> ignore

      match msg with
      | LifecycleEvent e ->
         match e with
         | PreStart ->
            logInfo
               ctx
               $"PreStart ({opts.Service}): Initialize Rabbit Consumer Queue Source"

            return! processing ctx queueSettings.MaxParallelism
         | _ -> return ignored ()
      | msg ->
         logError ctx $"Unknown Message ({opts.Service}): {msg}"
         return unhandled ()
   }

   and processing (ctx: Actor<obj>) (limit: int) =
      initStream killSwitch ctx limit

      actor {
         let! msg = ctx.Receive()

         match msg with
         | :? CircuitBreakerMessage as msg ->
            match msg with
            | BreakerHalfOpen ->
               logWarning
                  ctx
                  $"CircuitBreakerHalfOpen ({opts.Service}): Try processing one"

               return! processing ctx 1
            | BreakerClosed ->
               logWarning
                  ctx
                  $"CircuitBreakerClosed ({opts.Service}): Resume processing"

               return! processing ctx queueSettings.MaxParallelism
         | LifecycleEvent e ->
            match e with
            | PostStop ->
               logWarning
                  ctx
                  $"PostStop RabbitMq Queue Consumer ({opts.Service})"

               killSwitch.Shutdown()
               return ignored ()
            | _ -> return ignored ()
         | msg ->
            logError ctx $"Unknown Message ({opts.Service}): {msg}"
            return unhandled ()
      }

   props init
