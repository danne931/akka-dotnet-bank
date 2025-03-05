module Lib.Rabbit

open System
open Akka.Streams.Amqp.RabbitMq

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
