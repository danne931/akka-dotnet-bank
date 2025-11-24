module AkkaDeadLetterMonitor

open Akka.Actor
open Akka.Event
open Akka.Persistence
open Akkling

// TODO: Do more than just printing dead letter messages.

let actorProps (system: ActorSystem) =
   let handler (ctx: Actor<_>) =
      EventStreaming.subscribe ctx.Self system.EventStream |> ignore

      logInfo ctx "Auditor subscribed to system event stream"

      let logError = logError ctx

      let rec loop () = actor {
         let! (deadLetter: AllDeadLetters) = ctx.Receive()

         let msg = string deadLetter

         match deadLetter with
         | :? DeadLetter as msg ->
            let msg = $"<DeadLetter> {msg}"

            match deadLetter.Message with
            | :? SaveSnapshotSuccess -> logWarning ctx msg
            | _ -> logError msg
         // Envelope published to EventStream for messages dropped
         // due to overfull queues or routers with no routees.
         | :? Dropped ->
            let logMsg = $"<Dropped> {msg}"
            logError logMsg
         // Published to EventStream when an Actor receives a
         // message it does not understand.
         | :? UnhandledMessage ->
            let logMsg = $"<UnhandledMessage> {msg}"
            logError logMsg
         // Internal messages for which ending up as dead-letter is
         // both expected and harmless.
         | :? SuppressedDeadLetter
         | _ -> ()

         return! loop ()
      }

      loop ()

   props handler
