module Lib.ReadModelSyncActor

open System
open System.Threading.Tasks
open Akka.Persistence
open Akka.Streams.Dsl
open Akkling
open Akkling.Persistence
open Akkling.Streams
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.Types
open ActorUtil

type ReadModelUpsert<'TEvent> = 'TEvent list -> Result<int list, Err> Task

/// If persisting an aggregate read model is necessary, use of
/// this config expects the consumer to be able to update an
/// aggregate by database update statements derived from events.
type Config<'TEvent> = {
   EventJournalTag: string
   Chunking: StreamChunkingEnvConfig
   RestartSettings: Akka.Streams.RestartSettings
   UpsertReadModels: ReadModelUpsert<'TEvent>
}

type ReadModelUpsert<'TAggregate, 'TEvent> =
   'TAggregate list * 'TEvent list -> Result<int list, Err> Task

/// Use of this config assumes an aggregate read model needs to
/// be persisted but the consumer is unable to update an aggregate
/// by deriving database update statements from events.
/// The consumer must supply a GetAggregate function so ReadModelSync
/// can fetch the latest aggregate state from the actor before
/// passing both the aggregate and events to your UpsertReadModels
/// implementation.
type Config<'TAggregate, 'TEvent> = {
   EventJournalTag: string
   GetAggregateIdFromEvent: 'TEvent -> Guid
   GetAggregate: Guid -> 'TAggregate option Task
   Chunking: StreamChunkingEnvConfig
   RestartSettings: Akka.Streams.RestartSettings
   UpsertReadModels: ReadModelUpsert<'TAggregate, 'TEvent>
}

type State = {
   Offset: Akka.Persistence.Query.Sequence
}

type Message = SaveOffset of Akka.Persistence.Query.Sequence

let initReadJournalSource
   (mailbox: Eventsourced<obj>)
   (state: State)
   (chunking: StreamChunkingEnvConfig)
   (restartSettings: Akka.Streams.RestartSettings)
   (eventJournalTag: string)
   : Source<'TEvent list * Query.Sequence option, Akka.NotUsed>
   =
   let source =
      (readJournal mailbox.System).EventsByTag(eventJournalTag, state.Offset)
      |> Source.groupedWithin chunking.Size chunking.Duration
      |> Source.map (fun evtSeq ->
         let evts: 'TEvent list =
            evtSeq |> Seq.map (fun env -> unbox env.Event) |> List.ofSeq

         let offset =
            match (Seq.last evtSeq).Offset with
            | :? Query.Sequence as offset -> Some offset
            | _ -> None

         evts, offset)

   RestartSource.WithBackoff((fun _ -> source), restartSettings)

let initSink mailbox name =
   let name = name + "-ReadModelSync"

   Sink.onComplete (function
      | None -> logWarning mailbox $"Rabbit Consumer Stream Completed ({name})"
      | Some err ->
         logError
            mailbox
            $"Rabbit Consumer Stream Completed With Error ({name}): {err}")

let bulkWriteFlow (mailbox: Eventsourced<obj>) name persist =
   Flow.id
   |> Flow.taskMap
      1
      (fun (statements: 'T list, offset: Query.Sequence option) -> task {
         let! res = persist statements

         return
            match res with
            | Ok res ->
               logInfo mailbox $"Saved read models for journal {name} {res}"

               offset
               |> Option.iter (fun offset -> mailbox.Self <! SaveOffset offset)

               statements
            | Error e -> failwith $"Bulk insert failed for {name}: {e}"
      })

let startProjection<'TEvent>
   (conf: Config<'TEvent>)
   (mailbox: Eventsourced<obj>)
   (state: State)
   =
   logInfo
      mailbox
      $"Start {conf.EventJournalTag} projection at offset {state.Offset.Value}."

   let system = mailbox.System

   let readJournalSource =
      initReadJournalSource
         mailbox
         state
         conf.Chunking
         conf.RestartSettings
         conf.EventJournalTag

   let flow =
      Flow.id<'TEvent list * Query.Sequence option, 'TEvent list>
      |> Flow.map (fun (events, offsetOpt) ->
         let offset =
            offsetOpt
            |> Option.map (fun o -> string o.Value)
            |> Option.defaultValue ""

         logInfo
            mailbox
            $"Will sync read models for {events.Length} events from journal {conf.EventJournalTag} from offset {offset}"

         events, offsetOpt)
      |> Flow.via (
         bulkWriteFlow mailbox conf.EventJournalTag conf.UpsertReadModels
      )

   let sink = initSink mailbox conf.EventJournalTag

   readJournalSource
   |> Source.via flow
   |> Source.runWith (system.Materializer()) sink

let startProjectionWithAggregateLookup<'TAggregate, 'TEvent>
   (conf: Config<'TAggregate, 'TEvent>)
   (mailbox: Eventsourced<obj>)
   (state: State)
   =
   logInfo
      mailbox
      $"Start {conf.EventJournalTag} projection at offset {state.Offset.Value}."

   let system = mailbox.System

   let readJournalSource =
      initReadJournalSource
         mailbox
         state
         conf.Chunking
         conf.RestartSettings
         conf.EventJournalTag

   let getAggregate ((aggId, evts): Guid * 'TEvent list) = task {
      let! res = conf.GetAggregate aggId
      return res |> Option.map (fun agg -> agg, evts)
   }

   let persist =
      List.fold
         (fun (aggAcc, eventsAcc) grouping ->
            let agg, aggEvents = grouping

            agg :: aggAcc, List.append aggEvents eventsAcc)
         ([], [])
      >> conf.UpsertReadModels

   let flow =
      Flow.id<'TEvent list * Query.Sequence option, 'TAggregate * 'TEvent list>
      |> Flow.asyncMap conf.Chunking.Size (fun (events, offsetOpt) -> async {
         let distinctAggregateIds =
            events
            |> List.fold
                  (fun acc evt ->
                     let aggId = conf.GetAggregateIdFromEvent evt

                     match Map.tryFind aggId acc with
                     | None -> Map.add aggId [ evt ] acc
                     | Some _ ->
                        Map.change
                           aggId
                           (Option.map (fun evts -> evt :: evts))
                           acc)
                  Map.empty<Guid, 'TEvent list>
            |> Map.toArray
            |> Array.map getAggregate

         let! res = Task.WhenAll distinctAggregateIds |> Async.AwaitTask
         let aggAndEvents = res |> Array.toList |> List.choose id
         return aggAndEvents, offsetOpt
      })
      |> Flow.via (bulkWriteFlow mailbox conf.EventJournalTag persist)

   let sink = initSink mailbox conf.EventJournalTag

   readJournalSource
   |> Source.via flow
   |> Source.runWith (system.Materializer()) sink

[<RequireQualifiedAccess>]
type ReadModelSyncConfig<'TAggregate, 'TEvent> =
   | DefaultMode of Config<'TEvent>
   | AggregateLookupMode of Config<'TAggregate, 'TEvent>

   member x.EventJournalTag =
      match x with
      | DefaultMode conf -> conf.EventJournalTag
      | AggregateLookupMode conf -> conf.EventJournalTag

let actorProps<'TAggregate, 'TEvent>
   (conf: ReadModelSyncConfig<'TAggregate, 'TEvent>)
   =
   let name = $"ReadModelSync-{conf.EventJournalTag}"

   let handler (mailbox: Eventsourced<obj>) =
      let logInfo = logInfo mailbox

      let rec loop (state: State) = actor {
         let! msg = mailbox.Receive()

         match box msg with
         | :? SnapshotOffer as o -> return! loop <| unbox o.Snapshot
         | :? Message as msg ->
            match msg with
            | SaveOffset offset ->
               logInfo $"Saving offset for {name}: {offset.Value}"

               let newState = { Offset = offset }

               match offset.Value % 10L = 0 with
               | true -> return! loop newState <@> SaveSnapshot newState
               | false -> return! loop newState
         | msg ->
            return
               PersistentActorEventHandler.handleEvent
                  {
                     PersistentActorEventHandler.init with
                        RecoveryCompleted =
                           fun mailbox ->
                              match conf with
                              | ReadModelSyncConfig.DefaultMode conf ->
                                 startProjection conf mailbox state |> ignored
                              | ReadModelSyncConfig.AggregateLookupMode conf ->
                                 startProjectionWithAggregateLookup
                                    conf
                                    mailbox
                                    state
                                 |> ignored
                        LifecyclePreRestart =
                           fun _ err msg ->
                              logInfo $"<PreRestart> {name} - {err} - {msg}"
                              ignored ()
                        LifecyclePreStart =
                           fun _ ->
                              logInfo $"<PreStart> {name}"
                              ignored ()
                        LifecyclePostStop =
                           fun _ ->
                              logInfo $"<PostStop> {name}"
                              SaveSnapshot state
                  }
                  mailbox
                  msg
      }

      loop { Offset = Query.Sequence 0L }

   propsPersist handler
