module ActorUtil

open System
open Akkling
open Akkling.Cluster.Sharding
open Akka.Actor
open Akka.Cluster.Sharding
open Akka.Persistence.Query
open Akka.Persistence.Sql.Query

let getChildActorRef<'t, 'r>
   (actorCtx: Actor<'t>)
   (name: string)
   : IActorRef<'r> option
   =
   let accountRef = actorCtx.UntypedContext.Child(name)

   match accountRef = ActorRefs.Nobody with
   | true -> None
   | false -> Some(typed accountRef)

type EntityRefGetter<'t> = Guid -> IEntityRef<'t>

module ActorMetadata =
   type ActorMetadata = { Name: string; Path: ActorPath option }

   type AccountShardRegionMarker() =
      class
      end

   type EmailMarker() =
      class
      end

   type AccountClosureMarker() =
      class
      end

   type DomesticTransferMarker() =
      class
      end

   type BillingCycleBulkWriteMarker() =
      class
      end

   let account = { Name = "account"; Path = None }

   let accountClosure = {
      Name = "account_closure"
      Path = Some(ActorPath.Parse "akka://bank/user/account_closure")
   }

   let internalTransfer = {
      Name = "internal_transfer_recipient"
      Path = None
   }

   let domesticTransfer = {
      Name = "domestic_transfer_recipient"
      Path = None
   }

   let billingCycle = { Name = "billing_cycle"; Path = None }

   let billingCycleBulkWrite = {
      Name = "billing_cycle_bulk_write"
      Path = None
   }

   let email = { Name = "email"; Path = None }

   let deadLettersMonitor = {
      Name = "dead_letters_monitor"
      Path = None
   }

let readJournal (system: ActorSystem) : SqlReadJournal =
   PersistenceQuery
      .Get(system)
      .ReadJournalFor<SqlReadJournal>(SqlReadJournal.Identifier)
(*
type private MessageExtractor() =
   inherit HashCodeMessageExtractor(maxNumberOfShards = 1000)

   override x.EntityId(msg: obj) =
      match msg with
      | :? ShardRegion.StartEntity as e ->
         printfn "entityID: START ENTITY %A" e
         e.EntityId
      | :? ShardEnvelope as e ->
         printfn "entityID: ENVELOPE %A" e.EntityId
         e.EntityId
      | _ -> failwith "MessageExtractor.EntityId: Unknown message"

   override x.EntityMessage(msg: obj) = (msg :?> ShardEnvelope).Message
   override x.ShardId(msg: obj) =
      let hash = Akka.Util.MurmurHash.StringHash (x.EntityId msg)
      let r = Math.Abs(hash) % x.MaxNumberOfShards |> string
      printfn "--Shard id:-- %A" r
      r
*)
(*
let messageExtractor () =
   HashCodeMessageExtractor.Create(
      maxNumberOfShards = 1000,
      entityIdExtractor = (fun msg -> 
         match msg with
         | :? ShardRegion.StartEntity as e ->
            printfn "entityID: START ENTITY %A" e
            e.EntityId
         | :? ShardEnvelope as e ->
            printfn "entityID: ENVELOPE %A" e.EntityId
            e.EntityId
         | _ -> failwith "MessageExtractor.EntityId: Unknown message"),
      messageExtractor = fun msg -> (msg :?> ShardEnvelope).Message
   )
*)

// TODO: Remove
let private ShardId = "shardid"

type MessageExtractor() =
   interface IMessageExtractor with
      member x.EntityId(msg: obj) =
         match msg with
         | :? ShardRegion.StartEntity as _ -> ShardId
         | :? ShardEnvelope as e -> e.EntityId
         | _ -> null

      member x.ShardId(msg: obj) =
         match msg with
         | :? ShardRegion.StartEntity as _ -> ShardId
         | :? ShardEnvelope as e -> e.ShardId
         | _ -> null

      member x.EntityMessage(msg: obj) = (msg :?> ShardEnvelope).Message

module AkklingExt =
   let getEntityRef system shardRegionName (entityId: Guid) : IEntityRef<'t> =
      let fac = {
         TypeName = shardRegionName
         ShardRegion = ClusterSharding.Get(system).ShardRegion(shardRegionName)
      }

      fac.RefFor ShardId <| string entityId
