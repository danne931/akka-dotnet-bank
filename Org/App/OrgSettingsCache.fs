module OrgSettingsCache

open Akka.Actor
open Akka.Cluster
open Akka.DistributedData
open Akkling.DistributedData
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open CachedOrgSettings

let private getKey (orgId: OrgId) =
   LWWReg.key<CachedOrgSettings>
      $"{ActorUtil.CRDT.PersistableKey}-org-settings-{orgId}"

let get
   (system: ActorSystem)
   (orgId: OrgId)
   : Async<Result<CachedOrgSettings option, Err>>
   =
   let ddata = DistributedData.Get system

   let key = getKey orgId

   ddata.AsyncGet(key, Consistency.readLocal)
   |> AsyncResult.ofAsync
   |> AsyncResult.catch Err.CacheError
   |> AsyncResult.map (Option.map _.Value)

let update
   (system: ActorSystem)
   (orgId: OrgId)
   (settings: CachedOrgSettings)
   : Async<Result<unit, Err>>
   =
   let cluster = Cluster.Get system
   let ddata = DistributedData.Get system

   let key = getKey orgId
   let settings = LWWReg.create cluster settings

   ddata.AsyncUpdate(key, settings, Consistency.writeLocal)
   |> AsyncResult.ofAsync
   |> AsyncResult.catch Err.CacheError
