module Util

open Expecto
open Akkling
open Akkling.Cluster.Sharding

open Lib.SharedTypes

let akkaTest
   (testDescription: string)
   (config: Akka.Configuration.Config option)
   (testBody: TestKit.Tck -> unit)
   =
   let akkaTester =
      match config with
      | Some config -> TestKit.test config
      | None -> TestKit.testDefault

   testCase testDescription <| fun () -> akkaTester <| testBody

let getOrgEntityRef (actorRef: IActorRef<_>) (orgId: OrgId) =
   let shardRegionName = "mock-org-shard"

   let fac = {
      TypeName = shardRegionName
      ShardRegion = untyped actorRef
   }

   let (OrgId id) = orgId
   fac.RefFor shardRegionName <| string id

let getAccountEntityRef (actorRef: IActorRef<_>) (accountId: AccountId) =
   let shardRegionName = "mock-account-shard"

   let fac = {
      TypeName = shardRegionName
      ShardRegion = untyped actorRef
   }

   let (AccountId id) = accountId
   fac.RefFor shardRegionName <| string id

let getEmployeeEntityRef (actorRef: IActorRef<_>) (employeeId: EmployeeId) =
   let shardRegionName = "mock-employee-shard"

   let fac = {
      TypeName = shardRegionName
      ShardRegion = untyped actorRef
   }

   let (EmployeeId id) = employeeId
   fac.RefFor shardRegionName <| string id
