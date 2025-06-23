[<RequireQualifiedAccess>]
module AutomaticTransferSchedulingActor

open Akka.Actor
open Akka.Hosting
open Akka.Streams
open Akkling.Streams
open Akkling
open Akkling.Cluster.Sharding
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Bank.Account.Domain
open ActorUtil
open Lib.Types
open Lib.Postgres
open AccountSqlMapper
open AutomaticTransfer

let actorProps
   (system: ActorSystem)
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (getOrgIds:
      CronSchedule
         -> Async<Result<(AccountId * ParentAccountId) list option, Err>>)
   (throttle: StreamThrottleEnvConfig)
   =
   let mat = system.Materializer()

   let handler (ctx: Actor<Message>) = actor {
      let! msg = ctx.Receive()

      let schedule =
         match msg with
         | Message.StartScheduledAutoTransfers schedule -> schedule

      logInfo
         ctx
         $"Getting accounts with configured auto transfer rules for schedule {schedule}."

      do!
         getOrgIds schedule
         |> Source.ofAsync
         |> Source.choose (fun res ->
            match res with
            | Error e ->
               logError ctx $"Error fetching accounts for auto transfer {e}"
               None
            | Ok opt ->
               if opt.IsNone then
                  logWarning
                     ctx
                     $"No accounts for auto transfer schedule {schedule}."

               opt)
         |> Source.collect id
         |> Source.throttle
               ThrottleMode.Shaping
               throttle.Burst
               throttle.Count
               throttle.Duration
         |> Source.runForEach mat (fun (accountId, parentAccountId) ->
            let msg =
               AccountMessage.AutoTransferCompute(
                  Frequency.Schedule schedule,
                  accountId
               )

            getAccountRef parentAccountId <! msg)

      logInfo ctx $"Finished running {schedule} balance management."

      return ignored ()
   }

   props handler

let get (system: ActorSystem) : IActorRef<Message> =
   typed
   <| ActorRegistry
      .For(system)
      .Get<ActorMetadata.AutoTransferSchedulingMarker>()

let getAccountsWithScheduledAutoTransfer (schedule: CronSchedule) = asyncResultOption {
   let field = AccountFields.autoTransferRuleFrequency

   let query =
      $"""
      SELECT {AccountFields.accountId}, {AccountFields.parentAccountId}
      FROM {AccountSqlMapper.table}
      WHERE {field} = @frequency::{AccountTypeCast.autoTransferRuleFrequency}
      """

   let frequency =
      Frequency.Schedule schedule
      |> Some
      |> AccountSqlWriter.autoTransferRuleFrequency

   let! ids =
      pgQuery<AccountId * ParentAccountId>
         query
         (Some [ "frequency", frequency ])
         (fun read ->
            AccountSqlReader.accountId read,
            AccountSqlReader.parentAccountId read)

   return ids
}

let initProps
   (system: ActorSystem)
   (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>)
   (throttle: StreamThrottleEnvConfig)
   =
   actorProps system getAccountRef getAccountsWithScheduledAutoTransfer throttle
