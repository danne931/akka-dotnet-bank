[<RequireQualifiedAccess>]
module BillingCycleActor

open Akka.Actor
open Akka.Hosting
open Akka.Streams
open Akkling.Streams
open Akkling
open System

open BillingStatement
open Bank.Account.Domain
open PartnerBankSqlMapper
open ActorUtil
open Lib.Types
open Lib.Postgres
open Lib.SharedTypes

let getBillingCycleReadyAccounts () =
   let prevCycle = Fields.lastBillingCycleDate

   let lookback =
      if Env.isProd then
         "'27 days'::interval"
      else
         "'1 minutes'::interval"

   pgQuery<ParentAccountId * OrgId>
      $"""
      SELECT {Fields.parentAccountId}, {Fields.orgId}
      FROM {PartnerBankSqlMapper.table}
      WHERE
         {Fields.status} = '{string ParentAccountStatus.Active}'
         AND (
            {prevCycle} IS NULL
            OR {prevCycle} < current_timestamp - {lookback}
         )
      """
      None
   <| fun read -> SqlReader.parentAccountId read, SqlReader.orgId read

let private fanOutBillingCycleMessage
   (ctx: Actor<_>)
   (throttle: StreamThrottle)
   (getSagaRef: unit -> IActorRef<AppSaga.AppSagaMessage>)
   =
   task {
      let mat = ctx.System.Materializer()

      do!
         getBillingCycleReadyAccounts ()
         |> Async.AwaitTask
         |> Source.ofAsync
         |> Source.throttle
               ThrottleMode.Shaping
               throttle.Burst
               throttle.Count
               throttle.Duration
         |> Source.choose (fun res ->
            match res with
            | Error e ->
               logError ctx $"Error fetching active account ids {e}"
               None
            | Ok opt ->
               if opt.IsNone then
                  logError ctx "No active accounts."

               opt)
         |> Source.collect id
         |> Source.runForEach mat (fun (parentAccountId, orgId) ->
            // This billing cycle actor is scheduled to run at the start of
            // every month.  The billing period refers to the previous month.
            let billingPeriod = DateTime.UtcNow.AddMonths -1

            let corrId = CorrelationId.create ()

            let msg =
               AppSaga.Message.billingStart orgId corrId {
                  BillingCycleDate = DateTime.UtcNow
                  BillingPeriod = {
                     Month = billingPeriod.Month
                     Year = billingPeriod.Year
                  }
                  ParentAccountId = parentAccountId
                  OrgId = orgId
                  CorrelationId = corrId
               }

            getSagaRef () <! msg)

      return BillingCycleMessage.BillingCycleFinished
   }

let actorProps
   (throttle: StreamThrottle)
   (getSagaRef: unit -> IActorRef<AppSaga.AppSagaMessage>)
   =
   let handler (ctx: Actor<BillingCycleMessage>) =
      function
      | BillingCycleMessage.BillingCycleFanout ->
         logInfo ctx "Start billing cycle"

         fanOutBillingCycleMessage ctx throttle getSagaRef |> Async.AwaitTask
         |!> retype ctx.Self
         |> ignored
      | BillingCycleMessage.BillingCycleFinished ->
         logInfo ctx "Billing cycle finished"
         ignored ()

   props <| actorOf2 handler

let get (system: ActorSystem) : IActorRef<BillingCycleMessage> =
   typed <| ActorRegistry.For(system).Get<ActorMetadata.BillingCycleMarker>()
