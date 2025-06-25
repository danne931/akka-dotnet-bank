[<RequireQualifiedAccess>]
module ScheduledTransfersLowBalanceWarningActor

open System
open Akka.Actor
open Akka.Hosting
open Akka.Streams
open Akkling.Streams
open Akkling
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Email
open ActorUtil
open Lib.Postgres
open AccountSqlMapper
open TransferSqlMapper
open TransferMessages

type private InsufficientBalance =
   OrgId * ScheduledTransferInsufficientBalanceWarning

// Send email warnings to organizations who have scheduled transfers whose
// associated accounts do not have enough available balance to cover the
// amount due within 3 days.
let actorProps
   (system: ActorSystem)
   (getEmailRef: unit -> IActorRef<EmailMessage>)
   (getInsufficientBalanceWarnings:
      unit -> Async<Result<InsufficientBalance list option, Err>>)
   =
   let mat = system.Materializer()

   let handler (ctx: Actor<ScheduledTransfersLowBalanceMessage>) = actor {
      let! _ = ctx.Receive()

      logInfo
         ctx
         $"Getting accounts with insufficient balance for covering scheduled transfers."

      do!
         getInsufficientBalanceWarnings ()
         |> Source.ofAsync
         |> Source.choose (fun res ->
            match res with
            | Error e ->
               logError
                  ctx
                  $"Error fetching accounts with insufficient balance {e}"

               None
            | Ok opt ->
               if opt.IsNone then
                  logInfo
                     ctx
                     $"No accounts with insufficient balance for covering scheduled transfers."

               opt)
         |> Source.collect id
         |> Source.runForEach mat (fun (orgId, emailInfo) ->
            let msg =
               EmailMessage.create
                  orgId
                  (System.Guid.NewGuid() |> CorrelationId)
                  (EmailInfo.ScheduledTransferInsufficientBalanceWarning
                     emailInfo)

            getEmailRef () <! msg)

      logInfo ctx $"Finished running scheduled transfer low balance check."

      return ignored ()
   }

   props handler

let get (system: ActorSystem) : IActorRef<ScheduledTransfersLowBalanceMessage> =
   typed
   <| ActorRegistry
      .For(system)
      .Get<ActorMetadata.ScheduledTransfersLowBalanceWarningMarker>()

// Get scheduled transfers in the next 3 days where the total amount of those
// transfers is greater than the available balance in an associated account.
let getAccountsWithInsufficientBalanceToCoverScheduledTransfers () = asyncResultOption {
   let availableBalance =
      $"a.{AccountFields.balance} - a.{AccountFields.pendingDeductionsMoney}"

   let transferAmount = $"t.{TransferFields.amount}"
   let scheduledAt = $"t.{TransferFields.scheduledAt}"

   let query =
      $"""
      SELECT
         a.{AccountFields.orgId},
         a.{AccountFields.accountId},
         a.{AccountFields.name} as sender_account_name,
         {availableBalance} as available_balance,
         COUNT(*) as scheduled_transfers_count,
         SUM({transferAmount}) as scheduled_transfers_amount,
         MIN({scheduledAt}) as imminent_scheduled_at
      FROM {TransferSqlMapper.Table.transfer} t
      LEFT JOIN {TransferSqlMapper.Table.domesticTransfer} td USING(transfer_id)
      LEFT JOIN {TransferSqlMapper.Table.internalTransferBetweenOrgs} tibo USING(transfer_id)
      JOIN {AccountSqlMapper.table} a ON a.{AccountFields.accountId} = t.{TransferFields.senderAccountId}
      WHERE
         (
            td.{TransferFields.Domestic.status} = 'Scheduled'
            OR tibo.{TransferFields.InternalBetweenOrgs.status} = 'Scheduled'
         )
         AND {scheduledAt} BETWEEN NOW() AND (NOW() + INTERVAL '3 days')
      GROUP BY a.{AccountFields.accountId}
      HAVING {availableBalance} < SUM({transferAmount});
      """

   let! info =
      pgQuery<OrgId * ScheduledTransferInsufficientBalanceWarning>
         query
         None
         (fun read ->
            OrgId(read.uuid "org_id"),
            {
               SenderAccountName = read.string "sender_account_name"
               AvailableBalance = read.decimal "available_balance"
               ScheduledTransfersCount = read.int "scheduled_transfers_count"
               ScheduledTransfersAmount =
                  read.decimal "scheduled_transfers_amount"
               ImminentScheduledTransferDate =
                  read.dateTime "imminent_scheduled_at"
            })

   return info
}

let initProps
   (system: ActorSystem)
   (getEmailRef: unit -> IActorRef<EmailMessage>)
   =
   actorProps
      system
      getEmailRef
      getAccountsWithInsufficientBalanceToCoverScheduledTransfers
