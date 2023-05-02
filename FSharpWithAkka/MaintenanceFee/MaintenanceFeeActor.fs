[<RequireQualifiedAccess>]
module MaintenanceFeeActor

open System
open System.Threading.Tasks
open Microsoft.FSharp.Core.Option
open Akkling

open Lib.Types
open BankTypes
open Bank.Account.Domain
open MaintenanceFee

let private canIssueMaintenanceFee
   (getAccountEvents: Guid -> AccountEvent List Option Task)
   lookBackDate
   accountId
   =
   let eventsOpt = getAccountEvents(accountId).Result

   eventsOpt
   |> Option.bind (fun events ->
      let res = computeFeeCriteria lookBackDate events

      if res.depositCriteria || res.balanceCriteria then
         printfn
            "Some criteria met for skipping the monthly maintenance fee: Deposit (%A) / Balance (%A)"
            res.depositCriteria
            res.balanceCriteria

         None
      else
         Some res)


let start
   (getAccountEvents: Guid -> AccountEvent List Option Task)
   (lookBackDate: _ -> DateTime)
   (scheduledAt: _ -> TimeSpan)
   (mailbox: Actor<_>)
   (accountId: Guid)
   =
   let handler (ctx: Actor<Guid>) id =
      let criteriaMet =
         canIssueMaintenanceFee getAccountEvents (lookBackDate ()) id

      if isSome criteriaMet then
         ctx.Parent()
         <! (DebitCommand(
                accountId,
                DateTime.UtcNow,
                Constants.Fee,
                Account.Constants.DebitOriginMaintenanceFee,
                "Monthly Maintenance Fee"
             )
             |> ActorStateChangeCommand.init)

      ignored ()

   let aref = spawn mailbox "monthly_maintenance_fee" (props (actorOf2 handler))

   mailbox.System.Scheduler.ScheduleTellRepeatedly(
      TimeSpan.FromMilliseconds 0,
      scheduledAt (),
      aref,
      accountId
   )

   aref
